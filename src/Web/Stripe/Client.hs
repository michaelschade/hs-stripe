{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Stripe.Client
    ( SConfig(..)
    , SResponseCode(..)
    , SFailure(..)
    , SError(..)
    , SErrorCode(..)
    , SRequest(..)
    , Stripe
    , StripeT(StripeT)
    , runStripeT
    , baseSReq
    , query
    , query_

    {- Re-Export -}
    , StdMethod(..)
    ) where

import Control.Monad        ( MonadPlus, liftM )
import Control.Monad.Error  ( Error, ErrorT, MonadIO, MonadError, runErrorT
                            , throwError, strMsg, noMsg
                            )
import Control.Monad.State  ( MonadState, StateT, runStateT, get )
import Control.Monad.Trans  ( liftIO )
import Data.Char            ( toLower )
import Data.List            ( intercalate )
import Data.Text            ( Text )
import Network.Curl         ( CurlOption(..), CurlResponse, CurlResponse_(..)
                            , curlGetResponse_, method_GET, method_HEAD
                            , method_POST
                            )
import Network.HTTP.Types   ( StdMethod(..) )
import Network.URI          ( URI(..), URIAuth(..) )
import Text.JSON            ( Result(..), JSObject, JSON(..), JSValue(..)
                            , decode, resultToEither, toJSObject, valFromObj
                            )
import Web.Stripe.Utils     ( jGet, mjGet )

import qualified Data.Text as T

------------------------
-- General Data Types --
------------------------

-- | Configuration for the 'StripeT' monad transformer.
data SConfig = SConfig
    { key    :: String
    , caFile :: FilePath
    } deriving Show

-- | This represents the possible successes that a connection to the Stripe
--   API can encounter. For specificity, a success can be represented by other
--   error codes, and so the same is true in this data type.
--
--   Please consult the official Stripe REST API documentation on error codes
--   at <https://stripe.com/docs/api#errors> for more information.
data SResponseCode = OK | Unknown Int deriving Show

-- | This represents the possible failures that a connection to the Stripe API
--   can encounter.
--
--   Please consult the official Stripe REST API documentation on error codes
--   at <https://stripe.com/docs/api#errors> for more information.
data SFailure
    = BadRequest            (Maybe SError)
    | Unauthorized          (Maybe SError)
    | NotFound              (Maybe SError)
    | PaymentRequired       (Maybe SError)
    | InternalServerError   (Maybe SError)
    | BadGateway            (Maybe SError)
    | ServiceUnavailable    (Maybe SError)
    | GatewayTimeout        (Maybe SError)
    | OtherFailure          (Maybe Text)
    deriving Show

-- | Describes a 'SFailure' in more detail, categorizing the error and
--   providing additional information about it. At minimum, this is a message,
--   and for 'CardError', this is a message, even more precise code
--   ('SErrorCode'), and potentially a paramter that helps suggest where an
--   error message should be displayed.
--
--   In case the appropriate error could not be determined from the specified
--   type, 'UnkownError' will be returned with the supplied type and message.
--
--   Please consult the official Stripe REST API documentation on error codes
--   at <https://stripe.com/docs/api#errors> for more information.
data SError
    = InvalidRequestError
        { ireMessage :: String }
    | APIError
        { apiMessage :: String }
    | CardError
        { ceMessage  :: String
        , ceCode     :: SErrorCode
        , ceParam    :: Maybe String
        }
    | UnknownError
        { ueType     :: String
        , ueMessage  :: String
        }
    deriving Show

-- | Attempts to describe a 'CardError' in more detail, classifying in what
--   specific way it failed.
--
--   Please consult the official Stripe REST API documentation on error codes
--   at <https://stripe.com/docs/api#errors> for more information.
data SErrorCode
    = InvalidNumber
    | IncorrectNumber
    | InvalidExpiryMonth
    | InvalidExpiryYear
    | InvalidCVC
    | ExpiredCard
    | InvalidAmount
    | IncorrectCVC
    | CardDeclined
    | Missing
    | DuplicateTransaction
    | ProcessingError
    | UnknownErrorCode Text -- ^ Could not be matched; text gives error name.
    deriving Show

-- | Represents a request to the Stripe API, providing the fields necessary to
--   specify a Stripe resource. More generally, 'baseSReq' will be desired as
--   it provides sensible defaults that can be overriden as needed.
data SRequest = SRequest
    { sMethod       :: StdMethod
    , sDestination  :: [String]
    , sData         :: [(String, String)]
    , sBody         :: String
    } deriving Show

------------------
-- Stripe Monad --
------------------

-- | A convenience specialization of the 'StripeT' monad transformer in which
--   the underlying monad is IO.
type Stripe a = StripeT IO a

-- | Defines the monad transformer under which all Stripe REST API resource
--   calls take place.
newtype StripeT m a = StripeT
    { unStripeT :: StateT SConfig (ErrorT SFailure m) a
    } deriving  ( Functor, Monad, MonadIO, MonadPlus
                , MonadError SFailure
                , MonadState SConfig
                )

-- | Runs the 'StripeT' monad transformer with a given 'SConfig'. This will
--   handle all of the authorization dance steps necessary to utilize the
--   Stripe API.
--
--   Its use is demonstrated in other functions, such as 'query'.
runStripeT :: MonadIO m => SConfig -> StripeT m a -> m (Either SFailure a)
runStripeT cfg m =
    runErrorT . liftM fst . (`runStateT` cfg) . unStripeT $ m

--------------
-- Querying --
--------------

-- | The basic 'SRequest' environment upon which all other Stripe API requests
--   will be built. Standard usage involves overriding one or more of the
--   fields. E.g., for a request to \"https://api.stripe.com/v1/coupons\",
--   one would have:
--
-- > baseSReq { sDestinaton = ["charges"] }
baseSReq :: SRequest
baseSReq  = SRequest
    { sMethod       = GET
    , sDestination  = []
    , sData         = []
    , sBody         = ""
    }

-- | Queries the Stripe API. This returns the response body along with the
--   'SResponseCode' undecoded. Use 'query' to try to decode it into a 'JSON'
--   type. E.g.,
--
-- > let conf = SConfig "key" "secret"
-- >
-- > runStripeT conf $
-- >    query' baseSReq { sDestination = ["charges"] }
query' :: MonadIO m => SRequest -> StripeT m (SResponseCode, String)
query' req = do
    cfg  <- get
    let opts' = opts $ caFile cfg
    rsp  <- liftIO (request (show $ prepRq cfg req) opts' :: IO CurlResponse)
    code <- toCode (respStatus rsp) (respBody rsp)
    return (code, respBody rsp)
    where
        opts caf = CurlCAInfo caf : CurlFailOnError False : queryOptions req
        request  = curlGetResponse_

-- | Queries the Stripe API and attempts to parse the results into a data type
--   that is an instance of 'JSON'. This is primarily for internal use by other
--   Stripe submodules, which supply the request values accordingly. However,
--   it can also be used directly. E.g.,
--
-- > let conf = SConfig "key" "CA file"
-- >
-- > runStripeT conf $
-- >    query baseSReq { sDestination = ["charges"] }
query :: (MonadIO m, JSON a) => SRequest -> StripeT m (SResponseCode, a)
query req = query' req >>= \(code, ans) -> (,) code `liftM` decodeJ ans
    where
        decodeJ     = tryEither . resultToEither . decode
        tryEither   = either (throwError . strMsg) return

-- | Acts just like 'query', but on success, throws away the response. Errors
--   contacting the Stripe API will still be reported.
query_ :: MonadIO m => SRequest -> StripeT m ()
query_ req = query' req >> return ()

-- | Determines the appropriate 'CurlOption's for a given 'SRequest'.
--   Presently, this provides a User-Agent string, adds any available HTTP
--   'POST' data, and incorporates the proper HTTP method ('StdMethod').
queryOptions :: SRequest -> [CurlOption]
queryOptions req = CurlUserAgent ua : CurlPostFields dopts : mopts
    where
        ua    = "hs-stripe/0.1 libcurl"
        dopts = map (\(a, b) -> a ++ "=" ++ b) $ sData req -- Data
        mopts = case sMethod req of                        -- HTTP Method
            GET     -> method_GET
            POST    -> method_POST
            HEAD    -> method_HEAD
            PUT     -> [CurlCustomRequest "PUT"]
            DELETE  -> [CurlCustomRequest "DELETE"]
            TRACE   -> [CurlCustomRequest "TRACE"]
            CONNECT -> [CurlCustomRequest "CONNECT"]
            OPTIONS -> [CurlCustomRequest "OPTIONS"]

-- | Transforms a 'SRequest' into a more general 'URI', which can be used to
--   make an authenticated query to the Stripe server.
prepRq :: SConfig -> SRequest -> URI
prepRq cfg rq = uri { uriPath  = intercalate "/" (uriPath uri:sDestination rq) }
    where uri = baseURI (key cfg)

-- | Takes a Stripe API key (see 'SConfig') to produce a authentication-ready
--   URI to be used when querying the server. API. This defines fields with
--   the most sensible defaults, which are then overriden as needed.
baseURI :: String -> URI
baseURI k = URI
    { uriScheme     = "https:"
    , uriAuthority  = Just $ URIAuth (k ++ ":@") "api.stripe.com" ":443"
    , uriPath       = "/v1"
    , uriQuery      = ""
    , uriFragment   = ""
    }

--------------------
-- Error Handling --
--------------------

-- | Given an HTTP status code and the response body as input, this function
--   determines whether or not the status code represents an error as
--   per Stripe\'s REST API documentation. If it does, 'SFailure' is thrown as
--   an error. Otherwise, 'SResponseCode' is returned, representing the status
--   of the request.
--
--   If an error is encountered, this function will attempt to decode the
--   response body with 'errorMsg' to retrieve (and return) an explanation with
--   the 'SFailure'.
toCode :: Monad m => Int -> String -> StripeT m SResponseCode
toCode c body = case c of
    -- Successes
    200 -> return OK
    -- Failures
    400 -> throwError $ BadRequest e
    401 -> throwError $ Unauthorized e
    404 -> throwError $ NotFound e
    402 -> throwError $ PaymentRequired e
    500 -> throwError $ InternalServerError e
    502 -> throwError $ BadGateway e
    503 -> throwError $ ServiceUnavailable e
    504 -> throwError $ GatewayTimeout e
    -- Unknown; assume success
    _   -> return $ Unknown c
    where e = errorMsg body

-- | Converts a 'String'-represented error code into the 'SErrorCode' data
--   type to more descriptively classify errors.
--
--   If the string does not represent a known error code, 'UnknownErrorCode'
--   will be returned with the raw text representing the error code.
toCECode :: String -> SErrorCode
toCECode c = case map toLower c of
    "invalid_number"        -> InvalidNumber
    "incorrect_number"      -> IncorrectNumber
    "invalid_expiry_month"  -> InvalidExpiryMonth
    "invalid_expiry_year"   -> InvalidExpiryYear
    "invalid_cvc"           -> InvalidCVC
    "expired_card"          -> ExpiredCard
    "invalid_amount"        -> InvalidAmount
    "incorrect_cvc"         -> IncorrectCVC
    "card_declined"         -> CardDeclined
    "missing"               -> Missing
    "duplicate_transaction" -> DuplicateTransaction
    "processing_error"      -> ProcessingError
    _                       -> UnknownErrorCode $ T.pack c

-- | This function attempts to decode the contents of a response body as JSON
--   and retrieve an error message in an \"error\" field. E.g.,
--
-- >>> errorMsg "{\"error\":\"Oh no, an error!\"}"
-- Just "Oh no, an error!"
errorMsg :: String -> Maybe SError
errorMsg  =
    either (\_ -> Nothing) Just . resultToEither . valFromObj "error" . toBody

-- | Attempts to decode a response body to a 'JSObject' 'JSValue'. This is used
--   internally by functions such as 'errorMsg' which need to only grab, a
--   single value from a response body, rather than representing it first as a
--   more proper data type.
toBody :: String -> JSObject JSValue
toBody  = either (\_ -> toJSObject []) id . resultToEither . decode

-- | Attempts to parse error information provided with each error by the Stripe
--   API. In the parsing, the error is classified as a specific 'SError' and
--   any useful data, such as a message explaining the error, is extracted
--   accordingly.
instance JSON SError where
    readJSON (JSObject err) = do
        type_ <- jGet err "type"
        msg   <- jGet err "message"
        case map toLower type_ of
            "invalid_request_error" ->
                return $ InvalidRequestError msg
            "api_error"  ->
                return $ APIError msg
            "card_error" -> do
                code  <- jGet  err "code"
                param <- mjGet err "param"
                return $ CardError msg (toCECode code) param
            _ -> return $ UnknownError type_ msg
    readJSON _ = Error "Unable to read Stripe error."
    showJSON _ = undefined

-- | Defines the behavior for more general error messages that can be thrown
--   with 'noMsg' and 'strMsg' in combination with 'throwError'.
instance Error SFailure where
    noMsg  = OtherFailure Nothing
    strMsg = OtherFailure . Just . T.pack
