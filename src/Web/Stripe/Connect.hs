{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Connect
    ( authURL
    , getAccessToken
    , createCustomerToken

    , SecretKey (..)
    , StripeConnectTokens (..)
    , Scope (..)
    , Landing (..)
    , AuthCode
    , AccessToken
    , RefreshToken
    , UserId
    , ClientId
    , URL
    ) where


import           Control.Applicative   ((<$>), (<*>))
import           Control.Exception     (Exception, SomeException (..))
import           Control.Monad         (liftM, mzero)
import           Control.Monad.Error   (MonadIO)
import           Data.Aeson            (FromJSON (..), Value (..), decode, (.:))
import           Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as B
import           Data.Text             (Text, append)
import           Data.Text.Encoding    (encodeUtf8)
import           Data.Typeable         (Typeable)
import           Network.HTTP.Conduit  (Request (..), Response (..), httpLbs,
                                        parseUrl, urlEncodedBody, withManager)
import           Network.HTTP.Types    (Query, Status (..), StdMethod (..),
                                        hAccept, renderQuery)
import           Web.Stripe.Client     (SecretKey (..), StripeRequest (..),
                                        StripeT, query)
import           Web.Stripe.Customer   (CustomerId (..))
import           Web.Stripe.Token      (Token, tokRq)
import           Web.Stripe.Utils      (optionalArgs, textToByteString)


type URL = ByteString
type AccessToken = ByteString
type RefreshToken = ByteString
type UserId = ByteString
type ClientId = ByteString
type AuthCode = ByteString

newtype StripeConnectException = StripeConnectException String deriving (Show, Eq, Typeable)

data Scope = ReadOnly | ReadWrite deriving Eq
data Landing = Login | Register deriving Eq
data StripeConnectTokens = StripeConnectTokens
    { scAccessToken  :: AccessToken
    , scRefreshToken :: RefreshToken
    , scUserId       :: UserId
    } deriving Show


-- URIs ------------------------------------------------------------------------
authURL :: Maybe Scope -> Maybe Text -> Maybe Landing -> ClientId -> URL
authURL mScope mState mLanding clientId =
    B.append "https://connect.stripe.com/oauth/authorize" q
    where q = renderQuery True
              [ ("response_type", Just "code")
              , ("client_id", Just clientId)
              , ("scope", pack . show <$> mScope)
              , ("state", encodeUtf8 <$> mState)
              , ("stripe_landing", pack . show <$> mLanding)
              ]


accessTokenURL :: URL
accessTokenURL = "https://connect.stripe.com/oauth/token"


accessTokenQuery :: Maybe Scope -> AuthCode -> Query
accessTokenQuery mScope code =
    [ ("grant_type", Just "authorization_code")
    , ("scope", pack . show <$> mScope)
    , ("code", Just code)
    ]


{-
refreshTokenQuery :: Maybe Scope -> RefreshToken -> Query
refreshTokenQuery mScope token =
    [ ("grant_type", Just "refresh_token")
    , ("scope", pack . show <$> mScope)
    , ("refresh_token", Just token)
    ]
    -}


-- HTTP ------------------------------------------------------------------------
-- TODO getAccessToken should get the SecretKey from the StripeT monad.
getAccessToken :: SecretKey -> AuthCode -> IO (Maybe StripeConnectTokens)
getAccessToken key code = do
  req <- updateHeaders <$> parseUrl (B.unpack accessTokenURL)
  decode . responseBody <$> (withManager . httpLbs $ urlEncodedBody body req)
      where
        body              = optionalArgs $ accessTokenQuery Nothing code
        headers req       = json : auth : requestHeaders req
        auth              = ("Authorization", encodeUtf8 . append "Bearer " $ unSecretKey key)
        json              = (hAccept, "application/json")
        updateHeaders req =
            req
            { requestHeaders = headers req
            , checkStatus    = statusCodeChecker
            }


statusCodeChecker :: (Show a, Show b) => Status -> a -> b -> Maybe SomeException
statusCodeChecker s@(Status c _) h _
    | 200 <= c && c < 300 = Nothing
    | otherwise = Just . SomeException . StripeConnectException $ show s ++ show h



-- Stripe API ---------------------------------------------------------------------
createCustomerToken :: MonadIO m => CustomerId -> StripeT m Token
createCustomerToken cid =
    snd `liftM` query (tokRq []) { sMethod = POST, sData = fdata }
    where
      fdata = [("customer", textToByteString $ unCustomerId cid)]


-- Instances ----------------------------------------------------------------------
instance Show Scope where
    show ReadOnly  = "read_only"
    show ReadWrite = "read_write"


instance Show Landing where
    show Login    = "login"
    show Register = "register"


instance FromJSON StripeConnectTokens where
    parseJSON (Object o) = StripeConnectTokens
        <$> o .: "access_token"
        <*> o .: "refresh_token"
        <*> o .: "stripe_user_id"
    parseJSON _ = mzero


instance Exception StripeConnectException
