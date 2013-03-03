{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Connect
    ( authURL
    , getAccessToken

    , APIKey(..)
    , StripeConnectTokens(..)
    , AuthCode
    ) where


import           Control.Applicative   ((<$>), (<*>))
import           Control.Exception     (Exception, SomeException (..))
import           Control.Monad         (mzero)
import           Data.Aeson            (FromJSON (..), Value (..), decode, (.:))
import           Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as B
import           Data.Text             (Text, append)
import           Data.Text.Encoding    (encodeUtf8)
import           Data.Typeable         (Typeable)
import           Network.HTTP.Conduit  (Request (..), Response (..), httpLbs,
                                        parseUrl, urlEncodedBody, withManager)
import           Network.HTTP.Types    (Query, Status (..), hAccept,
                                        renderQuery)
import           Web.Stripe.Client     (APIKey (..))
import           Web.Stripe.Utils      (optionalArgs)


type URL = ByteString
type AccessToken = ByteString
type RefreshToken = ByteString
type UserId = ByteString
type ClientId = ByteString
type AuthCode = ByteString

data StripeConnectException = StripeConnectException String deriving (Show, Eq, Typeable)
data Scope = ReadOnly | ReadWrite deriving Eq
data Landing = Login | Register deriving Eq
data StripeConnectTokens = StripeConnectTokens
    { scAccessToken  :: AccessToken
    , scRefreshToken :: RefreshToken
    , scUserId       :: UserId
    } deriving Show


-- URIs ------------------------------------------------------------------------
authURL :: ClientId -> Maybe Scope -> Maybe Text -> Maybe Landing -> URL
authURL clientId mScope mState mLanding =
    B.append "https://connect.stripe.com/oauth/authorize" query
    where query = renderQuery True
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


refreshTokenQuery :: Maybe Scope -> RefreshToken -> Query
refreshTokenQuery mScope token =
    [ ("grant_type", Just "refresh_token")
    , ("scope", pack . show <$> mScope)
    , ("refresh_token", Just token)
    ]


-- HTTP ------------------------------------------------------------------------
getAccessToken :: APIKey -> AuthCode -> IO (Maybe StripeConnectTokens)
getAccessToken key code = do
  req <- updateHeaders <$> parseUrl (B.unpack accessTokenURL)
  decode . responseBody <$> (withManager . httpLbs $ urlEncodedBody body req)
      where
        body              = optionalArgs $ accessTokenQuery Nothing code
        headers req       = json : auth : requestHeaders req
        auth              = ("Authorization", encodeUtf8 . append "Bearer " $ unAPIKey key)
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
