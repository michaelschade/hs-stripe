module Web.Stripe.Token
    ( Token(..)
    , TokenId(..)
    , createToken
    , getToken

    {- Re-Export -}
    , UTCTime(..)
    , Amount(..)
    , Card(..)
    , Currency(..)
    , SConfig(..)
    , StripeT(..)
    , runStripeT
    ) where

import Control.Applicative  ( (<$>), (<*>))
import Control.Monad        ( liftM, mzero )
import Control.Monad.Error  ( MonadIO )
import Network.HTTP.Types   ( StdMethod(..) )
import Web.Stripe.Card      ( Card(..), RequestCard(..), rCardKV )
import Web.Stripe.Client    ( StripeT(..), SConfig(..), StripeRequest(..), baseSReq
                            , query, runStripeT
                            )
import Web.Stripe.Utils     ( Amount(..), Currency(..), UTCTime(..), fromSeconds
                            , optionalArgs, showByteString, textToByteString
                            )
import qualified Data.Text              as T
import           Data.Aeson (FromJSON (..), (.:), Value (..))

----------------
-- Data Types --
----------------

-- | Represents a token in the Stripe system.
data Token = Token
    { tokId         :: TokenId
    , tokLive       :: Bool
    , tokUsed       :: Bool
    , tokCreated    :: UTCTime
    , tokAmount     :: Amount
    , tokCurrency   :: Currency
    , tokCard       :: Card
    } deriving Show

-- | Represents the identifier for a given 'Token' in the Stripe system.
newtype TokenId = TokenId { unTokenId :: T.Text } deriving Show

-- | Creates a 'Token' in the Stripe system.
createToken :: MonadIO m => RequestCard -> Maybe Amount -> Maybe Currency
                         -> StripeT m Token
createToken rc ma mc =
    snd `liftM` query (tokRq []) { sMethod = POST, sData = fdata }
    where
        fdata   = rCardKV rc ++ optionalArgs mdata
        mdata   = [ ("amount",   showByteString . unAmount <$> ma)
                  , ("currency", textToByteString . unCurrency <$> mc)
                  ]

-- | Retrieves a specific 'Token' based on its 'Token'.
getToken :: MonadIO m => TokenId -> StripeT m Token
getToken (TokenId tid) = return . snd =<< query (tokRq [tid])

-- | Convenience function to create a 'StripeRequest' specific to tokens.
tokRq :: [T.Text] -> StripeRequest
tokRq pcs = baseSReq { sDestination = "tokens":pcs }

------------------
-- JSON Parsing --
------------------

-- | Attempts to parse JSON into a 'Token'.
instance FromJSON Token where
    parseJSON (Object o) = Token 
      <$> (TokenId      <$> o .: "id")
      <*> o .: "livemode"
      <*> o .: "used"
      <*> (fromSeconds  <$> o .: "created")
      <*> (Amount       <$> o .: "amount")
      <*> (Currency     <$> o .: "currency")
      <*> o .: "card"
    parseJSON _ = mzero
