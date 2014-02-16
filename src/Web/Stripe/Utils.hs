module Web.Stripe.Utils
    ( -- common types
      CustomerId(..)
    , SubscriptionId(..)
    , Amount(..)
    , Count(..)
    , Currency(..)
    , Description(..)
    , Offset(..)
    , optionalArgs
    -- helper functions
    , fromSeconds
    , toSeconds
    , stringToByteString
    , textToByteString
    , showByteString
    , module Export
    {- Re-Export -}
    ) where

import qualified Codec.Binary.UTF8.String as CodecUtf8
import           Control.Monad            (liftM)
import qualified Data.ByteString          as B
import           Data.Maybe               (mapMaybe)
import qualified Data.Text                as T
import           Data.Time.Clock          as Export (UTCTime (..))
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime,
                                           utcTimeToPOSIXSeconds)
import           Data.Time.Format         ()

import           Data.Aeson as Export     (withObject, FromJSON (..), Value (..), (.:), (.:?))
import           Control.Applicative      as Export ((<$>), (<*>))

showByteString :: Show a => a -> B.ByteString
showByteString = stringToByteString . show

textToByteString :: T.Text -> B.ByteString
textToByteString = B.pack . CodecUtf8.encode . T.unpack

stringToByteString :: String -> B.ByteString
stringToByteString = B.pack . CodecUtf8.encode

-----------------------
-- Common Data Types --
-----------------------
--
-- | Represents a 'Customer'\'s ID in the Stripe system.
newtype CustomerId = CustomerId { unCustomerId :: T.Text } deriving (Show, Eq)

newtype SubscriptionId = SubscriptionId { unSubscriptionId :: T.Text }
                         deriving (Show, Eq)

-- | Represents an amount in cents in the Stripe system.
newtype Amount = Amount { unAmount :: Int } deriving (Show, Eq)

-- | A maximum number of objects that the Stripe API will return. This value
--   should be between 1 and 100, inclusive.
newtype Count = Count { unCount :: Int } deriving (Show, Eq)

-- | Represents a currency (e.g., "usd") in the Stripe system. This is
--   a 3-letter ISO code.
newtype Currency = Currency { unCurrency :: T.Text } deriving (Show, Eq)

-- | Describes an object in the Stripe system.
newtype Description = Description { unDescription :: T.Text } deriving (Show, Eq)

-- | A positive integer that is an offset into the array of objects returned
--   by the Stripe API.
newtype Offset = Offset { unOffset :: Int } deriving (Show, Eq)

-----------------------
-- Utility Functions --
-----------------------

-- | Convert a time in seconds (from Stripe's servers) to 'UTCTime'. See
--   "Data.Time.Format" for more on working with 'UTCTime'.
fromSeconds :: Integer -> UTCTime
fromSeconds  = posixSecondsToUTCTime . fromInteger

-- | Convert a 'UTCTime' back to an Integer suitable for use with Stripe's API.
toSeconds :: UTCTime -> Integer
toSeconds  = round . utcTimeToPOSIXSeconds

-- | Takes a list of key-value arguments, where the value is optional, and
--   returns a list of key-value pairs with only the supplied values.
--
--   Essentially, this filters out all 'Nothing's and unwraps the 'Just's.
--
-- >>> optionalArgs [("k1", Just "supplied"), ("k2", Nothing)]
-- [("k1","supplied")]
optionalArgs :: [(a, Maybe b)] -> [(a, b)]
optionalArgs = mapMaybe . uncurry $ liftM . (,)
