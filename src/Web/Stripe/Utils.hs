module Web.Stripe.Utils
    ( Amount(..)
    , Count(..)
    , Currency(..)
    , Description(..)
    , Offset(..)
    , optionalArgs
    {- Re-Export -}
    , UTCTime(..)
    , fromSeconds
    , toSeconds
    , stringToByteString
    , textToByteString
    , showByteString
    ) where

import qualified Codec.Binary.UTF8.String as CodecUtf8
import           Control.Monad            (liftM)
import qualified Data.ByteString          as B
import           Data.Maybe               (mapMaybe)
import qualified Data.Text                as T
import           Data.Time.Clock          (UTCTime (..))
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime,
                                           utcTimeToPOSIXSeconds)
import           Data.Time.Format         ()

showByteString :: Show a => a -> B.ByteString
showByteString = stringToByteString . show

textToByteString :: T.Text -> B.ByteString
textToByteString = B.pack . CodecUtf8.encode . T.unpack

stringToByteString :: String -> B.ByteString
stringToByteString = B.pack . CodecUtf8.encode

-----------------------
-- Common Data Types --
-----------------------

-- | Represents an amount in cents in the Stripe system.
newtype Amount = Amount { unAmount :: Int } deriving Show

-- | A maximum number of objects that the Stripe API will return. This value
--   should be between 1 and 100, inclusive.
newtype Count = Count { unCount :: Int } deriving Show

-- | Represents a currency (e.g., "usd") in the Stripe system. This is
--   a 3-letter ISO code.
newtype Currency = Currency { unCurrency :: T.Text } deriving Show

-- | Describes an object in the Stripe system.
newtype Description = Description { unDescription :: T.Text } deriving Show

-- | A positive integer that is an offset into the array of objects returned
--   by the Stripe API.
newtype Offset = Offset { unOffset :: Int } deriving Show

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
