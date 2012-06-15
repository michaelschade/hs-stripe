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
    ) where

import Data.Time.Clock          ( UTCTime(..) )
import Data.Time.Clock.POSIX    ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds
                                )
import Data.Time.Format         ( ) -- imports Show instance for UTCTime

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
newtype Currency = Currency { unCurrency :: String } deriving Show

-- | Describes an object in the Stripe system.
newtype Description = Description { unDescription :: String } deriving Show

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
optionalArgs :: [(String, Maybe String)] -> [(String, String)]
optionalArgs []                 = []
optionalArgs ((_, Nothing):xs)  = optionalArgs xs
optionalArgs ((a, Just b):xs)   = (a, b):optionalArgs xs
