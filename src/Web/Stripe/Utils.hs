module Web.Stripe.Utils
    ( Amount(..)
    , Currency(..)
    , optionalArgs
    , jGet
    , mjGet
    ) where

import Text.JSON ( Result(..), JSObject, JSON(..), JSValue(..), resultToEither
                 , valFromObj
                 )

-----------------------
-- Common Data Types --
-----------------------

-- | Represents an amount in cents in the Stripe system.
newtype Amount = Amount { unAmount :: Int } deriving Show

-- | Represents a currency (e.g., "usd") in the Stripe system. This is
--   a 3-letter ISO code.
newtype Currency = Currency { unCurrency :: String } deriving Show

-----------------------
-- Utility Functions --
-----------------------

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

-- | Convenience function to get a field from a given 'JSON' object.
jGet :: JSON a => JSObject JSValue -> String -> Result a
jGet  = flip valFromObj

-- | Attempts to retrieve a field from a given 'JSON' object, failing
--   gracefully with 'Nothing' if such a field is not found.
mjGet :: JSON a => JSObject JSValue -> String -> Result (Maybe a)
mjGet obj = return . either (\_ -> Nothing) Just . resultToEither . jGet obj
