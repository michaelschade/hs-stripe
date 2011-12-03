module Web.Stripe.Utils
    ( jGet
    , mjGet
    ) where

import Text.JSON ( Result(..), JSObject, JSON(..), JSValue(..), resultToEither
                 , valFromObj
                 )

-- | Convenience function to get a field from a given 'JSON' object.
jGet :: JSON a => JSObject JSValue -> String -> Result a
jGet  = flip valFromObj

-- | Attempts to retrieve a field from a given 'JSON' object, failing
--   gracefully with 'Nothing' if such a field is not found.
mjGet :: JSON a => JSObject JSValue -> String -> Result (Maybe a)
mjGet obj = return . either (\_ -> Nothing) Just . resultToEither . jGet obj
