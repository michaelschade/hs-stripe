module Web.Stripe.Card
    ( Card(..)
    , RequestCard(..)
    , rCardKV
    ) where

import Control.Monad    ( liftM, ap )
import Text.JSON        ( Result(Error), JSON(..), JSValue(JSObject) )
import Web.Stripe.Utils ( jGet, optionalArgs )

-- | Represents a credit card in the Stripe system.
data Card = Card
    { cardType      :: String
    , cardCountry   :: String
    , cardLastFour  :: String
    , cardExpMonth  :: Int
    , cardExpYear   :: Int
    } deriving Show

-- | Represents a credit car (with full details) that is used as input to the
--   Stripe API.
data RequestCard = RequestCard
    { rCardNumber       :: String
    , rCardExpMonth     :: Int
    , rCardExpYear      :: Int
    , rCardCVC          :: Maybe String -- ^ Highly recommended to supply
    , rCardFullName     :: Maybe String
    , rCardAddrLineOne  :: Maybe String
    , rCardAddrLineTwo  :: Maybe String
    , rCardAddrZip      :: Maybe String
    , rCardAddrState    :: Maybe String
    , rCardAddrCountry  :: Maybe String
    } deriving Show

-- | Turns a 'RequestCard' into a list of key-value pairs that can be submitted
--   to the Stripe API in a query.
rCardKV :: RequestCard -> [(String, String)]
rCardKV rc = fd ++ optionalArgs md
    where
        -- Required
        fd = [ ("card[number]",          rCardNumber rc)
             , ("card[exp_month]",       show $ rCardExpMonth rc)
             , ("card[exp_year]",        show $ rCardExpYear  rc)
             ]
        -- Optional
        md = [ ("card[cvc]",             rCardCVC           rc)
             , ("card[name]",            rCardFullName      rc)
             , ("card[address_line_1]",  rCardAddrLineOne   rc)
             , ("card[address_line_2]",  rCardAddrLineTwo   rc)
             , ("card[address_zip]",     rCardAddrZip       rc)
             , ("card[address_state]",   rCardAddrState     rc)
             , ("card[address_country]", rCardAddrCountry   rc)
             ]

------------------
-- JSON Parsing --
------------------

-- | Attempts to parse JSON into a credit 'Card'.
instance JSON Card where
    readJSON (JSObject c) =
        Card `liftM` jGet c "type"
                `ap` jGet c "country"
                `ap` jGet c "last4"
                `ap` jGet c "exp_month"
                `ap` jGet c "exp_year"
    readJSON _ = Error "Unable to read Stripe credit card."
    showJSON _ = undefined
