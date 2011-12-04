module Web.Stripe.Token
    ( Token(..)
    , TokenId(..)
    , createToken
    , getToken

    {- Re-Export -}
    , Amount(..)
    , Card(..)
    , Currency(..)
    , SConfig(..)
    , StripeT(..)
    , runStripeT
    ) where

import Control.Applicative  ( (<$>) )
import Control.Monad        ( liftM, ap )
import Control.Monad.Error  ( MonadIO )
import Network.HTTP.Types   ( StdMethod(..) )
import Text.JSON            ( Result(Error), JSON(..), JSValue(JSObject) )
import Web.Stripe.Card      ( Card(..), RequestCard(..), rCardKV )
import Web.Stripe.Client    ( StripeT(..), SConfig(..), SRequest(..), baseSReq
                            , query, runStripeT
                            )
import Web.Stripe.Utils     ( Amount(..), Currency(..), jGet, optionalArgs )

----------------
-- Data Types --
----------------

-- | Represents a token in the Stripe system.
data Token = Token
    { tokId         :: TokenId
    , tokLive       :: Bool
    , tokUsed       :: Bool
    , tokCreated    :: Int
    , tokAmount     :: Amount
    , tokCurrency   :: Currency
    , tokCard       :: Card
    } deriving Show

-- | Represents the identifier for a given 'Token' in the Stripe system.
newtype TokenId = TokenId { unTokenId :: String } deriving Show

-- | Creates a 'Token' in the Stripe system.
createToken :: MonadIO m => RequestCard -> Maybe Amount -> Maybe Currency
                         -> StripeT m Token
createToken rc ma mc =
    snd `liftM` query (tokRq []) { sMethod = POST, sData = fdata }
    where
        fdata   = rCardKV rc ++ optionalArgs mdata
        mdata   = [ ("amount",   show . unAmount <$> ma)
                  , ("currency", unCurrency <$> mc)
                  ]

-- | Retrieves a specific 'Token' based on its 'Token'.
getToken :: MonadIO m => TokenId -> StripeT m Token
getToken (TokenId tid) = return . snd =<< query (tokRq [tid])

-- | Convenience function to create a 'SRequest' specific to tokens.
tokRq :: [String] -> SRequest
tokRq pcs = baseSReq { sDestination = "tokens":pcs }

------------------
-- JSON Parsing --
------------------

-- | Attempts to parse JSON into a 'Token'.
instance JSON Token where
    readJSON (JSObject c) =
        Token `liftM` (return . TokenId  =<< jGet c "id")
                 `ap` jGet c "livemode"
                 `ap` jGet c "used"
                 `ap` jGet c "created"
                 `ap` (return . Amount   =<< jGet c "amount")
                 `ap` (return . Currency =<< jGet c "currency")
                 `ap` jGet c "card"
    readJSON _ = Error "Unable to read Stripe token."
    showJSON _ = undefined
