module Web.Stripe.Charge
    ( Charge(..)
    , ChargeId(..)
    , chargeToken
    , chargeTokenById
    , chargeCustomer
    , chargeCustomerById
    , chargeRCard
    , getCharge
    , getCharges
    , partialRefund
    , partialRefundById
    , fullRefund
    , fullRefundById

    {- Re-Export -}
    , Amount(..)
    , Count(..)
    , Currency(..)
    , Description(..)
    , Offset(..)
    , UTCTime(..)
    , SConfig(..)
    , StripeT(StripeT)
    , runStripeT
    ) where

import Control.Applicative  ( (<$>) )
import Control.Monad        ( liftM, ap )
import Control.Monad.Error  ( MonadIO, throwError, strMsg )
import Network.HTTP.Types   ( StdMethod(..) )
import Text.JSON            ( Result(Error), JSON(..), JSValue(JSObject)
                            , resultToEither, valFromObj
                            )
import Web.Stripe.Card      ( Card, RequestCard, rCardKV )
import Web.Stripe.Customer  ( Customer(..), CustomerId(..) )
import Web.Stripe.Client    ( StripeT(..), SConfig(..), SRequest(..), baseSReq
                            , query, runStripeT
                            )
import Web.Stripe.Token     ( Token(..), TokenId(..) )
import Web.Stripe.Utils     ( Amount(..), Count(..), Currency(..)
                            , Description(..), Offset(..), UTCTime(..)
                            , fromSeconds, jGet, mjGet, optionalArgs
                            )

----------------
-- Data Types --
----------------

-- | Represents a charge in the Stripe system.
data Charge = Charge
    { chargeId          :: ChargeId
    , chargeCreated     :: UTCTime
    , chargeDescription :: Maybe Description
    , chargeCurrency    :: Currency
    , chargeAmount      :: Amount
    , chargeFee         :: Int
    , chargeLive        :: Bool
    , chargePaid        :: Bool
    , chargeRefunded    :: Bool
    , chargeCard        :: Card
    } deriving Show

-- | Represents the identifier for a given 'Charge' in the Stripe system.
newtype ChargeId = ChargeId { unChargeId :: String } deriving Show

-- | Submit a 'Charge' to the Stripe API using an already constructed 'Token'.
chargeToken :: MonadIO m => Token -> Amount -> Currency
            -> Maybe Description -> StripeT m Charge
chargeToken  = chargeTokenById . tokId

-- | Submit a 'Charge' to the Stripe API using a 'TokenId'.
chargeTokenById :: MonadIO m => TokenId -> Amount -> Currency
                -> Maybe Description -> StripeT m Charge
chargeTokenById (TokenId tid) = charge [("card", tid)]

-- | Submit a 'Charge' to the Stripe for a specific 'Customer' that already has
--   payment details on file.
chargeCustomer :: MonadIO m => Customer -> Amount -> Currency
               -> Maybe Description -> StripeT m Charge
chargeCustomer  = chargeCustomerById . custId

-- | Submit a 'Charge' to the Stripe for a specific 'Customer', identified by
--   its 'CustomerId', that already has payment details on file.
chargeCustomerById :: MonadIO m => CustomerId -> Amount -> Currency
                   -> Maybe Description -> StripeT m Charge
chargeCustomerById (CustomerId cid) = charge [("customer", cid)]

-- | Submit a 'Charge' to the Stripe API using a 'RequestCard' to describe
--   payment details.
chargeRCard :: MonadIO m => RequestCard -> Amount -> Currency
            -> Maybe Description -> StripeT m Charge
chargeRCard rc = charge (rCardKV rc)

-- | Internal convenience function to handle actually submitting a 'Charge'
--   request to the Stripe API.
charge :: MonadIO m => [(String, String)] -> Amount -> Currency
       -> Maybe Description -> StripeT m Charge
charge adata a c mcd =
    snd `liftM` query (chargeRq []) { sMethod = POST, sData = fdata }
    where
        fdata = head (optionalArgs odata) : adata ++ bdata
        odata = [ ("description",   unDescription <$> mcd) ]
        bdata = [ ("amount",        show . unAmount $ a)
                , ("currency",      unCurrency c)
                ]

-- | Retrieve a 'Charge' from the Stripe API, identified by 'ChargeId'.
getCharge :: MonadIO m  => ChargeId -> StripeT m Charge
getCharge (ChargeId cid) = snd `liftM` query (chargeRq [cid])

-- | Retrieve a list of 'Charge's from the Stripe API. The query can optionally
--   be refined to a specific:
--
--      * number of charges, via 'Count',
--      * page of results, via 'Offset', and
--      * 'Customer'.
getCharges :: MonadIO m => Maybe CustomerId -> Maybe Count -> Maybe Offset
           -> StripeT m [Charge]
getCharges mcid mc mo = do
    (_, rsp) <- query $ (chargeRq []) { sQString = optionalArgs oqs }
    either err return . resultToEither . valFromObj "data" $ rsp
    where
        oqs   = [ ("count",     show . unCount  <$> mc)
                , ("offset",    show . unOffset <$> mo)
                , ("customer",  unCustomerId    <$> mcid)
                ]
        err _ = throwError $ strMsg "Unable to parse charge list."

-- | Requests that Stripe issue a partial refund to a specific 'Charge' for a
--   particular 'Amount'.
partialRefund :: MonadIO m => Charge -> Amount -> StripeT m Charge
partialRefund  = partialRefundById . chargeId

-- | Requests that Stripe issue a partial refund to a specific 'Charge',
--   identified by 'ChargeId', for a particular 'Amount'.
partialRefundById :: MonadIO m => ChargeId -> Amount -> StripeT m Charge
partialRefundById cid = refundChargeById cid . Just

-- | Requests that Stripe issue a full refund to a specific 'Charge'.
fullRefund :: MonadIO m => Charge -> StripeT m Charge
fullRefund  = fullRefundById . chargeId

-- | Requests that Stripe issue a full refund to a specific 'Charge',
--   identified by 'ChargeId'.
fullRefundById :: MonadIO m => ChargeId -> StripeT m Charge
fullRefundById cid = refundChargeById cid Nothing

-- | Internal convenience function used to handle submitting a refund request
--   to Stripe.
refundChargeById :: MonadIO m => ChargeId -> Maybe Amount -> StripeT m Charge
refundChargeById (ChargeId cid) ma =
    snd `liftM` query (chargeRq [cid, "refund"]) { sMethod = POST, sData = fd }
    where fd = optionalArgs [("amount", show . unAmount <$> ma)]

-- | Convenience function to create a 'SRequest' specific to coupon-related
--   actions.
chargeRq :: [String] -> SRequest
chargeRq pcs = baseSReq { sDestination = "charges":pcs }

------------------
-- JSON Parsing --
------------------

-- | Attempts to parse JSON into a 'Charge'.
instance JSON Charge where
    readJSON (JSObject c) =
        Charge `liftM` (ChargeId          <$> jGet  c "id")
                  `ap` (fromSeconds       <$> jGet  c "created")
                  `ap` ((Description <$>) <$> mjGet c "description")
                  `ap` (Currency          <$> jGet  c "currency")
                  `ap` (Amount            <$> jGet  c "amount")
                  `ap` jGet  c "fee"
                  `ap` jGet  c "livemode"
                  `ap` jGet  c "paid"
                  `ap` jGet  c "refunded"
                  `ap` jGet  c "card"
    readJSON _ = Error "Unable to read Stripe charge."
    showJSON _ = undefined
