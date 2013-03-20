{-# LANGUAGE OverloadedStrings #-}

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
    , StripeConfig(..)
    , StripeT(StripeT)
    , runStripeT
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (liftM, mzero)
import           Control.Monad.Error (MonadIO)
import           Data.Aeson          (FromJSON (..), Value (..), (.:), (.:?))
import qualified Data.ByteString     as B
import qualified Data.Text           as T
import           Network.HTTP.Types  (StdMethod (..))
import           Web.Stripe.Card     (Card, RequestCard, rCardKV)
import           Web.Stripe.Client   (StripeConfig (..), StripeRequest (..),
                                      StripeT (..), baseSReq, query, queryData,
                                      runStripeT)
import           Web.Stripe.Customer (Customer (..), CustomerId (..))
import           Web.Stripe.Token    (Token (..), TokenId (..))
import           Web.Stripe.Utils    (Amount (..), Count (..), Currency (..),
                                      Description (..), Offset (..),
                                      UTCTime (..), fromSeconds, optionalArgs,
                                      showByteString, textToByteString)

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
    , chargeFee         :: Amount
    , chargeLive        :: Bool
    , chargePaid        :: Bool
    , chargeRefunded    :: Bool
    , chargeCard        :: Card
    } deriving Show

-- | Represents the identifier for a given 'Charge' in the Stripe system.
newtype ChargeId = ChargeId { unChargeId :: T.Text } deriving (Show, Eq)

-- | Submit a 'Charge' to the Stripe API using an already constructed 'Token'.
chargeToken :: MonadIO m => Token -> Amount -> Currency
            -> Maybe Description -> Maybe Amount -> StripeT m Charge
chargeToken  = chargeTokenById . tokId

-- | Submit a 'Charge' to the Stripe API using a 'TokenId'.
chargeTokenById :: MonadIO m => TokenId -> Amount -> Currency
                -> Maybe Description -> Maybe Amount -> StripeT m Charge
chargeTokenById (TokenId tid) = charge [("card", textToByteString tid)]

-- | Submit a 'Charge' to the Stripe for a specific 'Customer' that already has
--   payment details on file.
chargeCustomer :: MonadIO m => Customer -> Amount -> Currency
               -> Maybe Description -> Maybe Amount -> StripeT m Charge
chargeCustomer  = chargeCustomerById . custId

-- | Submit a 'Charge' to the Stripe for a specific 'Customer', identified by
--   its 'CustomerId', that already has payment details on file.
chargeCustomerById :: MonadIO m => CustomerId -> Amount -> Currency
                   -> Maybe Description -> Maybe Amount -> StripeT m Charge
chargeCustomerById (CustomerId cid) = charge [("customer", textToByteString cid)]

-- | Submit a 'Charge' to the Stripe API using a 'RequestCard' to describe
--   payment details.
chargeRCard :: MonadIO m => RequestCard -> Amount -> Currency
            -> Maybe Description -> Maybe Amount -> StripeT m Charge
chargeRCard rc = charge (rCardKV rc)

-- | Internal convenience function to handle actually submitting a 'Charge'
--   request to the Stripe API.
charge :: MonadIO m => [(B.ByteString, B.ByteString)] -> Amount -> Currency
       -> Maybe Description -> Maybe Amount -> StripeT m Charge
charge adata a c mcd maf =
    snd `liftM` query (chargeRq []) { sMethod = POST, sData = fdata }
    where
        fdata = optionalArgs odata ++ adata ++ bdata
        odata = [ ("description", textToByteString . unDescription <$> mcd)
                , ("application_fee", showByteString . unAmount <$> maf)
                ]
        bdata = [ ("amount",      showByteString . unAmount $ a)
                , ("currency",    textToByteString $ unCurrency c)
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
getCharges mcid mc mo = liftM snd $
                        queryData ((chargeRq []) { sQString = optionalArgs oqs })
  where
    oqs   = [ ("count",     show . unCount  <$> mc)
            , ("offset",    show . unOffset <$> mo)
            , ("customer",  T.unpack . unCustomerId    <$> mcid)
            ]
        -- err   = throwError $ strMsg "Unable to parse charge list."

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
    where fd = optionalArgs [("amount", showByteString . unAmount <$> ma)]

-- | Convenience function to create a 'StripeRequest' specific to coupon-related
--   actions.
chargeRq :: [T.Text] -> StripeRequest
chargeRq pcs = baseSReq { sDestination = "charges":pcs }

------------------
-- JSON Parsing --
------------------

-- | Attempts to parse JSON into a 'Charge'.
instance FromJSON Charge where
    parseJSON (Object v) = Charge
        <$> (ChargeId <$> v .: "id")
        <*> (fromSeconds       <$> v .: "created")
        <*> ((Description <$>) <$> v .:? "description")
        <*> (Currency          <$> v .: "currency")
        <*> (Amount            <$> v .: "amount")
        <*> (Amount            <$> v .: "fee")
        <*> v .: "livemode"
        <*> v .: "paid"
        <*> v .: "refunded"
        <*> v .: "card"
    parseJSON _ = mzero
