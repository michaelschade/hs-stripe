{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Subscription
    ( Subscription(..)
    , SubscriptionId(..)
    , SubStatus(..)
    , SubProrate(..)
    , SubTrialEnd(..)
    , SubAtPeriodEnd(..)
    , updateSubRCard
    , updateSubToken
    , updateSub
    , cancelSub

    {- Re-Export -}
    , UTCTime(..)
    , StripeConfig(..)
    , StripeT(StripeT)
    , runStripeT
    ) where

import           Control.Monad       (liftM, mzero)
import           Control.Monad.Error (MonadIO)
import           Data.Char           (toLower)
import           Network.HTTP.Types  (StdMethod (..))
import           Web.Stripe.Card     (RequestCard, rCardKV)
import           Web.Stripe.Client   (StripeConfig (..), StripeRequest (..),
                                      StripeT (..), baseSReq, query, runStripeT)
import           Web.Stripe.Coupon   (CpnId (..))
import           Web.Stripe.Discount (Discount)
import           Web.Stripe.Plan     (Plan, PlanId (..))
import           Web.Stripe.Token    (TokenId (..))
import           Web.Stripe.Utils    (SubscriptionId(..), CustomerId(..), UTCTime (..), fromSeconds, optionalArgs,
                                      showByteString, textToByteString)

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (..), Value (..), (.:), (.:?))
import qualified Data.ByteString     as B
import qualified Data.Text           as T

------------------
-- Subsriptions --
------------------

-- | Represents a subscription in the Stripe API.
data Subscription = Subscription
    { subId          :: SubscriptionId
    , subCustomerId  :: CustomerId
    , subPlan        :: Plan
    , subStatus      :: SubStatus
    , subStart       :: UTCTime
    , subTrialStart  :: Maybe UTCTime
    , subTrialEnd    :: Maybe UTCTime
    , subPeriodStart :: UTCTime -- ^ Current period start
    , subPeriodEnd   :: UTCTime -- ^ Current period end
    , subDiscount    :: Maybe Discount
    } deriving Show

-- | Describes the various stages that a
data SubStatus = Trialing | Active | PastDue | Unpaid | Canceled
               | UnknownStatus T.Text deriving (Show, Eq)

-- | A boolean flag that determines whether or not to prorate switching plans
--   during a billing cycle.
newtype SubProrate = SubProrate { unSubProrate :: Bool } deriving (Show, Eq)

-- | UTC integer timestamp representing the end of the trial period that the
--   customer receives before being charged for the first time.
newtype SubTrialEnd = SubTrialEnd { unSubTrialEnd :: Int } deriving (Show, Eq)

-- | A boolean flag that determines whether or not the cancellation of the
--   'Subscription' should be delayed until the end of the current period.
newtype SubAtPeriodEnd = SubAtPeriodEnd { unSubAtPeriodEnd :: Bool }
    deriving (Show, Eq)

-- | Update the subscription associated with a 'Customer', identified by
--   'CustomerId', in the Stripe system.
--
--   If 'SubTrialEnd' is provided, this will override the default trial period
--   of the plan to which the customer is subscribed.
updateSubRCard :: MonadIO m => RequestCard -> CustomerId -> PlanId
               -> Maybe CpnId -> Maybe SubProrate -> Maybe SubTrialEnd
               -> StripeT m Subscription
updateSubRCard  = updateSub . rCardKV

-- | Behaves precisely like 'updateSubRCard', but uses a 'Token', identified by
--   'TokenId', rather than a 'RequestCard'.
updateSubToken :: MonadIO m => TokenId -> CustomerId -> PlanId -> Maybe CpnId
               -> Maybe SubProrate -> Maybe SubTrialEnd
               -> StripeT m Subscription
updateSubToken (TokenId tid) = updateSub [("token", textToByteString tid)]

-- | Internal convenience function to update a 'Subscription'.
updateSub :: MonadIO m => [(B.ByteString, B.ByteString)] -> CustomerId -> PlanId
          -> Maybe CpnId -> Maybe SubProrate -> Maybe SubTrialEnd
          -> StripeT m Subscription
updateSub sdata cid pid mcpnid mspr mste =
    snd `liftM` query (subRq cid []) { sMethod = POST, sData = fdata }
    where
        fdata = ("plan", textToByteString $ unPlanId pid) : sdata ++ optionalArgs odata
        odata = [ ("coupon",    textToByteString . unCpnId              <$> mcpnid)
                , ("prorate",   showByteString . unSubProrate  <$> mspr)
                , ("trial_end", showByteString . unSubTrialEnd <$> mste)
                ]

-- | Cancels the 'Subscription' associated with a 'Customer', identified by
--   'CustomerId', in the Stripe system.
cancelSub :: MonadIO m => CustomerId -> Maybe SubAtPeriodEnd
          -> StripeT m Subscription
cancelSub cid mspe = snd `liftM`
    query (subRq cid []) { sMethod = DELETE, sData = optionalArgs odata }
    where odata = [("at_period_end", showByteString . unSubAtPeriodEnd <$> mspe)]

-- | Convenience function to create a 'StripeRequest' specific to
--   subscription-related actions.
subRq :: CustomerId -> [T.Text] -> StripeRequest
subRq (CustomerId cid) pcs =
    baseSReq { sDestination = "customers":cid:"subscription":pcs }

------------------
-- JSON Parsing --
------------------

-- | Convert a string to a 'SubStatus'. If the code is not known,
--   'UnkownStatus' will be returned with the originally provided code.
toSubStatus  :: T.Text -> SubStatus
toSubStatus s = case T.map toLower s of
    "trialing"  -> Trialing
    "active"    -> Active
    "past_due"  -> PastDue
    "canceled"  -> Canceled
    "unpaid"    -> Unpaid
    _           -> UnknownStatus s

-- | Attempts to parse JSON into a 'Subscription'.
instance FromJSON Subscription where
    parseJSON (Object o) = Subscription
      <$> (SubscriptionId <$> o .: "id")
      <*> (CustomerId     <$> o .: "customer")
      <*> o .: "plan"
      <*> (     toSubStatus <$> o .:  "status")
      <*> (     fromSeconds <$> o .:  "start")
      <*> (fmap fromSeconds <$> o .:? "trial_start")
      <*> (fmap fromSeconds <$> o .:? "trial_end")
      <*> (     fromSeconds <$> o .:  "current_period_start")
      <*> (     fromSeconds <$> o .:  "current_period_end")
      <*> o .:? "discount"
    parseJSON _ = mzero
