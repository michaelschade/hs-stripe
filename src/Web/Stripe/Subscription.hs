module Web.Stripe.Subscription
    ( Subscription(..)
    , SubStatus(..)
    , SubProrate(..)
    , SubTrialEnd(..)
    , SubAtPeriodEnd(..)
    , updateSubRCard
    , updateSubToken
    , cancelSub

    {- Re-Export -}
    , UTCTime(..)
    , SConfig(..)
    , StripeT(StripeT)
    , runStripeT
    ) where

import Control.Applicative  ( (<$>) )
import Control.Monad        ( liftM, ap )
import Control.Monad.Error  ( MonadIO )
import Data.Char            ( toLower )
import Network.HTTP.Types   ( StdMethod(..) )
import Text.JSON            ( Result(Error), JSON(..), JSValue(JSObject) )
import Web.Stripe.Card      ( RequestCard, rCardKV )
import Web.Stripe.Client    ( StripeT(..), SConfig(..), SRequest(..), baseSReq
                            , query, runStripeT
                            )
import Web.Stripe.Coupon    ( CpnId(..) )
import Web.Stripe.Customer  ( CustomerId(..) )
import Web.Stripe.Token     ( TokenId(..) )
import Web.Stripe.Plan      ( Plan, PlanId(..) )
import Web.Stripe.Utils     ( UTCTime(..), fromSeconds, jGet, optionalArgs )

------------------
-- Subsriptions --
------------------

-- | Represents a subscription in the Stripe API.
data Subscription = Subscription
    { subCustomerId     :: CustomerId
    , subPlan           :: Plan
    , subStatus         :: SubStatus
    , subStart          :: UTCTime
    , subTrialStart     :: UTCTime
    , subTrialEnd       :: UTCTime
    , subPeriodStart    :: UTCTime -- ^ Current period start
    , subPeriodEnd      :: UTCTime -- ^ Current period end
    } deriving Show

-- | Describes the various stages that a
data SubStatus = Trialing | Active | PastDue | Unpaid | Canceled
               | UnknownStatus String deriving Show

-- | A boolean flag that determines whether or not to prorate switching plans
--   during a billing cycle.
newtype SubProrate = SubProrate { unSubProrate :: Bool } deriving Show

-- | UTC integer timestamp representing the end of the trial period that the
--   customer receives before being charged for the first time.
newtype SubTrialEnd = SubTrialEnd { unSubTrialEnd :: Int } deriving Show

-- | A boolean flag that determines whether or not the cancellation of the
--   'Subscription' should be delayed until the end of the current period.
newtype SubAtPeriodEnd = SubAtPeriodEnd { unSubAtPeriodEnd :: Bool }
    deriving Show

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
updateSubToken (TokenId tid) = updateSub [("token", tid)]

-- | Internal convenience function to update a 'Subscription'.
updateSub :: MonadIO m => [(String, String)] -> CustomerId -> PlanId
          -> Maybe CpnId -> Maybe SubProrate -> Maybe SubTrialEnd
          -> StripeT m Subscription
updateSub sdata cid pid mcpnid mspr mste =
    snd `liftM` query (subRq cid []) { sMethod = POST, sData = fdata }
    where
        fdata = ("plan", unPlanId pid) : sdata ++ optionalArgs odata
        odata = [ ("coupon",    unCpnId              <$> mcpnid)
                , ("prorate",   show . unSubProrate  <$> mspr)
                , ("trial_end", show . unSubTrialEnd <$> mste)
                ]

-- | Cancels the 'Subscription' associated with a 'Customer', identified by
--   'CustomerId', in the Stripe system.
cancelSub :: MonadIO m => CustomerId -> Maybe SubAtPeriodEnd
          -> StripeT m Subscription
cancelSub cid mspe = snd `liftM`
    query (subRq cid []) { sMethod = DELETE, sData = optionalArgs odata }
    where odata = [("at_period_end", show . unSubAtPeriodEnd <$> mspe)]

-- | Convenience function to create a 'SRequest' specific to
--   subscription-related actions.
subRq :: CustomerId -> [String] -> SRequest
subRq (CustomerId cid) pcs =
    baseSReq { sDestination = "customers":cid:"subscription":pcs }

------------------
-- JSON Parsing --
------------------

-- | Convert a string to a 'SubStatus'. If the code is not known,
--   'UnkownStatus' will be returned with the originally provided code.
toSubStatus  :: String -> SubStatus
toSubStatus s = case map toLower s of
    "trialing"  -> Trialing
    "active"    -> Active
    "past_due"  -> PastDue
    "canceled"  -> Canceled
    "unpaid"    -> Unpaid
    _           -> UnknownStatus s

-- | Attempts to parse JSON into a 'Subscription'.
instance JSON Subscription where
    readJSON (JSObject c) =
        Subscription `liftM` (CustomerId  <$> jGet c "customer")
                        `ap` jGet c "plan"
                        `ap` (toSubStatus <$> jGet c "status")
                        `ap` (fromSeconds <$> jGet c "start")
                        `ap` (fromSeconds <$> jGet c "trial_start")
                        `ap` (fromSeconds <$> jGet c "trial_end")
                        `ap` (fromSeconds <$> jGet c "current_period_start")
                        `ap` (fromSeconds <$> jGet c "current_period_end")
    readJSON _ = Error "Unable to read Stripe subscription."
    showJSON _ = undefined
