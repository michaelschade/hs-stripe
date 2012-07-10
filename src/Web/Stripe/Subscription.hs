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

import Control.Monad        ( liftM, mzero )
import Control.Monad.Error  ( MonadIO )
import Data.Char            ( toLower )
import Network.HTTP.Types   ( StdMethod(..) )
import Web.Stripe.Card      ( RequestCard, rCardKV )
import Web.Stripe.Client    ( StripeT(..), SConfig(..), StripeRequest(..), baseSReq
                            , query, runStripeT
                            )
import Web.Stripe.Coupon    ( CpnId(..) )
import Web.Stripe.Customer  ( CustomerId(..) )
import Web.Stripe.Token     ( TokenId(..) )
import Web.Stripe.Plan      ( Plan, PlanId(..) )
import Web.Stripe.Utils     ( UTCTime(..), fromSeconds,  optionalArgs
                            , textToByteString, showByteString
                            )

import           Data.Aeson (FromJSON (..), (.:), Value (..))
import           Control.Applicative  ( (<$>), (<*>))
import qualified Data.Text              as T
import qualified Data.ByteString        as B

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
               | UnknownStatus T.Text deriving Show

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
      <$> (CustomerId  <$> o .: "customer")
      <*> o .: "plan"
      <*> (toSubStatus <$> o .: "status")
      <*> (fromSeconds <$> o .: "start")
      <*> (fromSeconds <$> o .: "trial_start")
      <*> (fromSeconds <$> o .: "trial_end")
      <*> (fromSeconds <$> o .: "current_period_start")
      <*> (fromSeconds <$> o .: "current_period_end")
    parseJSON _ = mzero
