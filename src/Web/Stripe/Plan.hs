{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Web.Stripe.Plan
    ( Plan(..)
    , PlanInterval(..)
    , PlanId(PlanId)
    , PlanTrialDays(PlanTrialDays)
    , createPlan
    , getPlan
    , getPlans
    , delPlan
    , delPlanById

    {- Re-Export -}
    , SConfig(..)
    , StripeT(StripeT)
    , runStripeT
    ) where

import Control.Monad        ( liftM, ap )
import Control.Monad.Error  ( MonadIO, throwError, strMsg )
import Data.Char            ( toLower )
import Network.HTTP.Types   ( StdMethod(..) )
import Text.JSON            ( Result(Error), JSON(..), JSValue(JSObject)
                            , resultToEither, valFromObj
                            )
import Web.Stripe.Client    ( StripeT(..), SConfig(..), SRequest(..), baseSReq
                            , query, query_, runStripeT
                            )
import Web.Stripe.Utils     ( jGet )

----------------
-- Data Types --
----------------

-- | Represents a plan in the Stripe system.
data Plan = Plan
    { planId        :: PlanId
    , planAmount    :: Int
    , planInterval  :: PlanInterval
    , planName      :: String
    , planCurrency  :: String
    } deriving Show

-- | Represents the billing cycle for a plan. If an interval identifier is not
--   knowm, 'UnknownPlan' is used to carry the original identifier supplied by
--   Stripe.
data PlanInterval = Monthly | Yearly | UnknownPlan String deriving Show

-- | Represents the identifier for a given 'Plan' in the Stripe system.
newtype PlanId = PlanId { unPlanId :: String } deriving Show

-- | Represents the length of the trial period. That is, the number of days
--   before the customer is billed.
newtype PlanTrialDays = PlanTrialDays { unPlanTrialDays :: Int } deriving Show

-- | Creates a 'Plan' in the Stripe system.
createPlan :: MonadIO m => Plan -> Maybe PlanTrialDays -> StripeT m ()
createPlan p mtd = query_ (planRq []) { sMethod = POST, sData = fdata }
    where
        fdata   = maybe pdata ((:pdata) . trialKV) mtd
        trialKV = (,) "trial_period_days" . show . unPlanTrialDays
        pdata   = [ ("id",       unPlanId $ planId p)
                  , ("amount",   show $ planAmount p)
                  , ("interval", fromPlanInterval $ planInterval p)
                  , ("name",     planName p)
                  , ("currency", planCurrency p)
                  ]

-- | Retrieves a specific 'Plan' based on its 'PlanId'.
getPlan :: MonadIO m => PlanId -> StripeT m Plan
getPlan (PlanId pid) = return . snd =<< query (planRq [pid])

-- | Retrieves a list of all 'Plan's.
getPlans :: MonadIO m => StripeT m [Plan]
getPlans  = do
    (_, rsp) <- query $ planRq []
    either err return . resultToEither . valFromObj "data" $ rsp
    where err _ = throwError $ strMsg "Unable to parse plan list."

-- | Deletes a 'Customer' if it exists. If it does not, an
--   'InvalidRequestError' will be thrown indicating this.
delPlan :: MonadIO m => Plan -> StripeT m Bool
delPlan  = delPlanById . planId

-- | Deletes a 'Customer', identified by its 'CustomerId', if it exists.  If it
--   does not, an 'InvalidRequestError' will be thrown indicating this.
delPlanById :: MonadIO m => PlanId -> StripeT m Bool
delPlanById (PlanId pid) = query req >>=
    either err return . resultToEither . valFromObj "deleted" . snd
    where
        err _   = throwError $ strMsg "Unable to parse plan delete."
        req     = (planRq [pid]) { sMethod = DELETE }

-- | Convenience function to create a 'SRequest' specific to customer-related
--   actions.
planRq :: [String] -> SRequest
planRq pcs = baseSReq { sDestination = "plans":pcs }

------------------
-- JSON Parsing --
------------------

-- | Converts a 'PlanInterval' to a string for input into the Stripe API. For
--   'UnknownPlan's, the original interval code will be used.
fromPlanInterval :: PlanInterval -> String
fromPlanInterval Monthly         = "month"
fromPlanInterval Yearly          = "year"
fromPlanInterval (UnknownPlan p) = p

-- | Convert a string to a 'PlanInterval'. Used for parsing output from the
--   Stripe API.
toPlanInterval  :: String -> PlanInterval
toPlanInterval p = case map toLower p of
    "month" -> Monthly
    "year"  -> Yearly
    _       -> UnknownPlan p

-- | Attempts to parse JSON into a 'Plan'.
instance JSON Plan where
    readJSON (JSObject c) =
        Plan `liftM` (return . PlanId =<< jGet c "id")
                `ap` jGet c "amount"
                `ap` (return . toPlanInterval =<< jGet c "interval")
                `ap` jGet c "name"
                `ap` jGet c "currency"
    readJSON _ = Error "Unable to read Stripe plan."
    showJSON _ = undefined
