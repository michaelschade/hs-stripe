module Web.Stripe.Plan
    ( Plan(..)
    , PlanInterval(..)
    , PlanId(..)
    , PlanTrialDays(..)
    , createPlan
    , getPlan
    , getPlans
    , delPlan
    , delPlanById

    {- Re-Export -}
    , Amount(..)
    , Count(..)
    , Currency(..)
    , Offset(..)
    , SConfig(..)
    , StripeT(StripeT)
    , runStripeT
    ) where

import Control.Applicative  ( (<$>) )
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
import Web.Stripe.Utils     ( Amount(..), Count(..), Currency(..), Offset(..)
                            , jGet, mjGet, optionalArgs
                            )

----------------
-- Data Types --
----------------

-- | Represents a plan in the Stripe system.
data Plan = Plan
    { planId        :: PlanId
    , planAmount    :: Amount
    , planInterval  :: PlanInterval
    , planName      :: String
    , planCurrency  :: Currency
    , planTrialDays :: Maybe PlanTrialDays
    } deriving Show

-- | Represents the billing cycle for a plan. If an interval identifier is not
--   known, 'UnknownPlan' is used to carry the original identifier supplied by
--   Stripe.
data PlanInterval = Monthly | Yearly | UnknownPlan String deriving Show

-- | Represents the identifier for a given 'Plan' in the Stripe system.
newtype PlanId = PlanId { unPlanId :: String } deriving Show

-- | Represents the length of the trial period. That is, the number of days
--   before the customer is billed.
newtype PlanTrialDays = PlanTrialDays { unPlanTrialDays :: Int } deriving Show

-- | Creates a 'Plan' in the Stripe system.
createPlan :: MonadIO m => Plan -> StripeT m ()
createPlan p = query_ (planRq []) { sMethod = POST, sData = fdata }
    where
        fdata   = pdata ++ optionalArgs odata
        pdata   = [ ("id",       unPlanId $ planId p)
                  , ("amount",   show . unAmount  $ planAmount p)
                  , ("interval", fromPlanInterval $ planInterval p)
                  , ("name",     planName p)
                  , ("currency", unCurrency $ planCurrency p)
                  ]
        odata   = [ ( "trial_period_days"
                    , show . unPlanTrialDays <$> planTrialDays p
                    )
                  ]

-- | Retrieves a specific 'Plan' based on its 'PlanId'.
getPlan :: MonadIO m => PlanId -> StripeT m Plan
getPlan (PlanId pid) = return . snd =<< query (planRq [pid])

-- | Retrieves a list of all 'Plan's. The query can optionally be refined to
--   a specific:
--
--      * number of charges, via 'Count' and
--      * page of results, via 'Offset'.
getPlans :: MonadIO m => Maybe Count -> Maybe Offset -> StripeT m [Plan]
getPlans mc mo = do
    (_, rsp) <- query (planRq []) { sQString = qs }
    either err return . resultToEither . valFromObj "data" $ rsp
    where
        qs    = optionalArgs [ ("count",  show . unCount  <$> mc)
                             , ("offset", show . unOffset <$> mo)
                             ]
        err _ = throwError $ strMsg "Unable to parse plan list."

-- | Deletes a 'Plan' if it exists. If it does not, an 'InvalidRequestError'
--   will be thrown indicating this.
delPlan :: MonadIO m => Plan -> StripeT m Bool
delPlan  = delPlanById . planId

-- | Deletes a 'Plan', identified by its 'PlanId', if it exists.  If it does
--   not, an 'InvalidRequestError' will be thrown indicating this.
delPlanById :: MonadIO m => PlanId -> StripeT m Bool
delPlanById (PlanId pid) = query req >>=
    either err return . resultToEither . valFromObj "deleted" . snd
    where
        err _   = throwError $ strMsg "Unable to parse plan delete."
        req     = (planRq [pid]) { sMethod = DELETE }

-- | Convenience function to create a 'SRequest' specific to plan-related
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
        Plan `liftM` (PlanId         <$> jGet c "id")
                `ap` (Amount         <$> jGet c "amount")
                `ap` (toPlanInterval <$> jGet c "interval")
                `ap` jGet c "name"
                `ap` (Currency       <$> jGet c "currency")
                `ap` ((PlanTrialDays <$>) <$> mjGet c "trial_period_days")
    readJSON _ = Error "Unable to read Stripe plan."
    showJSON _ = undefined
