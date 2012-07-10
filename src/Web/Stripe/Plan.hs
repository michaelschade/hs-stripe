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

import Control.Applicative  ( (<$>), (<*>) )
import Control.Monad        ( mzero )
import Control.Monad.Error  ( MonadIO, throwError, strMsg )
import Data.Char            ( toLower )
import Network.HTTP.Types   ( StdMethod(..) )
import Web.Stripe.Client    ( StripeT(..), SConfig(..), StripeRequest(..), baseSReq
                            , queryData, query, query_, runStripeT
                            )
import Web.Stripe.Utils     ( Amount(..), Count(..), Currency(..), Offset(..) 
                            , valFromRawJson , optionalArgs
                            , textToByteString, showByteString )
import           Data.Aeson (FromJSON (..), (.:), (.:?), Value (..))
import           Data.Aeson.Types (parseMaybe)
import qualified Data.Text   as T

----------------
-- Data Types --
----------------

-- | Represents a plan in the Stripe system.
data Plan = Plan
    { planId        :: PlanId
    , planAmount    :: Amount
    , planInterval  :: PlanInterval
    , planName      :: T.Text
    , planCurrency  :: Currency
    , planTrialDays :: Maybe PlanTrialDays
    } deriving Show

-- | Represents the billing cycle for a plan. If an interval identifier is not
--   known, 'UnknownPlan' is used to carry the original identifier supplied by
--   Stripe.
data PlanInterval = Monthly | Yearly | UnknownPlan T.Text deriving Show

-- | Represents the identifier for a given 'Plan' in the Stripe system.
newtype PlanId = PlanId { unPlanId :: T.Text } deriving Show

-- | Represents the length of the trial period. That is, the number of days
--   before the customer is billed.
newtype PlanTrialDays = PlanTrialDays { unPlanTrialDays :: Int } deriving Show

-- | Creates a 'Plan' in the Stripe system.
createPlan :: MonadIO m => Plan -> StripeT m ()
createPlan p = query_ (planRq []) { sMethod = POST, sData = fdata }
    where
        fdata   = pdata ++ optionalArgs odata
        pdata   = [ ("id", textToByteString . unPlanId $ planId p)
                  , ("amount",   showByteString . unAmount  $ planAmount p)
                  , ("interval", textToByteString . fromPlanInterval $ planInterval p)
                  , ("name",     textToByteString $ planName p)
                  , ("currency", textToByteString . unCurrency $ planCurrency p)
                  ]
        odata   = [ ( "trial_period_days"
                    , showByteString . unPlanTrialDays <$> planTrialDays p
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
    queryData (planRq []) { sQString = qs } >>= return . snd
  where
    qs    = optionalArgs [ ("count",  show . unCount  <$> mc)
                         , ("offset", show . unOffset <$> mo)
                         ]

-- | Deletes a 'Plan' if it exists. If it does not, an 'InvalidRequestError'
--   will be thrown indicating this.
delPlan :: MonadIO m => Plan -> StripeT m Bool
delPlan  = delPlanById . planId

-- | Deletes a 'Plan', identified by its 'PlanId', if it exists.  If it does
--   not, an 'InvalidRequestError' will be thrown indicating this.
delPlanById :: MonadIO m => PlanId -> StripeT m Bool
delPlanById (PlanId pid) = queryData req >>= return . snd
    where
        req     = (planRq [pid]) { sMethod = DELETE }

-- | Convenience function to create a 'StripeRequest' specific to plan-related
--   actions.
planRq :: [T.Text] -> StripeRequest
planRq pcs = baseSReq { sDestination = "plans":pcs }

------------------
-- JSON Parsing --
------------------

-- | Converts a 'PlanInterval' to a T.Text for input into the Stripe API. For
--   'UnknownPlan's, the original interval code will be used.
fromPlanInterval :: PlanInterval -> T.Text
fromPlanInterval Monthly         = "month"
fromPlanInterval Yearly          = "year"
fromPlanInterval (UnknownPlan p) = p

-- | Convert a T.Text to a 'PlanInterval'. Used for parsing output from the
--   Stripe API.
toPlanInterval  :: T.Text -> PlanInterval
toPlanInterval p = case T.map toLower p of
    "month" -> Monthly
    "year"  -> Yearly
    _       -> UnknownPlan p

-- | Attempts to parse JSON into a 'Plan'.
instance FromJSON Plan where
    parseJSON (Object o) = Plan 
        <$> (PlanId         <$> o .: "id")
        <*> (Amount         <$> o .: "amount")
        <*> (toPlanInterval <$> o .: "interval")
        <*> o .: "name"
        <*> (Currency       <$> o .: "currency")
        <*> ((PlanTrialDays <$>) <$> o .:? "trial_period_days")
    parseJSON _ = mzero
