module Web.Stripe.Customer
    ( Customer(..)
    , CustomerId(..)
    , Email(..)
    , createCustomer
    , updateCustomer
    , updateCustomerById
    , getCustomer
    , getCustomers
    , delCustomer
    , delCustomerById

    {- Re-Export -}
    , UTCTime(..)
    , SConfig(..)
    , StripeT(StripeT)
    , runStripeT
    ) where

import Control.Applicative  ( (<$>) )
import Control.Monad        ( liftM, ap )
import Control.Monad.Error  ( Error, MonadIO, MonadError, throwError, strMsg )
import Data.Maybe           ( fromMaybe )
import Text.JSON            ( Result(..), JSON(..), JSValue(..), resultToEither
                            , valFromObj
                            )
import Web.Stripe.Card      ( Card, RequestCard, rCardKV )
import Web.Stripe.Client    ( StripeT(..), SConfig(..), SRequest(..)
                            , StdMethod(..), baseSReq, query, runStripeT
                            )
import Web.Stripe.Coupon    ( CpnId(..) )
import Web.Stripe.Plan      ( PlanId(..) )
import Web.Stripe.Utils     ( Description(..), UTCTime(..), fromSeconds, jGet
                            , mjGet, optionalArgs
                            )

----------------
-- Data Types --
----------------

-- | Represents a customer in the Stripe system.
data Customer = Customer
    { custId            :: CustomerId
    , custEmail         :: Email
    , custDescription   :: Maybe Description
    , custLive          :: Bool
    , custCreated       :: UTCTime
    , custActiveCard    :: Maybe Card
    } deriving Show

-- | Represents a 'Customer'\'s ID in the Stripe system.
newtype CustomerId = CustomerId { unCustomerId :: String } deriving Show

-- | Represents a standard email address.
newtype Email = Email { unEmail :: String } deriving Show

-- | Create a new 'Customer' in the Stripe system.
createCustomer  :: MonadIO m => Maybe RequestCard -> Maybe CpnId -> Maybe Email
                -> Maybe Description -> Maybe PlanId -> Maybe Int
                -> StripeT m Customer
createCustomer mrc mcid me md mpid mtime =
    snd `liftM` query (customerRq []) { sMethod = POST, sData = fdata }
    where
        fdata = fromMaybe [] (rCardKV <$> mrc) ++ optionalArgs odata
        odata = [ ("coupon",        unCpnId         <$> mcid)
                , ("email",         unEmail         <$> me)
                , ("description",   unDescription   <$> md)
                , ("plan",          unPlanId        <$> mpid)
                , ("trial_end",     show            <$> mtime)
                ]

-- | Update an existing 'Customer' in the Stripe system.
updateCustomer :: MonadIO m => Customer -> Maybe RequestCard -> Maybe CpnId
               -> Maybe Email -> Maybe Description -> StripeT m Customer
updateCustomer  = updateCustomerById . custId

-- | Update an existing 'Customer', identified by 'CustomerId', in the Stripe
--   system.
updateCustomerById :: MonadIO m => CustomerId -> Maybe RequestCard
                   -> Maybe CpnId -> Maybe Email -> Maybe Description
                   -> StripeT m Customer
updateCustomerById (CustomerId cid) mrc mcid me md =
    snd `liftM` query (customerRq [cid]) { sMethod = POST, sData = fdata }
    where
        fdata = fromMaybe [] (rCardKV <$> mrc) ++ optionalArgs odata
        odata = [ ("coupon",        unCpnId         <$> mcid)
                , ("email",         unEmail         <$> me)
                , ("description",   unDescription   <$> md)
                ]

-- | Retrieves a specific 'Customer' based on its 'CustomerId'.
getCustomer :: MonadIO m => CustomerId -> StripeT m Customer
getCustomer (CustomerId cid) =
    return . snd =<< query (customerRq [cid])

-- | Retrieves a list of all 'Customer's.
getCustomers :: MonadIO m => StripeT m [Customer]
getCustomers  = do
    (_, rsp) <- query $ customerRq []
    either err return . resultToEither . valFromObj "data" $ rsp
    where err _ = throwError $ strMsg "Unable to parse customer list."

-- | Deletes a 'Customer' if it exists. If it does not, an
--   'InvalidRequestError' will be thrown indicating this.
delCustomer :: MonadIO m => Customer -> StripeT m Bool
delCustomer  = delCustomerById . custId

-- | Deletes a 'Customer', identified by its 'CustomerId', if it exists.  If it
--   does not, an 'InvalidRequestError' will be thrown indicating this.
delCustomerById :: MonadIO m => CustomerId -> StripeT m Bool
delCustomerById (CustomerId cid) = query req >>=
    either err return . resultToEither . valFromObj "deleted" . snd
    where
        err _   = throwError $ strMsg "Unable to parse customer delete."
        req     = (customerRq [cid]) { sMethod = DELETE }

-- | Convenience function to create a 'SRequest' specific to customer-related
--   actions.
customerRq :: [String] -> SRequest
customerRq pcs = baseSReq { sDestination = "customers":pcs }

------------------
-- JSON Parsing --
------------------

-- | Attempts to parse JSON into a 'Customer'.
instance JSON Customer where
    readJSON (JSObject c) =
        Customer `liftM` (CustomerId   <$> jGet c "id")
                    `ap` (Email        <$> jGet c "email")
                    `ap` ((Description <$>) <$> mjGet c "description")
                    `ap` jGet  c "livemode"
                    `ap` (fromSeconds  <$> jGet  c "created")
                    `ap` mjGet c "active_card"
    readJSON _ = Error "Unable to read Stripe customer."
    showJSON _ = undefined
