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
    , Count(..)
    , Offset(..)
    , Description(..)
    , UTCTime(..)
    , SConfig(..)
    , StripeT(StripeT)
    , runStripeT
    ) where

import Control.Applicative  ( (<$>), (<*>))
import Control.Monad        ( liftM, mzero )
import Control.Monad.Error  ( Error, MonadIO, MonadError, throwError, strMsg )
import Data.Maybe           ( fromMaybe )
import Web.Stripe.Card      ( Card, RequestCard, rCardKV )
import Web.Stripe.Client    ( StripeT(..), SConfig(..), StripeRequest(..)
                            , StdMethod(..), baseSReq, query, runStripeT
                            )
import Web.Stripe.Coupon    ( CpnId(..) )
import Web.Stripe.Plan      ( PlanId(..) )
import Web.Stripe.Utils     ( Count(..), Offset(..), Description(..)
                            , UTCTime(..), valFromRawJson
                            , optionalArgs, textToByteString, showByteString
                            )

import           Data.Aeson (FromJSON (..), (.:), (.:?), Value (..))
import           Data.Aeson.Types (parseMaybe)
import qualified Data.Text   as T
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
newtype CustomerId = CustomerId { unCustomerId :: T.Text } deriving Show

-- | Represents a standard email address.
newtype Email = Email { unEmail :: T.Text } deriving Show

-- | Create a new 'Customer' in the Stripe system.
createCustomer  :: MonadIO m => Maybe RequestCard -> Maybe CpnId -> Maybe Email
                -> Maybe Description -> Maybe PlanId -> Maybe Int
                -> StripeT m Customer
createCustomer mrc mcid me md mpid mtime =
    snd `liftM` query (customerRq []) { sMethod = POST, sData = fdata }
    where
        fdata = fromMaybe [] (rCardKV <$> mrc) ++ optionalArgs odata
        odata = [ ("coupon",        textToByteString . unCpnId         <$> mcid)
                , ("email",         textToByteString . unEmail         <$> me)
                , ("description",   textToByteString . unDescription   <$> md)
                , ("plan",          textToByteString . unPlanId        <$> mpid)
                , ("trial_end",     showByteString  <$> mtime)
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
        odata = [ ("coupon",      textToByteString . unCpnId         <$> mcid)
                , ("email",       textToByteString . unEmail         <$> me)
                , ("description", textToByteString . unDescription   <$> md)
                ]

-- | Retrieves a specific 'Customer' based on its 'CustomerId'.
getCustomer :: MonadIO m => CustomerId -> StripeT m Customer
getCustomer (CustomerId cid) =
    return . snd =<< query (customerRq [cid])

-- | Retrieves a list of all 'Customer's. The query can optionally be refined
--   to a specific:
--
--      * number of charges, via 'Count' and
--      * page of results, via 'Offset'.
getCustomers :: MonadIO m => Maybe Count -> Maybe Offset -> StripeT m [Customer]
getCustomers mc mo = do
    (_, rsp) <- query $ (customerRq []) { sQString = qstring }
    wrapper <- maybe err return $ valFromRawJson "data" rsp
    maybe err return $ parseMaybe parseJSON wrapper
    where
        qstring = optionalArgs  [ ("count",  show . unCount  <$> mc)
                                , ("offset", show . unOffset <$> mo)
                                ]
        err     = throwError $ strMsg "Unable to parse customer list."

-- | Deletes a 'Customer' if it exists. If it does not, an
--   'InvalidRequestError' will be thrown indicating this.
delCustomer :: MonadIO m => Customer -> StripeT m Bool
delCustomer  = delCustomerById . custId

-- | Deletes a 'Customer', identified by its 'CustomerId', if it exists.  If it
--   does not, an 'InvalidRequestError' will be thrown indicating this.
delCustomerById :: MonadIO m => CustomerId -> StripeT m Bool
delCustomerById (CustomerId cid) = query req >>= \rsp -> do
    wrapper <- maybe err return . valFromRawJson "data" $ snd rsp
    maybe err return $ parseMaybe parseJSON wrapper
    where
        err     = throwError $ strMsg "Unable to parse customer delete."
        req     = (customerRq [cid]) { sMethod = DELETE }

-- | Convenience function to create a 'StripeRequest' specific to customer-related
--   actions.
customerRq :: [T.Text] -> StripeRequest
customerRq pcs = baseSReq { sDestination = "customers":pcs }

------------------
-- JSON Parsing --
------------------

-- | Attempts to parse JSON into a 'Customer'.
instance FromJSON Customer where
    parseJSON (Object o) = Customer 
        <$> (CustomerId   <$> o .: "id")
        <*> (Email        <$> o .: "email")
        <*> ((fmap . fmap) Description  (o .:? "description"))
        <*> o .: "livemode"
        <*> o .: "created"
        <*> o .:? "active_card"
    parseJSON _ = mzero
