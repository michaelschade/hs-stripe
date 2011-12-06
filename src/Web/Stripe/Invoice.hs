module Web.Stripe.Invoice
    (
    {- Invoices -}
      Invoice(..)
    , InvId(..)
    , getInv
    , getInvs
    , getUpcomingInv
    , getUpcomingInvByCid

    {- Invoice Items -}
    , InvoiceItem(..)
    , InvItemId(..)
    , createInvItem
    , updateInvItem
    , updateInvItemById
    , getInvItem
    , getInvItems

    {- Re-Export -}
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
import Web.Stripe.Client    ( StripeT(..), SConfig(..), SRequest(..), baseSReq
                            , query, runStripeT
                            )
import Web.Stripe.Customer  ( Customer(..), CustomerId(..) )
import Web.Stripe.Utils     ( Amount(..), Count(..), Currency(..)
                            , Description(..), Offset(..), UTCTime(..)
                            , fromSeconds, jGet, mjGet, optionalArgs
                            )

--------------
-- Invoices --
--------------

-- | Represents an invoice in the Stripe system.
data Invoice = Invoice
    { ivId          :: InvId
    , ivCreated     :: UTCTime
    , ivSubtotal    :: Amount
    , ivTotal       :: Amount
    , ivLines       :: [InvoiceItem]
    } deriving Show

-- | Identifier for an 'Invoice' in the Stripe system.
newtype InvId = InvId { unInvId :: String } deriving Show

-- | Retrieve an 'Invoice', identified by 'InvId', from the Stripe API.
getInv :: MonadIO m => InvId -> StripeT m Invoice
getInv (InvId iid) = snd `liftM` query (invRq [iid])

-- | Get a list of invoices matching a variety of criteria. Optional parameters 
--   to limit the search include a specific:
--
--      * 'Customer' (identified by 'CustomerId'),
--      * number of 'InvoiceItem's, via 'Count', and
--      * page of results, via 'Offset'
getInvs :: MonadIO m => Maybe CustomerId -> Maybe Count -> Maybe Offset
        -> StripeT m [Invoice]
getInvs  = getList (invItemRq []) "Unable to parse invoice list."

-- | Retrieve the upcoming 'Invoice' for a given 'Customer' from the Stripe
--   system.
getUpcomingInv :: MonadIO m => Customer -> StripeT m Invoice
getUpcomingInv  = getUpcomingInvByCid . custId

-- | Retrieve the upcoming 'Invoice' for a given 'Customer', identified by
--   'CustomerId', from the Stripe system.
getUpcomingInvByCid :: MonadIO m => CustomerId -> StripeT m Invoice
getUpcomingInvByCid (CustomerId cid) =
    snd `liftM` query (invRq ["upcoming"]) { sData = [("customer", cid)] }

-- | Convenience function to create a 'SRequest' specific to invoice-related
--   actions.
invRq :: [String] -> SRequest
invRq pcs = baseSReq { sDestination = "invoices":pcs }

-------------------
-- Invoice Items --
-------------------

-- | Represents an invoice item in the Stripe system.
data InvoiceItem = InvoiceItem
    { iviId             :: InvItemId
    , iviDate           :: UTCTime
    , iviDescription    :: Maybe Description
    , iviAmount         :: Amount
    , iviCurrency       :: Currency
    } deriving Show

-- | Represents the identifier for a given 'InvoiceItem' in the Stripe system.
newtype InvItemId = InvItemId { unInvItemId :: String } deriving Show

-- | Create an 'InvoiceItem' in the Stripe system according to the given
--   parameters.
createInvItem :: MonadIO m => CustomerId -> Amount -> Currency
              -> Maybe Description -> StripeT m InvoiceItem
createInvItem cid amt cnc md = snd `liftM`
    query (invItemRq []) { sMethod = POST, sData = rdata ++ optionalArgs odata }
    where
        rdata = [ ("customer",  unCustomerId cid)
                , ("amount",    show $ unAmount amt)
                , ("currency",  unCurrency cnc)
                ]
        odata = [ ("description", unDescription <$> md) ]

-- | Update an 'InvoiceItem' according to the given parameters.
updateInvItem :: MonadIO m => InvoiceItem -> Amount -> Currency
              -> Maybe Description -> StripeT m InvoiceItem
updateInvItem  = updateInvItemById . iviId

-- | Update an 'InvoiceItem', identified by 'InvItemId', according to the given
--   parameters.
updateInvItemById :: MonadIO m => InvItemId -> Amount -> Currency
                  -> Maybe Description -> StripeT m InvoiceItem
updateInvItemById (InvItemId iid) amt cnc md = snd `liftM`
    query (invItemRq [iid]) { sMethod = POST, sData = fdata }
    where
        fdata = rdata ++ optionalArgs odata
        rdata = [ ("amount",    show $ unAmount amt)
                , ("currency",  unCurrency cnc)
                ]
        odata = [ ("description", unDescription <$> md) ]

-- | Retrieve an 'InvoiceItem', referenced by 'InvItemId', from the Stripe API.
getInvItem :: MonadIO m => InvItemId -> StripeT m InvoiceItem
getInvItem (InvItemId iid) = snd `liftM` query (invItemRq [iid])

-- | Retrieve a list of 'InvoiceItem's from the Stripe API. Optional parameters
--   to limit the search include a specific:
--
--      * 'Customer',
--      * number of 'InvoiceItem's, via 'Count', and
--      * page of results, via 'Offset'
getInvItems :: MonadIO m => Maybe CustomerId -> Maybe Count -> Maybe Offset
            -> StripeT m [InvoiceItem]
getInvItems  = getList (invItemRq []) "Unable to parse invoice item list."

-- | Internal convenience function to retrieve a list of objects from the
--   Stripe API. 'SRequest' is used to designate the appropriate API endpoint,
--   after which the interface is the same.
getList :: (JSON a, MonadIO m) => SRequest -> String -> Maybe CustomerId
        -> Maybe Count -> Maybe Offset -> StripeT m a
getList sr em mcid mc mo = do
    (_, rsp) <- query $ sr { sData = optionalArgs odata }
    either err return . resultToEither . valFromObj "data" $ rsp
    where
        odata = [ ("count",     show . unCount  <$> mc)
                , ("offset",    show . unOffset <$> mo)
                , ("customer",  unCustomerId    <$> mcid)
                ]
        err _ = throwError $ strMsg em


-- | Convenience function to create a 'SRequest' specific to invoice
--   item-related actions.
invItemRq :: [String] -> SRequest
invItemRq pcs = baseSReq { sDestination = "invoiceitems":pcs }

------------------
-- JSON Parsing --
------------------

-- | Attempts to parse JSON into a 'Invoice'.
instance JSON Invoice where
    readJSON (JSObject c) =
        Invoice  `liftM` (InvId         <$> jGet c "id")
                    `ap` (fromSeconds   <$> jGet c "created")
                    `ap` (Amount        <$> jGet c "subtotal")
                    `ap` (Amount        <$> jGet c "total")
                    `ap` jGet c "lines"
    readJSON _ = Error "Unable to read Stripe invoice."
    showJSON _ = undefined

-- | Attempts to parse JSON into a 'InvoiceItem'.
instance JSON InvoiceItem where
    readJSON (JSObject c) =
        InvoiceItem  `liftM` (InvItemId         <$> jGet  c "id")
                        `ap` (fromSeconds       <$> jGet  c "date")
                        `ap` ((Description <$>) <$> mjGet c "description")
                        `ap` (Amount            <$> jGet  c "amount")
                        `ap` (Currency          <$> jGet  c "currency")
    readJSON _ = Error "Unable to read Stripe invoice item."
    showJSON _ = undefined
