module Web.Stripe.Coupon
    ( Coupon(..)
    , CpnId(..)
    , CpnDuration(..)
    , CpnPercentOff(..)
    , CpnMaxRedeems(..)
    , CpnRedeemBy(..)
    , createCoupon
    , getCoupon
    , getCoupons
    , delCoupon
    , delCouponById

    {- Re-Export -}
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
import Web.Stripe.Utils     ( jGet, mjGet, optionalArgs )

----------------
-- Data Types --
----------------

-- | Represents a coupon in the Stripe system.
data Coupon = Coupon
    { cpnId         :: Maybe CpnId
    , cpnDuration   :: CpnDuration
    , cpnPercentOff :: CpnPercentOff
    } deriving Show

-- | Represents the identifier for a given 'Coupon' in the Stripe system.
newtype CpnId = CpnId { unCpnId :: String } deriving Show

-- | Represents the duration of a coupon. If an interval identifier is not
--   known, 'UnknownDuration' is used to carry the original identifier supplied
--   by Stripe.
data CpnDuration
    = Once
    | Repeating Int -- ^ Field specifies how long (months) discount is in effect
    | Forever
    | UnknownDuration String
    deriving Show

-- | Represents the percent off that is applied by a coupon. This must be
--   between 1 and 100.
newtype CpnPercentOff = CpnPercentOff { unCpnPercentOff :: Int } deriving Show

-- | A positive number representing the maximum number of times that a coupon
--   can be redeemed. 
newtype CpnMaxRedeems = CpnMaxRedeems { unCpnMaxRedeems :: Int } deriving Show

-- | UTC timestamp specifying the last time at which the coupon can be
--   redeemed.
newtype CpnRedeemBy = CpnRedeemBy { unCpnRedeemBy :: Int } deriving Show

-- | Creates a 'Coupon' in the Stripe system.
createCoupon :: MonadIO m => Coupon -> Maybe CpnMaxRedeems -> Maybe CpnRedeemBy
                          -> StripeT m ()
createCoupon c mmr mrb = query_ (cpnRq []) { sMethod = POST, sData = fdata }
    where
        fdata = poff:cpnDurationKV (cpnDuration c) ++ optionalArgs odata
        poff  = ("percent_off", show . unCpnPercentOff . cpnPercentOff $ c)
        odata = [ ("id",              unCpnId <$> cpnId c)
                , ("max_redemptions", show . unCpnMaxRedeems <$> mmr)
                , ("redeem_by",       show . unCpnRedeemBy <$> mrb)
                ]

-- | Retrieves a specific 'Coupon' based on its 'CpnId'.
getCoupon :: MonadIO m => CpnId -> StripeT m Coupon
getCoupon (CpnId cid) = return . snd =<< query (cpnRq [cid])

-- | Retrieves a list of all 'Coupon's.
getCoupons :: MonadIO m => StripeT m [Coupon]
getCoupons  = do
    (_, rsp) <- query $ cpnRq []
    either err return . resultToEither . valFromObj "data" $ rsp
    where err _ = throwError $ strMsg "Unable to parse coupon list."

-- | Deletes a 'Coupon' if it exists. If it does not, an
--   'InvalidRequestError' will be thrown indicating this.
delCoupon :: MonadIO m => Coupon -> StripeT m Bool
delCoupon  = handleCpnId . cpnId
    where
        handleCpnId Nothing    = throwError $ strMsg "No coupon ID provided."
        handleCpnId (Just cid) = delCouponById cid

-- | Deletes a 'Coupon', identified by its 'CpnId', if it exists.  If it
--   does not, an 'InvalidRequestError' will be thrown indicating this.
delCouponById :: MonadIO m => CpnId -> StripeT m Bool
delCouponById (CpnId cid) = query (cpnRq [cid]) { sMethod = DELETE } >>=
    either err return . resultToEither . valFromObj "deleted" . snd
    where err _ = throwError $ strMsg "Unable to parse coupon delete."

-- | Convenience function to create a 'SRequest' specific to coupon-related
--   actions.
cpnRq :: [String] -> SRequest
cpnRq pcs = baseSReq { sDestination = "coupons":pcs }

-- | Returns a list of key-value pairs representing duration specifications for
--   use as input in the Stripe API.
cpnDurationKV :: CpnDuration -> [ (String, String) ]
cpnDurationKV d@(Repeating m) = [ ("duration",           fromCpnDuration d)
                                , ("duration_in_months", show m)
                                ]
cpnDurationKV d               = [ ("duration", fromCpnDuration d) ]

------------------
-- JSON Parsing --
------------------

-- | Converts a 'CpnDuration' to a string for input into the Stripe API. For
--   'UnknownDuration's, the original interval code will be used.
fromCpnDuration :: CpnDuration -> String
fromCpnDuration Once                = "once"
fromCpnDuration (Repeating _)       = "repeating"
fromCpnDuration Forever             = "forever"
fromCpnDuration (UnknownDuration d) = d

-- | Convert a string to a 'CpnDuration'. Used for parsing output from the
--   Stripe API.
toCpnDuration  :: String -> Maybe Int -> CpnDuration
toCpnDuration d Nothing = case map toLower d of
    "once"      -> Once
    "forever"   -> Forever
    _           -> UnknownDuration d
toCpnDuration d (Just ms) = case map toLower d of
    "repeating" -> Repeating ms
    _           -> UnknownDuration d

-- | Attempts to parse JSON into a 'Coupon'.
instance JSON Coupon where
    readJSON (JSObject c) = do
        drn  <- jGet  c "duration"
        drns <- mjGet c "duration_in_months"
        Coupon `liftM` (return . Just . CpnId  =<< jGet c "id")
                  `ap` return (toCpnDuration drn drns)
                  `ap` (return . CpnPercentOff =<< jGet  c "percent_off")
    readJSON _ = Error "Unable to read Stripe coupon."
    showJSON _ = undefined
