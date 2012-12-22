{-# LANGUAGE OverloadedStrings #-}

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
    , Count(..)
    , Offset(..)
    , SConfig(..)
    , StripeT(StripeT)
    , runStripeT
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (mzero)
import           Control.Monad.Error (MonadIO, strMsg, throwError)
import           Data.Aeson          (FromJSON (..), Value (..), parseJSON,
                                      (.:), (.:?))
import           Data.Aeson.Types    (parseMaybe)
import qualified Data.ByteString     as B
import           Data.Char           (toLower)
import qualified Data.Text           as T
import           Network.HTTP.Types  (StdMethod (..))
import           Web.Stripe.Client   (SConfig (..), StripeRequest (..),
                                      StripeT (..), baseSReq, query, queryData,
                                      query_, runStripeT)
import           Web.Stripe.Utils    (Count (..), Offset (..), optionalArgs,
                                      showByteString, textToByteString,
                                      valFromRawJson)

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
newtype CpnId = CpnId { unCpnId :: T.Text } deriving Show

-- | Represents the duration of a coupon. If an interval identifier is not
--   known, 'UnknownDuration' is used to carry the original identifier supplied
--   by Stripe.
data CpnDuration
    = Once
    | Repeating Int -- ^ Field specifies how long (months) discount is in effect
    | Forever
    | UnknownDuration T.Text
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
createCoupon
    :: MonadIO m
    => Coupon
    -> Maybe CpnMaxRedeems
    -> Maybe CpnRedeemBy
    -> StripeT m ()
createCoupon c mmr mrb = query_ (cpnRq []) { sMethod = POST, sData = fdata }
    where
        fdata = poff:cpnDurationKV (cpnDuration c) ++ optionalArgs odata
        poff  = ("percent_off", showByteString . unCpnPercentOff . cpnPercentOff $ c)
        odata = [ ("id", (textToByteString . unCpnId) <$> cpnId c)
                , ("max_redemptions", showByteString . unCpnMaxRedeems <$> mmr)
                , ("redeem_by",       showByteString . unCpnRedeemBy <$> mrb)
                ]

-- | Retrieves a specific 'Coupon' based on its 'CpnId'.
getCoupon :: MonadIO m => CpnId -> StripeT m Coupon
getCoupon (CpnId cid) = return . snd =<< query (cpnRq [cid])

-- | Retrieves a list of all 'Coupon's. The query can optionally be refined to
--   a specific:
--
--      * number of charges, via 'Count' and
--      * page of results, via 'Offset'.
getCoupons :: MonadIO m => Maybe Count -> Maybe Offset -> StripeT m [Coupon]
getCoupons mc mo = queryData (cpnRq []) { sQString = qs } >>= return . snd
  where
    qs = optionalArgs [ ("count",  show . unCount  <$> mc)
                      , ("offset", show . unOffset <$> mo)
                      ]

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
delCouponById (CpnId cid) = queryData (cpnRq [cid]) { sMethod = DELETE } >>= return . snd

-- | Convenience function to create a 'StripeRequest' specific to coupon-related
--   actions.
cpnRq :: [T.Text] -> StripeRequest
cpnRq pcs = baseSReq { sDestination = "coupons":pcs }

-- | Returns a list of key-value pairs representing duration specifications for
--   use as input in the Stripe API.
cpnDurationKV :: CpnDuration -> [ (B.ByteString, B.ByteString) ]
cpnDurationKV d@(Repeating m) = [ ("duration", textToByteString $           fromCpnDuration d)
                                , ("duration_in_months", showByteString m)
                                ]
cpnDurationKV d               = [ ("duration", textToByteString $ fromCpnDuration d) ]

------------------
-- JSON Parsing --
------------------

-- | Converts a 'CpnDuration' to a string for input into the Stripe API. For
--   'UnknownDuration's, the original interval code will be used.
fromCpnDuration :: CpnDuration -> T.Text
fromCpnDuration Once                = "once"
fromCpnDuration (Repeating _)       = "repeating"
fromCpnDuration Forever             = "forever"
fromCpnDuration (UnknownDuration d) = d

-- | Convert a string to a 'CpnDuration'. Used for parsing output from the
--   Stripe API.
toCpnDuration  :: T.Text -> Maybe Int -> CpnDuration
toCpnDuration d Nothing = case T.map toLower d of
    "once"      -> Once
    "forever"   -> Forever
    _           -> UnknownDuration d
toCpnDuration d (Just ms) = case T.map toLower d of
    "repeating" -> Repeating ms
    _           -> UnknownDuration d

-- | Attempts to parse JSON into a 'Coupon'.
instance FromJSON Coupon where
    parseJSON (Object c) = do
        drn  <- c .: "duration"
        drns <- c .: "duration_in_months"
        cId  <- c .:? "id"
        pctOff <- c .: "percent_off"
        return $ Coupon (CpnId <$> cId) (toCpnDuration drn drns) (CpnPercentOff pctOff)
    parseJSON _ = mzero
