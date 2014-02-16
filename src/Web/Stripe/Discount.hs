{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Discount
    ( Discount(..)
    ) where

import Web.Stripe.Utils

import Web.Stripe.Coupon

data Discount = Discount {
    disCustomerId :: CustomerId
  , disCoupon :: Coupon
  , disSubscriptionId :: Maybe SubscriptionId
  , disStart :: Maybe UTCTime
  , disEnd   :: Maybe UTCTime
  } deriving (Show)

instance FromJSON Discount where
    parseJSON = withObject "Discount" $ \o -> Discount
      <$> (     CustomerId <$> o .: "customer")
      <*> o .: "coupon"
      <*> (fmap SubscriptionId <$> o .:? "subscription")
      <*> (fmap fromSeconds <$> o .:? "start")
      <*> (fmap fromSeconds <$> o .:? "end")
