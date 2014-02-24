{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Card
    ( Card(..)
    , RequestCard(..)
    , CardChecks(..)
    , CardCheckResult(..)
    , rCardKV
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON (..), Value (..), (.:), (.:?))
import qualified Data.ByteString     as B
import qualified Data.Text           as T
import           Web.Stripe.Utils    (optionalArgs, showByteString,
                                      textToByteString)

-- | Represents a credit card in the Stripe system.
data Card = Card
    { cardType        :: T.Text
    , cardCountry     :: Maybe T.Text
    , cardLastFour    :: T.Text
    , cardExpMonth    :: Int
    , cardExpYear     :: Int
    , cardFingerprint :: T.Text
    , cardChecks      :: CardChecks
    } deriving Show

-- | Represents a credit car (with full details) that is used as input to the
--   Stripe API.
data RequestCard = RequestCard
    { rCardNumber      :: T.Text
    , rCardExpMonth    :: Int
    , rCardExpYear     :: Int
    , rCardCVC         :: Maybe T.Text -- ^ Highly recommended to supply
    , rCardFullName    :: Maybe T.Text
    , rCardAddrLineOne :: Maybe T.Text
    , rCardAddrLineTwo :: Maybe T.Text
    , rCardCity        :: Maybe T.Text
    , rCardAddrZip     :: Maybe T.Text
    , rCardAddrState   :: Maybe T.Text
    , rCardAddrCountry :: Maybe T.Text
    } deriving Show

data CardChecks = CardChecks
    { checkCVC         :: CardCheckResult
    , checkAddrLineOne :: CardCheckResult
    , checkZip         :: CardCheckResult
    } deriving Show

data CardCheckResult = NotProvided | NotChecked | Passed | Failed
    deriving (Show, Eq)

-- | Turns a 'RequestCard' into a list of key-value pairs that can be submitted
--   to the Stripe API in a query.
rCardKV :: RequestCard -> [(B.ByteString, B.ByteString)]
rCardKV rc = fd ++ optionalArgs md
    where
        -- Required
        fd = [ ("card[number]",  textToByteString $ rCardNumber rc)
             , ("card[exp_month]", showByteString $ rCardExpMonth rc)
             , ("card[exp_year]",  showByteString $ rCardExpYear  rc)
             ]
        -- Optional
        md = [ ("card[cvc]",             textToByteString <$> rCardCVC           rc)
             , ("card[name]",            textToByteString <$> rCardFullName      rc)
             , ("card[address_line1]",   textToByteString <$> rCardAddrLineOne   rc)
             , ("card[address_line2]",   textToByteString <$> rCardAddrLineTwo   rc)
             , ("card[address_city]",    textToByteString <$> rCardCity   rc)
             , ("card[address_zip]",     textToByteString <$> rCardAddrZip       rc)
             , ("card[address_state]",   textToByteString <$> rCardAddrState     rc)
             , ("card[address_country]", textToByteString <$> rCardAddrCountry   rc)
             ]

------------------
-- JSON Parsing --
------------------

-- | Attempts to parse JSON into a credit 'Card'.
instance FromJSON Card where
  parseJSON (Object v) = Card
    <$> v .:  "type"
    <*> v .:? "country"
    <*> v .:  "last4"
    <*> v .:  "exp_month"
    <*> v .:  "exp_year"
    <*> v .:  "fingerprint"
    <*> (CardChecks
      <$> v .: "cvc_check"
      <*> v .: "address_line1_check"
      <*> v .: "address_zip_check"
    )
  parseJSON _ = mzero

instance FromJSON CardCheckResult where
  parseJSON Null = return NotProvided
  parseJSON (String s)
    | s == "unchecked" = return NotChecked
    | s == "pass" = return Passed
  parseJSON _ = return Failed
