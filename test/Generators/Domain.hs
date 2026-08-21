{-# LANGUAGE OverloadedStrings #-}

module Generators.Domain
  ( genValidTicker
  , shrinkValidTicker
  , genValidCustomerId
  , shrinkValidCustomerId
  , genValidPercentage
  , shrinkValidPercentage
  , genValidPositiveMoney
  , shrinkValidPositiveMoney
  , genAssetClass
  , genSector
  , genCustomerProfile
  , genCreditScore
  , genCustomer
  , genAssetWithTicker
  , genPosition
  ) where

import qualified Data.Text as T
import Test.QuickCheck

import Domain.Asset (Asset (..))
import Domain.AssetClassification (AssetClass (..), Sector (..))
import Domain.CreditScore (CreditScore (..))
import Domain.Customer (Customer (..))
import Domain.CustomerId (CustomerId, mkCustomerId, unCustomerId)
import Domain.CustomerProfile (CustomerProfile (..))
import Domain.Money (PositiveMoney, mkPositiveMoney, unPositiveMoney)
import Domain.Percentage (Percentage, mkPercentage, unPercentage)
import Domain.Position (Position (..))
import Domain.Ticker (Ticker, mkTicker, unTicker)

rightToMaybe :: Either e a -> Maybe a
rightToMaybe = either (const Nothing) Just

suchThatMapGen :: Gen a -> (a -> Maybe b) -> Gen b
suchThatMapGen genValue project =
  genValue >>= maybe (suchThatMapGen genValue project) pure . project

genValidTicker :: Gen Ticker
genValidTicker =
  suchThatMapGen validTickerText (rightToMaybe . mkTicker)
  where
    validTickerText :: Gen T.Text
    validTickerText = do
      size <- choose (1, 10)
      T.pack <$> vectorOf size (elements (['A' .. 'Z'] ++ ['0' .. '9']))

shrinkValidTicker :: Ticker -> [Ticker]
shrinkValidTicker tickerValue =
  [ shrunkTicker
  | shrunk <- shrink (T.unpack (unTicker tickerValue))
  , not (null shrunk)
  , Right shrunkTicker <- [mkTicker (T.pack shrunk)]
  ]

genValidCustomerId :: Gen CustomerId
genValidCustomerId =
  suchThatMapGen validCustomerIdText (rightToMaybe . mkCustomerId)
  where
    validCustomerIdText :: Gen T.Text
    validCustomerIdText = do
      size <- choose (1, 16)
      T.pack <$> vectorOf size (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ "-_"))

shrinkValidCustomerId :: CustomerId -> [CustomerId]
shrinkValidCustomerId customerIdValue =
  [ shrunkCustomerId
  | shrunk <- shrink (T.unpack (unCustomerId customerIdValue))
  , not (null shrunk)
  , Right shrunkCustomerId <- [mkCustomerId (T.pack shrunk)]
  ]

genValidPercentage :: Gen Percentage
genValidPercentage =
  suchThatMapGen (choose (0.0, 1.0)) (rightToMaybe . mkPercentage)

shrinkValidPercentage :: Percentage -> [Percentage]
shrinkValidPercentage percentageValue =
  [ shrunkPercentage
  | shrunk <- shrink (unPercentage percentageValue)
  , Right shrunkPercentage <- [mkPercentage shrunk]
  ]

genValidPositiveMoney :: Gen PositiveMoney
genValidPositiveMoney =
  suchThatMapGen (choose (0.01, 1.0e9)) (rightToMaybe . mkPositiveMoney)

shrinkValidPositiveMoney :: PositiveMoney -> [PositiveMoney]
shrinkValidPositiveMoney moneyValue =
  [ shrunkMoney
  | shrunk <- shrink (unPositiveMoney moneyValue)
  , Right shrunkMoney <- [mkPositiveMoney shrunk]
  ]

genAssetClass :: Gen AssetClass
genAssetClass = elements [Equity, FixedIncome, Crypto, PrivateCredit, Fund, Cash]

genSector :: Gen Sector
genSector = elements [Financial, Energy, Utilities, Technology, Consumer, Other]

genCustomerProfile :: Gen CustomerProfile
genCustomerProfile = elements [Conservative, Moderate, Aggressive]

genCreditScore :: Gen CreditScore
genCreditScore = elements [LowScore, MediumScore, HighScore]

genCustomer :: Gen Customer
genCustomer =
  Customer
    <$> genValidCustomerId
    <*> genCustomerProfile
    <*> genCreditScore

genAssetWithTicker :: T.Text -> Gen Asset
genAssetWithTicker tickerText =
  case mkTicker tickerText of
    Right tickerValue -> Asset tickerValue <$> genAssetClass <*> genSector
    Left _ -> error ("genAssetWithTicker recebeu ticker invalido: " <> show tickerText)

genPosition :: Gen Position
genPosition = do
  assetValue <- genAssetWithTicker "TEST"
  percentageValue <- genValidPercentage
  pure Position {asset = assetValue, weight = percentageValue}