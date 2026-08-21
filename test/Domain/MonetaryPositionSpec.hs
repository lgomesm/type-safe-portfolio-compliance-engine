{-# LANGUAGE OverloadedStrings #-}

module Domain.MonetaryPositionSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Text as T
import Domain.Asset (Asset (..))
import Domain.AssetClassification (AssetClass (..), Sector (..))
import Domain.Error (DomainError (..))
import Domain.Money (mkPositiveMoney)
import Domain.MonetaryPosition
  ( MonetaryPosition (..)
  , monetaryPositionsToRawPortfolio
  )
import Domain.Percentage (unPercentage)
import Domain.Position (Position (..))
import Domain.RawPortfolio (RawPortfolio (..))
import Domain.Ticker (mkTicker, unTicker)

spec :: Spec
spec = describe "Domain.MonetaryPosition" $ do
  it "normalizes monetary amounts into portfolio weights" $ do
    weightsFor [30000, 30000, 20000, 20000]
      `shouldBe` Right [0.30, 0.30, 0.20, 0.20]

  it "normalizes a single positive amount to a total allocation of one" $ do
    weightsFor [1250] `shouldBe` Right [1.0]

  it "represents an empty monetary input as an empty raw portfolio" $ do
    monetaryPositionsToRawPortfolio [] `shouldBe` Right (RawPortfolio [])

  it "rejects a total amount that overflows Double" $ do
    case monetaryPositionsToRawPortfolio
        [ monetaryPosition "PETR4" 1.0e308
        , monetaryPosition "ITUB4" 1.0e308
        ] of
      Left (NonFiniteMoney totalAmount) -> totalAmount `shouldSatisfy` isInfinite
      other -> expectationFailure ("expected a finite-total error, got " <> show other)

  it "preserves the association between each asset and its normalized amount" $ do
    case monetaryPositionsToRawPortfolio
        [ monetaryPosition "PETR4" 30
        , monetaryPosition "ITUB4" 70
        ] of
      Right (RawPortfolio positions) -> do
        map (unTicker . ticker . asset) positions `shouldBe` ["PETR4", "ITUB4"]
        map (unPercentage . weight) positions `shouldBe` [0.3, 0.7]
      Left err -> expectationFailure (show err)

  it "keeps the same allocation when all monetary amounts are scaled equally" $
    forAll (vectorOf 4 (choose (0.01, 100000))) $ \amounts ->
      forAll (choose (0.1, 1000)) $ \scaleFactor ->
        let tickers = ["PETR4", "ITUB4", "VALE3", "WEGE3"]
            original = zipWith monetaryPosition tickers amounts
            scaled = zipWith monetaryPosition tickers (map (* scaleFactor) amounts)
            originalWeights = normalizedWeights original
            scaledWeights = normalizedWeights scaled
        in counterexample (show (originalWeights, scaledWeights))
             (and (zipWith closeEnough originalWeights scaledWeights))

weightsFor :: [Double] -> Either String [Double]
weightsFor amounts =
  case monetaryPositionsToRawPortfolio (zipWith monetaryPosition tickers amounts) of
    Left err -> Left (show err)
    Right (RawPortfolio positions) -> Right (map (unPercentage . weight) positions)
  where
    tickers = ["PETR4", "ITUB4", "VALE3", "WEGE3"]

normalizedWeights :: [MonetaryPosition] -> [Double]
normalizedWeights positions =
  case monetaryPositionsToRawPortfolio positions of
    Left err -> error (show err)
    Right (RawPortfolio normalizedPositions) ->
      map (unPercentage . weight) normalizedPositions

closeEnough :: Double -> Double -> Bool
closeEnough first second = abs (first - second) <= 1e-12

monetaryPosition :: String -> Double -> MonetaryPosition
monetaryPosition tickerText amountValue =
  case (mkTicker (T.pack tickerText), mkPositiveMoney amountValue) of
    (Right tickerValue, Right moneyValue) ->
      MonetaryPosition
        { monetaryAsset =
            Asset
              { ticker = tickerValue
              , assetClass = Equity
              , sector = Other
              }
        , monetaryAmount = moneyValue
        }
    other -> error ("invalid monetary test input: " <> show other)