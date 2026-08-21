module Examples.PortfoliosSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Domain.AssetClassification (AssetClass (Crypto, PrivateCredit))
import Domain.CustomerProfile (CustomerProfile (Aggressive, Conservative))
import Domain.Portfolio (mkPortfolio)
import Domain.Ticker (Ticker, mkTicker)
import Examples.Customers (aggressiveHighScore, conservativeHighScore)
import Examples.Portfolios
  ( approved
  , cryptoRisk
  , assetAllocationRisk
  , privateCreditGlobal
  , privateCreditProfile
  , underDiversified
  )
import Risk.Report (PortfolioStatus (..), buildReport, riskLevel, status, violations)
import Risk.RiskLevel (RiskLevel (Medium))
import Risk.SuitabilityPolicy (defaultSuitabilityPolicy)
import Risk.Violation (Violation (..))
import Test.PercentageFixtures (percentageLiteral)

spec :: Spec
spec = describe "Examples.Portfolios" $ do
  it "'approved' is structurally valid and passes compliance" $
    case mkPortfolio approved of
      Left err ->
        expectationFailure ("scenario 'approved' is invalid: " <> show err)
      Right portfolioValue -> do
        let report = buildReport defaultSuitabilityPolicy conservativeHighScore portfolioValue
        status report `shouldBe` Approved
        violations report `shouldBe` []

  it "'asset-allocation-risk' produces the expected asset allocation violations" $
    case mkPortfolio assetAllocationRisk of
      Left err ->
        expectationFailure ("scenario 'asset-allocation-risk' is invalid: " <> show err)
      Right portfolioValue -> do
        let report = buildReport defaultSuitabilityPolicy aggressiveHighScore portfolioValue
        status report `shouldBe` Rejected
        riskLevel report `shouldBe` Medium
        violations report
          `shouldBe`
            [ AssetAllocationExceeded (unsafeTicker "PETR4") (percentageLiteral 0.30) (percentageLiteral 0.40)
            , AssetAllocationExceeded (unsafeTicker "ITUB4") (percentageLiteral 0.30) (percentageLiteral 0.35)
            ]

  it "'crypto-risk' accumulates rule and suitability crypto violations" $
    case mkPortfolio cryptoRisk of
      Left err ->
        expectationFailure ("scenario 'crypto-risk' is invalid: " <> show err)
      Right portfolioValue -> do
        let report = buildReport defaultSuitabilityPolicy aggressiveHighScore portfolioValue
        status report `shouldBe` Rejected
        violations report
          `shouldBe`
            [ AssetClassExposureExceeded Crypto (percentageLiteral 0.10) (percentageLiteral 0.25)
            , CustomerProfileCryptoExceeded Aggressive (percentageLiteral 0.10) (percentageLiteral 0.25)
            ]

  it "'under-diversified' accumulates allocation and diversification violations" $
    case mkPortfolio underDiversified of
      Left err ->
        expectationFailure ("scenario 'under-diversified' is invalid: " <> show err)
      Right portfolioValue -> do
        let report = buildReport defaultSuitabilityPolicy conservativeHighScore portfolioValue
        status report `shouldBe` Rejected
        violations report
          `shouldBe`
            [ AssetAllocationExceeded (unsafeTicker "PETR4") (percentageLiteral 0.30) (percentageLiteral 0.50)
            , AssetAllocationExceeded (unsafeTicker "ITUB4") (percentageLiteral 0.30) (percentageLiteral 0.50)
            , MinimumDiversificationNotMet 3 2
            ]

  it "'private-credit-global' violates only the global private credit limit" $
    case mkPortfolio privateCreditGlobal of
      Left err ->
        expectationFailure ("scenario 'private-credit-global' is invalid: " <> show err)
      Right portfolioValue -> do
        let report = buildReport defaultSuitabilityPolicy aggressiveHighScore portfolioValue
        status report `shouldBe` Rejected
        violations report
          `shouldBe`
            [ AssetClassExposureExceeded PrivateCredit (percentageLiteral 0.20) (percentageLiteral 0.25)
            ]

  it "'private-credit-profile' violates only the profile private credit limit" $
    case mkPortfolio privateCreditProfile of
      Left err ->
        expectationFailure ("scenario 'private-credit-profile' is invalid: " <> show err)
      Right portfolioValue -> do
        let report = buildReport defaultSuitabilityPolicy conservativeHighScore portfolioValue
        status report `shouldBe` Rejected
        violations report
          `shouldBe`
            [ AssetClassExposureExceededForProfile PrivateCredit Conservative (percentageLiteral 0.05) (percentageLiteral 0.15)
            ]

unsafeTicker :: String -> Ticker
unsafeTicker tickerText =
  case mkTicker (T.pack tickerText) of
    Right tickerValue -> tickerValue
    Left err -> error ("invalid test ticker literal: " <> show err)