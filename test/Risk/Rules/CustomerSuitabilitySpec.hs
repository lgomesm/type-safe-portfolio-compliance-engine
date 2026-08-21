{-# LANGUAGE OverloadedStrings #-}

module Risk.Rules.CustomerSuitabilitySpec (spec) where

import Data.Text (pack)
import Test.Hspec

import Domain.CreditScore (CreditScore (..))
import Domain.Customer (Customer (..))
import Domain.CustomerId (CustomerId, mkCustomerId)
import Domain.CustomerProfile (CustomerProfile (..))
import Test.PercentageFixtures (percentageLiteral)
import Domain.Portfolio (mkPortfolio)
import Risk.Rules.CustomerSuitability (validateCustomerSuitability)
import Risk.SuitabilityPolicy (defaultSuitabilityPolicy)
import Risk.Violation (Violation (..))
import Test.PortfolioFixtures (positionsOfWithClass)

spec :: Spec
spec = describe "Risk.Rules.CustomerSuitability" $ do
  it "rejects crypto exposure above the profile limit" $ do
    rawPortfolio <-
      positionsOfWithClass
        [ ("BTC", 0.25, "Crypto")
        , ("PETR4", 0.75, "Equity")
        ]
    case mkPortfolio rawPortfolio of
      Left err ->
        expectationFailure (show err)
      Right portfolioValue ->
        validateCustomerSuitability defaultSuitabilityPolicy conservativeHighScore portfolioValue
          `shouldBe`
            [ CustomerProfileCryptoExceeded Conservative (percentageLiteral 0.00) (percentageLiteral 0.25)
            ]

  it "rejects total equity plus crypto exposure above the score limit" $ do
    rawPortfolio <-
      positionsOfWithClass
        [ ("PETR4", 0.50, "Equity")
        , ("TESOURO", 0.50, "FixedIncome")
        ]
    case mkPortfolio rawPortfolio of
      Left err ->
        expectationFailure (show err)
      Right portfolioValue ->
        validateCustomerSuitability defaultSuitabilityPolicy lowScoreCustomer portfolioValue
          `shouldBe`
            [ CustomerCreditRiskExposureExceeded LowScore (percentageLiteral 0.40) (percentageLiteral 0.50)
            ]

  it "accepts a portfolio that fits both profile and score limits" $ do
    rawPortfolio <-
      positionsOfWithClass
        [ ("BTC", 0.05, "Crypto")
        , ("TESOURO", 0.95, "FixedIncome")
        ]
    case mkPortfolio rawPortfolio of
      Left err ->
        expectationFailure (show err)
      Right portfolioValue ->
        validateCustomerSuitability defaultSuitabilityPolicy moderateHighScore portfolioValue
          `shouldBe` []

conservativeHighScore :: Customer
conservativeHighScore =
  Customer
    { customerId = unsafeCustomerId "conservative-high"
    , customerProfile = Conservative
    , creditScore = HighScore
    }

lowScoreCustomer :: Customer
lowScoreCustomer =
  Customer
    { customerId = unsafeCustomerId "low-score"
    , customerProfile = Aggressive
    , creditScore = LowScore
    }

moderateHighScore :: Customer
moderateHighScore =
  Customer
    { customerId = unsafeCustomerId "moderate-high"
    , customerProfile = Moderate
    , creditScore = HighScore
    }

unsafeCustomerId :: String -> CustomerId
unsafeCustomerId rawCustomerId =
  case mkCustomerId (pack rawCustomerId) of
    Right customerIdValue -> customerIdValue
    Left err -> error ("invalid test customer id literal: " <> show err)