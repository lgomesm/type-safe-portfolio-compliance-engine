{-# LANGUAGE OverloadedStrings #-}

module Risk.Rules.PrivateCreditSuitabilitySpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Domain.CreditScore (CreditScore (HighScore))
import Domain.Customer (Customer (..))
import Domain.CustomerId (CustomerId, mkCustomerId)
import Domain.CustomerProfile (CustomerProfile (..))
import Domain.AssetClassification (AssetClass (PrivateCredit))
import Test.PercentageFixtures (percentageLiteral)
import Domain.Portfolio (mkPortfolio)
import Risk.Rules.PrivateCreditSuitability
  ( privateCreditLimitByProfile
  , validatePrivateCreditByProfile
  )
import Risk.Violation (Violation (..))
import Test.PortfolioFixtures (positionsOfWithClass)

spec :: Spec
spec = describe "Risk.Rules.PrivateCreditSuitability" $ do
  it "reflects the documented limits for each customer profile" $ do
    privateCreditLimitByProfile Conservative `shouldBe` percentageLiteral 0.05
    privateCreditLimitByProfile Moderate `shouldBe` percentageLiteral 0.15
    privateCreditLimitByProfile Aggressive `shouldBe` percentageLiteral 0.25

  it "reports a profile violation for conservative clients with 15% in private credit" $ do
    rawPortfolio <-
      positionsOfWithClass
        [ ("TESOURO", 0.50, "FixedIncome")
        , ("CDB001", 0.35, "FixedIncome")
        , ("DEB001", 0.15, "PrivateCredit")
        ]
    case mkPortfolio rawPortfolio of
      Left err ->
        expectationFailure (show err)
      Right portfolioValue ->
        validatePrivateCreditByProfile conservativeCustomer portfolioValue
          `shouldBe`
            [ AssetClassExposureExceededForProfile PrivateCredit Conservative (percentageLiteral 0.05) (percentageLiteral 0.15)
            ]

  it "does not report a violation for aggressive clients with 20% in private credit" $ do
    rawPortfolio <-
      positionsOfWithClass
        [ ("TESOURO", 0.50, "FixedIncome")
        , ("DEB001", 0.20, "PrivateCredit")
        , ("PETR4", 0.30, "Equity")
        ]
    case mkPortfolio rawPortfolio of
      Left err ->
        expectationFailure (show err)
      Right portfolioValue ->
        validatePrivateCreditByProfile aggressiveCustomer portfolioValue `shouldBe` []

  it "does not report a violation when there is no private credit exposure" $ do
    rawPortfolio <-
      positionsOfWithClass
        [ ("TESOURO", 0.60, "FixedIncome")
        , ("PETR4", 0.40, "Equity")
        ]
    case mkPortfolio rawPortfolio of
      Left err ->
        expectationFailure (show err)
      Right portfolioValue ->
        validatePrivateCreditByProfile conservativeCustomer portfolioValue `shouldBe` []

conservativeCustomer :: Customer
conservativeCustomer =
  Customer
    { customerId = unsafeCustomerId "conservative-private-credit"
    , customerProfile = Conservative
    , creditScore = HighScore
    }

aggressiveCustomer :: Customer
aggressiveCustomer =
  Customer
    { customerId = unsafeCustomerId "aggressive-private-credit"
    , customerProfile = Aggressive
    , creditScore = HighScore
    }

unsafeCustomerId :: Text -> CustomerId
unsafeCustomerId rawCustomerId =
  case mkCustomerId rawCustomerId of
    Right customerIdValue -> customerIdValue
    Left err -> error ("invalid test customer id literal: " <> show err)