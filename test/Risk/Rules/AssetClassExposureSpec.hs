module Risk.Rules.AssetClassExposureSpec (spec) where

import Test.Hspec

import Domain.AssetClassification (AssetClass (..))
import Test.PercentageFixtures (percentageLiteral)
import Domain.Portfolio (mkPortfolio)
import Risk.Policy (Policy (PrivateBankingPolicy), policyConfigFor)
import Risk.Rules.AssetClassExposure
  ( assetClassLimit
  , validateMaxAssetClassExposure
  , validateMaxAssetClassExposureWithPolicy
  )
import Risk.Violation (Violation (..))
import Test.PortfolioFixtures (positionsOfWithClass)

spec :: Spec
spec = describe "Risk.Rules.AssetClassExposure" $ do
  it "keeps the documented global limits for classes that have one" $ do
    assetClassLimit Crypto `shouldBe` Just (percentageLiteral 0.10)
    assetClassLimit PrivateCredit `shouldBe` Just (percentageLiteral 0.20)
    assetClassLimit Equity `shouldBe` Nothing

  it "reports a violation when crypto exposure exceeds 10%" $ do
    rawPortfolio <-
      positionsOfWithClass
        [ ("BTC", 0.25, "Crypto")
        , ("PETR4", 0.75, "Equity")
        ]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        length (validateMaxAssetClassExposure portfolioValue) `shouldBe` 1
      Left err ->
        expectationFailure (show err)

  it "does not report a violation when crypto exposure is exactly 10%" $ do
    rawPortfolio <-
      positionsOfWithClass
        [ ("BTC", 0.10, "Crypto")
        , ("PETR4", 0.90, "Equity")
        ]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxAssetClassExposure portfolioValue `shouldBe` []
      Left err ->
        expectationFailure (show err)

  it "reports a violation when private credit exposure exceeds 20%" $ do
    rawPortfolio <-
      positionsOfWithClass
        [ ("TESOURO", 0.45, "FixedIncome")
        , ("PETR4", 0.30, "Equity")
        , ("DEB001", 0.25, "PrivateCredit")
        ]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxAssetClassExposure portfolioValue
          `shouldBe`
            [ AssetClassExposureExceeded PrivateCredit (percentageLiteral 0.20) (percentageLiteral 0.25)
            ]
      Left err ->
        expectationFailure (show err)

  it "does not report a violation when private credit exposure is exactly 20%" $ do
    rawPortfolio <-
      positionsOfWithClass
        [ ("TESOURO", 0.50, "FixedIncome")
        , ("PETR4", 0.30, "Equity")
        , ("DEB001", 0.20, "PrivateCredit")
        ]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxAssetClassExposure portfolioValue `shouldBe` []
      Left err ->
        expectationFailure (show err)

  it "does not report a violation when there is no limited asset class exposure" $ do
    rawPortfolio <-
      positionsOfWithClass
        [ ("PETR4", 0.50, "Equity")
        , ("ITUB4", 0.50, "Equity")
        ]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxAssetClassExposure portfolioValue `shouldBe` []
      Left err ->
        expectationFailure (show err)

  it "accepts 12% crypto exposure under PrivateBankingPolicy" $ do
    rawPortfolio <-
      positionsOfWithClass
        [ ("BTC", 0.12, "Crypto")
        , ("TESOURO", 0.48, "FixedIncome")
        , ("PETR4", 0.20, "Equity")
        , ("WEGE3", 0.20, "Equity")
        ]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxAssetClassExposureWithPolicy (policyConfigFor PrivateBankingPolicy) portfolioValue `shouldBe` []
      Left err ->
        expectationFailure (show err)