module Risk.Rules.DiversificationSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Domain.Portfolio (mkPortfolio)
import Risk.Rules.Diversification
  ( DiversificationEvidence (..)
  , DiversificationRule (..)
  , evaluateDiversification
  , minimumDistinctAssets
  , validateMinimumDiversification
  )
import Risk.Rules.Types (RuleEvaluation (..), evaluateRule, evaluateViolations, violationsOf)
import Generators.Portfolio (genAnyValidPortfolio)
import Risk.Violation (Violation (..))
import Test.PortfolioFixtures (positionsOf)

spec :: Spec
spec = describe "Risk.Rules.Diversification" $ do
  it "uses Portfolio as the associated input of DiversificationRule" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.50), ("ITUB4", 0.50)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue -> do
        expectDiversificationEvaluation (evaluateRule DiversificationRule portfolioValue)
        evaluateViolations DiversificationRule portfolioValue
          `shouldBe` validateMinimumDiversification portfolioValue
      Left err -> expectationFailure (show err)

  it "keeps distinct count and minimum inside typed evidence" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.50), ("ITUB4", 0.50)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        case evaluateDiversification portfolioValue of
          RuleEvaluation (DiversificationEvidence count minimumCount) foundViolations -> do
            count `shouldBe` 2
            minimumCount `shouldBe` 3
            foundViolations `shouldBe` [MinimumDiversificationNotMet minimumDistinctAssets 2]
      Left err -> expectationFailure (show err)

  it "produces evidence even when diversification is approved" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.34), ("ITUB4", 0.33), ("VALE3", 0.33)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        case evaluateDiversification portfolioValue of
          RuleEvaluation evidence foundViolations -> do
            distinctAssetCount evidence `shouldBe` 3
            foundViolations `shouldBe` []
      Left err -> expectationFailure (show err)

  it "keeps the legacy facade aligned with typed evaluation" $
    property $
      forAll genAnyValidPortfolio $ \portfolioValue ->
        validateMinimumDiversification portfolioValue
          == violationsOf (evaluateDiversification portfolioValue)

  it "reports a violation with only two distinct tickers" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.50), ("ITUB4", 0.50)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMinimumDiversification portfolioValue
          `shouldBe` [MinimumDiversificationNotMet minimumDistinctAssets 2]
      Left err ->
        expectationFailure (show err)

  it "does not report a violation with three distinct tickers" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.34), ("ITUB4", 0.33), ("VALE3", 0.33)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMinimumDiversification portfolioValue `shouldBe` []
      Left err ->
        expectationFailure (show err)

  it "does not count repeated tickers as new distinct assets" $ do
    rawPortfolio <-
      positionsOf
        [ ("PETR4", 0.25), ("PETR4", 0.25)
        , ("ITUB4", 0.25), ("ITUB4", 0.25)
        ]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMinimumDiversification portfolioValue
          `shouldBe` [MinimumDiversificationNotMet minimumDistinctAssets 2]
      Left err ->
        expectationFailure (show err)

expectDiversificationEvaluation :: RuleEvaluation DiversificationRule -> Expectation
expectDiversificationEvaluation (RuleEvaluation evidence _) =
  minimumDistinctAssetCount evidence `shouldBe` minimumDistinctAssets