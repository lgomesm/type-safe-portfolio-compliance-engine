module Risk.Rules.AssetAllocationSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import qualified Test.QuickCheck as QC

import Domain.Asset (Asset (..), ticker)
import Domain.AssetClassification (AssetClass (Equity), Sector (Other))
import Domain.Percentage (mkPercentage)
import Domain.Portfolio (mkPortfolio)
import Domain.Position (Position (..))
import Domain.RawPortfolio (RawPortfolio (..))
import qualified Domain.Ticker as Ticker
import Risk.Policy (Policy (PrivateBankingPolicy), policyConfigFor)
import Risk.Rules.AssetAllocation
  ( AssetAllocationEvidence (..)
  , AssetAllocationObservation (..)
  , AssetAllocationRule (..)
  , assetAllocationRulePolicy
  , evaluateAssetAllocation
  , maxAssetAllocation
  , validateMaxAssetAllocation
  , validateMaxAssetAllocationWithPolicy
  )
import Risk.Violation (Violation (..))
import Risk.Rules.Types (RuleEvaluation (..), violationsOf)
import Risk.Rules.Input (PolicyPortfolioInput (..))
import Risk.Rules.Types (evaluateRule, evaluateViolations)
import Test.PercentageFixtures (percentageLiteral)
import Test.PortfolioFixtures (positionsOf)

spec :: Spec
spec = describe "Risk.Rules.AssetAllocation" $ do
  it "uses a named policy and portfolio input through the generic interface" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.40), ("ITUB4", 0.35), ("VALE3", 0.25)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue -> do
        let input = PolicyPortfolioInput (assetAllocationRulePolicy (policyConfigFor PrivateBankingPolicy)) portfolioValue
        expectAssetAllocationEvaluation (evaluateRule AssetAllocationRule input)
        evaluateViolations AssetAllocationRule input
          `shouldBe` validateMaxAssetAllocationWithPolicy (policyConfigFor PrivateBankingPolicy) portfolioValue
      Left err -> expectationFailure (show err)

  it "records observed allocations and the runtime policy limit in evidence" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.40), ("ITUB4", 0.35), ("VALE3", 0.25)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        case evaluateAssetAllocation (policyConfigFor PrivateBankingPolicy) portfolioValue of
          RuleEvaluation (AssetAllocationEvidence observations) foundViolations -> do
            length observations `shouldBe` 3
            case observations of
              firstObservation : _ -> do
                observedAllocation firstObservation `shouldBe` percentageLiteral 0.40
                allowedAllocation firstObservation `shouldBe` percentageLiteral 0.35
                foundViolations `shouldBe` violationsOf (evaluateAssetAllocation (policyConfigFor PrivateBankingPolicy) portfolioValue)
              [] -> expectationFailure "expected allocation observations"
      Left err -> expectationFailure (show err)

  it "aggregates duplicate tickers into one observation in first-occurrence order" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.20), ("ITUB4", 0.30), ("PETR4", 0.20), ("VALE3", 0.30)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        case evaluateAssetAllocation (policyConfigFor PrivateBankingPolicy) portfolioValue of
          RuleEvaluation (AssetAllocationEvidence observations) _ -> do
            map (ticker . observedAsset) observations
              `shouldBe` map unsafeTicker ["PETR4", "ITUB4", "VALE3"]
            map observedAllocation observations
              `shouldBe` map percentageLiteral [0.40, 0.30, 0.30]
      Left err -> expectationFailure (show err)

  it "reports the aggregated amount once when duplicate positions exceed the limit" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.20), ("PETR4", 0.20), ("VALE3", 0.30), ("ITUB4", 0.30)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxAssetAllocation portfolioValue
          `shouldBe`
            [ AssetAllocationExceeded
                (unsafeTicker "PETR4")
                maxAssetAllocation
                (percentageLiteral 0.40)
            ]
      Left err -> expectationFailure (show err)

  it "does not report a violation when the aggregated amount is exactly at the limit" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.15), ("PETR4", 0.15), ("ITUB4", 0.25), ("VALE3", 0.25), ("WEGE3", 0.20)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxAssetAllocation portfolioValue `shouldBe` []
      Left err -> expectationFailure (show err)

  it "maxAssetAllocation is a valid percentage literal" $
    mkPercentage 0.30 `shouldBe` Right maxAssetAllocation

  it "does not report violations when no asset exceeds 30%" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.25), ("ITUB4", 0.25), ("VALE3", 0.25), ("WEGE3", 0.25)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxAssetAllocation portfolioValue `shouldBe` []
      Left err ->
        expectationFailure (show err)

  it "reports one violation per asset that exceeds the limit" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.40), ("ITUB4", 0.35), ("VALE3", 0.25)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxAssetAllocation portfolioValue
          `shouldBe`
            [ AssetAllocationExceeded (unsafeTicker "PETR4") maxAssetAllocation (percentageLiteral 0.40)
            , AssetAllocationExceeded (unsafeTicker "ITUB4") maxAssetAllocation (percentageLiteral 0.35)
            ]
      Left err ->
        expectationFailure (show err)

  it "does not report a violation for an asset exactly at 30%" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.30), ("ITUB4", 0.30), ("VALE3", 0.20), ("WEGE3", 0.20)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxAssetAllocation portfolioValue `shouldBe` []
      Left err ->
        expectationFailure (show err)

  it "can approve a 35% concentration under PrivateBankingPolicy" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.35), ("ITUB4", 0.25), ("VALE3", 0.20), ("WEGE3", 0.20)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxAssetAllocationWithPolicy (policyConfigFor PrivateBankingPolicy) portfolioValue `shouldBe` []
      Left err ->
        expectationFailure (show err)

  it "applies the runtime policy limit after aggregating duplicate tickers" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.175), ("PETR4", 0.175), ("ITUB4", 0.25), ("VALE3", 0.20), ("WEGE3", 0.20)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxAssetAllocationWithPolicy (policyConfigFor PrivateBankingPolicy) portfolioValue
          `shouldBe` []
      Left err ->
        expectationFailure (show err)

  it "keeps concentration unchanged when one position is split into the same ticker" $
    QC.property $
      QC.forAll genSplitWeights $ \(firstPart, secondPart) ->
        let unsplitPortfolio =
              RawPortfolio
                [ testPosition "PETR4" (firstPart + secondPart)
                , testPosition "ITUB4" 0.25
                , testPosition "VALE3" 0.25
                , testPosition "WEGE3" (0.50 - firstPart - secondPart)
                ]
            splitPortfolio =
              RawPortfolio
                [ testPosition "PETR4" firstPart
                , testPosition "PETR4" secondPart
                , testPosition "ITUB4" 0.25
                , testPosition "VALE3" 0.25
                , testPosition "WEGE3" (0.50 - firstPart - secondPart)
                ]
         in case (mkPortfolio unsplitPortfolio, mkPortfolio splitPortfolio) of
              (Right unsplit, Right split) ->
                QC.property (validateMaxAssetAllocation unsplit == validateMaxAssetAllocation split)
              result ->
                QC.counterexample ("expected both generated portfolios to be valid: " <> show result) False

unsafeTicker :: String -> Ticker.Ticker
unsafeTicker tickerText =
  case Ticker.mkTicker (T.pack tickerText) of
    Right tickerValue -> tickerValue
    Left err -> error ("invalid test ticker literal: " <> show err)

expectAssetAllocationEvaluation :: RuleEvaluation AssetAllocationRule -> Expectation
expectAssetAllocationEvaluation (RuleEvaluation (AssetAllocationEvidence observations) _) =
  length observations `shouldBe` 3

genSplitWeights :: QC.Gen (Double, Double)
genSplitWeights = do
  total <- QC.choose (0.10, 0.50)
  firstPart <- QC.choose (0.00, total)
  pure (firstPart, total - firstPart)

testPosition :: String -> Double -> Position
testPosition tickerText rawWeight =
  Position
    { asset = Asset (unsafeTicker tickerText) Equity Other
    , weight = percentageLiteral rawWeight
    }