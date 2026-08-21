{-# LANGUAGE OverloadedStrings #-}

module Cli.ScenarioBatterySpec (spec) where

import Control.Monad (forM_)

import Cli.ScenarioInput
  ( CustomScenario (..)
  , ScenarioInputError (InvalidPositionField)
  , loadCustomScenario
  )
import Domain.Error (DomainError (..))
import Domain.Portfolio (mkPortfolio)
import Risk.Approval
  ( ApprovalDecision (..)
  , ApprovalLevel (..)
  , ApprovalReport (..)
  , evaluateApproval
  )
import Risk.Policy (Locality (Brazil), policyForLocality)
import Risk.Report
  ( PortfolioReport (..)
  , PortfolioStatus (..)
  , buildReportWithPolicy
  )
import Risk.RiskLevel (RiskLevel (..))
import Risk.Violation (Violation (..))
import Test.Hspec

data ViolationKind
  = AssetAllocationKind
  | SectorExposureKind
  | AssetClassExposureKind
  | DiversificationKind
  | ProfileCryptoKind
  | CreditRiskExposureKind
  | ProfileAssetClassKind
  deriving (Eq, Show)

data ExpectedEvaluation = ExpectedEvaluation
  { expectedStatus :: PortfolioStatus
  , expectedRisk :: RiskLevel
  , expectedDecision :: ApprovalDecision
  , expectedRequiredLevel :: ApprovalLevel
  , expectedViolations :: [ViolationKind]
  }

data BatteryCase = BatteryCase
  { caseNumber :: Int
  , caseFixture :: FilePath
  , caseExpectation :: ExpectedEvaluation
  }

spec :: Spec
spec = describe "JSON scenario regression battery" $ do
  describe "scenarios that reach the compliance engine" $
    forM_ evaluationCases $ \batteryCase ->
      it (caseLabel batteryCase) $ do
        scenarioResult <- loadCustomScenario (caseFixture batteryCase)
        scenario <- shouldBeRight scenarioResult

        portfolio <- shouldBeRight (mkPortfolio (customScenarioPortfolio scenario))

        let report =
              buildReportWithPolicy
                (policyForLocality Brazil)
                (customScenarioCustomer scenario)
                portfolio
            approval = evaluateApproval report
            expectation = caseExpectation batteryCase

        status report `shouldBe` expectedStatus expectation
        riskLevel report `shouldBe` expectedRisk expectation
        approvalDecision approval `shouldBe` expectedDecision expectation
        requiredLevel approval `shouldBe` expectedRequiredLevel expectation
        map violationKind (violations report)
          `shouldBe` expectedViolations expectation
        topViewIsAvailable (topPositions report)
          `shouldBe` (caseNumber batteryCase `notElem` [11, 12])

  describe "scenarios rejected at the JSON/domain boundary" $ do
    it "29 - rejects an out-of-range Percentage before a scenario exists" $ do
      result <- loadCustomScenario fixture29
      result
        `shouldBe` Left (InvalidPositionField 0 "weight" (PercentageOutOfRange 1.2))

    it "30 - accepts positions but rejects a portfolio whose weights sum to 90%" $ do
      result <- loadCustomScenario fixture30
      case result of
        Left err -> expectationFailure ("fixture 30 should decode: " <> show err)
        Right scenario ->
          case mkPortfolio (customScenarioPortfolio scenario) of
            Left (PortfolioWeightsDoNotSumToOne total) ->
              abs (total - 0.9) `shouldSatisfy` (< 1e-9)
            Left err -> expectationFailure ("unexpected domain error: " <> show err)
            Right _ -> expectationFailure "fixture 30 unexpectedly constructed a Portfolio"

fixture29 :: FilePath
fixture29 = "test/fixtures/battery/29-invalid-percentage.json"

fixture30 :: FilePath
fixture30 = "test/fixtures/battery/30-invalid-portfolio-weight-sum.json"

caseLabel :: BatteryCase -> String
caseLabel batteryCase =
  show (caseNumber batteryCase) <> " - " <> caseFixture batteryCase

shouldBeRight :: Show a => Either a b -> IO b
shouldBeRight result =
  case result of
    Left err -> fail ("expected Right, got Left " <> show err)
    Right value -> pure value

violationKind :: Violation -> ViolationKind
violationKind (AssetAllocationExceeded _ _ _) = AssetAllocationKind
violationKind (SectorExposureExceeded _ _ _) = SectorExposureKind
violationKind (AssetClassExposureExceeded _ _ _) = AssetClassExposureKind
violationKind (MinimumDiversificationNotMet _ _) = DiversificationKind
violationKind (CustomerProfileCryptoExceeded _ _ _) = ProfileCryptoKind
violationKind (CustomerCreditRiskExposureExceeded _ _ _) = CreditRiskExposureKind
violationKind (AssetClassExposureExceededForProfile _ _ _ _) = ProfileAssetClassKind

topViewIsAvailable :: Either errorValue topValue -> Bool
topViewIsAvailable (Left _) = False
topViewIsAvailable (Right _) = True

evaluationCases :: [BatteryCase]
evaluationCases =
  [ evaluated 1 "01-approved-balanced-multiclass.json" Approved Low AutoApproved Automatic []
  , evaluated 2 "02-approved-moderate-boundaries.json" Approved Low AutoApproved Automatic []
  , evaluated 3 "03-approved-aggressive-crypto-boundary.json" Approved Low AutoApproved Automatic []
  , evaluated 4 "04-approved-conservative-private-credit-boundary.json" Approved Low AutoApproved Automatic []
  , evaluated 5 "05-approved-moderate-private-credit-boundary.json" Approved Low AutoApproved Automatic []
  , evaluated 6 "06-approved-private-credit-global-boundary.json" Approved Low AutoApproved Automatic []
  , evaluated 7 "07-approved-sector-boundary.json" Approved Low AutoApproved Automatic []
  , evaluated 8 "08-approved-asset-boundary.json" Approved Low AutoApproved Automatic []
  , evaluated 9 "09-approved-low-score-boundary.json" Approved Low AutoApproved Automatic []
  , evaluated 10 "10-approved-non-risk-classes.json" Approved Low AutoApproved Automatic []
  , evaluated 11 "11-rejected-single-position.json" Rejected High (RequiresManualReview Manager) Manager
      [AssetAllocationKind, SectorExposureKind, DiversificationKind]
  , evaluated 12 "12-rejected-two-assets-under-diversified.json" Rejected High (RequiresManualReview Analyst) Analyst
      [AssetAllocationKind, AssetAllocationKind, DiversificationKind]
  , evaluated 13 "13-rejected-single-asset-over-limit.json" Rejected Medium (RequiresManualReview Analyst) Analyst
      [AssetAllocationKind]
  , evaluated 14 "14-rejected-sector-exposure.json" Rejected Medium (RequiresManualReview Manager) Manager
      [SectorExposureKind]
  , evaluated 15 "15-rejected-aggressive-crypto-eleven.json" Rejected High (RequiresManualReview Manager) Manager
      [AssetClassExposureKind, ProfileCryptoKind]
  , evaluated 16 "16-rejected-moderate-crypto-profile-only.json" Rejected Medium (RequiresManualReview Manager) Manager
      [ProfileCryptoKind]
  , evaluated 17 "17-rejected-conservative-any-crypto.json" Rejected Medium (RequiresManualReview Manager) Manager
      [ProfileCryptoKind]
  , evaluated 18 "18-rejected-low-score-risk-exposure.json" Rejected Medium (RequiresManualReview Manager) Manager
      [CreditRiskExposureKind]
  , evaluated 19 "19-rejected-medium-score-boundary-plus-one.json" Rejected Medium (RequiresManualReview Manager) Manager
      [CreditRiskExposureKind]
  , evaluated 20 "20-rejected-private-credit-global-only.json" Rejected Medium (RequiresManualReview CreditCommittee) CreditCommittee
      [AssetClassExposureKind]
  , evaluated 21 "21-rejected-conservative-private-credit-profile.json" Rejected Medium (RequiresManualReview CreditCommittee) CreditCommittee
      [ProfileAssetClassKind]
  , evaluated 22 "22-rejected-moderate-private-credit-profile.json" Rejected Medium (RequiresManualReview Manager) Manager
      [ProfileAssetClassKind]
  , evaluated 23 "23-rejected-private-credit-high-committee.json" Rejected High RejectedByPolicy CreditCommittee
      [AssetClassExposureKind, ProfileAssetClassKind]
  , evaluated 24 "24-rejected-multiple-suitability-violations.json" Rejected High RejectedByPolicy CreditCommittee
      [AssetClassExposureKind, AssetClassExposureKind, ProfileCryptoKind, CreditRiskExposureKind, ProfileAssetClassKind]
  , evaluated 25 "25-rejected-asset-and-sector.json" Rejected Medium (RequiresManualReview Manager) Manager
      [AssetAllocationKind, SectorExposureKind]
  , evaluated 26 "26-rejected-duplicate-tickers-diversification.json" Rejected High (RequiresManualReview Analyst) Analyst
      [AssetAllocationKind, AssetAllocationKind, DiversificationKind]
  , evaluated 27 "27-approved-exactly-three-distinct-tickers.json" Rejected Medium (RequiresManualReview Analyst) Analyst
      [AssetAllocationKind]
  , evaluated 28 "28-rejected-cross-class-sector-aggregation.json" Rejected Medium (RequiresManualReview Manager) Manager
      [SectorExposureKind]
  ]

evaluated :: Int -> FilePath -> PortfolioStatus -> RiskLevel -> ApprovalDecision -> ApprovalLevel -> [ViolationKind] -> BatteryCase
evaluated number fixture expectedStatusValue expectedRiskValue expectedDecisionValue expectedLevelValue expectedViolationKinds =
  BatteryCase
    { caseNumber = number
    , caseFixture = "test/fixtures/battery/" <> fixture
    , caseExpectation =
        ExpectedEvaluation
          { expectedStatus = expectedStatusValue
          , expectedRisk = expectedRiskValue
          , expectedDecision = expectedDecisionValue
          , expectedRequiredLevel = expectedLevelValue
          , expectedViolations = expectedViolationKinds
          }
    }