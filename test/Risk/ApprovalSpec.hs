{-# LANGUAGE OverloadedStrings #-}

module Risk.ApprovalSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Domain.AssetClassification (AssetClass (Crypto, PrivateCredit), Sector (Energy))
import Domain.CustomerProfile (CustomerProfile (Conservative))
import Domain.Portfolio (mkPortfolio)
import qualified Domain.Ticker
import Examples.Customers (conservativeHighScore)
import Examples.Scenarios
  ( Scenario (..)
  , resolveScenarioPolicy
  , scenarios
  )
import Generators.Domain (genCustomer)
import Generators.Portfolio (genAnyValidPortfolio)
import Risk.Approval
  ( ApprovalCase
  , ApprovalDecision (..)
  , ApprovalEvaluation (..)
  , ApprovalLevel (..)
  , ApprovalReport (..)
  , ApprovalTransition
      ( CompleteAnalystReview
      , CompleteCreditCommitteeReview
      , CompleteManagerReview
      , RejectAnalystReview
      , RejectCreditCommitteeReview
      , RejectManagerReview
      )
  , Approved
  , AwaitingAnalyst
  , AwaitingCreditCommittee
  , AwaitingManager
  , Rejected
  , applyTransition
  , approvalCaseReport
  , beginApproval
  , completeAnalystReview
  , completeCreditCommitteeReview
  , completeManagerReview
  , evaluateApproval
  , evaluateApprovalCase
  , rejectAnalystReview
  , rejectCreditCommitteeReview
  , rejectManagerReview
  , renderApprovalReport
  , requiredLevelForViolation
  )
import Risk.Report (PortfolioReport (..), buildReport, buildReportWithPolicy, riskLevel, violations)
import Risk.RiskLevel (RiskLevel (High, Medium))
import Risk.SuitabilityPolicy (defaultSuitabilityPolicy)
import Risk.Violation (Violation (..))
import Test.PortfolioFixtures (positionsOfWithClass)
import Test.PercentageFixtures (percentageLiteral)

spec :: Spec
spec = describe "Risk.Approval" $ do
  describe "requiredLevelForViolation" $ do
    it "maps diversification and asset allocation violations to Analyst" $
      requiredLevelForViolation (MinimumDiversificationNotMet 3 2)
        `shouldBe` Analyst

    it "maps sector and crypto exposure violations to Manager" $ do
      requiredLevelForViolation (SectorExposureExceeded Energy (percentageLiteral 0.50) (percentageLiteral 0.60))
        `shouldBe` Manager
      requiredLevelForViolation (AssetClassExposureExceeded Crypto (percentageLiteral 0.10) (percentageLiteral 0.25))
        `shouldBe` Manager

    it "maps private credit global and conservative profile violations to CreditCommittee" $ do
      requiredLevelForViolation (AssetClassExposureExceeded PrivateCredit (percentageLiteral 0.20) (percentageLiteral 0.25))
        `shouldBe` CreditCommittee
      requiredLevelForViolation
        (AssetClassExposureExceededForProfile PrivateCredit Conservative (percentageLiteral 0.05) (percentageLiteral 0.15))
        `shouldBe` CreditCommittee

  describe "evaluateApproval on example scenarios" $ do
    it "auto-approves the approved scenario" $
      approvalDecisionFor "approved" `shouldBe` AutoApproved

    it "routes asset-allocation-risk to analyst review" $
      approvalDecisionFor "asset-allocation-risk" `shouldBe` RequiresManualReview Analyst

    it "routes crypto-risk to manager review" $
      approvalDecisionFor "crypto-risk" `shouldBe` RequiresManualReview Manager

    it "routes under-diversified to analyst review" $
      approvalDecisionFor "under-diversified" `shouldBe` RequiresManualReview Analyst

    it "routes private-credit-global to credit committee review" $
      approvalDecisionFor "private-credit-global" `shouldBe` RequiresManualReview CreditCommittee

    it "routes private-credit-profile to credit committee review" $
      approvalDecisionFor "private-credit-profile" `shouldBe` RequiresManualReview CreditCommittee

    it "routes conservative-crypto to manager review" $
      approvalDecisionFor "conservative-crypto" `shouldBe` RequiresManualReview Manager

  describe "policy cut-off" $
    it "rejects by policy when committee escalation and high risk happen together" $ do
      rawPortfolio <-
        positionsOfWithClass
          [ ("DEB001", 0.30, "PrivateCredit")
          , ("PETR4", 0.35, "Equity")
          , ("ITUB4", 0.35, "Equity")
          ]
      case mkPortfolio rawPortfolio of
        Left err ->
          expectationFailure (show err)
        Right portfolioValue -> do
          let approvalReport =
                evaluateApproval
                  (buildReport defaultSuitabilityPolicy conservativeHighScore portfolioValue)
          approvalDecision approvalReport `shouldBe` RejectedByPolicy
          requiredLevel approvalReport `shouldBe` CreditCommittee
          riskLevel (originalReport approvalReport) `shouldBe` High

  describe "renderApprovalReport" $
    it "includes approval decision and required level in the rendered output" $
      case Map.lookup "private-credit-profile" scenarios of
        Nothing ->
          expectationFailure "missing scenario: private-credit-profile"
        Just scenario ->
          case ( resolveScenarioPolicy Nothing (scenarioPolicySource scenario)
               , mkPortfolio (scenarioPortfolio scenario)
               ) of
            (Right policyConfig, Right portfolioValue) -> do
              let rendered =
                    renderApprovalReport
                      (evaluateApproval (buildReportWithPolicy policyConfig (scenarioCustomer scenario) portfolioValue))
              rendered `shouldSatisfy` T.isInfixOf "Approval Decision: RequiresManualReview (CreditCommittee)"
              rendered `shouldSatisfy` T.isInfixOf "Required Level: CreditCommittee"
            (Left err, _) ->
              expectationFailure ("could not resolve test policy: " <> show err)
            (_, Left err) ->
              expectationFailure (show err)

  describe "phantom type workflow" $ do
    it "classifies a compliant report as ApprovalCase Approved" $ do
      let report = portfolioReportFor "approved"
      case evaluateApprovalCase (beginApproval report) of
        AutomaticallyApproved approvedCase ->
          expectApproved report approvedCase
        other ->
          expectationFailure ("expected AutomaticallyApproved, got " <> show other)

    it "classifies analyst-level violations as AwaitingAnalyst" $ do
      let report = portfolioReportFor "asset-allocation-risk"
      case evaluateApprovalCase (beginApproval report) of
        AnalystReviewRequired analystCase ->
          approvalCaseReport analystCase `shouldBe` report
        other ->
          expectationFailure ("expected AnalystReviewRequired, got " <> show other)

    it "classifies manager-level violations as AwaitingManager" $ do
      let report = portfolioReportFor "crypto-risk"
      case evaluateApprovalCase (beginApproval report) of
        ManagerReviewRequired managerCase ->
          approvalCaseReport managerCase `shouldBe` report
        other ->
          expectationFailure ("expected ManagerReviewRequired, got " <> show other)

    it "classifies private credit violations as AwaitingCreditCommittee" $ do
      let report = portfolioReportFor "private-credit-profile"
      case evaluateApprovalCase (beginApproval report) of
        CreditCommitteeReviewRequired committeeCase ->
          approvalCaseReport committeeCase `shouldBe` report
        other ->
          expectationFailure ("expected CreditCommitteeReviewRequired, got " <> show other)

    it "classifies the policy cut-off as ApprovalCase Rejected" $ do
      rawPortfolio <-
        positionsOfWithClass
          [ ("DEB001", 0.30, "PrivateCredit")
          , ("PETR4", 0.35, "Equity")
          , ("ITUB4", 0.35, "Equity")
          ]
      case mkPortfolio rawPortfolio of
        Left err ->
          expectationFailure (show err)
        Right portfolioValue -> do
          let report = buildReport defaultSuitabilityPolicy conservativeHighScore portfolioValue
          case evaluateApprovalCase (beginApproval report) of
            PolicyRejected rejectedCase ->
              expectRejected report rejectedCase
            other ->
              expectationFailure ("expected PolicyRejected, got " <> show other)

    it "keeps credit committee as the highest required level" $ do
      let baseReport = portfolioReportFor "approved"
          mixedReport =
            baseReport
              { riskLevel = Medium
              , violations =
                  [ AssetAllocationExceeded (unsafeTicker "PETR4") (percentageLiteral 0.30) (percentageLiteral 0.40)
                  , SectorExposureExceeded Energy (percentageLiteral 0.50) (percentageLiteral 0.60)
                  , AssetClassExposureExceeded PrivateCredit (percentageLiteral 0.20) (percentageLiteral 0.25)
                  ]
              }
      case evaluateApprovalCase (beginApproval mixedReport) of
        CreditCommitteeReviewRequired committeeCase ->
          approvalCaseReport committeeCase `shouldBe` mixedReport
        other ->
          expectationFailure ("expected CreditCommitteeReviewRequired, got " <> show other)

    it "promotes AwaitingAnalyst to Approved after analyst review" $ do
      let report = portfolioReportFor "asset-allocation-risk"
      case evaluateApprovalCase (beginApproval report) of
        AnalystReviewRequired analystCase ->
          expectApproved report (completeAnalystReview analystCase)
        _ ->
          expectationFailure "expected analyst review"

    it "promotes AwaitingManager to Approved after manager review" $ do
      let report = portfolioReportFor "crypto-risk"
      case evaluateApprovalCase (beginApproval report) of
        ManagerReviewRequired managerCase ->
          expectApproved report (completeManagerReview managerCase)
        _ ->
          expectationFailure "expected manager review"

    it "promotes AwaitingCreditCommittee to Approved after committee review" $ do
      let report = portfolioReportFor "private-credit-profile"
      case evaluateApprovalCase (beginApproval report) of
        CreditCommitteeReviewRequired committeeCase ->
          expectApproved report (completeCreditCommitteeReview committeeCase)
        _ ->
          expectationFailure "expected credit committee review"

    it "rejects AwaitingAnalyst after analyst review" $ do
      let report = portfolioReportFor "asset-allocation-risk"
      case evaluateApprovalCase (beginApproval report) of
        AnalystReviewRequired analystCase ->
          expectRejected report (rejectAnalystReview analystCase)
        _ ->
          expectationFailure "expected analyst review"

    it "rejects AwaitingManager after manager review" $ do
      let report = portfolioReportFor "crypto-risk"
      case evaluateApprovalCase (beginApproval report) of
        ManagerReviewRequired managerCase ->
          expectRejected report (rejectManagerReview managerCase)
        _ ->
          expectationFailure "expected manager review"

    it "rejects AwaitingCreditCommittee after committee review" $ do
      let report = portfolioReportFor "private-credit-profile"
      case evaluateApprovalCase (beginApproval report) of
        CreditCommitteeReviewRequired committeeCase ->
          expectRejected report (rejectCreditCommitteeReview committeeCase)
        _ ->
          expectationFailure "expected credit committee review"

  describe "GADT approval transitions" $ do
    it "preserves automatic approval through the pending transition" $ do
      let report = portfolioReportFor "approved"
      case evaluateApprovalCase (beginApproval report) of
        AutomaticallyApproved approvedCase ->
          expectApproved report approvedCase
        other ->
          expectationFailure ("expected AutomaticallyApproved, got " <> show other)

    it "produces a case typed as AwaitingAnalyst" $ do
      let report = portfolioReportFor "asset-allocation-risk"
      case evaluateApprovalCase (beginApproval report) of
        AnalystReviewRequired analystCase ->
          expectAwaitingAnalyst report analystCase
        other ->
          expectationFailure ("expected AnalystReviewRequired, got " <> show other)

    it "produces a case typed as AwaitingManager" $ do
      let report = portfolioReportFor "crypto-risk"
      case evaluateApprovalCase (beginApproval report) of
        ManagerReviewRequired managerCase ->
          expectAwaitingManager report managerCase
        other ->
          expectationFailure ("expected ManagerReviewRequired, got " <> show other)

    it "produces a case typed as AwaitingCreditCommittee" $ do
      let report = portfolioReportFor "private-credit-profile"
      case evaluateApprovalCase (beginApproval report) of
        CreditCommitteeReviewRequired committeeCase ->
          expectAwaitingCreditCommittee report committeeCase
        other ->
          expectationFailure ("expected CreditCommitteeReviewRequired, got " <> show other)

    it "applies the GADT transition from Analyst to Approved" $ do
      let report = portfolioReportFor "asset-allocation-risk"
      case evaluateApprovalCase (beginApproval report) of
        AnalystReviewRequired analystCase ->
          expectApproved report (applyTransition CompleteAnalystReview analystCase)
        _ ->
          expectationFailure "expected analyst review"

    it "applies the GADT transition from Manager to Approved" $ do
      let report = portfolioReportFor "crypto-risk"
      case evaluateApprovalCase (beginApproval report) of
        ManagerReviewRequired managerCase ->
          expectApproved report (applyTransition CompleteManagerReview managerCase)
        _ ->
          expectationFailure "expected manager review"

    it "applies the GADT transition from CreditCommittee to Approved" $ do
      let report = portfolioReportFor "private-credit-profile"
      case evaluateApprovalCase (beginApproval report) of
        CreditCommitteeReviewRequired committeeCase ->
          expectApproved report (applyTransition CompleteCreditCommitteeReview committeeCase)
        _ ->
          expectationFailure "expected credit committee review"

    it "applies the GADT rejection transition from Analyst" $ do
      let report = portfolioReportFor "asset-allocation-risk"
      case evaluateApprovalCase (beginApproval report) of
        AnalystReviewRequired analystCase ->
          expectRejected report (applyTransition RejectAnalystReview analystCase)
        _ ->
          expectationFailure "expected analyst review"

    it "applies the GADT rejection transition from Manager" $ do
      let report = portfolioReportFor "crypto-risk"
      case evaluateApprovalCase (beginApproval report) of
        ManagerReviewRequired managerCase ->
          expectRejected report (applyTransition RejectManagerReview managerCase)
        _ ->
          expectationFailure "expected manager review"

    it "applies the GADT rejection transition from CreditCommittee" $ do
      let report = portfolioReportFor "private-credit-profile"
      case evaluateApprovalCase (beginApproval report) of
        CreditCommitteeReviewRequired committeeCase ->
          expectRejected report (applyTransition RejectCreditCommitteeReview committeeCase)
        _ ->
          expectationFailure "expected credit committee review"

    it "keeps the Phantom Type API aligned with the GADT transition" $ do
      let report = portfolioReportFor "asset-allocation-risk"
      case evaluateApprovalCase (beginApproval report) of
        AnalystReviewRequired analystCase ->
          approvalCaseReport (completeAnalystReview analystCase)
            `shouldBe` approvalCaseReport (applyTransition CompleteAnalystReview analystCase)
        _ ->
          expectationFailure "expected analyst review"

    it "keeps the rejection helper aligned with the GADT transition" $ do
      let report = portfolioReportFor "asset-allocation-risk"
      case evaluateApprovalCase (beginApproval report) of
        AnalystReviewRequired analystCase ->
          approvalCaseReport (rejectAnalystReview analystCase)
            `shouldBe` approvalCaseReport (applyTransition RejectAnalystReview analystCase)
        _ ->
          expectationFailure "expected analyst review"

  describe "properties" $ do
    it "auto-approved implies no violations" $
      forAll genCustomer $ \customerValue ->
        forAll genAnyValidPortfolio $ \portfolioValue ->
          let approvalReport =
                evaluateApproval
                  (buildReport defaultSuitabilityPolicy customerValue portfolioValue)
           in case approvalDecision approvalReport of
                AutoApproved ->
                  counterexample "AutoApproved report carried violations" $
                    null (violations (originalReport approvalReport))
                _ -> property True

    it "rejected-by-policy implies high risk and committee level" $
      forAll genCustomer $ \customerValue ->
        forAll genAnyValidPortfolio $ \portfolioValue ->
          let approvalReport =
                evaluateApproval
                  (buildReport defaultSuitabilityPolicy customerValue portfolioValue)
           in case approvalDecision approvalReport of
                RejectedByPolicy ->
                  counterexample "RejectedByPolicy without high risk and committee level" $
                    riskLevel (originalReport approvalReport) == High
                      .&&. requiredLevel approvalReport == CreditCommittee
                _ -> property True

    it "aggregated level is monotonic over sublists of violations" $
      forAll genCustomer $ \customerValue ->
        forAll genAnyValidPortfolio $ \portfolioValue ->
          let report = buildReport defaultSuitabilityPolicy customerValue portfolioValue
              fullViolations = violations report
              prefixViolations = take (length fullViolations `div` 2) fullViolations
           in classify (null fullViolations) "without violations" $
                aggregateLevel fullViolations >= aggregateLevel prefixViolations

approvalDecisionFor :: T.Text -> ApprovalDecision
approvalDecisionFor scenarioName =
  case Map.lookup scenarioName scenarios of
    Nothing ->
      error ("missing scenario in test: " <> T.unpack scenarioName)
    Just scenario ->
      case (resolveScenarioPolicy Nothing (scenarioPolicySource scenario), mkPortfolio (scenarioPortfolio scenario)) of
        (Right policyConfig, Right portfolioValue) ->
          approvalDecision
            (evaluateApproval (buildReportWithPolicy policyConfig (scenarioCustomer scenario) portfolioValue))
        (Left err, _) ->
          error ("could not resolve test policy: " <> show err)
        (_, Left err) ->
          error ("invalid test scenario: " <> show err)

portfolioReportFor :: T.Text -> PortfolioReport
portfolioReportFor scenarioName =
  case Map.lookup scenarioName scenarios of
    Nothing ->
      error ("missing scenario in test: " <> T.unpack scenarioName)
    Just scenario ->
      case (resolveScenarioPolicy Nothing (scenarioPolicySource scenario), mkPortfolio (scenarioPortfolio scenario)) of
        (Right policyConfig, Right portfolioValue) ->
          buildReportWithPolicy policyConfig (scenarioCustomer scenario) portfolioValue
        (Left err, _) ->
          error ("could not resolve test policy: " <> show err)
        (_, Left err) ->
          error ("invalid test scenario: " <> show err)

expectApproved :: PortfolioReport -> ApprovalCase Approved -> Expectation
expectApproved expectedReport approvedCase =
  approvalCaseReport approvedCase `shouldBe` expectedReport

expectRejected :: PortfolioReport -> ApprovalCase Rejected -> Expectation
expectRejected expectedReport rejectedCase =
  approvalCaseReport rejectedCase `shouldBe` expectedReport

expectAwaitingAnalyst :: PortfolioReport -> ApprovalCase AwaitingAnalyst -> Expectation
expectAwaitingAnalyst expectedReport analystCase =
  approvalCaseReport analystCase `shouldBe` expectedReport

expectAwaitingManager :: PortfolioReport -> ApprovalCase AwaitingManager -> Expectation
expectAwaitingManager expectedReport managerCase =
  approvalCaseReport managerCase `shouldBe` expectedReport

expectAwaitingCreditCommittee :: PortfolioReport -> ApprovalCase AwaitingCreditCommittee -> Expectation
expectAwaitingCreditCommittee expectedReport committeeCase =
  approvalCaseReport committeeCase `shouldBe` expectedReport

unsafeTicker :: T.Text -> Domain.Ticker.Ticker
unsafeTicker rawTicker =
  case Domain.Ticker.mkTicker rawTicker of
    Right tickerValue -> tickerValue
    Left err -> error ("invalid ticker in test fixture: " <> show err)

aggregateLevel :: [Violation] -> ApprovalLevel
aggregateLevel foundViolations =
  maximum (Automatic : map requiredLevelForViolation foundViolations)