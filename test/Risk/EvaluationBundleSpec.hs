{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Risk.EvaluationBundleSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Domain.HList (HList (HNil, (:&)))
import Domain.Portfolio (Portfolio, mkPortfolio)
import Domain.Position (Position)
import Domain.SafeVector (N3, Vec, sN3, snatToInt, vecToList)
import Examples.Customers (conservativeHighScore)
import Generators.Domain (genCustomer)
import Generators.Portfolio (genAnyValidPortfolio)
import Risk.Approval (evaluateApproval)
import Risk.Concentration
  ( ConcentrationError (..)
  , ConcentrationMode (Dynamic, Static)
  , ConcentrationRequest (DynamicTop, StaticTop)
  , SomeTopPositions (..)
  , analyzeConcentration
  )
import Risk.EvaluationBundle
  ( ConcentrationEvidence (..)
  , EvaluationBundle
  , buildEvaluationBundle
  , renderEvidenceList
  , renderEvaluationBundle
  )
import Risk.Policy (defaultPolicyConfig)
import Risk.Report (buildReportWithPolicy, violations)
import Risk.Violation (Violation (MinimumDiversificationNotMet))
import Test.PortfolioFixtures (positionsOf, positionsOfWithSectorAndClass)

spec :: Spec
spec = describe "Risk.EvaluationBundle" $ do
  it "builds the statically typed Top 3 bundle" $ do
    portfolioValue <- concentrationPortfolio
    expectStaticBundle (buildEvaluationBundle (StaticTop sN3) defaultPolicyConfig conservativeHighScore portfolioValue)

  it "builds the dynamically typed bundle" $ do
    portfolioValue <- largeConcentrationPortfolio
    expectDynamicBundle (buildEvaluationBundle (DynamicTop 5) defaultPolicyConfig conservativeHighScore portfolioValue)

  it "stores the same portfolio report produced by the report API" $ do
    portfolioValue <- concentrationPortfolio
    case buildEvaluationBundle (StaticTop sN3) defaultPolicyConfig conservativeHighScore portfolioValue of
      report :& _ :& _ :& HNil ->
        report `shouldBe` buildReportWithPolicy defaultPolicyConfig conservativeHighScore portfolioValue

  it "derives approval from the report stored in the bundle" $ do
    portfolioValue <- concentrationPortfolio
    case buildEvaluationBundle (StaticTop sN3) defaultPolicyConfig conservativeHighScore portfolioValue of
      report :& _ :& approval :& HNil -> approval `shouldBe` evaluateApproval report

  it "keeps static concentration aligned with analyzeConcentration" $ do
    portfolioValue <- concentrationPortfolio
    case buildEvaluationBundle (StaticTop sN3) defaultPolicyConfig conservativeHighScore portfolioValue of
      _ :& ConcentrationEvidence _ bundled :& _ :& HNil ->
        sameStaticResult bundled (analyzeConcentration (StaticTop sN3) portfolioValue) `shouldBe` True

  it "keeps dynamic concentration aligned with analyzeConcentration" $ do
    portfolioValue <- largeConcentrationPortfolio
    case buildEvaluationBundle (DynamicTop 5) defaultPolicyConfig conservativeHighScore portfolioValue of
      _ :& ConcentrationEvidence _ bundled :& _ :& HNil ->
        sameDynamicResult bundled (analyzeConcentration (DynamicTop 5) portfolioValue) `shouldBe` True

  it "keeps report and approval evidence when Top 3 is unavailable" $ do
    portfolioValue <- twoPositionPortfolio
    case buildEvaluationBundle (StaticTop sN3) defaultPolicyConfig conservativeHighScore portfolioValue of
      report :& ConcentrationEvidence _ concentrationResult :& approval :& HNil -> do
        concentrationResult `shouldBe` Left NotEnoughPositions {requiredPositions = 3, actualPositions = 2}
        violations report `shouldSatisfy` elem (MinimumDiversificationNotMet 3 2)
        approval `shouldBe` evaluateApproval report

  it "renders report, concentration and approval evidence in schema order" $ do
    portfolioValue <- concentrationPortfolio
    let rendered = renderEvaluationBundle (buildEvaluationBundle (StaticTop sN3) defaultPolicyConfig conservativeHighScore portfolioValue)
    rendered `shouldSatisfy` T.isInfixOf "Portfolio Compliance Report"
    rendered `shouldSatisfy` T.isInfixOf "Top 3 Positions:"
    rendered `shouldSatisfy` T.isInfixOf "Approval Decision:"

  it "traverses every renderable evidence generically in schema order" $ do
    portfolioValue <- concentrationPortfolio
    let renderedEvidence = renderEvidenceList (buildEvaluationBundle (StaticTop sN3) defaultPolicyConfig conservativeHighScore portfolioValue)
    case renderedEvidence of
      [portfolioText, concentrationText, approvalText] -> do
        portfolioText `shouldSatisfy` T.isPrefixOf "Portfolio Compliance Report"
        concentrationText `shouldSatisfy` T.isPrefixOf "Top 3 Positions:"
        approvalText `shouldSatisfy` T.isPrefixOf "Portfolio Compliance Report"
      _ -> expectationFailure "expected exactly three rendered evidences"

  it "keeps approval equal to the evaluation of the stored report for generated inputs" $
    property $
      forAll genAnyValidPortfolio $ \portfolioValue ->
        forAll genCustomer $ \customerValue ->
          case buildEvaluationBundle (StaticTop sN3) defaultPolicyConfig customerValue portfolioValue of
            report :& _ :& approval :& HNil -> approval == evaluateApproval report

expectStaticBundle :: EvaluationBundle ('Static N3) -> Expectation
expectStaticBundle (_ :& ConcentrationEvidence (StaticTop size) result :& _ :& HNil) =
  case result of
    Right positions -> length (vecToList positions) `shouldBe` snatToInt size
    Left err -> expectationFailure (show err)

expectDynamicBundle :: EvaluationBundle 'Dynamic -> Expectation
expectDynamicBundle (_ :& ConcentrationEvidence (DynamicTop requested) result :& _ :& HNil) =
  case result of
    Right (SomeTopPositions size positions) -> do
      snatToInt size `shouldBe` requested
      length (vecToList positions) `shouldBe` requested
    Left err -> expectationFailure (show err)

sameStaticResult :: Either ConcentrationError (Vec N3 Position) -> Either ConcentrationError (Vec N3 Position) -> Bool
sameStaticResult (Right leftPositions) (Right rightPositions) = vecToList leftPositions == vecToList rightPositions
sameStaticResult (Left leftError) (Left rightError) = leftError == rightError
sameStaticResult _ _ = False

sameDynamicResult :: Either ConcentrationError SomeTopPositions -> Either ConcentrationError SomeTopPositions -> Bool
sameDynamicResult (Right (SomeTopPositions leftSize leftPositions)) (Right (SomeTopPositions rightSize rightPositions)) =
  snatToInt leftSize == snatToInt rightSize && vecToList leftPositions == vecToList rightPositions
sameDynamicResult (Left leftError) (Left rightError) = leftError == rightError
sameDynamicResult _ _ = False

concentrationPortfolio :: IO Portfolio
concentrationPortfolio = portfolioFrom
  [ ("CASH", 0.40, "Other", "Cash")
  , ("ITUB4", 0.30, "Financial", "Equity")
  , ("PETR4", 0.20, "Energy", "Equity")
  , ("BTC", 0.10, "Other", "Crypto")
  ]

largeConcentrationPortfolio :: IO Portfolio
largeConcentrationPortfolio = portfolioFrom
  [ ("A", 0.30, "Energy", "Equity")
  , ("B", 0.25, "Financial", "Equity")
  , ("C", 0.20, "Other", "Equity")
  , ("D", 0.15, "Technology", "Equity")
  , ("E", 0.10, "Consumer", "Equity")
  ]

twoPositionPortfolio :: IO Portfolio
twoPositionPortfolio = do
  rawPortfolio <- positionsOf [("PETR4", 0.50), ("ITUB4", 0.50)]
  case mkPortfolio rawPortfolio of
    Right portfolioValue -> pure portfolioValue
    Left err -> expectationFailure (show err) >> fail "invalid test portfolio"

portfolioFrom :: [(String, Double, String, String)] -> IO Portfolio
portfolioFrom rows = do
  rawPortfolio <- positionsOfWithSectorAndClass rows
  case mkPortfolio rawPortfolio of
    Right portfolioValue -> pure portfolioValue
    Left err -> expectationFailure (show err) >> fail "invalid test portfolio"