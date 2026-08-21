{-# LANGUAGE OverloadedStrings #-}

module Examples.ScenariosSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Domain.Portfolio (mkPortfolio)
import Examples.Scenarios
  ( PolicyResolutionError (..)
  , PolicySource (..)
  , Scenario (..)
  , resolveScenarioPolicy
  , scenarioNames
  , scenarios
  )
import Risk.Report (PortfolioStatus (..), buildReportWithPolicy, status, violations)
import Risk.Policy
  ( Locality (..)
  , Policy (..)
  , defaultPolicyConfig
  , policyConfigFor
  , policyForLocality
  )
import Risk.Violation (Violation (..))

spec :: Spec
spec = describe "Examples.Scenarios" $ do
  it "lists the conservative-crypto scenario in the CLI catalog" $
    scenarioNames `shouldContain` ["conservative-crypto"]

  it "lists the private credit scenarios in the CLI catalog" $
    scenarioNames `shouldContain` ["private-credit-global", "private-credit-profile"]

  it "lists the policy-driven scenarios in the CLI catalog" $
    scenarioNames `shouldContain` ["policy-retail", "policy-private-banking"]

  it "approved remains approved when paired with a suitable customer" $
    case Map.lookup "approved" scenarios of
      Nothing ->
        expectationFailure "missing scenario: approved"
      Just scenario ->
        case (resolveScenarioPolicy Nothing (scenarioPolicySource scenario), mkPortfolio (scenarioPortfolio scenario)) of
          (Right policyConfig, Right portfolioValue) ->
            status (buildReportWithPolicy policyConfig (scenarioCustomer scenario) portfolioValue)
              `shouldBe` Approved
          (Left err, _) ->
            expectationFailure ("could not resolve approved policy: " <> show err)
          (_, Left err) ->
            expectationFailure ("invalid approved scenario: " <> show err)

  it "records default and segment policy sources explicitly" $ do
    scenarioPolicySource <$> Map.lookup "approved" scenarios
      `shouldBe` Just DefaultPolicy
    scenarioPolicySource <$> Map.lookup "policy-retail" scenarios
      `shouldBe` Just (SegmentPolicy RetailPolicy)

  describe "resolveScenarioPolicy" $ do
    it "resolves default scenarios from the requested locality" $ do
      resolveScenarioPolicy Nothing DefaultPolicy
        `shouldBe` Right defaultPolicyConfig
      resolveScenarioPolicy (Just Brazil) DefaultPolicy
        `shouldBe` Right (policyForLocality Brazil)
      resolveScenarioPolicy (Just Chile) DefaultPolicy
        `shouldBe` Right (policyForLocality Chile)

    it "resolves segment policies when no locality is requested" $ do
      resolveScenarioPolicy Nothing (SegmentPolicy RetailPolicy)
        `shouldBe` Right (policyConfigFor RetailPolicy)
      resolveScenarioPolicy Nothing (SegmentPolicy PrivateBankingPolicy)
        `shouldBe` Right (policyConfigFor PrivateBankingPolicy)

    it "rejects every segment and locality combination until it is modeled" $ do
      resolveScenarioPolicy (Just Brazil) (SegmentPolicy RetailPolicy)
        `shouldBe` Left (SegmentPolicyCannotUseLocality RetailPolicy Brazil)
      resolveScenarioPolicy (Just Chile) (SegmentPolicy RetailPolicy)
        `shouldBe` Left (SegmentPolicyCannotUseLocality RetailPolicy Chile)
      resolveScenarioPolicy (Just Brazil) (SegmentPolicy PrivateBankingPolicy)
        `shouldBe` Left (SegmentPolicyCannotUseLocality PrivateBankingPolicy Brazil)
      resolveScenarioPolicy (Just Chile) (SegmentPolicy PrivateBankingPolicy)
        `shouldBe` Left (SegmentPolicyCannotUseLocality PrivateBankingPolicy Chile)

  it "produces different compliance outcomes for Brazil and Chile" $
    case Map.lookup "approved" scenarios of
      Nothing ->
        expectationFailure "missing scenario: approved"
      Just scenario ->
        case ( mkPortfolio (scenarioPortfolio scenario)
             , resolveScenarioPolicy (Just Brazil) (scenarioPolicySource scenario)
             , resolveScenarioPolicy (Just Chile) (scenarioPolicySource scenario)
             ) of
          (Right portfolioValue, Right brazilPolicy, Right chilePolicy) -> do
            status (buildReportWithPolicy brazilPolicy (scenarioCustomer scenario) portfolioValue)
              `shouldBe` Approved
            status (buildReportWithPolicy chilePolicy (scenarioCustomer scenario) portfolioValue)
              `shouldBe` Rejected
          _ ->
            expectationFailure "could not resolve locality policies or approved scenario"

  it "conservative-crypto accumulates suitability violations" $
    case Map.lookup "conservative-crypto" scenarios of
      Nothing ->
        expectationFailure "missing scenario: conservative-crypto"
      Just scenario ->
        case (resolveScenarioPolicy Nothing (scenarioPolicySource scenario), mkPortfolio (scenarioPortfolio scenario)) of
          (Right policyConfig, Right portfolioValue) -> do
            let report = buildReportWithPolicy policyConfig (scenarioCustomer scenario) portfolioValue
            status report `shouldBe` Rejected
            violations report `shouldSatisfy` any isProfileViolation
            violations report `shouldSatisfy` any isCreditViolation
          (Left err, _) ->
            expectationFailure ("could not resolve conservative-crypto policy: " <> show err)
          (_, Left err) ->
            expectationFailure ("invalid conservative-crypto scenario: " <> show err)

  it "private-credit-profile accumulates the private credit profile violation" $
    case Map.lookup "private-credit-profile" scenarios of
      Nothing ->
        expectationFailure "missing scenario: private-credit-profile"
      Just scenario ->
        case (resolveScenarioPolicy Nothing (scenarioPolicySource scenario), mkPortfolio (scenarioPortfolio scenario)) of
          (Right policyConfig, Right portfolioValue) -> do
            let report = buildReportWithPolicy policyConfig (scenarioCustomer scenario) portfolioValue
            status report `shouldBe` Rejected
            violations report `shouldSatisfy` any isPrivateCreditProfileViolation
          (Left err, _) ->
            expectationFailure ("could not resolve private-credit-profile policy: " <> show err)
          (_, Left err) ->
            expectationFailure ("invalid private-credit-profile scenario: " <> show err)

  it "keeps different segment policies for the same portfolio" $ do
    let retailScenario = Map.lookup "policy-retail" scenarios
        privateBankingScenario = Map.lookup "policy-private-banking" scenarios
    case (retailScenario, privateBankingScenario) of
      (Just retail, Just privateBanking) ->
        case ( mkPortfolio (scenarioPortfolio retail)
             , mkPortfolio (scenarioPortfolio privateBanking)
             , resolveScenarioPolicy Nothing (scenarioPolicySource retail)
             , resolveScenarioPolicy Nothing (scenarioPolicySource privateBanking)
             ) of
          (Right retailValue, Right privateValue, Right retailPolicy, Right privatePolicy) -> do
            let retailReport = buildReportWithPolicy retailPolicy (scenarioCustomer retail) retailValue
                privateReport = buildReportWithPolicy privatePolicy (scenarioCustomer privateBanking) privateValue
            status retailReport `shouldBe` Rejected
            status privateReport `shouldBe` Approved
          _ ->
            expectationFailure "invalid policy-based scenarios"
      _ ->
        expectationFailure "missing policy-based scenarios"

isProfileViolation :: Violation -> Bool
isProfileViolation (CustomerProfileCryptoExceeded _ _ _) = True
isProfileViolation _ = False

isCreditViolation :: Violation -> Bool
isCreditViolation (CustomerCreditRiskExposureExceeded _ _ _) = True
isCreditViolation _ = False

isPrivateCreditProfileViolation :: Violation -> Bool
isPrivateCreditProfileViolation (AssetClassExposureExceededForProfile _ _ _ _) = True
isPrivateCreditProfileViolation _ = False