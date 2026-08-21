module Risk.EngineSpec (spec) where

import Test.Hspec

import Domain.AssetClassification (AssetClass (PrivateCredit))
import Domain.CustomerProfile (CustomerProfile (Conservative))
import Domain.Portfolio (mkPortfolio)
import Examples.Customers (conservativeLowScore, moderateMediumScore)
import Risk.Engine (validateCompliance, validateComplianceWithPolicy, validatePortfolio)
import Risk.Policy (Policy (PrivateBankingPolicy, RetailPolicy), policyConfigFor)
import Risk.SuitabilityPolicy (defaultSuitabilityPolicy)
import Risk.Violation (Violation (..))
import Test.PortfolioFixtures (positionsOfWithClass, positionsOfWithSectorAndClass)
import Test.PercentageFixtures (percentageLiteral)

spec :: Spec
spec = describe "Risk.Engine.validatePortfolio" $ do
  it "returns an empty list for a portfolio that violates no rule" $ do
    rawPortfolio <-
      positionsOfWithSectorAndClass
        [ ("PETR4", 0.25, "Energy", "Equity")
        , ("ITUB4", 0.25, "Financial", "Equity")
        , ("VALE3", 0.25, "Other", "Equity")
        , ("WEGE3", 0.25, "Technology", "Equity")
        ]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validatePortfolio portfolioValue `shouldBe` []
      Left err ->
        expectationFailure (show err)

  it "aggregates violations from multiple rules simultaneously" $ do
    rawPortfolio <-
      positionsOfWithSectorAndClass
        [ ("PETR4", 0.60, "Energy", "Equity")
        , ("BTC", 0.40, "Other", "Crypto")
        ]
    case mkPortfolio rawPortfolio of
      Right portfolioValue -> do
        let violations = validatePortfolio portfolioValue
        length violations `shouldBe` 5
        violations `shouldSatisfy` any isAssetAllocation
        violations `shouldSatisfy` any isCryptoExposure
        violations `shouldSatisfy` any isDiversification
      Left err ->
        expectationFailure (show err)

  it "returns violations in a deterministic order across executions" $ do
    rawPortfolio <-
      positionsOfWithSectorAndClass
        [ ("PETR4", 0.60, "Energy", "Equity")
        , ("BTC", 0.40, "Other", "Crypto")
        ]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validatePortfolio portfolioValue `shouldBe` validatePortfolio portfolioValue
      Left err ->
        expectationFailure (show err)

  describe "validateCompliance" $
    it "adds customer suitability violations on top of portfolio violations" $ do
      rawPortfolio <-
        positionsOfWithSectorAndClass
          [ ("BTC", 0.25, "Other", "Crypto")
          , ("PETR4", 0.75, "Energy", "Equity")
          ]
      case mkPortfolio rawPortfolio of
        Right portfolioValue -> do
          let violations = validateCompliance defaultSuitabilityPolicy conservativeLowScore portfolioValue
          violations `shouldSatisfy` any isCryptoExposure
          violations `shouldSatisfy` any isCustomerProfileCrypto
          violations `shouldSatisfy` any isCustomerCreditExposure
        Left err ->
          expectationFailure (show err)

  describe "private credit integration" $ do
    it "reports the global private credit limit inside validatePortfolio" $ do
      rawPortfolio <-
        positionsOfWithSectorAndClass
          [ ("TESOURO", 0.25, "Other", "FixedIncome")
          , ("PETR4", 0.25, "Energy", "Equity")
          , ("DEB001", 0.25, "Financial", "PrivateCredit")
          , ("WEGE3", 0.25, "Technology", "Equity")
          ]
      case mkPortfolio rawPortfolio of
        Right portfolioValue ->
          validatePortfolio portfolioValue
            `shouldBe`
              [ AssetClassExposureExceeded PrivateCredit (percentageLiteral 0.20) (percentageLiteral 0.25)
              ]
        Left err ->
          expectationFailure (show err)

    it "accumulates global and profile private credit violations simultaneously" $ do
      rawPortfolio <-
        positionsOfWithClass
          [ ("DEB001", 0.30, "PrivateCredit")
          , ("TESOURO", 0.40, "FixedIncome")
          , ("PETR4", 0.30, "Equity")
          ]
      case mkPortfolio rawPortfolio of
        Right portfolioValue -> do
          let violations = validateCompliance defaultSuitabilityPolicy conservativeLowScore portfolioValue
          violations `shouldSatisfy` any isPrivateCreditGlobal
          violations `shouldSatisfy` any isPrivateCreditProfile
        Left err ->
          expectationFailure (show err)

  describe "policy-aware validation" $
    it "changes the outcome of an 8% crypto portfolio between retail and private banking" $ do
      rawPortfolio <-
        positionsOfWithClass
          [ ("BTC", 0.08, "Crypto")
          , ("TESOURO", 0.52, "FixedIncome")
          , ("PETR4", 0.20, "Equity")
          , ("WEGE3", 0.20, "Equity")
          ]
      case mkPortfolio rawPortfolio of
        Right portfolioValue -> do
          let retailViolations =
                validateComplianceWithPolicy (policyConfigFor RetailPolicy) moderateMediumScore portfolioValue
              privateBankingViolations =
                validateComplianceWithPolicy (policyConfigFor PrivateBankingPolicy) moderateMediumScore portfolioValue
          retailViolations `shouldSatisfy` any isCustomerProfileCrypto
          privateBankingViolations `shouldNotSatisfy` any isCustomerProfileCrypto
        Left err ->
          expectationFailure (show err)

isAssetAllocation :: Violation -> Bool
isAssetAllocation (AssetAllocationExceeded _ _ _) = True
isAssetAllocation _ = False

isCryptoExposure :: Violation -> Bool
isCryptoExposure (AssetClassExposureExceeded _ _ _) = True
isCryptoExposure _ = False

isPrivateCreditGlobal :: Violation -> Bool
isPrivateCreditGlobal (AssetClassExposureExceeded PrivateCredit _ _) = True
isPrivateCreditGlobal _ = False

isPrivateCreditProfile :: Violation -> Bool
isPrivateCreditProfile (AssetClassExposureExceededForProfile PrivateCredit Conservative _ _) = True
isPrivateCreditProfile _ = False

isCustomerProfileCrypto :: Violation -> Bool
isCustomerProfileCrypto (CustomerProfileCryptoExceeded _ _ _) = True
isCustomerProfileCrypto _ = False

isCustomerCreditExposure :: Violation -> Bool
isCustomerCreditExposure (CustomerCreditRiskExposureExceeded _ _ _) = True
isCustomerCreditExposure _ = False

isDiversification :: Violation -> Bool
isDiversification (MinimumDiversificationNotMet _ _) = True
isDiversification _ = False