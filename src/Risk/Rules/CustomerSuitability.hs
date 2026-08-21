{-# LANGUAGE TypeFamilies #-}

-- Regras de suitability dependentes do perfil e score do cliente
module Risk.Rules.CustomerSuitability (CustomerSuitabilityRule (..), CustomerSuitabilityEvidence (..), 
  validateCustomerSuitability, validateCustomerSuitabilityWithPolicy, evaluateCustomerSuitability) where

import Domain.AssetClassification (AssetClass (Crypto, Equity))
import Domain.CreditScore (CreditScore)
import Domain.Customer (Customer (creditScore, customerProfile))
import Domain.CustomerProfile (CustomerProfile)
import Domain.Percentage (Percentage, percentageExceeds)
import Domain.Portfolio (Portfolio)
import Risk.Exposure (exposureByAssetClass, exposureByAssetClasses)
import Risk.Policy (PolicyConfig, cryptoProfileLimits, profileLimitFor, riskScoreLimits, scoreLimitFor, 
  policyConfigWithSuitability)
import Risk.SuitabilityPolicy (SuitabilityPolicy)
import Risk.Rules.Input (SuitabilityInput (..))
import Risk.Rules.Types (ComplianceRule (..), RuleEvaluation (..), RuleEvidence, violationsOf)
import Risk.Violation (Violation (..))

data CustomerSuitabilityRule = CustomerSuitabilityRule

data CustomerSuitabilityEvidence = CustomerSuitabilityEvidence
  { evaluatedCustomerProfile :: CustomerProfile
  , observedCryptoExposure :: Percentage
  , allowedCryptoExposure :: Percentage
  , evaluatedCreditScore :: CreditScore
  , observedRiskExposure :: Percentage
  , allowedRiskExposure :: Percentage
  }
  deriving (Eq, Show)

type instance RuleEvidence CustomerSuitabilityRule = CustomerSuitabilityEvidence

instance ComplianceRule CustomerSuitabilityRule where
  type RuleInput CustomerSuitabilityRule = SuitabilityInput
  evaluateRule CustomerSuitabilityRule input =
    evaluateCustomerSuitability
      (suitabilityPolicy input)
      (suitabilityCustomer input)
      (suitabilityPortfolio input)

validateCustomerSuitability :: SuitabilityPolicy -> Customer -> Portfolio -> [Violation]
validateCustomerSuitability policy =
  validateCustomerSuitabilityWithPolicy (policyConfigWithSuitability policy)

validateCustomerSuitabilityWithPolicy :: PolicyConfig -> Customer -> Portfolio -> [Violation]
validateCustomerSuitabilityWithPolicy policyConfig customerValue =
  violationsOf . evaluateCustomerSuitability policyConfig customerValue

evaluateCustomerSuitability :: PolicyConfig -> Customer -> Portfolio -> RuleEvaluation CustomerSuitabilityRule
evaluateCustomerSuitability policyConfig customerValue portfolioValue =
  RuleEvaluation evidence (violationsFromEvidence evidence)
  where
    profileValue :: CustomerProfile
    profileValue = customerProfile customerValue

    scoreValue :: CreditScore
    scoreValue = creditScore customerValue

    evidence :: CustomerSuitabilityEvidence
    evidence =
      CustomerSuitabilityEvidence
        { evaluatedCustomerProfile = profileValue
        , observedCryptoExposure = exposureByAssetClass Crypto portfolioValue
        , allowedCryptoExposure = profileLimitFor (cryptoProfileLimits policyConfig) profileValue
        , evaluatedCreditScore = scoreValue
        , observedRiskExposure = exposureByAssetClasses [Equity, Crypto] portfolioValue
        , allowedRiskExposure = scoreLimitFor (riskScoreLimits policyConfig) scoreValue
        }

violationsFromEvidence :: CustomerSuitabilityEvidence -> [Violation]
violationsFromEvidence evidence = cryptoViolations <> riskViolations
  where
    cryptoViolations
      | percentageExceeds (observedCryptoExposure evidence) (allowedCryptoExposure evidence) =
          [ CustomerProfileCryptoExceeded
              (evaluatedCustomerProfile evidence)
              (allowedCryptoExposure evidence)
              (observedCryptoExposure evidence)
          ]
      | otherwise = []

    riskViolations
      | percentageExceeds (observedRiskExposure evidence) (allowedRiskExposure evidence) =
          [ CustomerCreditRiskExposureExceeded
              (evaluatedCreditScore evidence)
              (allowedRiskExposure evidence)
              (observedRiskExposure evidence)
          ]
      | otherwise = []