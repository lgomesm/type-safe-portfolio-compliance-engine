{-# LANGUAGE TypeFamilies #-}

-- Regra de limite de credito privado por perfil do cliente
module Risk.Rules.PrivateCreditSuitability (PrivateCreditSuitabilityRule (..), PrivateCreditSuitabilityEvidence (..), 
  privateCreditLimitByProfile, evaluatePrivateCreditSuitability, validatePrivateCreditByProfile, 
  validatePrivateCreditByProfileWithPolicy) where

import Domain.AssetClassification (AssetClass (PrivateCredit))
import Domain.Customer (Customer (customerProfile))
import Domain.CustomerProfile (CustomerProfile (..))
import Domain.Percentage (Percentage, percentageExceeds)
import Domain.Portfolio (Portfolio)
import Risk.Exposure (exposureByAssetClass)
import Risk.Policy (PolicyConfig, defaultPolicyConfig, privateCreditProfileLimits, profileLimitFor)
import Risk.Rules.Input (SuitabilityInput (..))
import Risk.Rules.Types (ComplianceRule (..), RuleEvaluation (..), RuleEvidence, violationsOf)
import Risk.Violation (Violation (..))

data PrivateCreditSuitabilityRule = PrivateCreditSuitabilityRule

data PrivateCreditSuitabilityEvidence = PrivateCreditSuitabilityEvidence
  { evaluatedPrivateCreditProfile :: CustomerProfile
  , observedPrivateCreditExposure :: Percentage
  , allowedPrivateCreditExposure :: Percentage
  }
  deriving (Eq, Show)

type instance RuleEvidence PrivateCreditSuitabilityRule = PrivateCreditSuitabilityEvidence

instance ComplianceRule PrivateCreditSuitabilityRule where
  type RuleInput PrivateCreditSuitabilityRule = SuitabilityInput
  evaluateRule PrivateCreditSuitabilityRule input =
    evaluatePrivateCreditSuitability
      (suitabilityPolicy input)
      (suitabilityCustomer input)
      (suitabilityPortfolio input)

privateCreditLimitByProfile :: CustomerProfile -> Percentage
privateCreditLimitByProfile =
  profileLimitFor (privateCreditProfileLimits defaultPolicyConfig)

validatePrivateCreditByProfile :: Customer -> Portfolio -> [Violation]
validatePrivateCreditByProfile =
  validatePrivateCreditByProfileWithPolicy defaultPolicyConfig

validatePrivateCreditByProfileWithPolicy :: PolicyConfig -> Customer -> Portfolio -> [Violation]
validatePrivateCreditByProfileWithPolicy policyConfig customerValue =
  violationsOf . evaluatePrivateCreditSuitability policyConfig customerValue

evaluatePrivateCreditSuitability :: PolicyConfig -> Customer -> Portfolio -> RuleEvaluation PrivateCreditSuitabilityRule
evaluatePrivateCreditSuitability policyConfig customerValue portfolioValue =
  RuleEvaluation evidence (violationsFromEvidence evidence)
  where
    evidence :: PrivateCreditSuitabilityEvidence
    evidence =
      PrivateCreditSuitabilityEvidence
        { evaluatedPrivateCreditProfile = customerProfile customerValue
        , observedPrivateCreditExposure = exposureByAssetClass PrivateCredit portfolioValue
        , allowedPrivateCreditExposure = profileLimitFor (privateCreditProfileLimits policyConfig) (customerProfile customerValue)
        }

violationsFromEvidence :: PrivateCreditSuitabilityEvidence -> [Violation]
violationsFromEvidence evidence
  | percentageExceeds (observedPrivateCreditExposure evidence) (allowedPrivateCreditExposure evidence) =
      [ AssetClassExposureExceededForProfile
          PrivateCredit
          (evaluatedPrivateCreditProfile evidence)
          (allowedPrivateCreditExposure evidence)
          (observedPrivateCreditExposure evidence)
      ]
  | otherwise = []