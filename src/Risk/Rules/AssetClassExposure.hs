{-# LANGUAGE TypeFamilies #-}

-- Regras de limite global por classe de ativo
module Risk.Rules.AssetClassExposure (AssetClassExposureRule (..), AssetClassExposureObservation (..), 
  AssetClassExposureEvidence (..), assetClassExposureRulePolicy, cryptoExposurePolicyLimit, 
  privateCreditExposurePolicyLimit, assetClassLimit, evaluateAssetClassExposure, validateMaxAssetClassExposure, 
  validateMaxAssetClassExposureWithPolicy) where

import Domain.AssetClassification (AssetClass (..))
import Domain.Percentage (Percentage, percentageExceeds)
import Domain.Portfolio (Portfolio)
import Risk.Policy (AssetClassLimits (..), PolicyConfig, assetClassLimitFor, assetClassLimits, defaultPolicyConfig)
import Risk.Exposure (exposureByAssetClass)
import Risk.Rules.Input (PolicyPortfolioInput (..))
import Risk.Rules.Policy (RulePolicy)
import Risk.Rules.Types (ComplianceRule (..), RuleEvaluation (..), RuleEvidence, violationsOf)
import Risk.Violation (Violation (..))

data AssetClassExposureRule = AssetClassExposureRule

data AssetClassExposureObservation = AssetClassExposureObservation
  { observedAssetClass :: AssetClass
  , observedClassExposure :: Percentage
  , allowedClassExposure :: Percentage
  }
  deriving (Eq, Show)

newtype AssetClassExposureEvidence = AssetClassExposureEvidence
  { assetClassExposureObservations :: [AssetClassExposureObservation]
  }
  deriving (Eq, Show)

type instance RuleEvidence AssetClassExposureRule = AssetClassExposureEvidence

data instance RulePolicy AssetClassExposureRule = AssetClassExposureRulePolicy
  { cryptoExposurePolicyLimit :: Maybe Percentage
  , privateCreditExposurePolicyLimit :: Maybe Percentage
  }
  deriving (Eq, Show)

assetClassExposureRulePolicy :: PolicyConfig -> RulePolicy AssetClassExposureRule
assetClassExposureRulePolicy policyConfig =
  AssetClassExposureRulePolicy
    { cryptoExposurePolicyLimit = cryptoAssetClassLimit limits
    , privateCreditExposurePolicyLimit = privateCreditAssetClassLimit limits
    }
  where
    limits :: AssetClassLimits
    limits = assetClassLimits policyConfig

instance ComplianceRule AssetClassExposureRule where
  type RuleInput AssetClassExposureRule = PolicyPortfolioInput AssetClassExposureRule
  evaluateRule AssetClassExposureRule input =
    evaluateWithRulePolicy (inputRulePolicy input) (inputPortfolio input)

-- O limite é resolvido por classe pq tds essas exposicoes seguem a mesma regra e diferem só pelo valor permitido pela 
-- politica
assetClassLimit :: AssetClass -> Maybe Percentage
assetClassLimit = assetClassLimitFor defaultPolicyConfig

validateMaxAssetClassExposure :: Portfolio -> [Violation]
validateMaxAssetClassExposure =
  validateMaxAssetClassExposureWithPolicy defaultPolicyConfig

validateMaxAssetClassExposureWithPolicy :: PolicyConfig -> Portfolio -> [Violation]
validateMaxAssetClassExposureWithPolicy policyConfig =
  violationsOf . evaluateAssetClassExposure policyConfig

evaluateAssetClassExposure :: PolicyConfig -> Portfolio -> RuleEvaluation AssetClassExposureRule
evaluateAssetClassExposure policyConfig portfolioValue =
  evaluateRule
    AssetClassExposureRule
    (PolicyPortfolioInput (assetClassExposureRulePolicy policyConfig) portfolioValue)

evaluateWithRulePolicy :: RulePolicy AssetClassExposureRule -> Portfolio -> RuleEvaluation AssetClassExposureRule
evaluateWithRulePolicy rulePolicy portfolioValue =
  RuleEvaluation evidence (violationsFromEvidence evidence)
  where
    evidence :: AssetClassExposureEvidence
    evidence =
      AssetClassExposureEvidence
        [ AssetClassExposureObservation assetClassValue exposure limit
        | (assetClassValue, Just limit) <- classLimits rulePolicy
        , let exposure = exposureByAssetClass assetClassValue portfolioValue
        ]

classLimits :: RulePolicy AssetClassExposureRule -> [(AssetClass, Maybe Percentage)]
classLimits rulePolicy =
  [ (Crypto, cryptoExposurePolicyLimit rulePolicy)
  , (PrivateCredit, privateCreditExposurePolicyLimit rulePolicy)
  ]

violationsFromEvidence :: AssetClassExposureEvidence -> [Violation]
violationsFromEvidence (AssetClassExposureEvidence observations) =
  [ AssetClassExposureExceeded assetClassValue allowedExposure observedExposure
  | AssetClassExposureObservation assetClassValue observedExposure allowedExposure <- observations
  , percentageExceeds observedExposure allowedExposure
  ]