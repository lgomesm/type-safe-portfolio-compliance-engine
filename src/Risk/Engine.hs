-- Ponto central de execucao das regras de compliance
module Risk.Engine (ComplianceEvaluation (..), evaluateComplianceWithPolicy, complianceViolations, validateCompliance, 
  validateComplianceWithPolicy, validatePortfolio, validatePortfolioWithPolicy) where

import Domain.Customer (Customer)
import Domain.Portfolio (Portfolio)
import Risk.Policy (PolicyConfig, defaultPolicyConfig, policyConfigWithSuitability)
import Risk.Rules.AssetAllocation (AssetAllocationRule (..), assetAllocationRulePolicy)
import Risk.Rules.AssetClassExposure (AssetClassExposureRule (..), assetClassExposureRulePolicy)
import Risk.Rules.CustomerSuitability (CustomerSuitabilityRule (..))
import Risk.Rules.Diversification (DiversificationRule (..))
import Risk.Rules.Input (PolicyPortfolioInput (..), SuitabilityInput (..))
import Risk.Rules.PrivateCreditSuitability (PrivateCreditSuitabilityRule (..))
import Risk.Rules.SectorExposure (SectorExposureRule (..), sectorExposureRulePolicy)
import Risk.Rules.Types (RuleEvaluation, evaluateRule, evaluateViolations, violationsOf)
import Risk.SuitabilityPolicy (SuitabilityPolicy)
import Risk.Violation (Violation)

validatePortfolio :: Portfolio -> [Violation]
validatePortfolio =
  validatePortfolioWithPolicy defaultPolicyConfig

validatePortfolioWithPolicy :: PolicyConfig -> Portfolio -> [Violation]
validatePortfolioWithPolicy policyConfig portfolioValue =
  let assetAllocationInput = PolicyPortfolioInput (assetAllocationRulePolicy policyConfig) portfolioValue
      sectorExposureInput = PolicyPortfolioInput (sectorExposureRulePolicy policyConfig) portfolioValue
      assetClassExposureInput = PolicyPortfolioInput (assetClassExposureRulePolicy policyConfig) portfolioValue
   in
  concat
    [ evaluateViolations AssetAllocationRule assetAllocationInput
    , evaluateViolations SectorExposureRule sectorExposureInput
    , evaluateViolations AssetClassExposureRule assetClassExposureInput
    , evaluateViolations DiversificationRule portfolioValue
    ]

validateCompliance :: SuitabilityPolicy -> Customer -> Portfolio -> [Violation]
validateCompliance policy =
  validateComplianceWithPolicy (policyConfigWithSuitability policy)

validateComplianceWithPolicy :: PolicyConfig -> Customer -> Portfolio -> [Violation]
validateComplianceWithPolicy policyConfig customerValue portfolioValue =
  complianceViolations (evaluateComplianceWithPolicy policyConfig customerValue portfolioValue)

-- Mantendo as avaliacoes separadas pra preservar a evidencia produzida por cada regra, mesmo quando o consumidor precisa apenas das violacoes
-- finais
data ComplianceEvaluation = ComplianceEvaluation
  { assetAllocationEvaluation :: RuleEvaluation AssetAllocationRule
  , sectorExposureEvaluation :: RuleEvaluation SectorExposureRule
  , assetClassExposureEvaluation :: RuleEvaluation AssetClassExposureRule
  , diversificationEvaluation :: RuleEvaluation DiversificationRule
  , customerSuitabilityEvaluation :: RuleEvaluation CustomerSuitabilityRule
  , privateCreditSuitabilityEvaluation :: RuleEvaluation PrivateCreditSuitabilityRule
  }

evaluateComplianceWithPolicy :: PolicyConfig -> Customer -> Portfolio -> ComplianceEvaluation
evaluateComplianceWithPolicy policyConfig customerValue portfolioValue =
  let assetAllocationInput = PolicyPortfolioInput (assetAllocationRulePolicy policyConfig) portfolioValue
      sectorExposureInput = PolicyPortfolioInput (sectorExposureRulePolicy policyConfig) portfolioValue
      assetClassExposureInput = PolicyPortfolioInput (assetClassExposureRulePolicy policyConfig) portfolioValue
      suitabilityInput = SuitabilityInput policyConfig customerValue portfolioValue
   in
  ComplianceEvaluation
    { assetAllocationEvaluation = evaluateRule AssetAllocationRule assetAllocationInput
    , sectorExposureEvaluation = evaluateRule SectorExposureRule sectorExposureInput
    , assetClassExposureEvaluation = evaluateRule AssetClassExposureRule assetClassExposureInput
    , diversificationEvaluation = evaluateRule DiversificationRule portfolioValue
    , customerSuitabilityEvaluation = evaluateRule CustomerSuitabilityRule suitabilityInput
    , privateCreditSuitabilityEvaluation = evaluateRule PrivateCreditSuitabilityRule suitabilityInput
    }

-- As violacoes sao reunidas só no final pra que cada regra continue podendo produzir sua propria evidencia de forma independente
complianceViolations :: ComplianceEvaluation -> [Violation]
complianceViolations evaluation =
  concat
    [ violationsOf (assetAllocationEvaluation evaluation)
    , violationsOf (sectorExposureEvaluation evaluation)
    , violationsOf (assetClassExposureEvaluation evaluation)
    , violationsOf (diversificationEvaluation evaluation)
    , violationsOf (customerSuitabilityEvaluation evaluation)
    , violationsOf (privateCreditSuitabilityEvaluation evaluation)
    ]