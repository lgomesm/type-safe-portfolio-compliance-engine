{-# LANGUAGE TypeFamilies #-}

-- Regra que limita a exposicao total de cada setor na carteira
module Risk.Rules.SectorExposure (SectorExposureRule (..), SectorExposureObservation (..), SectorExposureEvidence (..), 
  sectorExposureRulePolicy, sectorExposurePolicyLimit, maxSectorExposure, evaluateSectorExposure, validateMaxSectorExposure, 
  validateMaxSectorExposureWithPolicy) where

import qualified Data.Map.Strict as Map

import Domain.Asset (Asset (..))
import Domain.AssetClassification (Sector)
import Domain.Percentage (Percentage, percentageExceeds, unPercentage)
import Domain.Percentage.Internal (clampCalculatedPercentage, percentageLiteral)
import Domain.Portfolio (Portfolio, portfolioPositions)
import Domain.Position (Position (..))
import Risk.Policy (PolicyConfig, defaultPolicyConfig, maxSectorExposureLimit)
import Risk.Rules.Input (PolicyPortfolioInput (..))
import Risk.Rules.Policy (RulePolicy)
import Risk.Rules.Types (ComplianceRule (..), RuleEvaluation (..), RuleEvidence, violationsOf)
import Risk.Violation (Violation (..))

data SectorExposureRule = SectorExposureRule

data SectorExposureObservation = SectorExposureObservation
  { observedSector :: Sector
  , observedSectorExposure :: Percentage
  , allowedSectorExposure :: Percentage
  }
  deriving (Eq, Show)

newtype SectorExposureEvidence = SectorExposureEvidence
  { sectorExposureObservations :: [SectorExposureObservation]
  }
  deriving (Eq, Show)

type instance RuleEvidence SectorExposureRule = SectorExposureEvidence

data instance RulePolicy SectorExposureRule = SectorExposureRulePolicy
  { sectorExposurePolicyLimit :: Percentage
  }
  deriving (Eq, Show)

sectorExposureRulePolicy :: PolicyConfig -> RulePolicy SectorExposureRule
sectorExposureRulePolicy policyConfig =
  SectorExposureRulePolicy (maxSectorExposureLimit policyConfig)

instance ComplianceRule SectorExposureRule where
  type RuleInput SectorExposureRule = PolicyPortfolioInput SectorExposureRule
  evaluateRule SectorExposureRule input =
    evaluateWithRulePolicy (inputRulePolicy input) (inputPortfolio input)

maxSectorExposure :: Percentage
maxSectorExposure = percentageLiteral 0.50

validateMaxSectorExposure :: Portfolio -> [Violation]
validateMaxSectorExposure =
  validateMaxSectorExposureWithPolicy defaultPolicyConfig

validateMaxSectorExposureWithPolicy :: PolicyConfig -> Portfolio -> [Violation]
validateMaxSectorExposureWithPolicy policyConfig =
  violationsOf . evaluateSectorExposure policyConfig

evaluateSectorExposure :: PolicyConfig -> Portfolio -> RuleEvaluation SectorExposureRule
evaluateSectorExposure policyConfig portfolio =
  evaluateRule
    SectorExposureRule
    (PolicyPortfolioInput (sectorExposureRulePolicy policyConfig) portfolio)

evaluateWithRulePolicy :: RulePolicy SectorExposureRule -> Portfolio -> RuleEvaluation SectorExposureRule
evaluateWithRulePolicy (SectorExposureRulePolicy limitValue) portfolio =
  RuleEvaluation evidence (violationsFromEvidence evidence)
  where
    evidence :: SectorExposureEvidence
    evidence =
      SectorExposureEvidence
        [ SectorExposureObservation sectorValue (clampCalculatedPercentage totalExposure) limitValue
        | (sectorValue, totalExposure) <- exposureBySector (portfolioPositions portfolio)
        ]

violationsFromEvidence :: SectorExposureEvidence -> [Violation]
violationsFromEvidence (SectorExposureEvidence observations) =
  [ SectorExposureExceeded sectorValue allowedExposure observedExposure
  | SectorExposureObservation sectorValue observedExposure allowedExposure <- observations
  , percentageExceeds observedExposure allowedExposure
  ]

-- Agrupando por setor pq a regra precisa avaliar a exposicao total, e nao cada posicao individualmente
exposureBySector :: [Position] -> [(Sector, Double)]
exposureBySector =
  Map.toList
    . Map.fromListWith (+)
    . map (\positionValue -> (sector (asset positionValue), unPercentage (weight positionValue)))