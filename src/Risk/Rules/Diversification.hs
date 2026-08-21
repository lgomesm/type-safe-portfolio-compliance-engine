{-# LANGUAGE TypeFamilies #-}

-- A carteira precisa ter pelo menos 3 ativos distintos
module Risk.Rules.Diversification (DiversificationRule (..), DiversificationEvidence (..), minimumDistinctAssets, 
  evaluateDiversification, validateMinimumDiversification) where

import Data.List (nub)

import Domain.Asset (Asset (..))
import Domain.Portfolio (Portfolio, portfolioPositions)
import Domain.Position (Position (asset))
import Risk.Rules.Types (ComplianceRule (..), RuleEvaluation (..), RuleEvidence, violationsOf)
import Risk.Violation (Violation (..))

data DiversificationRule = DiversificationRule

data DiversificationEvidence = DiversificationEvidence
  { distinctAssetCount :: Int
  , minimumDistinctAssetCount :: Int
  }
  deriving (Eq, Show)

type instance RuleEvidence DiversificationRule = DiversificationEvidence

instance ComplianceRule DiversificationRule where
  type RuleInput DiversificationRule = Portfolio
  evaluateRule DiversificationRule = evaluateDiversification

minimumDistinctAssets :: Int
minimumDistinctAssets = 3

validateMinimumDiversification :: Portfolio -> [Violation]
validateMinimumDiversification = violationsOf . evaluateDiversification

evaluateDiversification :: Portfolio -> RuleEvaluation DiversificationRule
evaluateDiversification portfolio =
  RuleEvaluation evidence (violationsFromEvidence evidence)
  where
    evidence :: DiversificationEvidence
    evidence =
      DiversificationEvidence
        { distinctAssetCount =
            length (nub (map (ticker . asset) (portfolioPositions portfolio)))
        , minimumDistinctAssetCount = minimumDistinctAssets
        }

violationsFromEvidence :: DiversificationEvidence -> [Violation]
violationsFromEvidence evidence
  | distinctAssetCount evidence < minimumDistinctAssetCount evidence =
      [MinimumDiversificationNotMet (minimumDistinctAssetCount evidence) (distinctAssetCount evidence)]
  | otherwise = []