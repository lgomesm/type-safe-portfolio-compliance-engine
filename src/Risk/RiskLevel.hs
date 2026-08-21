-- Nivel de risco agregado derivado das violacoes de compliance
module Risk.RiskLevel (RiskLevel (..), violationSeverity, riskLevelFor) where

import Risk.Violation (Violation (..))

data RiskLevel
  = Low
  | Medium
  | High
  deriving (Eq, Ord, Show)

-- Algumas violacoes recebem peso maior pq representam problemas mais relevantes pra classificacao agregada de risco
violationSeverity :: Violation -> Int
violationSeverity (AssetAllocationExceeded _ _ _) = 1
violationSeverity (SectorExposureExceeded _ _ _) = 1
violationSeverity (AssetClassExposureExceeded _ _ _) = 2
violationSeverity (MinimumDiversificationNotMet _ _) = 2
violationSeverity (CustomerProfileCryptoExceeded _ _ _) = 2
violationSeverity (CustomerCreditRiskExposureExceeded _ _ _) = 2
violationSeverity (AssetClassExposureExceededForProfile _ _ _ _) = 2

riskLevelFor :: [Violation] -> RiskLevel
riskLevelFor violationsFound
  | score == 0 = Low
  | score <= 2 = Medium
  | otherwise = High
  where
    score :: Int
    score = sum (map violationSeverity violationsFound)