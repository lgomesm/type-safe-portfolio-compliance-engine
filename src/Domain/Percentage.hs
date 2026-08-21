-- Usando fracoes pra manter uma unica convencao e evitar ambiguidades como por ex: 30 e 0.30
module Domain.Percentage (Percentage, clampPercentage, percentageExceeds, mkPercentage, unPercentage) where

import Domain.Error (DomainError (..))

newtype Percentage = Percentage Double
  deriving (Eq, Ord, Show)

mkPercentage :: Double -> Either DomainError Percentage
mkPercentage value
  | isNaN value || isInfinite value = Left (NonFinitePercentage value)
  | value < 0 || value > 1 = Left (PercentageOutOfRange value)
  | otherwise = Right (Percentage value)

clampPercentage :: Double -> Either DomainError Percentage
clampPercentage value
  | isNaN value || isInfinite value = Left (NonFinitePercentage value)
  | value <= 0 = mkPercentage 0
  | value >= 1 = mkPercentage 1
  | otherwise = mkPercentage value

-- Usando uma pequena tolerância na comparacao pq operacoes com Double tem diferencas numericas irrelevantes perto de um 
-- limite
percentageExceeds :: Percentage -> Percentage -> Bool
percentageExceeds observed allowed =
  unPercentage observed - unPercentage allowed > percentageComparisonTolerance
  where
    percentageComparisonTolerance :: Double
    percentageComparisonTolerance = 1e-9

unPercentage :: Percentage -> Double
unPercentage (Percentage percentageValue) = percentageValue