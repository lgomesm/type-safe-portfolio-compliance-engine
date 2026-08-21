-- Construtor oculto pra garantir que toda Portfolio respeite as invariantes antes de ser usada pelo resto do dominio
module Domain.Portfolio (Portfolio, mkPortfolio, portfolioPositions, weightSumTolerance) where

import Domain.Error (DomainError (..))
import Domain.Percentage (unPercentage)
import Domain.Position (Position (..))
import Domain.RawPortfolio (RawPortfolio (..))

newtype Portfolio = Portfolio [Position]
  deriving (Eq, Show)

-- Tolerancia devido as operacoes com Double que produzem algumas pequenas diferencas de arredondamento mesmo quando os 
-- pesos somam 100%
weightSumTolerance :: Double
weightSumTolerance = 1e-6

mkPortfolio :: RawPortfolio -> Either DomainError Portfolio
mkPortfolio (RawPortfolio positions)
  | null positions = Left EmptyPortfolio
  | isNaN totalWeight || isInfinite totalWeight =
      Left (PortfolioWeightsDoNotSumToOne totalWeight)
  | abs (totalWeight - 1) > weightSumTolerance =
      Left (PortfolioWeightsDoNotSumToOne totalWeight)
  | otherwise = Right (Portfolio positions)
  where
    totalWeight :: Double
    totalWeight = sum (map (unPercentage . weight) positions)

portfolioPositions :: Portfolio -> [Position]
portfolioPositions (Portfolio positions) = positions