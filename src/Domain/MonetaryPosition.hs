-- Esse tipo existe basicamente pra aceitar valores absolutos na entrada sem alterar a representacao que uso no restante 
-- do dominio (que trabalha com os percentuais)
module Domain.MonetaryPosition (MonetaryPosition (..), monetaryPositionsToRawPortfolio) where

import Domain.Asset (Asset)
import Domain.Error (DomainError (..))
import Domain.Money (PositiveMoney, unPositiveMoney)
import Domain.Percentage (mkPercentage)
import Domain.Position (Position (..))
import Domain.RawPortfolio (RawPortfolio (..))

data MonetaryPosition = MonetaryPosition
  { monetaryAsset :: Asset
  , monetaryAmount :: PositiveMoney
  }
  deriving (Eq, Show)

-- Mantendo a carteira como RawPortfolio pq a validacao estrutural continua sendo responsabilidade do mkPortfolio
monetaryPositionsToRawPortfolio :: [MonetaryPosition] -> Either DomainError RawPortfolio
monetaryPositionsToRawPortfolio [] = Right (RawPortfolio [])
monetaryPositionsToRawPortfolio positions
  | isNaN totalAmount || isInfinite totalAmount = Left (NonFiniteMoney totalAmount)
  | otherwise = RawPortfolio <$> traverse (toPosition totalAmount) positions
  where
    totalAmount :: Double
    totalAmount = sum (map (unPositiveMoney . monetaryAmount) positions)

    -- O peso passa por mkPercentage pra que o resultado do calculo tb respeite as garantias definidas pros percentuais
    toPosition :: Double -> MonetaryPosition -> Either DomainError Position
    toPosition totalAmountValue monetaryPosition = do
      normalizedWeight <-
        mkPercentage
          (unPositiveMoney (monetaryAmount monetaryPosition) / totalAmountValue)
      pure
        Position
          { asset = monetaryAsset monetaryPosition
          , weight = normalizedWeight
          }