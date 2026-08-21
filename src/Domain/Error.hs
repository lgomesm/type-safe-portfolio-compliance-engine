-- Mantendo um unico tipo de erro pra que as validacoes do dominio sigam o mesmo contrato e possam ser combinadas 
module Domain.Error (DomainError (..)) where

import Data.Text (Text)

data DomainError
  = EmptyTicker
  | EmptyCustomerId
  | PercentageOutOfRange Double
  | NonFinitePercentage Double
  | NonPositiveMoney Double
  | NonFiniteMoney Double
  | UnknownAssetClass Text
  | UnknownSector Text
  | EmptyPortfolio
  | PortfolioWeightsDoNotSumToOne Double
  deriving (Eq, Show)