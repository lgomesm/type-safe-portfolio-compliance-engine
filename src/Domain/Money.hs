-- Deixando o construtor oculto pra garantir que td valor monetario seja validado antes de entrar no dominio
module Domain.Money (PositiveMoney, mkPositiveMoney, unPositiveMoney) where

import Domain.Error (DomainError (..))

newtype PositiveMoney = PositiveMoney Double
  deriving (Eq, Ord, Show)

-- Verifico a finitude primeiro ppq NaN e infinitos nao seriam rejeitados certinho só pela comparacao com zero
mkPositiveMoney :: Double -> Either DomainError PositiveMoney
mkPositiveMoney value
  | isNaN value || isInfinite value = Left (NonFiniteMoney value)
  | value <= 0 = Left (NonPositiveMoney value)
  | otherwise = Right (PositiveMoney value)

unPositiveMoney :: PositiveMoney -> Double
unPositiveMoney (PositiveMoney moneyValue) = moneyValue