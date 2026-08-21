module Domain.CreditScore (CreditScore (..)) where

data CreditScore
  = LowScore
  | MediumScore
  | HighScore
  deriving (Eq, Show, Ord, Enum, Bounded)