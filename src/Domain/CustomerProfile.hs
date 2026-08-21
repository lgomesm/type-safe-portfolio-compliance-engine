module Domain.CustomerProfile (CustomerProfile (..)) where

data CustomerProfile
  = Conservative
  | Moderate
  | Aggressive
  deriving (Eq, Show, Ord, Enum, Bounded)
