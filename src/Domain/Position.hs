-- Sem smart constructor pq Position só combina um ativo e um percentual que ja chegam validos
module Domain.Position (Position (..)) where

import Domain.Asset (Asset)
import Domain.Percentage (Percentage)

data Position = Position
  { asset :: Asset
  , weight :: Percentage
  }
  deriving (Eq, Show)