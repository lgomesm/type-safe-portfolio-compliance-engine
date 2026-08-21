-- Nao tem smart constructor pq o Asset só agrupa os valores que ja carregam suas proprias garantias de validade. Se eu 
-- adicionasse outra camada de validacao aqui nao criaria uma nova garantia pro dominio
module Domain.Asset ( Asset (..)) where

import Domain.AssetClassification (AssetClass, Sector)
import Domain.Ticker (Ticker)

data Asset = Asset
  { ticker :: Ticker
  , assetClass :: AssetClass
  , sector :: Sector
  }
  deriving (Eq, Show)
