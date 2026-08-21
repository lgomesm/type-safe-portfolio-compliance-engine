-- Criei esse tipo pra separar de maneira mais clara os dados recebidos de uma Portfolio ja validada. O construtor fica 
-- exposto pq valores nesse estagio ainda nao precisam satisfazer as invariantes da carteira também
module Domain.RawPortfolio  (RawPortfolio (..)) where

import Domain.Position (Position)

newtype RawPortfolio = RawPortfolio
  { rawPositions :: [Position]
  }
  deriving (Eq, Show)