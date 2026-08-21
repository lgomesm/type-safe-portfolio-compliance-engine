{-# LANGUAGE OverloadedStrings #-}

-- Usando adts fechados aqui pq esses valores fazem parte das regras do dominio e tem um conjunto "conhecido" de 
-- possibilidades. Entao, estados invalidos nao podem ser representados só por causa de um texto incorreto
module Domain.AssetClassification (AssetClass (..), Sector (..), parseAssetClass, parseSector) where

import Data.Text (Text)
import qualified Data.Text as T

import Domain.Error (DomainError (..))

data AssetClass
  = Equity
  | FixedIncome
  | Crypto
  | PrivateCredit
  | Fund
  | Cash
  deriving (Eq, Ord, Show, Enum, Bounded)

data Sector
  = Financial
  | Energy
  | Utilities
  | Technology
  | Consumer
  | Other
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Aceitando pequenas variacoes de formatacao pq elas nao mudam o significado do valor pro dominio
parseAssetClass :: Text -> Either DomainError AssetClass
parseAssetClass raw =
  case T.toLower (T.strip raw) of
    "equity" -> Right Equity
    "fixedincome" -> Right FixedIncome
    "crypto" -> Right Crypto
    "privatecredit" -> Right PrivateCredit
    "fund" -> Right Fund
    "cash" -> Right Cash
    _ -> Left (UnknownAssetClass raw)

-- Seguindo a mesma regra de normalizacao do parseAssetClass pra manter uma fronteira de entrada consistente pras 
-- classificacoes do dominio
parseSector :: Text -> Either DomainError Sector
parseSector raw =
  case T.toLower (T.strip raw) of
    "financial" -> Right Financial
    "energy" -> Right Energy
    "utilities" -> Right Utilities
    "technology" -> Right Technology
    "consumer" -> Right Consumer
    "other" -> Right Other
    _ -> Left (UnknownSector raw)