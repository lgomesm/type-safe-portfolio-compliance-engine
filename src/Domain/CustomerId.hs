{-# LANGUAGE OverloadedStrings #-}

-- Mantendo o construtor oculto pra garantir que todo CustomerId seja criado a partir de um valor validado realmente
-- Por enquanto, a unica regra do dominio e impedir os identificadores vazios mesmp
module Domain.CustomerId (CustomerId, mkCustomerId, unCustomerId) where

import Data.Text (Text)
import qualified Data.Text as T

import Domain.Error (DomainError (..))

newtype CustomerId = CustomerId Text
  deriving (Eq, Ord, Show)

mkCustomerId :: Text -> Either DomainError CustomerId
mkCustomerId raw
  | T.null trimmed = Left EmptyCustomerId
  | otherwise = Right (CustomerId trimmed)
  where
    trimmed = T.strip raw

unCustomerId :: CustomerId -> Text
unCustomerId (CustomerId customerIdValue) = customerIdValue