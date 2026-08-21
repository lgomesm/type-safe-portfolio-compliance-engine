{-# LANGUAGE OverloadedStrings #-}

module Domain.Ticker (Ticker, mkTicker, unTicker) where

import Data.Text (Text)
import qualified Data.Text as T

import Domain.Error (DomainError (..))

newtype Ticker = Ticker Text
  deriving (Eq, Ord, Show)

-- Removo os espacos nas bordas pq eles nao fazem parte do identificador e faziam um valor vazio parecer valido
mkTicker :: Text -> Either DomainError Ticker
mkTicker raw
  | T.null trimmed = Left EmptyTicker
  | otherwise = Right (Ticker trimmed)
  where
    trimmed = T.strip raw

unTicker :: Ticker -> Text
unTicker (Ticker tickerText) = tickerText