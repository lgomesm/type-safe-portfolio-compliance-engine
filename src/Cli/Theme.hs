{-# LANGUAGE OverloadedStrings #-}

-- A identidade visual fica isolada da composicao do relatorio para que trocar
-- a paleta ou os simbolos nao exija alterar regras de compliance.
module Cli.Theme
  ( CliTheme (..)
  , Symbols (..)
  , RenderCapabilities (..)
  , defaultTheme
  , unicodeSymbols
  , asciiSymbols
  , capabilitiesForHandle
  , symbolsForCapabilities
  ) where

import Data.Text (Text)
import System.Console.ANSI (hSupportsANSIColor)
import System.IO (Handle, hIsTerminalDevice)

import Prettyprinter.Render.Terminal
  ( AnsiStyle
  , Color (..)
  , bold
  , color
  , colorDull
  )

data CliTheme = CliTheme
  { titleStyle :: AnsiStyle
  , labelStyle :: AnsiStyle
  , accentStyle :: AnsiStyle
  , successStyle :: AnsiStyle
  , warningStyle :: AnsiStyle
  , failureStyle :: AnsiStyle
  , mutedStyle :: AnsiStyle
  }

-- Os nomes expressam o significado visual, nao a cor concreta, para manter o
-- renderer independente de uma paleta especifica.
defaultTheme :: CliTheme
defaultTheme =
  CliTheme
    { titleStyle = bold <> color Cyan
    , labelStyle = bold
    , accentStyle = bold <> color Cyan
    , successStyle = bold <> color Green
    , warningStyle = bold <> color Yellow
    , failureStyle = bold <> color Red
    , mutedStyle = colorDull White
    }

data Symbols = Symbols
  { successSymbol :: Text
  , failureSymbol :: Text
  , warningSymbol :: Text
  , bulletSymbol :: Text
  , horizontalBar :: Text
  }

-- Simbolos Unicode comuns deixam a saida mais legivel sem exigir Nerd Font.
unicodeSymbols :: Symbols
unicodeSymbols = Symbols "\x2713" "\x2717" "\x21" "\x2022" "\x2500"

-- O conjunto ASCII preserva a hierarquia quando o terminal nao e adequado
-- para Unicode ou quando a saida esta sendo capturada por outra ferramenta.
asciiSymbols :: Symbols
asciiSymbols = Symbols "[OK]" "[X]" "[!]" "-" "-"

data RenderCapabilities = RenderCapabilities
  { ansiEnabled :: Bool
  , unicodeEnabled :: Bool
  }
  deriving (Eq, Show)

capabilitiesForHandle :: Handle -> IO RenderCapabilities
capabilitiesForHandle handle = do
  ansi <- hSupportsANSIColor handle
  terminal <- hIsTerminalDevice handle
  pure
    RenderCapabilities
      -- Mesmo que a biblioteca reporte suporte ANSI, um handle redirecionado
      -- nao deve receber sequencias de controle no arquivo ou no pipe.
      { ansiEnabled = ansi && terminal
      , unicodeEnabled = terminal
      }

symbolsForCapabilities :: RenderCapabilities -> Symbols
symbolsForCapabilities capabilities
  | unicodeEnabled capabilities = unicodeSymbols
  | otherwise = asciiSymbols
