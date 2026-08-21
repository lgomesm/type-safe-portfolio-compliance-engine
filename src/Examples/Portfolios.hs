{-# LANGUAGE OverloadedStrings #-}

-- | Cenários prontos pra demonstrar o sistema de ponta a ponta e facilitar a utilização/validação do motor
-- Estou mantendo as carteiras como RawPortfolio pra que os exemplos passem pelas mesmas validacoes estruturais aplicadas 
-- a qlqr outra entrada
module Examples.Portfolios (approved, assetAllocationRisk, cryptoRisk, policySensitiveCrypto, privateCreditGlobal, 
  privateCreditProfile, underDiversified, scenarios, scenarioNames) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)

import Domain.Asset (Asset (..))
import Domain.AssetClassification (AssetClass (..), Sector (..))
import Domain.Percentage (Percentage, mkPercentage)
import Domain.Position (Position (..))
import Domain.RawPortfolio (RawPortfolio (..))
import Domain.Ticker (Ticker, mkTicker)

-- Referencia de uma carteira que nao viola nenhuma das regras demonstradas pelos demais cenarios
approved :: RawPortfolio
approved =
  RawPortfolio
    [ position "PETR4" Equity Energy 0.30
    , position "ITUB4" Equity Financial 0.30
    , position "VALE3" Equity Other 0.20
    , position "WEGE3" Equity Technology 0.20
    ]

-- Usando duas concentracoes acima do limite pra demonstrar como multiplas violacoes podem contribuir pro nivel de risco 
-- final
assetAllocationRisk :: RawPortfolio
assetAllocationRisk =
  RawPortfolio
    [ position "PETR4" Equity Energy 0.40
    , position "ITUB4" Equity Financial 0.35
    , position "VALE3" Equity Other 0.25
    ]

-- Mantendo as demais exposicoes dentro dos limites pra destacar só a violacao causada pela concentracao em cripto
cryptoRisk :: RawPortfolio
cryptoRisk =
  RawPortfolio
    [ position "BTC" Crypto Other 0.25
    , position "ITUB4" Equity Financial 0.30
    , position "VALE3" Equity Other 0.25
    , position "WEGE3" Equity Technology 0.20
    ]

-- Usando uma exposicao a cripto pra produzir resultados diferentes quando a mesma carteira é avaliada por politicas com 
-- limites distintos
policySensitiveCrypto :: RawPortfolio
policySensitiveCrypto =
  RawPortfolio
    [ position "BTC" Crypto Other 0.08
    , position "CASH" Cash Consumer 0.12
    , position "TESOURO" FixedIncome Utilities 0.25
    , position "PETR4" Equity Energy 0.22
    , position "WEGE3" Equity Technology 0.18
    , position "ITUB4" Equity Financial 0.08
    , position "VALE3" Equity Other 0.07
    ]

-- Manetendo a exposicao dentro do limite associado ao perfil pra que a violacao observada venha apenas do limite global 
-- de credito privado
privateCreditGlobal :: RawPortfolio
privateCreditGlobal =
  RawPortfolio
    [ position "TESOURO" FixedIncome Other 0.25
    , position "DEB001" PrivateCredit Financial 0.25
    , position "PETR4" Equity Energy 0.25
    , position "WEGE3" Equity Technology 0.25
    ]

-- Mantendo a exposicao abaixo do limite global pra que o cenario destaque só a restricao de credito privado associada 
-- ao perfil do cliente
privateCreditProfile :: RawPortfolio
privateCreditProfile =
  RawPortfolio
    [ position "TESOURO" FixedIncome Other 0.30
    , position "CDB001" FixedIncome Utilities 0.30
    , position "DEB001" PrivateCredit Financial 0.15
    , position "PETR4" Equity Energy 0.25
    ]

-- Usando só dois ativos pra demonstrar a regra de diversificacao. Nesse caso, a concentracao elevada é esperada pq, com 
-- só dois ativos somando 100%, tb é inevitavel ultrapassar o limite individual configurado
underDiversified :: RawPortfolio
underDiversified =
  RawPortfolio
    [ position "PETR4" Equity Energy 0.50
    , position "ITUB4" Equity Financial 0.50
    ]

scenarios :: Map Text RawPortfolio
scenarios =
  Map.fromList
    [ ("approved", approved)
    , ("asset-allocation-risk", assetAllocationRisk)
    , ("crypto-risk", cryptoRisk)
    , ("policy-sensitive-crypto", policySensitiveCrypto)
    , ("private-credit-global", privateCreditGlobal)
    , ("private-credit-profile", privateCreditProfile)
    , ("under-diversified", underDiversified)
    ]

scenarioNames :: [Text]
scenarioNames =
  [ "approved"
  , "asset-allocation-risk"
  , "crypto-risk"
  , "policy-sensitive-crypto"
  , "private-credit-global"
  , "private-credit-profile"
  , "under-diversified"
  ]

position :: Text -> AssetClass -> Sector -> Double -> Position
position tickerText assetClassValue sectorValue rawWeight =
  Position
    { asset = mkAsset tickerText assetClassValue sectorValue
    , weight = mkWeight rawWeight
    }

mkAsset :: Text -> AssetClass -> Sector -> Asset
mkAsset tickerText assetClassValue sectorValue =
  Asset
    { ticker = mkTickerUnsafe tickerText
    , assetClass = assetClassValue
    , sector = sectorValue
    }

mkTickerUnsafe :: Text -> Ticker
mkTickerUnsafe rawTicker =
  case mkTicker rawTicker of
    Right tickerValue -> tickerValue
    Left err -> error ("cenario de exemplo com ticker invalido: " <> show err)

mkWeight :: Double -> Percentage
mkWeight rawWeight =
  case mkPercentage rawWeight of
    Right percentageValue -> percentageValue
    Left err -> error ("cenario de exemplo com percentual invalido: " <> show err)