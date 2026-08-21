{-# LANGUAGE OverloadedStrings #-}

-- Mantendo essa composicao separada das carteiras pra que os mesmos exemplos possam ser reutilizados com diferentes 
-- contextos de cliente e politica tb
module Examples.Scenarios (PolicyResolutionError (..), PolicySource (..), Scenario (..), resolveScenarioPolicy, 
  scenarioNames, scenarios) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)

import Domain.Customer (Customer)
import Domain.RawPortfolio (RawPortfolio)
import Examples.Customers (aggressiveHighScore, conservativeHighScore, conservativeLowScore, moderateMediumScore)
import Examples.Portfolios (approved, cryptoRisk, assetAllocationRisk, policySensitiveCrypto, privateCreditGlobal, 
  privateCreditProfile, underDiversified)
import Risk.Policy (Locality, Policy (PrivateBankingPolicy, RetailPolicy), PolicyConfig, defaultPolicyConfig, 
  policyConfigFor, policyForLocality)

-- Guardando a origem da politica em vez da config pronta pra que a politica correta possa ser resolvida de acordo com o 
-- contexto da execucao
data PolicySource
  = DefaultPolicy
  | SegmentPolicy Policy
  deriving (Eq, Show)

data Scenario = Scenario
  { scenarioCustomer :: Customer
  , scenarioPortfolio :: RawPortfolio
  , scenarioPolicySource :: PolicySource
  }
  deriving (Eq, Show)

data PolicyResolutionError
  = SegmentPolicyCannotUseLocality Policy Locality
  deriving (Eq, Show)

resolveScenarioPolicy :: Maybe Locality -> PolicySource -> Either PolicyResolutionError PolicyConfig
resolveScenarioPolicy Nothing DefaultPolicy = Right defaultPolicyConfig
resolveScenarioPolicy (Just locality) DefaultPolicy = Right (policyForLocality locality)
resolveScenarioPolicy Nothing (SegmentPolicy policy) = Right (policyConfigFor policy)
resolveScenarioPolicy (Just locality) (SegmentPolicy policy) =
  Left (SegmentPolicyCannotUseLocality policy locality)

scenarios :: Map Text Scenario
scenarios =
  Map.fromList
    [ ("approved", Scenario conservativeHighScore approved DefaultPolicy)
    , ("asset-allocation-risk", Scenario aggressiveHighScore assetAllocationRisk DefaultPolicy)
    , ("crypto-risk", Scenario aggressiveHighScore cryptoRisk DefaultPolicy)
    , ("private-credit-global", Scenario aggressiveHighScore privateCreditGlobal DefaultPolicy)
    , ("private-credit-profile", Scenario conservativeHighScore privateCreditProfile DefaultPolicy)
    , ("under-diversified", Scenario conservativeHighScore underDiversified DefaultPolicy)
    , ("conservative-crypto", Scenario conservativeLowScore cryptoRisk DefaultPolicy)
    , ("policy-retail", Scenario moderateMediumScore policySensitiveCrypto (SegmentPolicy RetailPolicy))
    , ("policy-private-banking", Scenario moderateMediumScore policySensitiveCrypto (SegmentPolicy PrivateBankingPolicy))
    ]

scenarioNames :: [Text]
scenarioNames =
  [ "approved"
  , "asset-allocation-risk"
  , "crypto-risk"
  , "private-credit-global"
  , "private-credit-profile"
  , "under-diversified"
  , "conservative-crypto"
  , "policy-retail"
  , "policy-private-banking"
  ]