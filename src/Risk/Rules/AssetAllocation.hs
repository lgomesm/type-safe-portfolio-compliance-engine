{-# LANGUAGE TypeFamilies #-}

-- Regra que limita a concentracao de cada ativo na carteira
module Risk.Rules.AssetAllocation (AssetAllocationRule (..), AssetAllocationObservation (..), AssetAllocationEvidence (..), 
  assetAllocationRulePolicy, assetAllocationPolicyLimit, maxAssetAllocation, evaluateAssetAllocation, 
  validateMaxAssetAllocation, validateMaxAssetAllocationWithPolicy) where

import Data.List (nub)
import qualified Data.Map.Strict as Map

import Domain.Asset (Asset (..))
import Domain.Percentage (Percentage, percentageExceeds, unPercentage)
import Domain.Percentage.Internal (clampCalculatedPercentage, percentageLiteral)
import Domain.Portfolio (Portfolio, portfolioPositions)
import Domain.Position (Position (..))
import Domain.Ticker (Ticker)
import Risk.Policy (PolicyConfig, defaultPolicyConfig, maxAssetAllocationLimit)
import Risk.Rules.Input (PolicyPortfolioInput (..))
import Risk.Rules.Policy (RulePolicy)
import Risk.Rules.Types (ComplianceRule (..), RuleEvaluation (..), RuleEvidence, violationsOf)
import Risk.Violation (Violation (..))

data AssetAllocationRule = AssetAllocationRule

data AssetAllocationObservation = AssetAllocationObservation
  { observedAsset :: Asset
  , observedAllocation :: Percentage
  , allowedAllocation :: Percentage
  }
  deriving (Eq, Show)

newtype AssetAllocationEvidence = AssetAllocationEvidence
  { assetAllocationObservations :: [AssetAllocationObservation]
  }
  deriving (Eq, Show)

type instance RuleEvidence AssetAllocationRule = AssetAllocationEvidence

data instance RulePolicy AssetAllocationRule = AssetAllocationRulePolicy
  { assetAllocationPolicyLimit :: Percentage
  }
  deriving (Eq, Show)

assetAllocationRulePolicy :: PolicyConfig -> RulePolicy AssetAllocationRule
assetAllocationRulePolicy policyConfig =
  AssetAllocationRulePolicy (maxAssetAllocationLimit policyConfig)

instance ComplianceRule AssetAllocationRule where
  type RuleInput AssetAllocationRule = PolicyPortfolioInput AssetAllocationRule
  evaluateRule AssetAllocationRule input =
    evaluateWithRulePolicy (inputRulePolicy input) (inputPortfolio input)

maxAssetAllocation :: Percentage
maxAssetAllocation = percentageLiteral 0.30

validateMaxAssetAllocation :: Portfolio -> [Violation]
validateMaxAssetAllocation =
  validateMaxAssetAllocationWithPolicy defaultPolicyConfig

validateMaxAssetAllocationWithPolicy :: PolicyConfig -> Portfolio -> [Violation]
validateMaxAssetAllocationWithPolicy policyConfig =
  violationsOf . evaluateAssetAllocation policyConfig

evaluateAssetAllocation :: PolicyConfig -> Portfolio -> RuleEvaluation AssetAllocationRule
evaluateAssetAllocation policyConfig portfolio =
  evaluateRule
    AssetAllocationRule
    (PolicyPortfolioInput (assetAllocationRulePolicy policyConfig) portfolio)

evaluateWithRulePolicy :: RulePolicy AssetAllocationRule -> Portfolio -> RuleEvaluation AssetAllocationRule
evaluateWithRulePolicy (AssetAllocationRulePolicy limitValue) portfolio =
  RuleEvaluation evidence (violationsFromEvidence evidence)
  where
    evidence :: AssetAllocationEvidence
    evidence =
      AssetAllocationEvidence
        -- Somo a exposição como Double, mas volto pra Percentage antes de entrar na evidencia pra preservar as garantias 
        -- do dominio
        [ AssetAllocationObservation assetValue (clampCalculatedPercentage allocationValue) limitValue
        | (assetValue, allocationValue) <- aggregateAllocationsByTicker (portfolioPositions portfolio)
        ]

-- Agrupando por ticker pq a regra deve considerar a exposicao total ao mesmo ativo, mesmo quando ele aparece em mais de 
-- uma posicao
aggregateAllocationsByTicker :: [Position] -> [(Asset, Double)]
aggregateAllocationsByTicker positions =
  [ (representativeAsset aggregate, aggregatedExposure aggregate)
  | tickerValue <- firstTickers
  , Just aggregate <- [Map.lookup tickerValue aggregates]
  ]
  where
    aggregates :: Map.Map Ticker TickerAggregate
    aggregates =
      Map.fromListWith combineAggregates
        [ (ticker (asset positionValue), TickerAggregate (asset positionValue) (unPercentage (weight positionValue)))
        | positionValue <- positions
        ]

    -- Preservando a ordem da primeira aparicao pra que a evidencia mantenha a mesma ordem observada na carteira original
    firstTickers :: [Ticker]
    firstTickers = nub [ticker (asset positionValue) | positionValue <- positions]

    -- Mantendo o primeiro Asset como representante pq varias posicoes com o mesmo ticker devem gerar uma unica observacao
    combineAggregates :: TickerAggregate -> TickerAggregate -> TickerAggregate
    combineAggregates newer older =
      TickerAggregate
        { representativeAsset = representativeAsset older
        , aggregatedExposure = aggregatedExposure newer + aggregatedExposure older
        }

data TickerAggregate = TickerAggregate
  { representativeAsset :: Asset
  , aggregatedExposure :: Double
  }

violationsFromEvidence :: AssetAllocationEvidence -> [Violation]
violationsFromEvidence (AssetAllocationEvidence observations) =
  [ AssetAllocationExceeded (ticker assetValue) allowedValue observedValue
  | AssetAllocationObservation assetValue observedValue allowedValue <- observations
  , percentageExceeds observedValue allowedValue
  ]