-- Configuracoes de politica de compliance
module Risk.Policy (AssetClassLimits (..), Locality (..), Policy (..), PolicyConfig (..), ProfileLimits (..), 
  ScoreLimits (..), assetClassLimitFor, defaultPolicyConfig, policyConfigFor, policyConfigWithSuitability, 
  policyForLocality, profileLimitFor, scoreLimitFor) where

import Domain.AssetClassification (AssetClass (..))
import Domain.CreditScore (CreditScore (..))
import Domain.CustomerProfile (CustomerProfile (..))
import Domain.Percentage (Percentage)
import Domain.Percentage.Internal (clampCalculatedPercentage, percentageLiteral)
import Risk.SuitabilityPolicy (SuitabilityPolicy, maxCryptoExposureByProfile, maxRiskExposureByScore)

data Policy
  = RetailPolicy
  | PrivateBankingPolicy
  deriving (Eq, Show, Enum, Bounded)

data Locality
  = Brazil
  | Chile
  deriving (Eq, Show, Enum, Bounded)

data ProfileLimits = ProfileLimits
  { conservativeLimit :: Percentage
  , moderateLimit :: Percentage
  , aggressiveLimit :: Percentage
  }
  deriving (Eq, Show)

data ScoreLimits = ScoreLimits
  { lowScoreLimit :: Percentage
  , mediumScoreLimit :: Percentage
  , highScoreLimit :: Percentage
  }
  deriving (Eq, Show)

data AssetClassLimits = AssetClassLimits
  { cryptoAssetClassLimit :: Maybe Percentage
  , privateCreditAssetClassLimit :: Maybe Percentage
  }
  deriving (Eq, Show)

data PolicyConfig = PolicyConfig
  { maxAssetAllocationLimit :: Percentage
  , maxSectorExposureLimit :: Percentage
  , assetClassLimits :: AssetClassLimits
  , cryptoProfileLimits :: ProfileLimits
  , privateCreditProfileLimits :: ProfileLimits
  , riskScoreLimits :: ScoreLimits
  }
  deriving (Eq, Show)

-- A politica padrao usa os mesmos limites da politica de varejo pra que as chamadas sem config explicita mantenham um 
-- comportamento "previsivel"
defaultPolicyConfig :: PolicyConfig
defaultPolicyConfig = policyConfigFor RetailPolicy

policyConfigFor :: Policy -> PolicyConfig
policyConfigFor RetailPolicy =
  PolicyConfig
    { maxAssetAllocationLimit = percentageLiteral 0.30
    , maxSectorExposureLimit = percentageLiteral 0.50
    , assetClassLimits =
        AssetClassLimits
          { cryptoAssetClassLimit = Just (percentageLiteral 0.10)
          , privateCreditAssetClassLimit = Just (percentageLiteral 0.20)
          }
    , cryptoProfileLimits =
        ProfileLimits
          { conservativeLimit = percentageLiteral 0.00
          , moderateLimit = percentageLiteral 0.05
          , aggressiveLimit = percentageLiteral 0.10
          }
    , privateCreditProfileLimits =
        ProfileLimits
          { conservativeLimit = percentageLiteral 0.05
          , moderateLimit = percentageLiteral 0.15
          , aggressiveLimit = percentageLiteral 0.25
          }
    , riskScoreLimits =
        ScoreLimits
          { lowScoreLimit = percentageLiteral 0.40
          , mediumScoreLimit = percentageLiteral 0.70
          , highScoreLimit = percentageLiteral 1.00
          }
    }
policyConfigFor PrivateBankingPolicy =
  PolicyConfig
    { maxAssetAllocationLimit = percentageLiteral 0.35
    , maxSectorExposureLimit = percentageLiteral 0.60
    , assetClassLimits =
        AssetClassLimits
          { cryptoAssetClassLimit = Just (percentageLiteral 0.15)
          , privateCreditAssetClassLimit = Just (percentageLiteral 0.25)
          }
    , cryptoProfileLimits =
        ProfileLimits
          { conservativeLimit = percentageLiteral 0.02
          , moderateLimit = percentageLiteral 0.10
          , aggressiveLimit = percentageLiteral 0.15
          }
    , privateCreditProfileLimits =
        ProfileLimits
          { conservativeLimit = percentageLiteral 0.08
          , moderateLimit = percentageLiteral 0.20
          , aggressiveLimit = percentageLiteral 0.30
          }
    , riskScoreLimits =
        ScoreLimits
          { lowScoreLimit = percentageLiteral 0.50
          , mediumScoreLimit = percentageLiteral 0.80
          , highScoreLimit = percentageLiteral 1.00
          }
    }

policyForLocality :: Locality -> PolicyConfig
policyForLocality Brazil = defaultPolicyConfig
policyForLocality Chile =
  PolicyConfig
    { maxAssetAllocationLimit = percentageLiteral 0.25
    , maxSectorExposureLimit = percentageLiteral 0.45
    , assetClassLimits =
        AssetClassLimits
          { cryptoAssetClassLimit = Just (percentageLiteral 0.05)
          , privateCreditAssetClassLimit = Just (percentageLiteral 0.15)
          }
    , cryptoProfileLimits =
        ProfileLimits
          { conservativeLimit = percentageLiteral 0.00
          , moderateLimit = percentageLiteral 0.03
          , aggressiveLimit = percentageLiteral 0.08
          }
    , privateCreditProfileLimits =
        ProfileLimits
          { conservativeLimit = percentageLiteral 0.03
          , moderateLimit = percentageLiteral 0.10
          , aggressiveLimit = percentageLiteral 0.18
          }
    , riskScoreLimits =
        ScoreLimits
          { lowScoreLimit = percentageLiteral 0.35
          , mediumScoreLimit = percentageLiteral 0.60
          , highScoreLimit = percentageLiteral 0.90
          }
    }

assetClassLimitFor :: PolicyConfig -> AssetClass -> Maybe Percentage
assetClassLimitFor policyConfig Crypto =
  cryptoAssetClassLimit (assetClassLimits policyConfig)
assetClassLimitFor policyConfig PrivateCredit =
  privateCreditAssetClassLimit (assetClassLimits policyConfig)
assetClassLimitFor _ _ = Nothing

profileLimitFor :: ProfileLimits -> CustomerProfile -> Percentage
profileLimitFor limits Conservative = conservativeLimit limits
profileLimitFor limits Moderate = moderateLimit limits
profileLimitFor limits Aggressive = aggressiveLimit limits

scoreLimitFor :: ScoreLimits -> CreditScore -> Percentage
scoreLimitFor limits LowScore = lowScoreLimit limits
scoreLimitFor limits MediumScore = mediumScoreLimit limits
scoreLimitFor limits HighScore = highScoreLimit limits

policyConfigWithSuitability :: SuitabilityPolicy -> PolicyConfig
policyConfigWithSuitability suitabilityPolicy =
  defaultPolicyConfig
    { cryptoProfileLimits =
        ProfileLimits
          { conservativeLimit = clampCalculatedPercentage (maxCryptoExposureByProfile suitabilityPolicy Conservative)
          , moderateLimit = clampCalculatedPercentage (maxCryptoExposureByProfile suitabilityPolicy Moderate)
          , aggressiveLimit = clampCalculatedPercentage (maxCryptoExposureByProfile suitabilityPolicy Aggressive)
          }
    , riskScoreLimits =
        ScoreLimits
          { lowScoreLimit = clampCalculatedPercentage (maxRiskExposureByScore suitabilityPolicy LowScore)
          , mediumScoreLimit = clampCalculatedPercentage (maxRiskExposureByScore suitabilityPolicy MediumScore)
          , highScoreLimit = clampCalculatedPercentage (maxRiskExposureByScore suitabilityPolicy HighScore)
          }
    }