-- Politica configuravel de suitability aplicada sobre um cliente
module Risk.SuitabilityPolicy (SuitabilityPolicy (..), defaultSuitabilityPolicy) where

import Domain.CreditScore (CreditScore (..))
import Domain.CustomerProfile (CustomerProfile (..))

data SuitabilityPolicy = SuitabilityPolicy
  { maxCryptoExposureByProfile :: CustomerProfile -> Double
  , maxRiskExposureByScore :: CreditScore -> Double
  }

defaultSuitabilityPolicy :: SuitabilityPolicy
defaultSuitabilityPolicy =
  SuitabilityPolicy
    { maxCryptoExposureByProfile = cryptoLimit
    , maxRiskExposureByScore = riskLimit
    }
  where
    cryptoLimit Conservative = 0.00
    cryptoLimit Moderate = 0.05
    cryptoLimit Aggressive = 0.10

    riskLimit LowScore = 0.40
    riskLimit MediumScore = 0.70
    riskLimit HighScore = 1.00