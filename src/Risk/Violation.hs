-- Violacoes de regras de compliance avaliadas em runtime
module Risk.Violation (Violation (..)) where

import Domain.AssetClassification (AssetClass, Sector)
import Domain.CreditScore (CreditScore)
import Domain.CustomerProfile (CustomerProfile)
import Domain.Percentage (Percentage)
import Domain.Ticker (Ticker)

data Violation
  = AssetAllocationExceeded Ticker Percentage Percentage
  | SectorExposureExceeded Sector Percentage Percentage
  | AssetClassExposureExceeded AssetClass Percentage Percentage
  | MinimumDiversificationNotMet Int Int
  | CustomerProfileCryptoExceeded CustomerProfile Percentage Percentage
  | CustomerCreditRiskExposureExceeded CreditScore Percentage Percentage
  | AssetClassExposureExceededForProfile AssetClass CustomerProfile Percentage Percentage
  deriving (Eq, Show)