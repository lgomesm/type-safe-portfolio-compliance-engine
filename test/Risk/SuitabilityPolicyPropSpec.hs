module Risk.SuitabilityPolicyPropSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Domain.CreditScore (CreditScore (..))
import Domain.CustomerProfile (CustomerProfile (..))
import Risk.SuitabilityPolicy
  ( defaultSuitabilityPolicy
  , maxCryptoExposureByProfile
  , maxRiskExposureByScore
  )

spec :: Spec
spec = describe "Risk.SuitabilityPolicy (properties)" $ do
  it "keeps crypto limits within the closed percentage interval" $
    property $
      conjoin
        [ counterexample (show profileValue) $
            let value = maxCryptoExposureByProfile defaultSuitabilityPolicy profileValue
             in value >= 0 .&&. value <= 1
        | profileValue <- [minBound .. maxBound]
        ]

  it "keeps score-based risk limits within the closed percentage interval" $
    property $
      conjoin
        [ counterexample (show scoreValue) $
            let value = maxRiskExposureByScore defaultSuitabilityPolicy scoreValue
             in value >= 0 .&&. value <= 1
        | scoreValue <- [minBound .. maxBound]
        ]

  it "is monotonic across customer profiles" $
    property $
      maxCryptoExposureByProfile defaultSuitabilityPolicy Conservative
        <= maxCryptoExposureByProfile defaultSuitabilityPolicy Moderate
        .&&. maxCryptoExposureByProfile defaultSuitabilityPolicy Moderate
        <= maxCryptoExposureByProfile defaultSuitabilityPolicy Aggressive

  it "is monotonic across credit score bands" $
    property $
      maxRiskExposureByScore defaultSuitabilityPolicy LowScore
        <= maxRiskExposureByScore defaultSuitabilityPolicy MediumScore
        .&&. maxRiskExposureByScore defaultSuitabilityPolicy MediumScore
        <= maxRiskExposureByScore defaultSuitabilityPolicy HighScore