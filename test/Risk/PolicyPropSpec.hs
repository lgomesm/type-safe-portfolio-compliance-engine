module Risk.PolicyPropSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Domain.Percentage (unPercentage)
import Risk.Policy
  ( Locality (..)
  , assetClassLimitFor
  , policyConfigFor
  , policyForLocality
  )
import Domain.AssetClassification (AssetClass (Crypto, PrivateCredit))

spec :: Spec
spec = describe "Risk.Policy (properties)" $ do
  it "keeps configured asset class limits inside the closed percentage interval" $
    property $
      conjoin
        [ maybe (property True) (\limitValue -> counterexample (show (policyValue, assetClassValue)) $
              unPercentage limitValue >= 0 .&&. unPercentage limitValue <= 1)
            (assetClassLimitFor (policyConfigFor policyValue) assetClassValue)
        | policyValue <- [minBound .. maxBound]
        , assetClassValue <- [Crypto, PrivateCredit]
        ]

  it "returns policy configs for every declared locality" $
    property $
      conjoin
        [ counterexample (show localityValue) $
            let _ = policyForLocality localityValue
             in property True
        | localityValue <- [minBound .. maxBound :: Locality]
        ]