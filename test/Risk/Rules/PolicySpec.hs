module Risk.Rules.PolicySpec (spec) where

import Test.Hspec

import Risk.Policy
  ( Policy (..)
  , assetClassLimits
  , cryptoAssetClassLimit
  , maxAssetAllocationLimit
  , maxSectorExposureLimit
  , policyConfigFor
  , privateCreditAssetClassLimit
  )
import Risk.Rules.AssetAllocation
  ( assetAllocationPolicyLimit
  , assetAllocationRulePolicy
  )
import Risk.Rules.AssetClassExposure
  ( assetClassExposureRulePolicy
  , cryptoExposurePolicyLimit
  , privateCreditExposurePolicyLimit
  )
import Risk.Rules.SectorExposure
  ( sectorExposurePolicyLimit
  , sectorExposureRulePolicy
  )

spec :: Spec
spec = describe "Risk.Rules.Policy" $ do
  it "projects the asset allocation limit for each policy" $ do
    let retail = policyConfigFor RetailPolicy
        privateBanking = policyConfigFor PrivateBankingPolicy
    assetAllocationPolicyLimit (assetAllocationRulePolicy retail)
      `shouldBe` maxAssetAllocationLimit retail
    assetAllocationPolicyLimit (assetAllocationRulePolicy privateBanking)
      `shouldBe` maxAssetAllocationLimit privateBanking

  it "projects the sector exposure limit for each policy" $ do
    let retail = policyConfigFor RetailPolicy
        privateBanking = policyConfigFor PrivateBankingPolicy
    sectorExposurePolicyLimit (sectorExposureRulePolicy retail)
      `shouldBe` maxSectorExposureLimit retail
    sectorExposurePolicyLimit (sectorExposureRulePolicy privateBanking)
      `shouldBe` maxSectorExposureLimit privateBanking

  it "projects optional class limits without inventing a fallback value" $ do
    let retail = policyConfigFor RetailPolicy
        privateBanking = policyConfigFor PrivateBankingPolicy
    cryptoExposurePolicyLimit (assetClassExposureRulePolicy retail)
      `shouldBe` cryptoAssetClassLimit (assetClassLimits retail)
    privateCreditExposurePolicyLimit (assetClassExposureRulePolicy privateBanking)
      `shouldBe` privateCreditAssetClassLimit (assetClassLimits privateBanking)