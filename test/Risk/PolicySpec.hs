{-# LANGUAGE OverloadedStrings #-}

module Risk.PolicySpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Domain.AssetClassification (AssetClass (..))
import Test.PercentageFixtures (percentageLiteral)
import Domain.Portfolio (mkPortfolio)
import Examples.Customers (moderateMediumScore)
import Generators.Domain (genCustomer)
import Generators.Portfolio (genAnyValidPortfolio)
import Risk.Engine
  ( validateCompliance
  , validateComplianceWithPolicy
  )
import Risk.Policy
  ( Locality (..)
  , Policy (..)
  , assetClassLimitFor
  , defaultPolicyConfig
  , policyConfigFor
  , policyForLocality
  )
import Risk.SuitabilityPolicy (defaultSuitabilityPolicy)
import Risk.Violation (Violation (..))
import Test.PortfolioFixtures (positionsOfWithClass)

spec :: Spec
spec = describe "Risk.Policy" $ do
  describe "defaultPolicyConfig regression" $
    it "preserves the legacy behavior of validateCompliance" $
      forAll genCustomer $ \customerValue ->
        forAll genAnyValidPortfolio $ \portfolioValue ->
          validateCompliance defaultSuitabilityPolicy customerValue portfolioValue
            === validateComplianceWithPolicy defaultPolicyConfig customerValue portfolioValue

  describe "policyConfigFor" $ do
    it "keeps retail aligned with the historic defaults" $ do
      let retailPolicy = policyConfigFor RetailPolicy
      assetClassLimitFor retailPolicy Crypto `shouldBe` Just (percentageLiteral 0.10)
      assetClassLimitFor retailPolicy PrivateCredit `shouldBe` Just (percentageLiteral 0.20)

    it "makes private banking strictly more permissive for crypto than retail" $ do
      let retailCryptoLimit =
            assetClassLimitFor (policyConfigFor RetailPolicy) Crypto
          privateCryptoLimit =
            assetClassLimitFor (policyConfigFor PrivateBankingPolicy) Crypto
      retailCryptoLimit `shouldBe` Just (percentageLiteral 0.10)
      privateCryptoLimit `shouldBe` Just (percentageLiteral 0.15)

  describe "policyForLocality" $
    it "maps Brazil to the default policy and Chile to a distinct one" $ do
      policyForLocality Brazil `shouldBe` defaultPolicyConfig
      policyForLocality Chile `shouldNotBe` defaultPolicyConfig

  describe "policy-based validation" $
    it "rejects an 8% crypto portfolio under RetailPolicy, but approves under PrivateBankingPolicy" $ do
      rawPortfolio <-
        positionsOfWithClass
          [ ("BTC", 0.08, "Crypto")
          , ("TESOURO", 0.52, "FixedIncome")
          , ("PETR4", 0.20, "Equity")
          , ("WEGE3", 0.20, "Equity")
          ]
      case mkPortfolio rawPortfolio of
        Left err ->
          expectationFailure (show err)
        Right portfolioValue -> do
          let retailViolations =
                validateComplianceWithPolicy (policyConfigFor RetailPolicy) moderateMediumScore portfolioValue
              privateBankingViolations =
                validateComplianceWithPolicy (policyConfigFor PrivateBankingPolicy) moderateMediumScore portfolioValue
          retailViolations `shouldSatisfy` any isCryptoProfileViolation
          privateBankingViolations `shouldNotSatisfy` any isCryptoProfileViolation

isCryptoProfileViolation :: Violation -> Bool
isCryptoProfileViolation (CustomerProfileCryptoExceeded _ _ _) = True
isCryptoProfileViolation _ = False