{-# LANGUAGE OverloadedStrings #-}

module Risk.RiskLevelSpec (spec) where

import Test.Hspec

import Domain.AssetClassification (AssetClass (Crypto, PrivateCredit), Sector (Energy))
import Domain.CustomerProfile (CustomerProfile (Conservative))
import Test.PercentageFixtures (percentageLiteral)
import Domain.Ticker (mkTicker)
import Risk.RiskLevel (RiskLevel (..), riskLevelFor, violationSeverity)
import Risk.Violation (Violation (..))

spec :: Spec
spec = describe "Risk.RiskLevel" $ do
  describe "violationSeverity" $ do
    it "assigns weight 1 to asset allocation violations" $
      case mkTicker "PETR4" of
        Right tickerValue ->
          violationSeverity
            (AssetAllocationExceeded tickerValue (percentageLiteral 0.30) (percentageLiteral 0.40))
            `shouldBe` 1
        Left err ->
          expectationFailure (show err)

    it "assigns weight 1 to sector exposure violations" $
      violationSeverity
        (SectorExposureExceeded Energy (percentageLiteral 0.50) (percentageLiteral 0.60))
        `shouldBe` 1

    it "assigns weight 2 to asset class exposure violations" $
      violationSeverity
        (AssetClassExposureExceeded Crypto (percentageLiteral 0.10) (percentageLiteral 0.25))
        `shouldBe` 2

    it "assigns weight 2 to diversification violations" $
      violationSeverity (MinimumDiversificationNotMet 3 2) `shouldBe` 2

    it "assigns weight 2 to suitability violations" $ do
      violationSeverity
        (CustomerProfileCryptoExceeded Conservative (percentageLiteral 0.00) (percentageLiteral 0.25))
        `shouldBe` 2
      violationSeverity
        (AssetClassExposureExceededForProfile PrivateCredit Conservative (percentageLiteral 0.05) (percentageLiteral 0.15))
        `shouldBe` 2

  describe "riskLevelFor" $ do
    it "returns Low for an empty list of violations" $
      riskLevelFor [] `shouldBe` Low

    it "returns Medium at the upper boundary of the medium band" $
      riskLevelFor [MinimumDiversificationNotMet 3 2] `shouldBe` Medium

    it "returns High as soon as the score exceeds 2" $
      riskLevelFor
        [ MinimumDiversificationNotMet 3 2
        , SectorExposureExceeded Energy (percentageLiteral 0.50) (percentageLiteral 0.60)
        ]
        `shouldBe` High

    it "keeps two light violations as Medium rather than High" $
      riskLevelFor
        [ SectorExposureExceeded Energy (percentageLiteral 0.50) (percentageLiteral 0.60)
        , SectorExposureExceeded Energy (percentageLiteral 0.50) (percentageLiteral 0.55)
        ]
        `shouldBe` Medium