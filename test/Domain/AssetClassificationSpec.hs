{-# LANGUAGE OverloadedStrings #-}

module Domain.AssetClassificationSpec (spec) where

import Test.Hspec

import Data.Text (Text)
import Domain.AssetClassification
  ( AssetClass (..)
  , Sector (..)
  , parseAssetClass
  , parseSector
  )
import Domain.Error (DomainError (..))

spec :: Spec
spec = do
  describe "Domain.AssetClassification.parseAssetClass" $ do
    it "recognizes every AssetClass in its canonical form" $
      mapM_
        (\(input, expected) -> parseAssetClass input `shouldBe` Right expected)
        [ ("Equity", Equity)
        , ("FixedIncome", FixedIncome)
        , ("Crypto", Crypto)
        , ("PrivateCredit", PrivateCredit)
        , ("Fund", Fund)
        , ("Cash", Cash)
        ]

    it "is case-insensitive and ignores surrounding whitespace" $
      parseAssetClass "  cRyPtO  " `shouldBe` Right Crypto

    it "rejects an unknown value while preserving the original input" $
      parseAssetClass "Stonks" `shouldBe` Left (UnknownAssetClass "Stonks")

    it "is exhaustive for every AssetClass constructor" $
      mapM_
        (\assetClassValue ->
          parseAssetClass (canonicalAssetClassText assetClassValue) `shouldBe` Right assetClassValue
        )
        [minBound .. maxBound]

  describe "Domain.AssetClassification.parseSector" $ do
    it "recognizes every Sector in its canonical form" $
      mapM_
        (\(input, expected) -> parseSector input `shouldBe` Right expected)
        [ ("Financial", Financial)
        , ("Energy", Energy)
        , ("Utilities", Utilities)
        , ("Technology", Technology)
        , ("Consumer", Consumer)
        , ("Other", Other)
        ]

    it "rejects an unknown value while preserving the original input" $
      parseSector "made-up-sector" `shouldBe` Left (UnknownSector "made-up-sector")

    it "is exhaustive for every Sector constructor" $
      mapM_
        (\sectorValue ->
          parseSector (canonicalSectorText sectorValue) `shouldBe` Right sectorValue
        )
        [minBound .. maxBound]

canonicalAssetClassText :: AssetClass -> Text
canonicalAssetClassText assetClassValue =
  case assetClassValue of
    Equity -> "Equity"
    FixedIncome -> "FixedIncome"
    Crypto -> "Crypto"
    PrivateCredit -> "PrivateCredit"
    Fund -> "Fund"
    Cash -> "Cash"

canonicalSectorText :: Sector -> Text
canonicalSectorText sectorValue =
  case sectorValue of
    Financial -> "Financial"
    Energy -> "Energy"
    Utilities -> "Utilities"
    Technology -> "Technology"
    Consumer -> "Consumer"
    Other -> "Other"