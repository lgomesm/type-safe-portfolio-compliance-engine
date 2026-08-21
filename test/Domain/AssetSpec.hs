{-# LANGUAGE OverloadedStrings #-}

module Domain.AssetSpec (spec) where

import Test.Hspec

import Domain.Asset (Asset (..))
import Domain.AssetClassification (AssetClass (..), Sector (..))
import Domain.Ticker (mkTicker)

spec :: Spec
spec = describe "Domain.Asset" $
  it "composes a validated ticker, asset class and sector" $
    case mkTicker "PETR4" of
      Left err ->
        expectationFailure ("ticker unexpectedly invalid: " <> show err)
      Right validatedTicker -> do
        let assetValue =
              Asset
                { ticker = validatedTicker
                , assetClass = Equity
                , sector = Energy
                }
        assetClass assetValue `shouldBe` Equity
        sector assetValue `shouldBe` Energy