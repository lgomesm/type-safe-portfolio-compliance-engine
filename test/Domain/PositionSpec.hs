{-# LANGUAGE OverloadedStrings #-}

module Domain.PositionSpec (spec) where

import Test.Hspec

import Domain.Asset (Asset (..))
import Domain.AssetClassification (AssetClass (..), Sector (..))
import Domain.Percentage (mkPercentage, unPercentage)
import Domain.Position (Position (..))
import Domain.Ticker (mkTicker)

spec :: Spec
spec = describe "Domain.Position" $
  it "composes a validated asset and percentage" $
    case (mkTicker "BTC", mkPercentage 0.25) of
      (Right validatedTicker, Right validatedPercentage) -> do
        let assetValue =
              Asset
                { ticker = validatedTicker
                , assetClass = Crypto
                , sector = Other
                }
            positionValue =
              Position
                { asset = assetValue
                , weight = validatedPercentage
                }
        assetClass (asset positionValue) `shouldBe` Crypto
        unPercentage (weight positionValue) `shouldBe` 0.25
      _ ->
        expectationFailure "ticker or percentage unexpectedly invalid"