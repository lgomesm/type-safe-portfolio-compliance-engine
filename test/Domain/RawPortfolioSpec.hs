{-# LANGUAGE OverloadedStrings #-}

module Domain.RawPortfolioSpec (spec) where

import Test.Hspec

import Domain.Asset (Asset (..))
import Domain.AssetClassification (AssetClass (..), Sector (..))
import Domain.Percentage (mkPercentage)
import Domain.Position (Position (..))
import Domain.RawPortfolio (RawPortfolio (..))
import Domain.Ticker (mkTicker)

spec :: Spec
spec = describe "Domain.RawPortfolio" $
  it "accepts any list of positions without validating the total weight" $
    case (mkTicker "PETR4", mkPercentage 0.9) of
      (Right validatedTicker, Right validatedPercentage) -> do
        let assetValue =
              Asset
                { ticker = validatedTicker
                , assetClass = Equity
                , sector = Energy
                }
            positionValue =
              Position
                { asset = assetValue
                , weight = validatedPercentage
                }
            rawPortfolio = RawPortfolio [positionValue]
        rawPositions rawPortfolio `shouldBe` [positionValue]
      _ ->
        expectationFailure "ticker or percentage unexpectedly invalid"