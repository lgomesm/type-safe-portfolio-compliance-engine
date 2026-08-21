{-# LANGUAGE OverloadedStrings #-}

module Test.PortfolioFixtures
  ( positionsOf
  , positionsOfWithSector
  , positionsOfWithClass
  , positionsOfWithSectorAndClass
  ) where

import qualified Data.Text as T

import Domain.Asset (Asset (..))
import Domain.AssetClassification
  ( parseAssetClass
  , parseSector
  )
import Domain.Percentage (mkPercentage)
import Domain.Position (Position (..))
import Domain.RawPortfolio (RawPortfolio (..))
import Domain.Ticker (mkTicker)

positionsOf :: [(String, Double)] -> IO RawPortfolio
positionsOf tickerWeights =
  positionsOfWithSectorAndClass
    [ (tickerText, rawWeight, "Other", "Equity")
    | (tickerText, rawWeight) <- tickerWeights
    ]

positionsOfWithSector :: [(String, Double, String)] -> IO RawPortfolio
positionsOfWithSector tickerWeightsSectors =
  positionsOfWithSectorAndClass
    [ (tickerText, rawWeight, sectorName, "Equity")
    | (tickerText, rawWeight, sectorName) <- tickerWeightsSectors
    ]

positionsOfWithClass :: [(String, Double, String)] -> IO RawPortfolio
positionsOfWithClass tickerWeightsClasses =
  positionsOfWithSectorAndClass
    [ (tickerText, rawWeight, "Other", assetClassName)
    | (tickerText, rawWeight, assetClassName) <- tickerWeightsClasses
    ]

positionsOfWithSectorAndClass :: [(String, Double, String, String)] -> IO RawPortfolio
positionsOfWithSectorAndClass rows = do
  positions <- mapM toPosition rows
  pure (RawPortfolio positions)
  where
    toPosition :: (String, Double, String, String) -> IO Position
    toPosition (tickerText, rawWeight, sectorText, assetClassText) =
      case
        ( mkTicker (T.pack tickerText)
        , mkPercentage rawWeight
        , parseSector (T.pack sectorText)
        , parseAssetClass (T.pack assetClassText)
        ) of
        (Right validatedTicker, Right validatedWeight, Right validatedSector, Right validatedAssetClass) ->
          pure
            Position
              { asset =
                  Asset
                    { ticker = validatedTicker
                    , assetClass = validatedAssetClass
                    , sector = validatedSector
                    }
              , weight = validatedWeight
              }
        _ ->
          fail ("invalid fixture row: " <> show (tickerText, rawWeight, sectorText, assetClassText))