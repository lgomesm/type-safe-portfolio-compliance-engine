{-# LANGUAGE OverloadedStrings #-}

module Generators.Portfolio
  ( genValidPortfolioRaw
  , genRawPortfolioWithTotal
  , genAnyValidPortfolio
  ) where

import Control.Monad (zipWithM)
import qualified Data.Text as T
import Test.QuickCheck

import Domain.Asset (Asset (..))
import Domain.AssetClassification (AssetClass (..), Sector (..))
import Domain.Percentage (mkPercentage)
import Domain.Portfolio (Portfolio, mkPortfolio)
import Domain.Position (Position (..))
import Domain.RawPortfolio (RawPortfolio (..))
import Generators.Domain (genAssetWithTicker)

genValidPortfolioRaw :: Int -> Gen RawPortfolio
genValidPortfolioRaw positionCount = do
  rawWeights <- vectorOf positionCount (choose (0.01, 1.0) :: Gen Double)
  let totalWeight = sum rawWeights
      normalizedWeights = map (/ totalWeight) rawWeights
  positions <- zipWithM attachWeight [1 .. positionCount] normalizedWeights
  pure (RawPortfolio positions)
  where
    attachWeight :: Int -> Double -> Gen Position
    attachWeight index normalizedWeight = do
      baseAsset <- genAssetWithTicker (T.pack ("A" <> show index))
      let assetValue =
            baseAsset
              { assetClass = classForIndex index
              , sector = sectorForIndex index
              }
      case mkPercentage normalizedWeight of
        Right percentageValue ->
          pure Position {asset = assetValue, weight = percentageValue}
        Left _ ->
          attachWeight index normalizedWeight

genRawPortfolioWithTotal :: Double -> Gen RawPortfolio
genRawPortfolioWithTotal targetTotal = do
  positionCount <- choose (max 2 (ceiling targetTotal), 10)
  let evenShare = targetTotal / fromIntegral positionCount
  positions <- mapM (attachWeight evenShare) [1 .. positionCount]
  pure (RawPortfolio positions)
  where
    attachWeight :: Double -> Int -> Gen Position
    attachWeight share index = do
      baseAsset <- genAssetWithTicker (T.pack ("B" <> show index))
      let assetValue =
            baseAsset
              { assetClass = classForIndex index
              , sector = sectorForIndex index
              }
      case mkPercentage share of
        Right percentageValue ->
          pure Position {asset = assetValue, weight = percentageValue}
        Left _ ->
          error "genRawPortfolioWithTotal produziu share invalido inesperadamente"

genAnyValidPortfolio :: Gen Portfolio
genAnyValidPortfolio = do
  positionCount <- choose (1, 30)
  rawPortfolio <- genValidPortfolioRaw positionCount
  case mkPortfolio rawPortfolio of
    Right portfolioValue -> pure portfolioValue
    Left _ -> genAnyValidPortfolio

classForIndex :: Int -> AssetClass
classForIndex index =
  case index `mod` 6 of
    0 -> Equity
    1 -> FixedIncome
    2 -> Crypto
    3 -> PrivateCredit
    4 -> Fund
    _ -> Cash

sectorForIndex :: Int -> Sector
sectorForIndex index =
  case index `mod` 6 of
    0 -> Financial
    1 -> Energy
    2 -> Utilities
    3 -> Technology
    4 -> Consumer
    _ -> Other