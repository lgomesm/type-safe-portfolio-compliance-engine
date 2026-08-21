module Risk.EnginePropSpec (spec) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

import Domain.Asset (Asset (assetClass, sector, ticker))
import Domain.AssetClassification
  ( AssetClass (Crypto, PrivateCredit)
  , Sector (..)
  )
import Domain.Percentage (unPercentage)
import Domain.Portfolio (Portfolio, portfolioPositions)
import Domain.Position (Position (asset, weight))
import Domain.Ticker (Ticker)
import Generators.Portfolio (genAnyValidPortfolio)
import Risk.Engine (validatePortfolio)
import Risk.Violation (Violation (..))

spec :: Spec
spec = describe "Risk.Engine (properties)" $ do
  it "crypto produces a class exposure violation iff manual aggregated exposure exceeds 10%" $
    forAll genAnyValidPortfolio $ \portfolioValue ->
      let cryptoExposure =
            sum
              [ unPercentage (weight positionValue)
              | positionValue <- portfolioPositions portfolioValue
              , assetClass (asset positionValue) == Crypto
              ]
          violationsFound = validatePortfolio portfolioValue
          hasCryptoViolation = any isCryptoViolation violationsFound
       in classify (cryptoExposure > 0.10) "exceeds crypto limit" $
            hasCryptoViolation === (cryptoExposure > 0.10)

  it "private credit produces a class exposure violation iff manual aggregated exposure exceeds 20%" $
    forAll genAnyValidPortfolio $ \portfolioValue ->
      let privateCreditExposure =
            sum
              [ unPercentage (weight positionValue)
              | positionValue <- portfolioPositions portfolioValue
              , assetClass (asset positionValue) == PrivateCredit
              ]
          violationsFound = validatePortfolio portfolioValue
          hasPrivateCreditViolation = any isPrivateCreditViolation violationsFound
       in classify (privateCreditExposure > 0.20) "exceeds private credit limit" $
            hasPrivateCreditViolation === (privateCreditExposure > 0.20)

  it "asset allocation violations match manually aggregated ticker exposure" $
    forAll genAnyValidPortfolio $ \portfolioValue ->
      let tickerOrder = nub (map (ticker . asset) (portfolioPositions portfolioValue))
          exposureByTicker =
            Map.fromListWith (+)
              [ (ticker (asset positionValue), unPercentage (weight positionValue))
              | positionValue <- portfolioPositions portfolioValue
              ]
          expectedTickers =
            [ tickerValue
            | tickerValue <- tickerOrder
            , Map.findWithDefault 0 tickerValue exposureByTicker > 0.30
            ]
          actualTickers =
            [ tickerValue
            | AssetAllocationExceeded tickerValue _ _ <- validatePortfolio portfolioValue
            ]
       in classify (null expectedTickers) "no asset allocation violations" $
            expectedTickers === actualTickers

  it "sector exposure violations match manual sector sums above 50%" $
    forAll genAnyValidPortfolio $ \portfolioValue ->
      let expectedSectors = manualSectorViolations portfolioValue
          actualSectors =
            [ sectorValue
            | SectorExposureExceeded sectorValue _ _ <- validatePortfolio portfolioValue
            ]
       in classify (null expectedSectors) "no sector violations" $
            expectedSectors === actualSectors

  it "diversification violation appears iff the portfolio has fewer than 3 distinct tickers" $
    forAll genAnyValidPortfolio $ \portfolioValue ->
      let distinctTickerCount = countDistinctTickers portfolioValue
          hasDiversificationViolation =
            any isDiversificationViolation (validatePortfolio portfolioValue)
       in classify (distinctTickerCount < 3) "under-diversified" $
            hasDiversificationViolation === (distinctTickerCount < 3)

isCryptoViolation :: Violation -> Bool
isCryptoViolation (AssetClassExposureExceeded Crypto _ _) = True
isCryptoViolation _ = False

isPrivateCreditViolation :: Violation -> Bool
isPrivateCreditViolation (AssetClassExposureExceeded PrivateCredit _ _) = True
isPrivateCreditViolation _ = False

isDiversificationViolation :: Violation -> Bool
isDiversificationViolation (MinimumDiversificationNotMet _ _) = True
isDiversificationViolation _ = False

manualSectorViolations :: Portfolio -> [Sector]
manualSectorViolations portfolioValue =
  [ sectorValue
  | sectorValue <- allSectorsInOrder
  , sectorExposure sectorValue > 0.50
  ]
  where
    sectorExposure :: Sector -> Double
    sectorExposure sectorValue =
      sum
        [ unPercentage (weight positionValue)
        | positionValue <- portfolioPositions portfolioValue
        , sector (asset positionValue) == sectorValue
        ]

allSectorsInOrder :: [Sector]
allSectorsInOrder =
  [ Financial
  , Energy
  , Utilities
  , Technology
  , Consumer
  , Other
  ]

countDistinctTickers :: Portfolio -> Int
countDistinctTickers portfolioValue =
  length (foldr insertIfMissing [] tickersInOrder)
  where
    tickersInOrder :: [Ticker]
    tickersInOrder = map (ticker . asset) (portfolioPositions portfolioValue)

    insertIfMissing :: Ticker -> [Ticker] -> [Ticker]
    insertIfMissing tickerValue seen
      | tickerValue `elem` seen = seen
      | otherwise = tickerValue : seen