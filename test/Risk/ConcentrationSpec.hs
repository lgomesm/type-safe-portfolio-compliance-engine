{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Risk.ConcentrationSpec (spec) where

import Data.List (sortBy)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Domain.Asset (Asset (ticker))
import Domain.Percentage (unPercentage)
import Domain.Portfolio (Portfolio, mkPortfolio, portfolioPositions)
import Domain.Position (Position (asset, weight))
import Domain.SafeVector (N2, N3, SNat, Vec, sN1, sN2, sN3, snatToInt, vecToList)
import Domain.Ticker (unTicker)
import Generators.Portfolio (genValidPortfolioRaw)
import Risk.Concentration
  ( ConcentrationError (..)
  , ConcentrationRequest (DynamicTop, StaticTop)
  , SomeTopPositions (..)
  , analyzeConcentration
  , renderConcentration
  , largestPosition
  , secondLargestPosition
  , thirdLargestPosition
  , topPositionsByCount
  , topNPositions
  , top3Positions
  )
import Risk.Rules.Diversification (validateMinimumDiversification)
import Test.PortfolioFixtures (positionsOf, positionsOfWithSectorAndClass)

spec :: Spec
spec = describe "Risk.Concentration" $ do
  it "calculates Vec N3 as the result for a static Top 3 request" $ do
    portfolioValue <- concentrationPortfolio
    expectStaticTop3 (analyzeConcentration (StaticTop sN3) portfolioValue)

  it "calculates Vec N2 as the result for a static Top 2 request" $ do
    portfolioValue <- concentrationPortfolio
    expectStaticTop2 (analyzeConcentration (StaticTop sN2) portfolioValue)

  it "calculates SomeTopPositions as the result for a dynamic request" $ do
    portfolioValue <- largeConcentrationPortfolio
    expectDynamicTop (analyzeConcentration (DynamicTop 5) portfolioValue)

  it "keeps descending order through a static request" $ do
    portfolioValue <- concentrationPortfolio
    case analyzeConcentration (StaticTop sN3) portfolioValue of
      Right positions -> map tickerOf (vecToList positions) `shouldBe` ["CASH", "ITUB4", "PETR4"]
      Left err -> expectationFailure (show err)

  it "keeps descending order through a dynamic request" $ do
    portfolioValue <- concentrationPortfolio
    case analyzeConcentration (DynamicTop 2) portfolioValue of
      Right (SomeTopPositions _ positions) -> map tickerOf (vecToList positions) `shouldBe` ["CASH", "ITUB4"]
      Left err -> expectationFailure (show err)

  it "keeps StaticTop sN3 aligned with the Top 3 facade" $ do
    portfolioValue <- concentrationPortfolio
    case (analyzeConcentration (StaticTop sN3) portfolioValue, top3Positions portfolioValue) of
      (Right requestedTop3, Right facadeTop3) -> vecToList requestedTop3 `shouldBe` vecToList facadeTop3
      (Left err, _) -> expectationFailure (show err)
      (_, Left err) -> expectationFailure (show err)

  it "keeps DynamicTop 3 semantically aligned with StaticTop sN3" $ do
    portfolioValue <- concentrationPortfolio
    case (analyzeConcentration (DynamicTop 3) portfolioValue, analyzeConcentration (StaticTop sN3) portfolioValue) of
      (Right (SomeTopPositions _ dynamicTop3), Right staticTop3) -> vecToList dynamicTop3 `shouldBe` vecToList staticTop3
      (Left err, _) -> expectationFailure (show err)
      (_, Left err) -> expectationFailure (show err)

  it "reports static insufficient cardinality through the unified API" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.50), ("ITUB4", 0.50)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        analyzeConcentration (StaticTop sN3) portfolioValue
          `shouldBe` Left NotEnoughPositions {requiredPositions = 3, actualPositions = 2}
      Left err -> expectationFailure (show err)

  it "reports dynamic insufficient cardinality through the unified API" $ do
    portfolioValue <- concentrationPortfolio
    case analyzeConcentration (DynamicTop 5) portfolioValue of
      Left NotEnoughPositions {requiredPositions = required, actualPositions = actual} -> do
        required `shouldBe` 5
        actual `shouldBe` 4
      Left err -> expectationFailure (show err)
      Right _ -> expectationFailure "expected insufficient positions"

  it "preserves invalid dynamic requests through the unified API" $ do
    portfolioValue <- concentrationPortfolio
    expectInvalidCount 0 (analyzeConcentration (DynamicTop 0) portfolioValue)

  it "renders a static request with its requested cardinality" $ do
    portfolioValue <- concentrationPortfolio
    case analyzeConcentration (StaticTop sN3) portfolioValue of
      Right positions -> renderConcentration (StaticTop sN3) positions `shouldSatisfy` T.isPrefixOf "Top 3 Positions:"
      Left err -> expectationFailure (show err)

  it "renders a dynamic request with its runtime cardinality" $ do
    portfolioValue <- largeConcentrationPortfolio
    case analyzeConcentration (DynamicTop 5) portfolioValue of
      Right positions -> renderConcentration (DynamicTop 5) positions `shouldSatisfy` T.isPrefixOf "Top 5 Positions:"
      Left err -> expectationFailure (show err)

  it "preserves static cardinality for singleton requests" $
    property $
      forAll genPortfolioWithAtLeastThree $ \portfolioValue ->
        conjoin
          [ preservesStaticLength sN1 portfolioValue
          , preservesStaticLength sN2 portfolioValue
          , preservesStaticLength sN3 portfolioValue
          ]

  it "preserves dynamic cardinality through the type-directed API" $
    property $
      forAll genPortfolioWithAtLeastThree $ \portfolioValue ->
        forAll (choose (1, 3)) $ \requested ->
          case analyzeConcentration (DynamicTop requested) portfolioValue of
            Right (SomeTopPositions size positions) ->
              property (snatToInt size == requested && length (vecToList positions) == requested)
            Left err -> counterexample (show err) (property False)

  it "orders the Top 3 positions from largest to smallest" $ do
    portfolioValue <- portfolioFrom
      [ ("PETR4", 0.20, "Energy", "Equity")
      , ("BTC", 0.10, "Other", "Crypto")
      , ("ITUB4", 0.30, "Financial", "Equity")
      , ("CASH", 0.40, "Other", "Cash")
      ]
    case top3Positions portfolioValue of
      Right top3 -> do
        tickerOf (largestPosition top3) `shouldBe` "CASH"
        tickerOf (secondLargestPosition top3) `shouldBe` "ITUB4"
        tickerOf (thirdLargestPosition top3) `shouldBe` "PETR4"
      Left err -> expectationFailure (show err)

  it "builds Top 3 for a portfolio with exactly three positions" $ do
    portfolioValue <- portfolioFrom
      [ ("PETR4", 0.30, "Energy", "Equity")
      , ("ITUB4", 0.40, "Financial", "Equity")
      , ("VALE3", 0.30, "Other", "Equity")
      ]
    case top3Positions portfolioValue of
      Right top3 -> length (vecToList top3) `shouldBe` 3
      Left err -> expectationFailure (show err)

  it "selects only the three largest positions when more positions exist" $ do
    portfolioValue <- portfolioFrom
      [ ("A", 0.35, "Energy", "Equity")
      , ("B", 0.25, "Financial", "Equity")
      , ("C", 0.20, "Other", "Equity")
      , ("D", 0.12, "Technology", "Equity")
      , ("E", 0.08, "Consumer", "Equity")
      ]
    case top3Positions portfolioValue of
      Right top3 -> map tickerOf (vecToList top3) `shouldBe` ["A", "B", "C"]
      Left err -> expectationFailure (show err)

  it "returns NotEnoughPositions for a portfolio with two positions" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.50), ("ITUB4", 0.50)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        top3Positions portfolioValue
          `shouldBe` Left NotEnoughPositions {requiredPositions = 3, actualPositions = 2}
      Left err -> expectationFailure (show err)

  it "preserves portfolio order when positions have equal weights" $ do
    portfolioValue <- portfolioFrom
      [ ("PETR4", 0.30, "Energy", "Equity")
      , ("ITUB4", 0.30, "Financial", "Equity")
      , ("BTC", 0.20, "Other", "Crypto")
      , ("CASH", 0.20, "Other", "Cash")
      ]
    case top3Positions portfolioValue of
      Right top3 -> map tickerOf (vecToList top3) `shouldBe` ["PETR4", "ITUB4", "BTC"]
      Left err -> expectationFailure (show err)

  it "does not replace the independent minimum-diversification rule" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.20), ("PETR4", 0.20), ("ITUB4", 0.60)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue -> do
        top3Positions portfolioValue `shouldSatisfy` isRight
        validateMinimumDiversification portfolioValue `shouldSatisfy` (not . null)
      Left err -> expectationFailure (show err)

  it "builds Top 2 with the same generic algorithm" $ do
    portfolioValue <- portfolioFrom
      [ ("CASH", 0.40, "Other", "Cash")
      , ("ITUB4", 0.30, "Financial", "Equity")
      , ("PETR4", 0.20, "Energy", "Equity")
      , ("BTC", 0.10, "Other", "Crypto")
      ]
    case topNPositions sN2 portfolioValue of
      Right top2 -> map tickerOf (vecToList top2) `shouldBe` ["CASH", "ITUB4"]
      Left err -> expectationFailure (show err)

  it "uses sN3 as the required size reported by the generic error" $ do
    rawPortfolio <- positionsOf [("PETR4", 0.50), ("ITUB4", 0.50)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        topNPositions sN3 portfolioValue
          `shouldBe` Left NotEnoughPositions {requiredPositions = 3, actualPositions = 2}
      Left err -> expectationFailure (show err)

  it "derives a different error size automatically for sN2" $ do
    rawPortfolio <- positionsOf [("PETR4", 1.00)]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        topNPositions sN2 portfolioValue
          `shouldBe` Left NotEnoughPositions {requiredPositions = 2, actualPositions = 1}
      Left err -> expectationFailure (show err)

  it "keeps the existential singleton and vector at the same dynamic size" $ do
    portfolioValue <- concentrationPortfolio
    case topPositionsByCount 3 portfolioValue of
      Right (SomeTopPositions size positions) ->
        snatToInt size `shouldBe` length (vecToList positions)
      Left err -> expectationFailure (show err)

  it "builds a dynamically requested Top 2 in descending order" $ do
    portfolioValue <- concentrationPortfolio
    case topPositionsByCount 2 portfolioValue of
      Right (SomeTopPositions size positions) -> do
        snatToInt size `shouldBe` 2
        map tickerOf (vecToList positions) `shouldBe` ["CASH", "ITUB4"]
      Left err -> expectationFailure (show err)

  it "keeps dynamic Top 3 aligned with the static API" $ do
    portfolioValue <- concentrationPortfolio
    case (topPositionsByCount 3 portfolioValue, top3Positions portfolioValue) of
      (Right (SomeTopPositions _ dynamicTop3), Right staticTop3) ->
        vecToList dynamicTop3 `shouldBe` vecToList staticTop3
      (Left err, _) -> expectationFailure (show err)
      (_, Left err) -> expectationFailure (show err)

  it "builds a dynamic Top 5 without predefined singleton constants" $ do
    portfolioValue <- largeConcentrationPortfolio
    case topPositionsByCount 5 portfolioValue of
      Right (SomeTopPositions size positions) -> do
        snatToInt size `shouldBe` 5
        map tickerOf (vecToList positions) `shouldBe` ["A", "B", "C", "D", "E"]
      Left err -> expectationFailure (show err)

  it "reports insufficient positions using the dynamic requested count" $ do
    portfolioValue <- concentrationPortfolio
    case topPositionsByCount 5 portfolioValue of
      Left NotEnoughPositions {requiredPositions = required, actualPositions = actual} -> do
        required `shouldBe` 5
        actual `shouldBe` 4
      Left err -> expectationFailure (show err)
      Right _ -> expectationFailure "expected insufficient positions"

  it "rejects Top 0 as an invalid financial request" $ do
    portfolioValue <- concentrationPortfolio
    expectInvalidCount 0 (topPositionsByCount 0 portfolioValue)

  it "rejects negative Top N as an invalid financial request" $ do
    portfolioValue <- concentrationPortfolio
    expectInvalidCount (-2) (topPositionsByCount (-2) portfolioValue)

  it "preserves runtime cardinality for dynamic requests" $
    property $
      forAll genPortfolioWithAtLeastThree $ \portfolioValue ->
        forAll (choose (1, 3)) $ \requested ->
          case topPositionsByCount requested portfolioValue of
            Right (SomeTopPositions size positions) ->
              property
                ( snatToInt size == requested
                    && length (vecToList positions) == requested
                )
            Left err -> counterexample (show err) (property False)

  it "always returns positions sorted in descending weight order" $
    property $
      forAll genPortfolioWithAtLeastThree $ \portfolioValue ->
        case top3Positions portfolioValue of
          Right top3 ->
            let weights = map (unPercentage . weight) (vecToList top3)
             in property (weights == sortBy (flip compare) weights)
          Left err -> counterexample (show err) (property False)

  it "returns only positions that existed in the original portfolio" $
    property $
      forAll genPortfolioWithAtLeastThree $ \portfolioValue ->
        case top3Positions portfolioValue of
          Right top3 ->
            property (all (`elem` portfolioPositions portfolioValue) (vecToList top3))
          Left err -> counterexample (show err) (property False)

  it "returns exactly three positions whenever construction succeeds" $
    property $
      forAll genPortfolioWithAtLeastThree $ \portfolioValue ->
        case top3Positions portfolioValue of
          Right top3 -> property (length (vecToList top3) == 3)
          Left err -> counterexample (show err) (property False)

portfolioFrom :: [(String, Double, String, String)] -> IO Portfolio
portfolioFrom rows = do
  rawPortfolio <- positionsOfWithSectorAndClass rows
  case mkPortfolio rawPortfolio of
    Right portfolioValue -> pure portfolioValue
    Left err -> expectationFailure (show err) >> fail "invalid test portfolio"

concentrationPortfolio :: IO Portfolio
concentrationPortfolio =
  portfolioFrom
    [ ("CASH", 0.40, "Other", "Cash")
    , ("ITUB4", 0.30, "Financial", "Equity")
    , ("PETR4", 0.20, "Energy", "Equity")
    , ("BTC", 0.10, "Other", "Crypto")
    ]

largeConcentrationPortfolio :: IO Portfolio
largeConcentrationPortfolio =
  portfolioFrom
    [ ("A", 0.30, "Energy", "Equity")
    , ("B", 0.25, "Financial", "Equity")
    , ("C", 0.20, "Other", "Equity")
    , ("D", 0.15, "Technology", "Equity")
    , ("E", 0.10, "Consumer", "Equity")
    ]

genPortfolioWithAtLeastThree :: Gen Portfolio
genPortfolioWithAtLeastThree = do
  positionCount <- choose (3, 10)
  rawPortfolio <- genValidPortfolioRaw positionCount
  case mkPortfolio rawPortfolio of
    Right portfolioValue -> pure portfolioValue
    Left _ -> genPortfolioWithAtLeastThree

tickerOf :: Position -> String
tickerOf = T.unpack . unTicker . ticker . asset

isRight :: Either error value -> Bool
isRight (Right _) = True
isRight (Left _) = False

expectInvalidCount :: Int -> Either ConcentrationError SomeTopPositions -> Expectation
expectInvalidCount expected result =
  case result of
    Left (InvalidRequestedCount found) -> found `shouldBe` expected
    Left err -> expectationFailure (show err)
    Right _ -> expectationFailure "expected an invalid requested count"

expectStaticTop3 :: Either ConcentrationError (Vec N3 Position) -> Expectation
expectStaticTop3 result =
  case result of
    Right positions -> length (vecToList positions) `shouldBe` 3
    Left err -> expectationFailure (show err)

expectStaticTop2 :: Either ConcentrationError (Vec N2 Position) -> Expectation
expectStaticTop2 result =
  case result of
    Right positions -> length (vecToList positions) `shouldBe` 2
    Left err -> expectationFailure (show err)

expectDynamicTop :: Either ConcentrationError SomeTopPositions -> Expectation
expectDynamicTop result =
  case result of
    Right (SomeTopPositions _ _) -> pure ()
    Left err -> expectationFailure (show err)

preservesStaticLength :: SNat n -> Portfolio -> Property
preservesStaticLength size portfolioValue =
  case analyzeConcentration (StaticTop size) portfolioValue of
    Right positions -> property (length (vecToList positions) == snatToInt size)
    Left err -> counterexample (show err) (property False)