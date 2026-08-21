module Domain.PortfolioSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Domain.Asset (Asset (..))
import Domain.AssetClassification (AssetClass (..), Sector (..))
import Domain.Error (DomainError (..))
import Domain.Percentage (mkPercentage)
import Domain.Portfolio
  ( mkPortfolio
  , portfolioPositions
  , weightSumTolerance
  )
import Domain.Position (Position (..))
import Domain.RawPortfolio (RawPortfolio (..))
import Domain.Ticker (mkTicker)

spec :: Spec
spec = describe "Domain.Portfolio" $ do
  describe "mkPortfolio" $ do
    it "accepts a portfolio whose weights sum exactly to 1.0" $ do
      rawPortfolio <- rawPortfolioWith [("PETR4", 0.40), ("ITUB4", 0.35), ("VALE3", 0.25)]
      case mkPortfolio rawPortfolio of
        Right portfolioValue ->
          length (portfolioPositions portfolioValue) `shouldBe` 3
        Left err ->
          expectationFailure ("expected success, got " <> show err)

    it "rejects an empty portfolio" $ do
      let rawPortfolio = RawPortfolio []
      mkPortfolio rawPortfolio `shouldBe` Left EmptyPortfolio

    it "rejects a portfolio whose weights sum to 0.9" $ do
      rawPortfolio <- rawPortfolioWith [("PETR4", 0.60), ("ITUB4", 0.30)]
      case mkPortfolio rawPortfolio of
        Left (PortfolioWeightsDoNotSumToOne _) -> pure ()
        other -> expectationFailure ("expected a weight-sum error, got " <> show other)

    it "rejects a portfolio whose weights sum to 1.2" $ do
      rawPortfolio <- rawPortfolioWith [("PETR4", 0.70), ("ITUB4", 0.50)]
      mkPortfolio rawPortfolio `shouldBe` Left (PortfolioWeightsDoNotSumToOne 1.2)

    it "accepts a total that differs from 1.0 only by Double rounding noise" $ do
      rawPortfolio <-
        rawPortfolioWith
          [ ("A1", 0.1), ("A2", 0.1), ("A3", 0.1), ("A4", 0.1), ("A5", 0.1)
          , ("A6", 0.1), ("A7", 0.1), ("A8", 0.1), ("A9", 0.1), ("A10", 0.1)
          ]
      case mkPortfolio rawPortfolio of
        Right _ ->
          pure ()
        Left err ->
          expectationFailure ("expected success within tolerance, got " <> show err)

    it "allows duplicate tickers at the structural validation layer" $ do
      rawPortfolio <- rawPortfolioWith [("PETR4", 0.50), ("PETR4", 0.50)]
      case mkPortfolio rawPortfolio of
        Right portfolioValue ->
          length (portfolioPositions portfolioValue) `shouldBe` 2
        Left err ->
          expectationFailure ("expected success, got " <> show err)

    it "documents the tolerance value used by the module" $
      weightSumTolerance `shouldBe` 1e-6

rawPortfolioWith :: [(String, Double)] -> IO RawPortfolio
rawPortfolioWith tickerWeights = do
  positions <- mapM toPosition tickerWeights
  pure (RawPortfolio positions)
  where
    toPosition :: (String, Double) -> IO Position
    toPosition (tickerText, rawWeight) =
      case (mkTicker (T.pack tickerText), mkPercentage rawWeight) of
        (Right validatedTicker, Right validatedPercentage) ->
          pure
            Position
              { asset =
                  Asset
                    { ticker = validatedTicker
                    , assetClass = Equity
                    , sector = Other
                    }
              , weight = validatedPercentage
              }
        _ ->
          fail ("invalid ticker or percentage in test input: " <> tickerText)