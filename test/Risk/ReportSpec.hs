{-# LANGUAGE OverloadedStrings #-}

module Risk.ReportSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Domain.Portfolio (mkPortfolio)
import Examples.Customers (aggressiveHighScore, conservativeHighScore)
import Examples.Portfolios (privateCreditProfile)
import Risk.Report
  ( PortfolioReport (..)
  , PortfolioStatus (..)
  , buildReport
  , renderReport
  )
import Risk.RiskLevel (RiskLevel (High, Low))
import Risk.SuitabilityPolicy (defaultSuitabilityPolicy)
import Test.PortfolioFixtures
  ( positionsOfWithSectorAndClass
  )

spec :: Spec
spec = describe "Risk.Report" $ do
  describe "buildReport" $ do
    it "returns Approved and Low for a portfolio without violations" $ do
      rawPortfolio <-
        positionsOfWithSectorAndClass
          [ ("PETR4", 0.25, "Energy", "Equity")
          , ("ITUB4", 0.25, "Financial", "Equity")
          , ("VALE3", 0.25, "Other", "Equity")
          , ("WEGE3", 0.25, "Technology", "Equity")
          ]
      case mkPortfolio rawPortfolio of
        Right portfolioValue -> do
          let report = buildReport defaultSuitabilityPolicy conservativeHighScore portfolioValue
          status report `shouldBe` Approved
          riskLevel report `shouldBe` Low
          violations report `shouldBe` []
        Left err ->
          expectationFailure (show err)

    it "returns Rejected and the corresponding risk level for a violating portfolio" $ do
      rawPortfolio <-
        positionsOfWithSectorAndClass
          [ ("PETR4", 0.60, "Energy", "Equity")
          , ("BTC", 0.40, "Other", "Crypto")
          ]
      case mkPortfolio rawPortfolio of
        Right portfolioValue -> do
          let report = buildReport defaultSuitabilityPolicy aggressiveHighScore portfolioValue
          status report `shouldBe` Rejected
          riskLevel report `shouldBe` High
          length (violations report) `shouldBe` 6
        Left err ->
          expectationFailure (show err)

  describe "renderReport" $ do
    it "renders the header, status and risk level" $ do
      rawPortfolio <-
        positionsOfWithSectorAndClass
          [ ("PETR4", 0.25, "Energy", "Equity")
          , ("ITUB4", 0.25, "Financial", "Equity")
          , ("VALE3", 0.25, "Other", "Equity")
          , ("WEGE3", 0.25, "Technology", "Equity")
          ]
      case mkPortfolio rawPortfolio of
        Right portfolioValue -> do
          let rendered = renderReport (buildReport defaultSuitabilityPolicy conservativeHighScore portfolioValue)
          rendered `shouldSatisfy` T.isInfixOf "Portfolio Compliance Report"
          rendered `shouldSatisfy` T.isInfixOf "Customer Profile: Conservative"
          rendered `shouldSatisfy` T.isInfixOf "Status: Approved"
          rendered `shouldSatisfy` T.isInfixOf "Risk Level: Low"
          rendered `shouldSatisfy` T.isInfixOf "No violations found."
          rendered `shouldSatisfy` T.isInfixOf "Top 3 Positions:"
          rendered `shouldSatisfy` T.isInfixOf "1. PETR4 - 25%"
        Left err ->
          expectationFailure (show err)

    it "renders each violation as a formatted line without unnecessary decimal places" $ do
      rawPortfolio <-
        positionsOfWithSectorAndClass
          [ ("PETR4", 0.60, "Energy", "Equity")
          , ("VALE3", 0.40, "Other", "Equity")
          ]
      case mkPortfolio rawPortfolio of
        Right portfolioValue -> do
          let rendered = renderReport (buildReport defaultSuitabilityPolicy conservativeHighScore portfolioValue)
          rendered `shouldSatisfy` T.isInfixOf "PETR4 allocation is 60%, above the 30% limit."
        Left err ->
          expectationFailure (show err)

    it "preserves real decimal places when formatting percentages" $ do
      rawPortfolio <-
        positionsOfWithSectorAndClass
          [ ("PETR4", 0.325, "Energy", "Equity")
          , ("VALE3", 0.675, "Other", "Equity")
          ]
      case mkPortfolio rawPortfolio of
        Right portfolioValue -> do
          let rendered = renderReport (buildReport defaultSuitabilityPolicy conservativeHighScore portfolioValue)
          rendered `shouldSatisfy` T.isInfixOf "32.5%"
        Left err ->
          expectationFailure (show err)

    it "renders deterministically across repeated calls" $ do
      rawPortfolio <-
        positionsOfWithSectorAndClass
          [ ("PETR4", 0.60, "Energy", "Equity")
          , ("BTC", 0.40, "Other", "Crypto")
          ]
      case mkPortfolio rawPortfolio of
        Right portfolioValue ->
          renderReport (buildReport defaultSuitabilityPolicy aggressiveHighScore portfolioValue)
            `shouldBe` renderReport (buildReport defaultSuitabilityPolicy aggressiveHighScore portfolioValue)
        Left err ->
          expectationFailure (show err)

    it "renders the private credit suitability violation with profile context" $
      case mkPortfolio privateCreditProfile of
        Right portfolioValue -> do
          let rendered = renderReport (buildReport defaultSuitabilityPolicy conservativeHighScore portfolioValue)
          rendered `shouldSatisfy` T.isInfixOf "Private Credit exposure is 15%, above the 5% limit for Conservative profile."
        Left err ->
          expectationFailure (show err)

    it "reports when the Top 3 view is unavailable without creating a violation" $ do
      rawPortfolio <- positionsOfWithSectorAndClass
        [ ("PETR4", 0.50, "Energy", "Equity")
        , ("ITUB4", 0.50, "Financial", "Equity")
        ]
      case mkPortfolio rawPortfolio of
        Right portfolioValue -> do
          let rendered = renderReport (buildReport defaultSuitabilityPolicy conservativeHighScore portfolioValue)
          rendered `shouldSatisfy` T.isInfixOf "Top 3 Positions:"
          rendered `shouldSatisfy` T.isInfixOf "unavailable: portfolio contains fewer than 3 positions"
        Left err -> expectationFailure (show err)