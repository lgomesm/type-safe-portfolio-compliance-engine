{-# LANGUAGE OverloadedStrings #-}

module Cli.RenderSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Cli.Render
  ( renderEvaluationBundleAnsi
  , renderEvaluationBundleText
  , renderErrorText
  , renderUsageText
  )
import Cli.Theme
  ( RenderCapabilities (..)
  , asciiSymbols
  , defaultTheme
  , symbolsForCapabilities
  , unicodeSymbols
  )
import Domain.Portfolio (Portfolio, mkPortfolio)
import Domain.SafeVector (sN3)
import Examples.Customers (conservativeHighScore)
import Risk.Concentration (ConcentrationRequest (DynamicTop, StaticTop))
import Risk.EvaluationBundle (buildEvaluationBundle)
import Risk.Policy (Locality (Chile), defaultPolicyConfig)
import Test.PortfolioFixtures (positionsOfWithSectorAndClass)

spec :: Spec
spec = describe "Cli.Render" $ do
  it "renders an approved bundle with semantic success markers" $ do
    portfolioValue <- approvedPortfolio
    let bundle = buildEvaluationBundle (StaticTop sN3) defaultPolicyConfig conservativeHighScore portfolioValue
        rendered = renderEvaluationBundleText unicodeSymbols defaultTheme (Just Chile) bundle
    rendered `shouldContainText` "Portfolio Compliance"
    rendered `shouldContainText` "Locality"
    rendered `shouldContainText` "Chile"
    rendered `shouldContainText` "\x2713 APPROVED"
    rendered `shouldContainText` "\x2713 LOW"
    rendered `shouldContainText` "\x2713 AUTO APPROVED"
    rendered `shouldContainText` "No violations found."
    rendered `shouldContainText` "Top 3 Positions"

  it "renders a rejected bundle as manual review without changing the decision" $ do
    portfolioValue <- concentratedPortfolio
    let bundle = buildEvaluationBundle (StaticTop sN3) defaultPolicyConfig conservativeHighScore portfolioValue
        rendered = renderEvaluationBundleText unicodeSymbols defaultTheme Nothing bundle
    rendered `shouldContainText` "\x2717 REJECTED"
    rendered `shouldContainText` "\x21 MEDIUM"
    rendered `shouldContainText` "\x21 MANUAL REVIEW"
    rendered `shouldContainText` "Analyst"
    rendered `shouldContainText` "Violations"
    rendered `shouldContainText` "PETR4 allocation is 40%, above the 30% limit."

  it "renders the requested dynamic Top N instead of recalculating concentration" $ do
    portfolioValue <- approvedPortfolio
    let bundle = buildEvaluationBundle (DynamicTop 2) defaultPolicyConfig conservativeHighScore portfolioValue
        rendered = renderEvaluationBundleText asciiSymbols defaultTheme Nothing bundle
    rendered `shouldContainText` "Top 2 Positions"
    rendered `shouldContainText` "  1  PETR4"
    rendered `shouldContainText` "  2  ITUB4"
    rendered `shouldNotContainText` "  3  "

  it "uses ASCII symbols without emitting ANSI in the plain projection" $ do
    portfolioValue <- approvedPortfolio
    let symbols = symbolsForCapabilities (RenderCapabilities False False)
        bundle = buildEvaluationBundle (StaticTop sN3) defaultPolicyConfig conservativeHighScore portfolioValue
        rendered = renderEvaluationBundleText symbols defaultTheme Nothing bundle
    rendered `shouldContainText` "[OK] APPROVED"
    rendered `shouldNotContainText` "\x2713"
    rendered `shouldNotContainText` "\ESC["

  it "keeps semantic styles available for the ANSI projection" $ do
    portfolioValue <- approvedPortfolio
    let bundle = buildEvaluationBundle (StaticTop sN3) defaultPolicyConfig conservativeHighScore portfolioValue
        rendered = renderEvaluationBundleAnsi unicodeSymbols defaultTheme Nothing bundle
    rendered `shouldContainText` "\ESC["
    rendered `shouldContainText` "APPROVED"

  it "keeps error and usage messages while adding visual hierarchy" $ do
    let errorText = renderErrorText asciiSymbols defaultTheme "Unknown locality: argentina"
        usageText = renderUsageText asciiSymbols defaultTheme "portfolio-compliance-engine" "cenario desconhecido" ["approved"]
    errorText `shouldContainText` "[X] Input Error"
    errorText `shouldContainText` "Unknown locality: argentina"
    usageText `shouldContainText` "[X] Usage Error"
    usageText `shouldContainText` "Erro: cenario desconhecido"
    usageText `shouldContainText` "  - approved"
    usageText `shouldNotContainText` "\ESC["

approvedPortfolio :: IO Portfolio
approvedPortfolio = portfolioFrom
  [ ("PETR4", 0.25, "Energy", "Equity")
  , ("ITUB4", 0.25, "Financial", "Equity")
  , ("VALE3", 0.25, "Other", "Equity")
  , ("WEGE3", 0.25, "Technology", "Equity")
  ]

concentratedPortfolio :: IO Portfolio
concentratedPortfolio = portfolioFrom
  [ ("PETR4", 0.40, "Energy", "Equity")
  , ("ITUB4", 0.25, "Financial", "Equity")
  , ("VALE3", 0.20, "Other", "Equity")
  , ("WEGE3", 0.15, "Technology", "Equity")
  ]

portfolioFrom :: [(String, Double, String, String)] -> IO Portfolio
portfolioFrom rows = do
  rawPortfolio <- positionsOfWithSectorAndClass rows
  case mkPortfolio rawPortfolio of
    Right portfolioValue -> pure portfolioValue
    Left err -> expectationFailure (show err) >> fail "invalid test portfolio"

shouldContainText :: T.Text -> T.Text -> Expectation
shouldContainText actual expected =
  actual `shouldSatisfy` T.isInfixOf expected

shouldNotContainText :: T.Text -> T.Text -> Expectation
shouldNotContainText actual unexpected =
  actual `shouldSatisfy` (not . T.isInfixOf unexpected)
