{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- Relatorio de compliance de uma carteira
module Risk.Report (PortfolioStatus (..), PortfolioReport (..), buildReport, buildReportWithPolicy, renderReport, 
  renderReportWithTopPositions, renderReportWithConcentration, renderViolation) where

import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

import Domain.AssetClassification (AssetClass (..), Sector (..))
import Domain.CreditScore (CreditScore (..))
import Domain.Customer (Customer (customerProfile))
import Domain.CustomerProfile (CustomerProfile (..))
import Domain.Percentage (Percentage, unPercentage)
import Domain.Portfolio (Portfolio)
import Domain.SafeVector (sN3)
import Domain.Ticker (unTicker)
import Risk.Concentration (ConcentrationError, ConcentrationRequest (StaticTop), ConcentrationResult, SomeTopPositions, 
  Top3Positions, analyzeConcentration, renderConcentration, renderSomeTopPositions, renderTop3)
import Risk.Engine (validateComplianceWithPolicy)
import Risk.Policy (PolicyConfig, policyConfigWithSuitability)
import Risk.RiskLevel (RiskLevel (..), riskLevelFor)
import Risk.SuitabilityPolicy (SuitabilityPolicy)
import Risk.Violation (Violation (..))

data PortfolioStatus
  = Approved
  | Rejected
  deriving (Eq, Show)

data PortfolioReport = PortfolioReport
  { customer :: Customer
  , status :: PortfolioStatus
  , riskLevel :: RiskLevel
  , violations :: [Violation]
  , topPositions :: Either ConcentrationError Top3Positions
  }
  deriving (Eq, Show)

-- Calculo status e nivel de risco separadamente pq representam aspectos diferentes do resultado, mesmo quando os dois 
-- partem das mesmas violacoes
buildReport :: SuitabilityPolicy -> Customer -> Portfolio -> PortfolioReport
buildReport policy =
  buildReportWithPolicy (policyConfigWithSuitability policy)

buildReportWithPolicy :: PolicyConfig -> Customer -> Portfolio -> PortfolioReport
buildReportWithPolicy policyConfig customerValue portfolioValue =
  PortfolioReport
    { customer = customerValue
    , status = if null violationsFound then Approved else Rejected
    , riskLevel = riskLevelFor violationsFound
    , violations = violationsFound
    , topPositions = analyzeConcentration (StaticTop sN3) portfolioValue
    }
  where
    violationsFound :: [Violation]
    violationsFound = validateComplianceWithPolicy policyConfig customerValue portfolioValue

renderReport :: PortfolioReport -> Text
renderReport report =
  renderReportWithTopLines report (renderTopPositions (topPositions report))

-- A concentracao pode ser fornecida externamente pra permitir outras cardinalidades sem alterar o restante da 
-- formatacao do relatorio tb
renderReportWithTopPositions :: PortfolioReport -> SomeTopPositions -> Text
renderReportWithTopPositions report positions =
  renderReportWithTopLines report (T.lines (renderSomeTopPositions positions))

renderReportWithConcentration :: PortfolioReport -> ConcentrationRequest mode -> ConcentrationResult mode -> Text
renderReportWithConcentration report request positions =
  renderReportWithTopLines report (T.lines (renderConcentration request positions))

renderReportWithTopLines :: PortfolioReport -> [Text] -> Text
renderReportWithTopLines report concentrationLines =
  T.unlines $
    [ "Portfolio Compliance Report"
    , ""
    , "Customer Profile: " <> renderCustomerProfile (customerProfile (customer report))
    , "Status: " <> renderStatus (status report)
    , "Risk Level: " <> renderRiskLevel (riskLevel report)
    , ""
    ]
      ++ renderViolations (violations report)
      ++ [""]
      ++ concentrationLines

renderTopPositions :: Either ConcentrationError Top3Positions -> [Text]
renderTopPositions (Right top3) = T.lines (renderTop3 top3)
renderTopPositions (Left _) =
  [ "Top 3 Positions:"
  , "- unavailable: portfolio contains fewer than 3 positions"
  ]

renderViolations :: [Violation] -> [Text]
renderViolations [] = ["No violations found."]
renderViolations foundViolations =
  "Violations:" : map (("- " <>) . renderViolation) foundViolations

renderStatus :: PortfolioStatus -> Text
renderStatus Approved = "Approved"
renderStatus Rejected = "Rejected"

renderRiskLevel :: RiskLevel -> Text
renderRiskLevel Low = "Low"
renderRiskLevel Medium = "Medium"
renderRiskLevel High = "High"

renderViolation :: Violation -> Text
renderViolation (AssetAllocationExceeded tickerValue limit found) =
  unTicker tickerValue
    <> " allocation is "
    <> formatPercentage found
    <> ", above the "
    <> formatPercentage limit
    <> " limit."
renderViolation (SectorExposureExceeded sectorValue limit found) =
  renderSector sectorValue
    <> " sector exposure is "
    <> formatPercentage found
    <> ", above the "
    <> formatPercentage limit
    <> " limit."
renderViolation (AssetClassExposureExceeded assetClassValue limit found) =
  renderAssetClass assetClassValue
    <> " exposure is "
    <> formatPercentage found
    <> ", above the "
    <> formatPercentage limit
    <> " limit."
renderViolation (MinimumDiversificationNotMet minimumRequired foundCount) =
  "Portfolio has only "
    <> T.pack (show foundCount)
    <> " distinct asset(s), below the minimum of "
    <> T.pack (show minimumRequired)
    <> "."
renderViolation (CustomerProfileCryptoExceeded profileValue limit found) =
  renderCustomerProfile profileValue
    <> " profile allows at most "
    <> formatPercentage limit
    <> " in Crypto, but portfolio has "
    <> formatPercentage found
    <> "."
renderViolation (CustomerCreditRiskExposureExceeded scoreValue limit found) =
  renderCreditScore scoreValue
    <> " credit score allows at most "
    <> formatPercentage limit
    <> " in Equity/Crypto exposure, but portfolio has "
    <> formatPercentage found
    <> "."
renderViolation (AssetClassExposureExceededForProfile assetClassValue profileValue limit found) =
  renderAssetClass assetClassValue
    <> " exposure is "
    <> formatPercentage found
    <> ", above the "
    <> formatPercentage limit
    <> " limit for "
    <> renderCustomerProfile profileValue
    <> " profile."

-- Removendo só o decimal .0 pq ele nao acrescenta informacao, enquanto valores fracionarios precisam continuar visiveis
formatPercentage :: Percentage -> Text
formatPercentage percentageValue = T.pack (stripTrailingZero raw) <> "%"
  where
    raw :: String
    raw = printf "%.1f" (unPercentage percentageValue * 100)

    stripTrailingZero :: String -> String
    stripTrailingZero formatted
      | ".0" `isSuffixOf` formatted = take (length formatted - 2) formatted
      | otherwise = formatted

renderSector :: Sector -> Text
renderSector Financial = "Financial"
renderSector Energy = "Energy"
renderSector Utilities = "Utilities"
renderSector Technology = "Technology"
renderSector Consumer = "Consumer"
renderSector Other = "Other"

renderAssetClass :: AssetClass -> Text
renderAssetClass Equity = "Equity"
renderAssetClass FixedIncome = "Fixed Income"
renderAssetClass Crypto = "Crypto"
renderAssetClass PrivateCredit = "Private Credit"
renderAssetClass Fund = "Fund"
renderAssetClass Cash = "Cash"

renderCustomerProfile :: CustomerProfile -> Text
renderCustomerProfile Conservative = "Conservative"
renderCustomerProfile Moderate = "Moderate"
renderCustomerProfile Aggressive = "Aggressive"

renderCreditScore :: CreditScore -> Text
renderCreditScore LowScore = "Low Score"
renderCreditScore MediumScore = "Medium Score"
renderCreditScore HighScore = "High Score"
