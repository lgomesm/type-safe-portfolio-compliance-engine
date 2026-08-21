{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Este modulo projeta valores ja avaliados para uma saida adequada ao
-- terminal. Ele nao reimplementa nenhuma regra nem substitui os renderers de
-- Risk, que continuam sendo a representacao textual pura da biblioteca.
module Cli.Render
  ( renderEvaluationBundleDoc
  , renderEvaluationBundleText
  , renderEvaluationBundleAnsi
  , renderBundleToHandle
  , renderErrorText
  , renderErrorToHandle
  , renderUsageText
  , renderUsageToHandle
  ) where

import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (Handle, hFlush)
import Text.Printf (printf)

import Prettyprinter
  ( Doc
  , annotate
  , defaultLayoutOptions
  , hardline
  , layoutPretty
  , pretty
  , unAnnotate
  , vsep
  , (<+>)
  )
import qualified Prettyprinter.Render.Terminal as RenderTerminal
import qualified Prettyprinter.Render.Text as RenderText
import Prettyprinter.Render.Terminal (AnsiStyle)

import Cli.Theme
  ( CliTheme (..)
  , RenderCapabilities (..)
  , Symbols (..)
  , capabilitiesForHandle
  , defaultTheme
  , symbolsForCapabilities
  )
import Domain.Asset (Asset (ticker))
import Domain.Customer (Customer (customerProfile))
import Domain.Percentage (Percentage, unPercentage)
import Domain.Position (Position (..))
import Domain.SafeVector (snatToInt, vecToList)
import Domain.Ticker (unTicker)
import Risk.Approval
  ( ApprovalDecision (..)
  , ApprovalLevel (..)
  , ApprovalReport (..)
  )
import Risk.Concentration
  ( ConcentrationError
  , ConcentrationRequest (..)
  , ConcentrationResult
  , SomeTopPositions (..)
  )
import Risk.EvaluationBundle
  ( ConcentrationEvidence (..)
  , EvaluationBundle
  )
import Risk.Policy (Locality (..))
import Risk.Report
  ( PortfolioReport (..)
  , PortfolioStatus (..)
  , renderViolation
  )
import Risk.RiskLevel (RiskLevel (..))
import Risk.Violation (Violation)
import Domain.HList (HList (HNil, (:&)))

renderEvaluationBundleDoc
  :: Symbols
  -> CliTheme
  -> Maybe Locality
  -> EvaluationBundle mode
  -> Doc AnsiStyle
renderEvaluationBundleDoc symbols theme locality (report :& concentration :& approval :& HNil) =
  vsep
    [ styled (titleStyle theme) (pretty ("Portfolio Compliance" :: Text))
    , styled (mutedStyle theme) (pretty (ruleLine symbols 76))
    , renderSummary symbols theme locality report approval
    , hardline
    , renderViolations symbols theme (violations report)
    , hardline
    , renderConcentration symbols theme concentration
    ]
    <> hardline

renderEvaluationBundleText
  :: Symbols
  -> CliTheme
  -> Maybe Locality
  -> EvaluationBundle mode
  -> Text
renderEvaluationBundleText symbols theme locality =
  renderPlain . renderEvaluationBundleDoc symbols theme locality

renderEvaluationBundleAnsi
  :: Symbols
  -> CliTheme
  -> Maybe Locality
  -> EvaluationBundle mode
  -> Text
renderEvaluationBundleAnsi symbols theme locality =
  renderStyled . renderEvaluationBundleDoc symbols theme locality

renderBundleToHandle :: Handle -> Maybe Locality -> EvaluationBundle mode -> IO ()
renderBundleToHandle handle locality bundle = do
  capabilities <- capabilitiesForHandle handle
  let symbols = symbolsForCapabilities capabilities
      document = renderEvaluationBundleDoc symbols defaultTheme locality bundle
  renderDocToHandle handle capabilities document

renderErrorText :: Symbols -> CliTheme -> Text -> Text
renderErrorText symbols theme message =
  renderPlain (renderErrorDoc symbols theme message)

renderErrorToHandle :: Handle -> Text -> IO ()
renderErrorToHandle handle message = do
  capabilities <- capabilitiesForHandle handle
  let symbols = symbolsForCapabilities capabilities
      document = renderErrorDoc symbols defaultTheme message
  renderDocToHandle handle capabilities document

renderUsageText :: Symbols -> CliTheme -> Text -> Text -> [Text] -> Text
renderUsageText symbols theme programName message names =
  renderPlain (renderUsageDoc symbols theme programName message names)

renderUsageToHandle :: Handle -> Text -> Text -> [Text] -> IO ()
renderUsageToHandle handle programName message names = do
  capabilities <- capabilitiesForHandle handle
  let symbols = symbolsForCapabilities capabilities
      document = renderUsageDoc symbols defaultTheme programName message names
  renderDocToHandle handle capabilities document

renderSummary
  :: Symbols
  -> CliTheme
  -> Maybe Locality
  -> PortfolioReport
  -> ApprovalReport
  -> Doc AnsiStyle
renderSummary symbols theme locality report approval =
  vsep (localityRow ++ [profileRow, statusRow, riskRow, approvalRow, requiredLevelRow])
  where
    localityRow :: [Doc AnsiStyle]
    localityRow =
      case locality of
        Nothing -> []
        Just value -> [summaryRow theme "Locality" (T.pack (show value)) (mutedStyle theme)]

    profileRow :: Doc AnsiStyle
    profileRow =
      summaryRow
        theme
        "Customer Profile"
        (T.pack (show (customerProfile (customer report))))
        (mutedStyle theme)

    statusRow :: Doc AnsiStyle
    statusRow =
      summaryRow
        theme
        "Status"
        (statusValue symbols (status report))
        (statusStyle theme (status report))

    riskRow :: Doc AnsiStyle
    riskRow =
      summaryRow
        theme
        "Risk Level"
        (riskValue symbols (riskLevel report))
        (riskStyle theme (riskLevel report))

    approvalRow :: Doc AnsiStyle
    approvalRow =
      summaryRow
        theme
        "Approval Decision"
        (approvalValue symbols (approvalDecision approval))
        (approvalStyle theme (approvalDecision approval))

    requiredLevelRow :: Doc AnsiStyle
    requiredLevelRow =
      summaryRow
        theme
        "Required Level"
        (renderApprovalLevel (requiredLevel approval))
        (mutedStyle theme)

summaryRow :: CliTheme -> Text -> Text -> AnsiStyle -> Doc AnsiStyle
summaryRow theme label value valueStyle =
  styled (labelStyle theme) (pretty (padRight 20 label))
    <+> styled valueStyle (pretty value)

statusValue :: Symbols -> PortfolioStatus -> Text
statusValue symbols Approved = successSymbol symbols <> " APPROVED"
statusValue symbols Rejected = failureSymbol symbols <> " REJECTED"

statusStyle :: CliTheme -> PortfolioStatus -> AnsiStyle
statusStyle theme Approved = successStyle theme
statusStyle theme Rejected = failureStyle theme

riskValue :: Symbols -> RiskLevel -> Text
riskValue symbols Low = successSymbol symbols <> " LOW"
riskValue symbols Medium = warningSymbol symbols <> " MEDIUM"
riskValue symbols High = failureSymbol symbols <> " HIGH"

riskStyle :: CliTheme -> RiskLevel -> AnsiStyle
riskStyle theme Low = successStyle theme
riskStyle theme Medium = warningStyle theme
riskStyle theme High = failureStyle theme

approvalValue :: Symbols -> ApprovalDecision -> Text
approvalValue symbols AutoApproved = successSymbol symbols <> " AUTO APPROVED"
approvalValue symbols (RequiresManualReview _) = warningSymbol symbols <> " MANUAL REVIEW"
approvalValue symbols RejectedByPolicy = failureSymbol symbols <> " REJECTED BY POLICY"

approvalStyle :: CliTheme -> ApprovalDecision -> AnsiStyle
approvalStyle theme AutoApproved = successStyle theme
approvalStyle theme (RequiresManualReview _) = warningStyle theme
approvalStyle theme RejectedByPolicy = failureStyle theme

renderApprovalLevel :: ApprovalLevel -> Text
renderApprovalLevel Automatic = "Automatic"
renderApprovalLevel Analyst = "Analyst"
renderApprovalLevel Manager = "Manager"
renderApprovalLevel CreditCommittee = "Credit Committee"

renderViolations :: Symbols -> CliTheme -> [Violation] -> Doc AnsiStyle
renderViolations symbols theme foundViolations =
  vsep
    [ styled (accentStyle theme) (pretty ("Violations" :: Text))
    , styled (mutedStyle theme) (pretty (ruleLine symbols 76))
    , if null foundViolations
        then styled (successStyle theme) (pretty (successSymbol symbols <> " No violations found."))
        else vsep (map renderOne foundViolations)
    ]
  where
    renderOne violation =
      styled (warningStyle theme)
        (pretty (warningSymbol symbols <> " " <> renderViolation violation))

renderConcentration :: Symbols -> CliTheme -> ConcentrationEvidence mode -> Doc AnsiStyle
renderConcentration symbols theme (ConcentrationEvidence request result) =
  case result of
    Left concentrationError ->
      vsep
        [ styled (accentStyle theme) (pretty concentrationTitle)
        , styled (mutedStyle theme) (pretty (ruleLine symbols 76))
        , styled (warningStyle theme) (pretty (warningSymbol symbols <> " " <> renderConcentrationError concentrationError))
        ]
    Right positions ->
      let (count, values) = positionsFor request positions
       in renderConcentrationTable symbols theme count values
  where
    concentrationTitle :: Text
    concentrationTitle = "Concentration"

positionsFor
  :: ConcentrationRequest mode
  -> ConcentrationResult mode
  -> (Int, [Position])
positionsFor (StaticTop size) positions = (snatToInt size, vecToList positions)
positionsFor (DynamicTop _) (SomeTopPositions size positions) = (snatToInt size, vecToList positions)

renderConcentrationTable :: Symbols -> CliTheme -> Int -> [Position] -> Doc AnsiStyle
renderConcentrationTable symbols theme count positions =
  vsep
    [ styled (accentStyle theme) (pretty ("Top " <> T.pack (show count) <> " Positions"))
    , styled (mutedStyle theme) (pretty (ruleLine symbols 76))
    , styled (labelStyle theme) (pretty ("#    Ticker        Weight" :: Text))
    , vsep (zipWith renderPositionRow [1 ..] positions)
    ]
  where
    renderPositionRow :: Int -> Position -> Doc AnsiStyle
    renderPositionRow number position =
      pretty
        ( padLeft 3 (T.pack (show number))
            <> "  "
            <> padRight 14 (unTicker (ticker (asset position)))
            <> padLeft 7 (formatPercentage (weight position))
        )

renderErrorDoc :: Symbols -> CliTheme -> Text -> Doc AnsiStyle
renderErrorDoc symbols theme message =
  vsep
    [ styled (failureStyle theme) (pretty (failureSymbol symbols <> " Input Error"))
    , styled (mutedStyle theme) (pretty (ruleLine symbols 76))
    , vsep (map pretty (T.lines message))
    ]
    <> hardline

renderUsageDoc :: Symbols -> CliTheme -> Text -> Text -> [Text] -> Doc AnsiStyle
renderUsageDoc symbols theme programName message names =
  vsep
    [ styled (failureStyle theme) (pretty (failureSymbol symbols <> " Usage Error"))
    , styled (mutedStyle theme) (pretty (ruleLine symbols 76))
    , pretty ("Erro: " <> message)
    , hardline
    , styled (labelStyle theme) (pretty ("Uso: " <> programName <> " <cenario> [--locality <brazil|chile>] [--top <quantidade>]"))
    , pretty ("  ou: " <> programName <> " --input <arquivo.json> [--locality <brazil|chile>] [--top <quantidade>]")
    , hardline
    , styled (labelStyle theme) (pretty ("Cenarios disponiveis:" :: Text))
    , vsep (map (\name -> pretty ("  " <> bulletSymbol symbols <> " " <> name)) names)
    ]
    <> hardline

renderConcentrationError :: ConcentrationError -> Text
renderConcentrationError concentrationError =
  "Concentration analysis unavailable: " <> T.pack (show concentrationError)

renderDocToHandle :: Handle -> RenderCapabilities -> Doc AnsiStyle -> IO ()
renderDocToHandle handle capabilities document = do
  if ansiEnabled capabilities
    then RenderTerminal.renderIO handle (layoutPretty defaultLayoutOptions document)
    else TIO.hPutStr handle (renderPlain document)
  hFlush handle

renderPlain :: Doc AnsiStyle -> Text
renderPlain = RenderText.renderStrict . layoutPretty defaultLayoutOptions . unAnnotate

renderStyled :: Doc AnsiStyle -> Text
renderStyled = RenderTerminal.renderStrict . layoutPretty defaultLayoutOptions

styled :: AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
styled = annotate

ruleLine :: Symbols -> Int -> Text
ruleLine symbols width = T.replicate width (horizontalBar symbols)

padRight :: Int -> Text -> Text
padRight width value = value <> T.replicate (max 0 (width - T.length value)) " "

padLeft :: Int -> Text -> Text
padLeft width value = T.replicate (max 0 (width - T.length value)) " " <> value

formatPercentage :: Percentage -> Text
formatPercentage percentageValue = T.pack (stripTrailingZero raw) <> "%"
  where
    raw :: String
    raw = printf "%.1f" (unPercentage percentageValue * 100)

    stripTrailingZero :: String -> String
    stripTrailingZero formatted
      | ".0" `isSuffixOf` formatted = take (length formatted - 2) formatted
      | otherwise = formatted
