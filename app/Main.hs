{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stdout)

import Cli.Options (CliOptions (..), InputSource (..), parseCliOptions, renderCliError)
import Cli.Render (renderBundleToHandle, renderErrorToHandle, renderUsageToHandle)
import Cli.ScenarioInput (CustomScenario (..), loadCustomScenario, renderScenarioInputError)
import Domain.Customer (Customer)
import Domain.Portfolio (Portfolio, mkPortfolio)
import Domain.RawPortfolio (RawPortfolio)
import Domain.SafeVector (sN3)
import Domain.HList (HList (HNil, (:&)))
import Examples.Scenarios (PolicyResolutionError (..), Scenario (..), resolveScenarioPolicy, scenarioNames, scenarios)
import Risk.Concentration  (ConcentrationError (..), ConcentrationRequest (DynamicTop, StaticTop))
import Risk.EvaluationBundle (ConcentrationEvidence (ConcentrationEvidence), buildEvaluationBundle)
import Risk.Policy (PolicyConfig, defaultPolicyConfig, policyForLocality)
import Risk.Report (PortfolioStatus (..), status)

main :: IO ()
main = do
  args <- getArgs
  case parseCliOptions args of
    Left cliError -> usageError (renderCliError cliError)
    Right options -> runInput options

-- Definindo só a entrada dos dados. Depois de obter o cliente e a carteira bruta, ambas as fontes já vão usar a mesma função de avaliação
-- Assim, o json não tem uma lógica própria, nem nada nesse sentido. Apenas mantém a mesma semântica usada pelos fixtures
runInput :: CliOptions -> IO ()
runInput options =
  case cliInputSource options of
    BuiltInScenario scenarioName -> runBuiltInScenario options (T.pack scenarioName)
    JsonFile inputPath -> runJsonScenario options inputPath

runBuiltInScenario :: CliOptions -> Text -> IO ()
runBuiltInScenario options scenarioName =
  case Map.lookup scenarioName scenarios of
    Nothing ->
      usageError ("cenario desconhecido: " <> T.unpack scenarioName)
    Just scenario ->
      case resolveScenarioPolicy (cliLocality options) (scenarioPolicySource scenario) of
        Left resolutionError ->
          usageError (renderPolicyResolutionError scenarioName resolutionError)
        Right policyConfig ->
          runEvaluation options policyConfig (scenarioCustomer scenario) (scenarioPortfolio scenario)

runJsonScenario :: CliOptions -> FilePath -> IO ()
runJsonScenario options inputPath = do
  customScenarioResult <- loadCustomScenario inputPath
  case customScenarioResult of
    Left inputError -> do
      renderErrorToHandle stdout (renderScenarioInputError inputError)
      exitWith (ExitFailure 2)
    Right customScenario ->
      -- O json fornece os dados da carteira, mas não define nenhum limite. Então, a política continua sendo responsabilidade da cli, 
      -- permitindo usar o mesmo arquivo pra diferentes localidades sem precisarmos duplicar regras
      let policyConfig = maybe defaultPolicyConfig policyForLocality (cliLocality options)
       in runEvaluation
            options
            policyConfig
            (customScenarioCustomer customScenario)
            (customScenarioPortfolio customScenario)

-- Esse é o único ponto que transforma os dados de entrada que já foram validados em uma carteira "estruturalmente" válida. Então, tanto os
-- fixtures quanto o json sempre passam pelo mkPortfolio, assim consigp manter a mesma validação antes de chegar ao motor
runEvaluation :: CliOptions -> PolicyConfig -> Customer -> RawPortfolio -> IO ()
runEvaluation options policyConfig customerValue rawPortfolio =
  case mkPortfolio rawPortfolio of
    Left domainErr -> do
      renderErrorToHandle
        stdout
        ("Unable to construct portfolio:\n" <> T.pack (show domainErr))
      exitWith (ExitFailure 2)
    Right portfolioValue ->
      evaluateScenario options policyConfig customerValue portfolioValue

-- Mantendo essa etapa na camada da aplicação porque minha ideia é que a localidade seja tratada como uma escolha feita em tempo de execução. 
-- Então, o motor de risco continua recebendo só PolicyConfig, mantendo a reutilização por qlqr coisa que não dependa de terminal 
evaluateScenario :: CliOptions -> PolicyConfig -> Customer -> Portfolio -> IO ()
evaluateScenario options policyConfig customerValue portfolioValue =
  case cliTopCount options of
    Nothing ->
      let bundle = buildEvaluationBundle (StaticTop sN3) policyConfig customerValue portfolioValue
       in case bundle of
            report :& _ :& _ :& HNil -> do
              renderBundleToHandle stdout (cliLocality options) bundle
              exitWith (exitCodeFor (status report))
    Just count ->
      let request = DynamicTop count
          bundle = buildEvaluationBundle request policyConfig customerValue portfolioValue
       in case bundle of
            report :& ConcentrationEvidence _ concentrationResult :& _ :& HNil ->
              case concentrationResult of
                Left concentrationErr -> concentrationUsageError concentrationErr
                Right _ -> do
                  renderBundleToHandle stdout (cliLocality options) bundle
                  exitWith (exitCodeFor (status report))

-- A localidade explica como o relatório foi gerado, mas não faz parte das infos que são produzidas por uma regra de compliance. Estou 
-- mantendo elas na fronteira da cli pra evitar acoplar o resultado do domínio a uma "preocupação" de apresentação 
renderPolicyResolutionError :: Text -> PolicyResolutionError -> String
renderPolicyResolutionError scenarioName (SegmentPolicyCannotUseLocality policy _) =
  "The scenario \"" <> T.unpack scenarioName
    <> "\" already selects the segment policy " <> show policy
    <> ".\n--locality cannot be combined with this scenario."

concentrationUsageError :: ConcentrationError -> IO ()
concentrationUsageError (InvalidRequestedCount _) =
  usageError "Top count must be greater than zero."
concentrationUsageError (NotEnoughPositions required actual) =
  usageError
    ( "Requested Top " <> show required
        <> ", but portfolio contains only " <> show actual <> " positions."
    )

exitCodeFor :: PortfolioStatus -> ExitCode
exitCodeFor Approved = ExitSuccess
exitCodeFor Rejected = ExitFailure 1

usageError :: String -> IO ()
usageError message = do
  progName <- getProgName
  renderUsageToHandle stdout (T.pack progName) (T.pack message) scenarioNames
  exitWith (ExitFailure 64)
