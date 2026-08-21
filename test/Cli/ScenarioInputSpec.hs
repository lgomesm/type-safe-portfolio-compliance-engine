{-# LANGUAGE OverloadedStrings #-}

module Cli.ScenarioInputSpec (spec) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

import Cli.ScenarioInput
  ( CustomScenario (..)
  , CustomerInput (..)
  , MonetaryPositionInput (..)
  , PortfolioInput (..)
  , PositionInput (..)
  , ScenarioFile (..)
  , ScenarioInputError (..)
  , buildCustomScenario
  , decodeScenarioFile
  , loadCustomScenario
  , parseCreditScore
  , parseCustomerProfile
  )
import Domain.AssetClassification (AssetClass (..), Sector (..))
import Domain.CreditScore (CreditScore (..))
import Domain.CustomerProfile (CustomerProfile (..))
import Domain.Error (DomainError (..))
import Domain.Portfolio (mkPortfolio)
import Examples.Scenarios (Scenario (..), scenarios)
import Risk.Approval (evaluateApproval)
import Risk.Policy (Locality (..), policyForLocality)
import Risk.Report (PortfolioStatus (..), buildReportWithPolicy, status, violations)
import Risk.Violation (Violation (AssetAllocationExceeded))

spec :: Spec
spec = describe "Cli.ScenarioInput" $ do
  describe "decodeScenarioFile" $ do
    it "decodes external DTOs before converting them to a custom scenario" $
      case decodeScenarioFile approvedJson of
        Right customScenario -> do
          customScenarioName customScenario `shouldBe` Just "custom-approved"
          mkPortfolio (customScenarioPortfolio customScenario) `shouldSatisfy` isRight
        Left err -> expectationFailure (show err)

    it "keeps malformed JSON separate from domain errors" $
      decodeScenarioFile "{\"customer\":"
        `shouldSatisfy` isJsonDecodeError

    it "reports schema failures as JSON input errors" $
      decodeScenarioFile "{\"customer\": {\"id\": \"customer-1\"}}"
        `shouldSatisfy` isJsonDecodeError

    it "requires the portfolio hierarchy and numeric weights in the JSON schema" $ do
      decodeScenarioFile "{\"customer\": {\"id\": \"customer-1\", \"profile\": \"moderate\", \"creditScore\": \"high\"}}"
        `shouldSatisfy` isJsonDecodeError
      decodeScenarioFile "{\"customer\": {\"id\": \"customer-1\", \"profile\": \"moderate\", \"creditScore\": \"high\"}, \"portfolio\": {}}"
        `shouldSatisfy` isJsonDecodeError
      decodeScenarioFile "{\"customer\": {\"id\": \"customer-1\", \"profile\": \"moderate\", \"creditScore\": \"high\"}, \"portfolio\": {\"positions\": [{\"ticker\": \"PETR4\", \"assetClass\": \"Equity\", \"sector\": \"Energy\", \"weight\": \"thirty\"}]}}"
        `shouldSatisfy` isJsonDecodeError

    it "rejects an unknown allocation mode as a protocol error" $
      decodeScenarioFile unknownAllocationModeJson
        `shouldBe` Left (UnknownPortfolioAllocationMode "bananas")

    it "requires amount fields when the allocation mode is monetary" $
      decodeScenarioFile missingAmountJson
        `shouldSatisfy` isJsonDecodeError

    it "rejects a position that mixes weight and amount" $
      decodeScenarioFile mixedAllocationJson
        `shouldSatisfy` isJsonDecodeError

  describe "buildCustomScenario" $ do
    it "accepts customer profile and credit score case-insensitively" $ do
      parseCustomerProfile "Conservative" `shouldBe` Right Conservative
      parseCustomerProfile "MODERATE" `shouldBe` Right Moderate
      parseCustomerProfile "aggressive" `shouldBe` Right Aggressive
      parseCreditScore "LOW" `shouldBe` Right LowScore
      parseCreditScore "medium" `shouldBe` Right MediumScore
      parseCreditScore "High" `shouldBe` Right HighScore

    it "rejects unknown customer profile and credit score before evaluation" $ do
      parseCustomerProfile "ultra-conservative"
        `shouldBe` Left (UnknownCustomerProfile "ultra-conservative")
      parseCreditScore "AAA" `shouldBe` Left (UnknownCreditScore "AAA")

    it "uses smart constructors and domain parsers for every position field" $ do
      buildCustomScenario (scenarioWithEmptyCustomerId)
        `shouldBe` Left (InvalidCustomerField "id" EmptyCustomerId)
      buildCustomScenario (scenarioWithPosition emptyTickerPosition)
        `shouldBe` Left (InvalidPositionField 0 "ticker" EmptyTicker)
      buildCustomScenario (scenarioWithPosition invalidClassPosition)
        `shouldBe` Left (InvalidPositionField 0 "assetClass" (UnknownAssetClass "Stock"))
      buildCustomScenario (scenarioWithPosition invalidSectorPosition)
        `shouldBe` Left (InvalidPositionField 0 "sector" (UnknownSector "Banking"))
      buildCustomScenario (scenarioWithPosition invalidWeightPosition)
        `shouldBe` Left (InvalidPositionField 0 "weight" (PercentageOutOfRange 1.1))

      buildCustomScenario monetaryScenarioWithZero
        `shouldBe` Left (InvalidPositionField 0 "amount" (NonPositiveMoney 0))

    it "accepts the inclusive percentage endpoints" $ do
      buildCustomScenario (scenarioWithPositions [position "CASH" Cash Consumer 0, position "PETR4" Equity Energy 1])
        `shouldSatisfy` isRight

    it "leaves empty portfolios and invalid weight sums for mkPortfolio" $ do
      case buildCustomScenario (scenarioWithPositions []) of
        Right customScenario ->
          mkPortfolio (customScenarioPortfolio customScenario)
            `shouldBe` Left EmptyPortfolio
        Left err -> expectationFailure (show err)

      case buildCustomScenario invalidSumScenarioFile of
        Right customScenario ->
          mkPortfolio (customScenarioPortfolio customScenario)
            `shouldBe` Left (PortfolioWeightsDoNotSumToOne 0.8)
        Left err -> expectationFailure (show err)

  describe "integration with the existing pipeline" $ do
    it "keeps the JSON fixture equivalent to the built-in approved scenario" $
      case (Map.lookup "approved" scenarios, decodeScenarioFile approvedJson) of
        (Just builtIn, Right customScenario) -> do
          customScenarioCustomer customScenario `shouldBe` scenarioCustomer builtIn
          customScenarioPortfolio customScenario `shouldBe` scenarioPortfolio builtIn
          case (mkPortfolio (customScenarioPortfolio customScenario), mkPortfolio (scenarioPortfolio builtIn)) of
            (Right customPortfolio, Right builtInPortfolio) -> do
              let customReport = buildReportWithPolicy (policyForLocality Brazil) (customScenarioCustomer customScenario) customPortfolio
                  builtInReport = buildReportWithPolicy (policyForLocality Brazil) (scenarioCustomer builtIn) builtInPortfolio
              customReport `shouldBe` builtInReport
              evaluateApproval customReport `shouldBe` evaluateApproval builtInReport
            _ -> expectationFailure "approved inputs should construct valid portfolios"
        _ -> expectationFailure "missing approved fixture or unable to decode JSON"

    it "keeps weighted and monetary JSON inputs equivalent" $
      case (decodeScenarioFile approvedJson, decodeScenarioFile amountJson) of
        (Right weightedScenario, Right monetaryScenario) -> do
          customScenarioPortfolio monetaryScenario
            `shouldBe` customScenarioPortfolio weightedScenario
          case
              ( mkPortfolio (customScenarioPortfolio weightedScenario)
              , mkPortfolio (customScenarioPortfolio monetaryScenario)
              ) of
            (Right weightedPortfolio, Right monetaryPortfolio) -> do
              let weightedReport =
                    buildReportWithPolicy
                      (policyForLocality Brazil)
                      (customScenarioCustomer weightedScenario)
                      weightedPortfolio
                  monetaryReport =
                    buildReportWithPolicy
                      (policyForLocality Brazil)
                      (customScenarioCustomer monetaryScenario)
                      monetaryPortfolio
              weightedReport `shouldBe` monetaryReport
              evaluateApproval weightedReport `shouldBe` evaluateApproval monetaryReport
            _ -> expectationFailure "equivalent inputs should build valid portfolios"
        other -> expectationFailure ("expected both inputs to decode: " <> show other)

    it "allows a valid JSON portfolio to be rejected by compliance" $
      case decodeScenarioFile concentratedJson of
        Right customScenario ->
          case mkPortfolio (customScenarioPortfolio customScenario) of
            Right portfolioValue -> do
              let report = buildReportWithPolicy (policyForLocality Brazil) (customScenarioCustomer customScenario) portfolioValue
              status report `shouldBe` Rejected
              violations report `shouldSatisfy` any isAssetAllocation
            Left err -> expectationFailure (show err)
        Left err -> expectationFailure (show err)

    it "evaluates the same JSON differently under Brazil and Chile policies" $
      case decodeScenarioFile approvedJson of
        Right customScenario ->
          case mkPortfolio (customScenarioPortfolio customScenario) of
            Right portfolioValue -> do
              let brazilReport = buildReportWithPolicy (policyForLocality Brazil) (customScenarioCustomer customScenario) portfolioValue
                  chileReport = buildReportWithPolicy (policyForLocality Chile) (customScenarioCustomer customScenario) portfolioValue
              status brazilReport `shouldBe` Approved
              status chileReport `shouldBe` Rejected
            Left err -> expectationFailure (show err)
        Left err -> expectationFailure (show err)

  describe "loadCustomScenario" $
    it "turns file, decoding and domain failures into controlled input errors" $ do
      missingFile <- loadCustomScenario "test/fixtures/does-not-exist.json"
      missingFile `shouldSatisfy` isInputFileReadError

      malformedFile <- loadCustomScenario "test/fixtures/custom-invalid-json.json"
      malformedFile `shouldSatisfy` isJsonDecodeError

      invalidWeightFile <- loadCustomScenario "test/fixtures/custom-invalid-weight.json"
      invalidWeightFile
        `shouldBe` Left (InvalidPositionField 0 "weight" (PercentageOutOfRange 1.3))

      invalidSumFile <- loadCustomScenario "test/fixtures/custom-invalid-sum.json"
      case invalidSumFile of
        Right customScenario ->
          mkPortfolio (customScenarioPortfolio customScenario)
            `shouldBe` Left (PortfolioWeightsDoNotSumToOne 0.8)
        Left err -> expectationFailure (show err)

      validFixture <- loadCustomScenario "test/fixtures/custom-approved.json"
      validFixture `shouldSatisfy` isRight

      amountFixture <- loadCustomScenario "test/fixtures/custom-approved-by-amount.json"
      amountFixture `shouldSatisfy` isRight

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isJsonDecodeError :: Either ScenarioInputError a -> Bool
isJsonDecodeError (Left (JsonDecodeError _)) = True
isJsonDecodeError _ = False

isInputFileReadError :: Either ScenarioInputError a -> Bool
isInputFileReadError (Left (InputFileReadError _ _)) = True
isInputFileReadError _ = False

isAssetAllocation :: Violation -> Bool
isAssetAllocation (AssetAllocationExceeded _ _ _) = True
isAssetAllocation _ = False

scenarioWithPosition :: PositionInput -> ScenarioFile
scenarioWithPosition positionInput = scenarioWithPositions [positionInput]

scenarioWithPositions :: [PositionInput] -> ScenarioFile
scenarioWithPositions positions =
  ScenarioFile
    { scenarioFileName = Nothing
    , scenarioFileCustomer = CustomerInput "customer-1" "moderate" "high"
    , scenarioFilePortfolio = WeightedPortfolioInput positions
    }

scenarioWithEmptyCustomerId :: ScenarioFile
scenarioWithEmptyCustomerId =
  (scenarioWithPosition (position "PETR4" Equity Energy 1))
    { scenarioFileCustomer = CustomerInput "   " "moderate" "high"
    }

position :: String -> AssetClass -> Sector -> Double -> PositionInput
position tickerName classValue sectorValue weightValue =
  PositionInput
    { positionInputTicker = T.pack tickerName
    , positionInputAssetClass = T.pack (show classValue)
    , positionInputSector = T.pack (show sectorValue)
    , positionInputWeight = weightValue
    }

emptyTickerPosition :: PositionInput
emptyTickerPosition = position "   " Equity Energy 1

invalidClassPosition :: PositionInput
invalidClassPosition =
  PositionInput "PETR4" "Stock" "Energy" 1

invalidSectorPosition :: PositionInput
invalidSectorPosition =
  PositionInput "PETR4" "Equity" "Banking" 1

invalidWeightPosition :: PositionInput
invalidWeightPosition = position "PETR4" Equity Energy 1.1

monetaryScenarioWithZero :: ScenarioFile
monetaryScenarioWithZero =
  ScenarioFile
    { scenarioFileName = Nothing
    , scenarioFileCustomer = CustomerInput "customer-1" "moderate" "high"
    , scenarioFilePortfolio =
        MonetaryPortfolioInput
          [ MonetaryPositionInput "PETR4" "Equity" "Energy" 0
          ]
    }

invalidSumScenarioFile :: ScenarioFile
invalidSumScenarioFile =
  scenarioWithPositions
    [ position "PETR4" Equity Energy 0.5
    , position "ITUB4" Equity Financial 0.3
    ]

approvedJson :: BS8.ByteString
approvedJson =
  BS8.pack
    "{\n\
    \  \"name\": \"custom-approved\",\n\
    \  \"customer\": {\"id\": \"customer-conservative-high\", \"profile\": \"conservative\", \"creditScore\": \"high\"},\n\
    \  \"portfolio\": {\"positions\": [\n\
    \    {\"ticker\": \"PETR4\", \"assetClass\": \"Equity\", \"sector\": \"Energy\", \"weight\": 0.30},\n\
    \    {\"ticker\": \"ITUB4\", \"assetClass\": \"Equity\", \"sector\": \"Financial\", \"weight\": 0.30},\n\
    \    {\"ticker\": \"VALE3\", \"assetClass\": \"Equity\", \"sector\": \"Other\", \"weight\": 0.20},\n\
    \    {\"ticker\": \"WEGE3\", \"assetClass\": \"Equity\", \"sector\": \"Technology\", \"weight\": 0.20}\n\
    \  ]}}"

concentratedJson :: BS8.ByteString
concentratedJson =
  BS8.pack
    "{\n\
    \  \"customer\": {\"id\": \"customer-moderate-high\", \"profile\": \"moderate\", \"creditScore\": \"high\"},\n\
    \  \"portfolio\": {\"positions\": [\n\
    \    {\"ticker\": \"PETR4\", \"assetClass\": \"Equity\", \"sector\": \"Energy\", \"weight\": 0.40},\n\
    \    {\"ticker\": \"ITUB4\", \"assetClass\": \"Equity\", \"sector\": \"Financial\", \"weight\": 0.35},\n\
    \    {\"ticker\": \"VALE3\", \"assetClass\": \"Equity\", \"sector\": \"Other\", \"weight\": 0.25}\n\
    \  ]}}"

amountJson :: BS8.ByteString
amountJson =
  BS8.pack
    "{\n\
    \  \"name\": \"custom-approved-by-amount\",\n\
    \  \"customer\": {\"id\": \"customer-conservative-high\", \"profile\": \"conservative\", \"creditScore\": \"high\"},\n\
    \  \"portfolio\": {\"allocationMode\": \"amount\", \"positions\": [\n\
    \    {\"ticker\": \"PETR4\", \"assetClass\": \"Equity\", \"sector\": \"Energy\", \"amount\": 30000},\n\
    \    {\"ticker\": \"ITUB4\", \"assetClass\": \"Equity\", \"sector\": \"Financial\", \"amount\": 30000},\n\
    \    {\"ticker\": \"VALE3\", \"assetClass\": \"Equity\", \"sector\": \"Other\", \"amount\": 20000},\n\
    \    {\"ticker\": \"WEGE3\", \"assetClass\": \"Equity\", \"sector\": \"Technology\", \"amount\": 20000}\n\
    \  ]}}"

unknownAllocationModeJson :: BS8.ByteString
unknownAllocationModeJson =
  BS8.pack
    "{\"customer\": {\"id\": \"customer-1\", \"profile\": \"moderate\", \"creditScore\": \"high\"}, \"portfolio\": {\"allocationMode\": \"bananas\", \"positions\": []}}"

missingAmountJson :: BS8.ByteString
missingAmountJson =
  BS8.pack
    "{\"customer\": {\"id\": \"customer-1\", \"profile\": \"moderate\", \"creditScore\": \"high\"}, \"portfolio\": {\"allocationMode\": \"amount\", \"positions\": [{\"ticker\": \"PETR4\", \"assetClass\": \"Equity\", \"sector\": \"Energy\"}]}}"

mixedAllocationJson :: BS8.ByteString
mixedAllocationJson =
  BS8.pack
    "{\"customer\": {\"id\": \"customer-1\", \"profile\": \"moderate\", \"creditScore\": \"high\"}, \"portfolio\": {\"allocationMode\": \"amount\", \"positions\": [{\"ticker\": \"PETR4\", \"assetClass\": \"Equity\", \"sector\": \"Energy\", \"amount\": 100, \"weight\": 1.0}]}}"