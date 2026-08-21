{-# LANGUAGE OverloadedStrings #-}

module Cli.ScenarioInput (CustomScenario (..), CustomerInput (..), MonetaryPositionInput (..), PortfolioInput (..), 
  PositionInput (..), ScenarioFile (..), ScenarioInputError (..), buildCustomScenario, decodeScenarioFile, 
  loadCustomScenario, parseCreditScore, parseCustomerProfile, renderScenarioInputError) where

import Control.Exception (IOException, displayException, try)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict', withObject, (.:), (.:?))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

import Domain.Asset (Asset (..))
import Domain.AssetClassification (parseAssetClass, parseSector)
import Domain.CreditScore (CreditScore (..))
import Domain.Customer (Customer (..))
import Domain.CustomerId (mkCustomerId)
import Domain.CustomerProfile (CustomerProfile (..))
import Domain.Error (DomainError)
import Domain.Money (mkPositiveMoney)
import Domain.MonetaryPosition (MonetaryPosition (..), monetaryPositionsToRawPortfolio)
import Domain.Percentage (mkPercentage)
import Domain.Position (Position (..))
import Domain.RawPortfolio (RawPortfolio (..))
import Domain.Ticker (mkTicker)

-- Mantendo text e double aqui pra poder decodificar o json sintaticamente válido mesmo quando os valores ainda serão 
-- rejeitados pelo domínio
data ScenarioFile = ScenarioFile
  { scenarioFileName :: Maybe Text
  , scenarioFileCustomer :: CustomerInput
  , scenarioFilePortfolio :: PortfolioInput
  }
  deriving (Eq, Show)

data CustomerInput = CustomerInput
  { customerInputId :: Text
  , customerInputProfile :: Text
  , customerInputCreditScore :: Text
  }
  deriving (Eq, Show)

-- Criando o terceiro construtor só pelo parser quando o json informa um modo que é desconhecido. Assim, minha camada de 
-- construção já produz um erro de protocolo tipado, em vez de tratar como p JsonDecodeError 
data PortfolioInput
  = WeightedPortfolioInput [PositionInput]
  | MonetaryPortfolioInput [MonetaryPositionInput]
  | InvalidPortfolioAllocationMode Text
  deriving (Eq, Show)

data PositionInput = PositionInput
  { positionInputTicker :: Text
  , positionInputAssetClass :: Text
  , positionInputSector :: Text
  , positionInputWeight :: Double
  }
  deriving (Eq, Show)

data MonetaryPositionInput = MonetaryPositionInput
  { monetaryPositionInputTicker :: Text
  , monetaryPositionInputAssetClass :: Text
  , monetaryPositionInputSector :: Text
  , monetaryPositionInputAmount :: Double
  }
  deriving (Eq, Show)

instance FromJSON ScenarioFile where
  parseJSON = withObject "ScenarioFile" $ \object ->
    ScenarioFile
      <$> object .:? "name"
      <*> object .: "customer"
      <*> object .: "portfolio"

instance FromJSON CustomerInput where
  parseJSON = withObject "CustomerInput" $ \object ->
    CustomerInput
      <$> object .: "id"
      <*> object .: "profile"
      <*> object .: "creditScore"

instance FromJSON PortfolioInput where
  parseJSON = withObject "PortfolioInput" $ \object ->
    do
      rawMode <- object .:? "allocationMode"
      case fmap normalize rawMode of
        Nothing -> WeightedPortfolioInput <$> object .: "positions"
        Just "weight" -> WeightedPortfolioInput <$> object .: "positions"
        Just "amount" -> MonetaryPortfolioInput <$> object .: "positions"
        Just _ -> pure (InvalidPortfolioAllocationMode (maybe "" id rawMode))

instance FromJSON PositionInput where
  parseJSON = withObject "PositionInput" $ \object -> do
    -- O aeson já esta ignorando os campos desconhecidos por padrão. Entçao rejeitando "amount" aqui evitamos que uma 
    -- entrada híbrida fique sendo interpretada como percentual 
    if KeyMap.member "amount" object
      then fail "weight positions cannot contain amount"
      else
        PositionInput
          <$> object .: "ticker"
          <*> object .: "assetClass"
          <*> object .: "sector"
          <*> object .: "weight"

instance FromJSON MonetaryPositionInput where
  parseJSON = withObject "MonetaryPositionInput" $ \object -> do
    -- Modo monetário tb precisa ser "exclusivo". Se eu aceitasse o weight como um campo extra ignorado, acabria criando 
    -- uma entrada ambígua
    if KeyMap.member "weight" object
      then fail "amount positions cannot contain weight"
      else
        MonetaryPositionInput
          <$> object .: "ticker"
          <*> object .: "assetClass"
          <*> object .: "sector"
          <*> object .: "amount"

data ScenarioInputError
  = InputFileReadError FilePath Text
  | JsonDecodeError Text
  | InvalidCustomerField Text DomainError
  | UnknownCustomerProfile Text
  | UnknownCreditScore Text
  | UnknownPortfolioAllocationMode Text
  | InvalidPositionField Int Text DomainError
  | InvalidPortfolioField Text DomainError
  deriving (Eq, Show)

data CustomScenario = CustomScenario
  { customScenarioName :: Maybe Text
  , customScenarioCustomer :: Customer
  , customScenarioPortfolio :: RawPortfolio
  }
  deriving (Eq, Show)

-- Decodificação e construção do domínio serpadas pra que o aeson apenas valide a estrutura do json, enquanto esse módulo 
-- delega as regras financeiras pros construtores que já vao aplicar elas pra tds os chamadores
decodeScenarioFile :: BS.ByteString -> Either ScenarioInputError CustomScenario
decodeScenarioFile inputBytes =
  case eitherDecodeStrict' inputBytes of
    Left decodeError -> Left (JsonDecodeError (T.pack decodeError))
    Right scenarioFile -> buildCustomScenario scenarioFile

loadCustomScenario :: FilePath -> IO (Either ScenarioInputError CustomScenario)
loadCustomScenario inputPath = do
  readResult <- try (BS.readFile inputPath) :: IO (Either IOException BS.ByteString)
  pure $
    case readResult of
      Left readError ->
        Left (InputFileReadError inputPath (T.pack (displayException readError)))
      Right inputBytes -> decodeScenarioFile inputBytes

buildCustomScenario :: ScenarioFile -> Either ScenarioInputError CustomScenario
buildCustomScenario scenarioFile = do
  customerValue <- toCustomer (scenarioFileCustomer scenarioFile)
  rawPortfolio <- toRawPortfolio (scenarioFilePortfolio scenarioFile)
  pure
    CustomScenario
      { customScenarioName = scenarioFileName scenarioFile
      , customScenarioCustomer = customerValue
      , customScenarioPortfolio = rawPortfolio
      }

toRawPortfolio :: PortfolioInput -> Either ScenarioInputError RawPortfolio
toRawPortfolio (InvalidPortfolioAllocationMode rawMode) =
  Left (UnknownPortfolioAllocationMode rawMode)
toRawPortfolio (WeightedPortfolioInput positions) = do
  validatedPositions <- traverse (uncurry toPosition) (zip [0 ..] positions)
  pure (RawPortfolio validatedPositions)
toRawPortfolio (MonetaryPortfolioInput positions) = do
  monetaryPositions <- traverse (uncurry toMonetaryPosition) (zip [0 ..] positions)
  case monetaryPositionsToRawPortfolio monetaryPositions of
    Left domainError -> Left (InvalidPortfolioField "amounts" domainError)
    Right rawPortfolio -> Right rawPortfolio

toCustomer :: CustomerInput -> Either ScenarioInputError Customer
toCustomer customerInput = do
  customerIdValue <- mapCustomerDomainError "id" (mkCustomerId (customerInputId customerInput))
  profileValue <- parseCustomerProfile (customerInputProfile customerInput)
  scoreValue <- parseCreditScore (customerInputCreditScore customerInput)
  pure
    Customer
      { customerId = customerIdValue
      , customerProfile = profileValue
      , creditScore = scoreValue
      }

-- Reutilizando os parsers do domínio em vez de recriar os adts fechados aqui pra qie existe apenas uma unica fonte pras
-- classes de ativos e setores aceitos
toPosition :: Int -> PositionInput -> Either ScenarioInputError Position
toPosition positionIndex positionInput = do
  assetValue <-
    toAsset
      positionIndex
      (positionInputTicker positionInput)
      (positionInputAssetClass positionInput)
      (positionInputSector positionInput)
  weightValue <- mapPositionDomainError positionIndex "weight" (mkPercentage (positionInputWeight positionInput))
  pure
    Position
      { asset = assetValue
      , weight = weightValue
      }

toMonetaryPosition :: Int -> MonetaryPositionInput -> Either ScenarioInputError MonetaryPosition
toMonetaryPosition positionIndex positionInput = do
  assetValue <-
    toAsset
      positionIndex
      (monetaryPositionInputTicker positionInput)
      (monetaryPositionInputAssetClass positionInput)
      (monetaryPositionInputSector positionInput)
  amountValue <-
    mapPositionDomainError
      positionIndex
      "amount"
      (mkPositiveMoney (monetaryPositionInputAmount positionInput))
  pure
    MonetaryPosition
      { monetaryAsset = assetValue
      , monetaryAmount = amountValue
      }

toAsset :: Int -> Text -> Text -> Text -> Either ScenarioInputError Asset
toAsset positionIndex tickerText assetClassText sectorText = do
  tickerValue <- mapPositionDomainError positionIndex "ticker" (mkTicker tickerText)
  assetClassValue <- mapPositionDomainError positionIndex "assetClass" (parseAssetClass assetClassText)
  sectorValue <- mapPositionDomainError positionIndex "sector" (parseSector sectorText)
  pure (Asset tickerValue assetClassValue sectorValue)

parseCustomerProfile :: Text -> Either ScenarioInputError CustomerProfile
parseCustomerProfile rawProfile =
  case normalize rawProfile of
    "conservative" -> Right Conservative
    "moderate" -> Right Moderate
    "aggressive" -> Right Aggressive
    _ -> Left (UnknownCustomerProfile rawProfile)

parseCreditScore :: Text -> Either ScenarioInputError CreditScore
parseCreditScore rawScore =
  case normalize rawScore of
    "low" -> Right LowScore
    "medium" -> Right MediumScore
    "high" -> Right HighScore
    _ -> Left (UnknownCreditScore rawScore)

normalize :: Text -> Text
normalize = T.toLower . T.strip

mapCustomerDomainError :: Text -> Either DomainError a -> Either ScenarioInputError a
mapCustomerDomainError fieldName = either (Left . InvalidCustomerField fieldName) Right

mapPositionDomainError :: Int -> Text -> Either DomainError a -> Either ScenarioInputError a
mapPositionDomainError positionIndex fieldName =
  either (Left . InvalidPositionField positionIndex fieldName) Right

renderScenarioInputError :: ScenarioInputError -> Text
renderScenarioInputError (InputFileReadError inputPath details) =
  T.unlines
    [ "Unable to read input file:"
    , T.pack inputPath
    , details
    ]
renderScenarioInputError (JsonDecodeError details) =
  "Invalid JSON input:\n" <> details
renderScenarioInputError (InvalidCustomerField fieldName domainError) =
  "Invalid customer field '" <> fieldName <> "':\n" <> T.pack (show domainError)
renderScenarioInputError (UnknownCustomerProfile rawProfile) =
  T.unlines
    [ "Unknown customer profile: " <> rawProfile
    , "Supported profiles:"
    , "- conservative"
    , "- moderate"
    , "- aggressive"
    ]
renderScenarioInputError (UnknownCreditScore rawScore) =
  T.unlines
    [ "Unknown credit score: " <> rawScore
    , "Supported scores:"
    , "- low"
    , "- medium"
    , "- high"
    ]
renderScenarioInputError (UnknownPortfolioAllocationMode rawMode) =
  T.unlines
    [ "Unknown portfolio allocation mode: " <> rawMode
    , "Supported modes:"
    , "- weight"
    , "- amount"
    ]
renderScenarioInputError (InvalidPositionField positionIndex fieldName domainError) =
  "Invalid field at portfolio.positions["
    <> T.pack (show positionIndex)
    <> "]."
    <> fieldName
    <> ":\n"
    <> T.pack (show domainError)
renderScenarioInputError (InvalidPortfolioField fieldName domainError) =
  "Invalid portfolio field '"
    <> fieldName
    <> "':\n"
    <> T.pack (show domainError)