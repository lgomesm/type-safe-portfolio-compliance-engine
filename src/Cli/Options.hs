module Cli.Options (CliError (..), CliOptions (..), InputSource (..), parseCliOptions, parseLocality, renderCliError) where

import Data.Char (toLower)
import Risk.Policy (Locality (..))
import Text.Read (readMaybe)

-- A cli pode pegar os dados de uma fixture compilada ou de um arquivo externo
-- Estou usando um tipo soma pra garantir que só uma das duas fontes seja escolhida. Assim, essa exclusividade não fica 
-- dependente de uma convenção qie eu tivesse que adicionar lá na Main e se tornar parte do resultado válido do parser
data InputSource
  = BuiltInScenario String
  | JsonFile FilePath
  deriving (Eq, Show)

-- A linha de comando interpretada é mantida separada da lista de args brutos também
-- Nesse sentido, estou mantendo tanto a fonte quanto a localidade como valores orientados ao domínio pra que a entrada 
-- textual seja interpretada só uma vez na frente da aplicação. Assim, meu núcleo não pode receber acidentalmente alguma 
-- localidade que não for suportada por ex ou uma combinação ambígua de cenário e arquivo json ao memso tempo
data CliOptions = CliOptions
  { cliInputSource :: InputSource
  , cliTopCount :: Maybe Int
  , cliLocality :: Maybe Locality
  }
  deriving (Eq, Show)

-- Erros causados por entradas inválidas na cli ficam fora do DomainError pq uma opção inválida é um problema na "forma" 
-- como a aplicação foi executada, e não uma falha de valor do domínio. Assim o arquivo Main poded usar o código de saída 
-- específico da cli sem deixar o modelo de erros do domínio mas fraco
data CliError
  = MissingInputSource
  | MissingValue String
  | InvalidTop String
  | UnknownLocality String
  | DuplicateOption String
  | MultipleInputSources
  | UnknownOption String
  | UnexpectedArgument String
  deriving (Eq, Show)

-- Tirei os padrões definidos no Main pq cada nova opção aumentava o número de casos possíveis e fazia com que a mesma 
-- combinação de opções tivesse comportamentos diferentes dependendo da ordem. Portanto, por isso do uso desse fold atual
-- que mantém cada opção independente, já rejeita duplicidades em vez de considerar a última opção informada e só cria 
-- o CliOptions quando existe essa fonte de entrada mesmo
parseCliOptions :: [String] -> Either CliError CliOptions
parseCliOptions = go Nothing Nothing Nothing
  where
    go Nothing _ _ [] = Left MissingInputSource
    go (Just inputSource) topCount locality [] =
      Right
        CliOptions
          { cliInputSource = inputSource
          , cliTopCount = topCount
          , cliLocality = locality
          }
    go inputSource topCount locality ("--input" : remaining) =
      case remaining of
        [] -> Left (MissingValue "--input")
        inputPath : rest
          | isOption inputPath -> Left (MissingValue "--input")
          | otherwise -> do
              nextSource <- addJsonInput inputSource inputPath
              go (Just nextSource) topCount locality rest
    go inputSource topCount locality ("--locality" : remaining) =
      case remaining of
        [] -> Left (MissingValue "--locality")
        rawLocality : rest
          | isOption rawLocality -> Left (MissingValue "--locality")
          | locality /= Nothing -> Left (DuplicateOption "--locality")
          | otherwise ->
              case parseLocality rawLocality of
                Left err -> Left err
                Right localityValue ->
                  go inputSource topCount (Just localityValue) rest
    go inputSource topCount locality ("--top" : remaining) =
      case remaining of
        [] -> Left (MissingValue "--top")
        rawTop : rest
          | isOption rawTop -> Left (MissingValue "--top")
          | topCount /= Nothing -> Left (DuplicateOption "--top")
          | otherwise ->
              case readMaybe rawTop of
                Nothing -> Left (InvalidTop rawTop)
                Just parsedTop -> go inputSource (Just parsedTop) locality rest
    go _ _ _ (argument : _)
      | isOption argument = Left (UnknownOption argument)
    go Nothing topCount locality (scenarioName : rest) =
      go (Just (BuiltInScenario scenarioName)) topCount locality rest
    go (Just (JsonFile _)) _ _ (_ : _) = Left MultipleInputSources
    go (Just (BuiltInScenario _)) _ _ (argument : _) = Left (UnexpectedArgument argument)

-- Mantendo a diferença entre uma opção duplicada e duas fontes de entrada conflitantes. O segundo caso precisa de uma
-- mensagem mais clara, pq as duas entradas podem ser válidas de maneira isolada
addJsonInput :: Maybe InputSource -> FilePath -> Either CliError InputSource
addJsonInput Nothing inputPath = Right (JsonFile inputPath)
addJsonInput (Just (JsonFile _)) _ = Left (DuplicateOption "--input")
addJsonInput (Just (BuiltInScenario _)) _ = Left MultipleInputSources

-- Aceitar a entrada sem diferenciar maiúsculas e minúsculas pra que o Risk.Policy não precise fazer buscas baseadas em 
-- strings e continue sendo a única fonte dos valores das políticas
parseLocality :: String -> Either CliError Locality
parseLocality rawLocality =
  case map toLower rawLocality of
    "brazil" -> Right Brazil
    "chile" -> Right Chile
    _ -> Left (UnknownLocality rawLocality)

renderCliError :: CliError -> String
renderCliError MissingInputSource =
  "Choose exactly one input source:\n\n- a built-in scenario\n- --input <file.json>"
renderCliError (MissingValue optionName) = "Missing value for " <> optionName
renderCliError (InvalidTop _) = "o valor de --top deve ser um inteiro"
renderCliError (UnknownLocality localityName) =
  "Unknown locality: " <> localityName
    <> "\n\nSupported localities:\n- brazil\n- chile"
renderCliError (DuplicateOption optionName) = optionName <> " may only be specified once"
renderCliError MultipleInputSources =
  "Choose exactly one input source:\n\n- a built-in scenario\n- --input <file.json>"
renderCliError (UnknownOption optionName) = "opcao desconhecida: " <> optionName
renderCliError (UnexpectedArgument argument) = "argumento inesperado: " <> argument

isOption :: String -> Bool
isOption ('-' : '-' : _) = True
isOption _ = False