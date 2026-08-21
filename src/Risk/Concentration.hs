{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- Visao das maiores posicoes de uma carteira
module Risk.Concentration (Top3Positions, ConcentrationError (..), ConcentrationMode (..), ConcentrationRequest (..), 
  ConcentrationResult, SomeTopPositions (..), analyzeConcentration, topNPositions, top3Positions, topPositionsByCount, 
  largestPosition, secondLargestPosition, thirdLargestPosition, renderTop3, renderSomeTopPositions, renderConcentration) where

import Data.Kind (Type)
import Data.List (isSuffixOf, sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

import Domain.Asset (Asset (ticker))
import Domain.Percentage (Percentage, unPercentage)
import Domain.Portfolio (Portfolio, portfolioPositions)
import Domain.Position (Position (..))
import Domain.SafeVector (Fin (..), Nat, N3, SNat, SomeSNat (..), Vec, indexVec, sN3, snatToInt, someSNatFromInt, takeVec, vecToList)
import Domain.Ticker (unTicker)

type Top3Positions = Vec N3 Position

-- A falta de posicoes nao invalida a carteira pq essa verificacao pertence só a essa visao de concentracao
data ConcentrationError
  = InvalidRequestedCount Int
  | NotEnoughPositions
      { requiredPositions :: Int
      , actualPositions :: Int
      }
  deriving (Eq, Show)

-- O tamanho fica existencial quando so é conhecido em runtime, mas a testemunha e o vetor ficam ligados pelo mesmo indice de tipo
data SomeTopPositions = forall n. SomeTopPositions (SNat n) (Vec n Position)

data ConcentrationMode
  = Static Nat
  | Dynamic

data ConcentrationRequest (mode :: ConcentrationMode) where
  StaticTop :: SNat n -> ConcentrationRequest ('Static n)
  DynamicTop :: Int -> ConcentrationRequest 'Dynamic

-- O resultado depende do modo pq um tamanho estatico pode permanecer visivel no tipo, enquanto um tamanho dinamico precisa ser ocultado
type family ConcentrationResult (mode :: ConcentrationMode) :: Type where
  ConcentrationResult ('Static n) = Vec n Position
  ConcentrationResult 'Dynamic = SomeTopPositions

analyzeConcentration :: ConcentrationRequest mode -> Portfolio -> Either ConcentrationError (ConcentrationResult mode)
analyzeConcentration (StaticTop size) portfolio = topNPositions size portfolio
analyzeConcentration (DynamicTop requested) portfolio = topPositionsByCount requested portfolio

topNPositions :: SNat n -> Portfolio -> Either ConcentrationError (Vec n Position)
topNPositions size portfolio =
  case takeVec size sortedPositions of
    Just positions -> Right positions
    Nothing ->
      Left
        NotEnoughPositions
          { requiredPositions = snatToInt size
          , actualPositions = length sortedPositions
          }
  where
    sortedPositions :: [Position]
    sortedPositions = sortPositionsByWeightDesc (portfolioPositions portfolio)

top3Positions :: Portfolio -> Either ConcentrationError Top3Positions
top3Positions = topNPositions sN3

topPositionsByCount :: Int -> Portfolio -> Either ConcentrationError SomeTopPositions
topPositionsByCount requested portfolio
  | requested <= 0 = Left (InvalidRequestedCount requested)
  | otherwise =
      case someSNatFromInt requested of
        Nothing -> Left (InvalidRequestedCount requested)
        Just (SomeSNat size) ->
          case topNPositions size portfolio of
            Left err -> Left err
            Right positions -> Right (SomeTopPositions size positions)

sortPositionsByWeightDesc :: [Position] -> [Position]
sortPositionsByWeightDesc = sortBy (flip (comparing weight))

largestPosition :: Top3Positions -> Position
largestPosition = indexVec FZ

secondLargestPosition :: Top3Positions -> Position
secondLargestPosition = indexVec (FS FZ)

thirdLargestPosition :: Top3Positions -> Position
thirdLargestPosition = indexVec (FS (FS FZ))

-- Usando os indices tipados pq o tamanho do Top 3 ja garante que essas tres posicoes existem
renderTop3 :: Top3Positions -> Text
renderTop3 top3 =
  T.unlines
    [ "Top 3 Positions:"
    , "1. " <> renderPosition (largestPosition top3)
    , "2. " <> renderPosition (secondLargestPosition top3)
    , "3. " <> renderPosition (thirdLargestPosition top3)
    ]

renderSomeTopPositions :: SomeTopPositions -> Text
renderSomeTopPositions (SomeTopPositions size positions) =
  renderPositionList (snatToInt size) (vecToList positions)

renderConcentration :: ConcentrationRequest mode -> ConcentrationResult mode -> Text
renderConcentration (StaticTop size) positions =
  renderPositionList (snatToInt size) (vecToList positions)
renderConcentration (DynamicTop _) positions = renderSomeTopPositions positions

renderPositionList :: Int -> [Position] -> Text
renderPositionList count positions =
  T.unlines
    ( ("Top " <> T.pack (show count) <> " Positions:")
        : zipWith renderNumberedPosition [1 :: Int ..] positions
    )

renderNumberedPosition :: Int -> Position -> Text
renderNumberedPosition positionNumber positionValue =
  T.pack (show positionNumber) <> ". " <> renderPosition positionValue

renderPosition :: Position -> Text
renderPosition positionValue =
  unTicker (ticker (asset positionValue)) <> " - " <> formatPercentage (weight positionValue)

formatPercentage :: Percentage -> Text
formatPercentage percentageValue = T.pack (stripTrailingZero raw) <> "%"
  where
    raw :: String
    raw = printf "%.1f" (unPercentage percentageValue * 100)

    stripTrailingZero :: String -> String
    stripTrailingZero formatted
      | ".0" `isSuffixOf` formatted = take (length formatted - 2) formatted
      | otherwise = formatted