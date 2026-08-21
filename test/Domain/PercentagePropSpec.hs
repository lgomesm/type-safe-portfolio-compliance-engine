module Domain.PercentagePropSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

import Domain.Error (DomainError (..))
import Domain.Percentage (clampPercentage, mkPercentage, unPercentage)
import Generators.Domain (genValidPercentage, shrinkValidPercentage)

spec :: Spec
spec = describe "Domain.Percentage (properties)" $ modifyMaxSuccess (const 200) $ do
  it "every valid Percentage stays inside the closed interval [0, 1]" $
    forAllShrink genValidPercentage shrinkValidPercentage $ \percentageValue ->
      let rawValue = unPercentage percentageValue
       in not (isNaN rawValue)
            && not (isInfinite rawValue)
            && rawValue >= 0
            && rawValue <= 1

  it "mkPercentage succeeds exactly for finite values inside [0, 1]" $
    property $ \rawValue ->
      isRight (mkPercentage rawValue)
        === isValidPercentageInput rawValue

  it "clampPercentage rejects non-finite values and validates every result" $
    property $ \rawValue ->
      case clampPercentage rawValue of
        Left (NonFinitePercentage _) ->
          property (isNaN rawValue || isInfinite rawValue)
        Left _ ->
          property False
        Right percentageValue ->
          let actual = unPercentage percentageValue
           in property
                (not (isNaN actual)
                  && not (isInfinite actual)
                  && actual >= 0
                  && actual <= 1)

  it "mkPercentage is identity on the raw value of valid percentages" $
    forAllShrink genValidPercentage shrinkValidPercentage $ \percentageValue ->
      case mkPercentage (unPercentage percentageValue) of
        Right rebuilt -> unPercentage rebuilt === unPercentage percentageValue
        Left err -> counterexample (show err) False

  it "values outside [0, 1] are rejected with PercentageOutOfRange" $
    forAll (oneof [choose (-1.0e6, -1.0e-9), choose (1.0 + 1.0e-9, 1.0e6)]) $ \rawValue ->
      mkPercentage rawValue === Left (PercentageOutOfRange rawValue)

isRight :: Either errorType valueType -> Bool
isRight (Right _) = True
isRight (Left _) = False

isValidPercentageInput :: Double -> Bool
isValidPercentageInput rawValue =
  not (isNaN rawValue)
    && not (isInfinite rawValue)
    && rawValue >= 0
    && rawValue <= 1