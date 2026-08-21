module Domain.PercentageSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Domain.Error (DomainError (..))
import Domain.Percentage
  ( clampPercentage
  , mkPercentage
  , percentageExceeds
  , unPercentage
  )

spec :: Spec
spec = describe "Domain.Percentage" $ do
  describe "mkPercentage" $ do
    it "accepts zero" $
      mkPercentage 0 `shouldSatisfy` isRight

    it "accepts one" $
      mkPercentage 1 `shouldSatisfy` isRight

    it "accepts an in-range intermediate value" $
      fmap unPercentage (mkPercentage 0.3) `shouldBe` Right 0.3

    it "rejects negative values" $
      mkPercentage (-0.1) `shouldBe` Left (PercentageOutOfRange (-0.1))

    it "rejects values greater than one" $
      mkPercentage 1.5 `shouldBe` Left (PercentageOutOfRange 1.5)

    it "rejects NaN as a non-finite percentage" $
      expectNonFinitePercentage (mkPercentage (0 / 0)) isNaN

    it "rejects positive infinity as a non-finite percentage" $
      expectNonFinitePercentage (mkPercentage (1 / 0)) isInfinite

    it "rejects negative infinity as a non-finite percentage" $
      expectNonFinitePercentage (mkPercentage (-1 / 0)) isInfinite

    it "only constructs successful values inside the closed interval" $
      property $ \x ->
        case mkPercentage x of
          Left _ -> True
          Right percentage ->
            let value = unPercentage percentage
             in not (isNaN value)
                  && not (isInfinite value)
                  && value >= 0
                  && value <= 1

  describe "clampPercentage" $ do
    it "clamps values below zero to zero" $
      fmap unPercentage (clampPercentage (-0.3)) `shouldBe` Right 0

    it "clamps values above one to one" $
      fmap unPercentage (clampPercentage 1.7) `shouldBe` Right 1

    it "preserves values already inside the valid interval" $
      fmap unPercentage (clampPercentage 0.42) `shouldBe` Right 0.42

    it "rejects NaN instead of assigning it a financial meaning" $
      expectNonFinitePercentage (clampPercentage (0 / 0)) isNaN

    it "rejects positive infinity instead of clamping it to one" $
      expectNonFinitePercentage (clampPercentage (1 / 0)) isInfinite

    it "rejects negative infinity instead of clamping it to zero" $
      expectNonFinitePercentage (clampPercentage (-1 / 0)) isInfinite

  describe "percentageExceeds" $ do
    it "does not treat Double rounding noise at an exact boundary as an excess" $ do
      case (mkPercentage (0.30 + 0.25 + 0.10 + 0.05), mkPercentage 0.70) of
        (Right observed, Right allowed) ->
          percentageExceeds observed allowed `shouldBe` False
        other -> expectationFailure ("expected valid percentages, got " <> show other)

    it "still detects a value that exceeds the boundary beyond the tolerance" $ do
      case (mkPercentage 0.70000001, mkPercentage 0.70) of
        (Right observed, Right allowed) ->
          percentageExceeds observed allowed `shouldBe` True
        other -> expectationFailure ("expected valid percentages, got " <> show other)
  where
    isRight (Right _) = True
    isRight (Left _) = False

    expectNonFinitePercentage result predicate =
      case result of
        Left (NonFinitePercentage value) -> value `shouldSatisfy` predicate
        other -> expectationFailure ("expected NonFinitePercentage, got " <> show other)