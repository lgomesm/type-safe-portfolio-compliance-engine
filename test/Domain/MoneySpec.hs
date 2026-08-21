module Domain.MoneySpec (spec) where

import Test.Hspec

import Domain.Error (DomainError (..))
import Domain.Money (mkPositiveMoney, unPositiveMoney)

spec :: Spec
spec = describe "Domain.Money" $ do
  describe "mkPositiveMoney" $ do
    it "accepts a positive value" $
      fmap unPositiveMoney (mkPositiveMoney 100) `shouldBe` Right 100

    it "rejects zero" $
      mkPositiveMoney 0 `shouldBe` Left (NonPositiveMoney 0)

    it "rejects negative values" $
      mkPositiveMoney (-50) `shouldBe` Left (NonPositiveMoney (-50))

    it "rejects NaN as a non-finite value" $
      expectNonFiniteMoney (mkPositiveMoney (0 / 0)) isNaN

    it "rejects positive infinity as a non-finite value" $
      expectNonFiniteMoney (mkPositiveMoney (1 / 0)) isInfinite

    it "rejects negative infinity as a non-finite value" $
      expectNonFiniteMoney (mkPositiveMoney (-1 / 0)) isInfinite
  where
    expectNonFiniteMoney result predicate =
      case result of
        Left (NonFiniteMoney value) -> value `shouldSatisfy` predicate
        other -> expectationFailure ("expected NonFiniteMoney, got " <> show other)