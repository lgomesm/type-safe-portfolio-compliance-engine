module Domain.PositiveMoneyPropSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

import Domain.Error (DomainError (..))
import Domain.Money (mkPositiveMoney, unPositiveMoney)
import Generators.Domain (genValidPositiveMoney, shrinkValidPositiveMoney)

spec :: Spec
spec = describe "Domain.PositiveMoney (properties)" $ modifyMaxSuccess (const 200) $ do
  it "every generated PositiveMoney is strictly greater than zero" $
    forAllShrink genValidPositiveMoney shrinkValidPositiveMoney $ \moneyValue ->
      unPositiveMoney moneyValue > 0

  it "mkPositiveMoney is identity on the raw value of valid money" $
    forAllShrink genValidPositiveMoney shrinkValidPositiveMoney $ \moneyValue ->
      case mkPositiveMoney (unPositiveMoney moneyValue) of
        Right rebuilt -> unPositiveMoney rebuilt === unPositiveMoney moneyValue
        Left err -> counterexample (show err) False

  it "non-positive values are rejected with NonPositiveMoney" $
    forAll (oneof [pure 0.0, choose (-1.0e9, -1.0e-9)]) $ \rawValue ->
      mkPositiveMoney rawValue === Left (NonPositiveMoney rawValue)

  it "non-finite values are rejected with NonFiniteMoney" $
    forAll (elements [0 / 0, 1 / 0, -1 / 0]) $ \rawValue ->
      case mkPositiveMoney rawValue of
        Left (NonFiniteMoney value) ->
          counterexample (show value) (isNaN value || isInfinite value)
        other -> counterexample (show other) False