module Domain.PortfolioPropSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Domain.Percentage (unPercentage)
import Domain.Portfolio (mkPortfolio, portfolioPositions, weightSumTolerance)
import Domain.Position (weight)
import Generators.Portfolio (genRawPortfolioWithTotal, genValidPortfolioRaw)

spec :: Spec
spec = describe "Domain.Portfolio (properties)" $ do
  it "every successfully built Portfolio sums approximately to 1.0" $
    forAll (choose (1, 30)) $ \positionCount ->
      forAll (genValidPortfolioRaw positionCount) $ \rawPortfolio ->
        case mkPortfolio rawPortfolio of
          Right portfolioValue ->
            let totalWeight = sum (map (unPercentage . weight) (portfolioPositions portfolioValue))
             in counterexample ("total weight = " <> show totalWeight) $
                  abs (totalWeight - 1) <= weightSumTolerance
          Left err ->
            counterexample ("mkPortfolio failed unexpectedly: " <> show err) False

  it "every total well outside the tolerance band is rejected" $
    forAll
      (oneof [choose (0.0, 1.0 - weightSumTolerance * 100), choose (1.0 + weightSumTolerance * 100, 3.0)])
      $ \targetTotal ->
        forAll (genRawPortfolioWithTotal targetTotal) $ \rawPortfolio ->
          case mkPortfolio rawPortfolio of
            Left _ -> True
            Right _ -> False