module Risk.ReportPropSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Generators.Domain (genCustomer)
import Generators.Portfolio (genAnyValidPortfolio)
import Risk.Engine (validateCompliance)
import Risk.Report (PortfolioStatus (..), buildReport, status)
import Risk.SuitabilityPolicy (defaultSuitabilityPolicy)

spec :: Spec
spec = describe "Risk.Report (properties)" $
  it "status is Approved if and only if there are no violations" $
    forAll genCustomer $ \customerValue ->
      forAll genAnyValidPortfolio $ \portfolioValue ->
        let violationsFound = validateCompliance defaultSuitabilityPolicy customerValue portfolioValue
            report = buildReport defaultSuitabilityPolicy customerValue portfolioValue
         in classify (null violationsFound) "without violations" $
              classify (not (null violationsFound)) "with violations" $
                (status report == Approved) === null violationsFound