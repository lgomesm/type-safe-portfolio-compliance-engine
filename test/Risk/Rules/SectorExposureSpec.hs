module Risk.Rules.SectorExposureSpec (spec) where

import Domain.AssetClassification (Sector (Financial))
import Domain.Percentage (mkPercentage)
import Domain.Portfolio (mkPortfolio)
import Risk.Policy (Policy (PrivateBankingPolicy), policyConfigFor)
import Risk.Rules.SectorExposure
  ( maxSectorExposure
  , validateMaxSectorExposure
  , validateMaxSectorExposureWithPolicy
  )
import Risk.Violation (Violation (..))
import Test.Hspec
import Test.PortfolioFixtures (positionsOfWithSector)

spec :: Spec
spec = describe "Risk.Rules.SectorExposure" $ do
  it "maxSectorExposure is a valid percentage literal" $
    mkPercentage 0.50 `shouldBe` Right maxSectorExposure

  it "reports a violation when a sector exceeds 50%" $ do
    rawPortfolio <-
      positionsOfWithSector
        [ ("ITUB4", 0.30, "Financial")
        , ("BBAS3", 0.30, "Financial")
        , ("PETR4", 0.40, "Energy")
        ]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxSectorExposure portfolioValue `shouldSatisfy` any isFinancialViolation
      Left err ->
        expectationFailure (show err)

  it "does not report a violation when every sector is within the limit" $ do
    rawPortfolio <-
      positionsOfWithSector
        [ ("ITUB4", 0.25, "Financial")
        , ("PETR4", 0.35, "Energy")
        , ("VALE3", 0.40, "Other")
        ]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxSectorExposure portfolioValue `shouldBe` []
      Left err ->
        expectationFailure (show err)

  it "accepts 55% sector exposure under PrivateBankingPolicy" $ do
    rawPortfolio <-
      positionsOfWithSector
        [ ("ITUB4", 0.30, "Financial")
        , ("BBAS3", 0.25, "Financial")
        , ("PETR4", 0.45, "Energy")
        ]
    case mkPortfolio rawPortfolio of
      Right portfolioValue ->
        validateMaxSectorExposureWithPolicy (policyConfigFor PrivateBankingPolicy) portfolioValue `shouldBe` []
      Left err ->
        expectationFailure (show err)

isFinancialViolation :: Violation -> Bool
isFinancialViolation (SectorExposureExceeded Financial _ _) = True
isFinancialViolation _ = False