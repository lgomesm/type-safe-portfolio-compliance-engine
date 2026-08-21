-- Calcula exposicoes agregadas por classe de ativo
module Risk.Exposure (exposureByAssetClass, exposureByAssetClasses) where

import Domain.Asset (Asset (assetClass))
import Domain.AssetClassification (AssetClass)
import Domain.Percentage (Percentage, unPercentage)
import Domain.Percentage.Internal (clampCalculatedPercentage)
import Domain.Portfolio (Portfolio, portfolioPositions)
import Domain.Position (Position (asset, weight))

exposureByAssetClasses :: [AssetClass] -> Portfolio -> Percentage
exposureByAssetClasses acceptedClasses portfolioValue =
  clampCalculatedPercentage $
    sum
      [ unPercentage (weight positionValue)
      | positionValue <- portfolioPositions portfolioValue
      , assetClass (asset positionValue) `elem` acceptedClasses
      ]

exposureByAssetClass :: AssetClass -> Portfolio -> Percentage
exposureByAssetClass assetClassValue =
  exposureByAssetClasses [assetClassValue]