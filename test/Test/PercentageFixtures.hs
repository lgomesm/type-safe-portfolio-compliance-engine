module Test.PercentageFixtures
  ( percentageLiteral
  ) where

import Domain.Percentage (Percentage, mkPercentage)

percentageLiteral :: Double -> Percentage
percentageLiteral value =
  case mkPercentage value of
    Right percentage -> percentage
    Left err -> error ("invalid test percentage literal: " <> show err)
