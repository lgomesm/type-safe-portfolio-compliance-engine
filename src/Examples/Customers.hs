{-# LANGUAGE OverloadedStrings #-}

-- Alguns clientes de exemplo usados pelos cenarios da cli
module Examples.Customers (aggressiveHighScore, conservativeHighScore, conservativeLowScore, moderateMediumScore) where

import Data.Text (Text)

import Domain.CreditScore (CreditScore (..))
import Domain.Customer (Customer (..))
import Domain.CustomerId (CustomerId, mkCustomerId)
import Domain.CustomerProfile (CustomerProfile (..))

aggressiveHighScore :: Customer
aggressiveHighScore =
  Customer
    { customerId = unsafeCustomerId "customer-aggressive-high"
    , customerProfile = Aggressive
    , creditScore = HighScore
    }

conservativeHighScore :: Customer
conservativeHighScore =
  Customer
    { customerId = unsafeCustomerId "customer-conservative-high"
    , customerProfile = Conservative
    , creditScore = HighScore
    }

conservativeLowScore :: Customer
conservativeLowScore =
  Customer
    { customerId = unsafeCustomerId "customer-conservative-low"
    , customerProfile = Conservative
    , creditScore = LowScore
    }

moderateMediumScore :: Customer
moderateMediumScore =
  Customer
    { customerId = unsafeCustomerId "customer-moderate-medium"
    , customerProfile = Moderate
    , creditScore = MediumScore
    }

unsafeCustomerId :: Text -> CustomerId
unsafeCustomerId rawCustomerId =
  case mkCustomerId rawCustomerId of
    Right customerIdValue -> customerIdValue
    Left err -> error ("cenario de exemplo com customer id invalido: " <> show err)