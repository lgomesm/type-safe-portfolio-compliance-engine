-- Expondo o construtor diretamente pq cada campo ja carrega suas proprias garantias de validade. Customer só reune esses 
-- valores e nao adiciona uma nova invariante que precise ser validada
module Domain.Customer (Customer (..)) where

import Domain.CreditScore (CreditScore)
import Domain.CustomerId (CustomerId)
import Domain.CustomerProfile (CustomerProfile)

data Customer = Customer
  { customerId :: CustomerId
  , customerProfile :: CustomerProfile
  , creditScore :: CreditScore
  }
  deriving (Eq, Show)