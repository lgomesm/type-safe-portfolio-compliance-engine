{-# LANGUAGE TypeFamilies #-}

module Risk.Rules.Input (PolicyPortfolioInput (..), SuitabilityInput (..)) where

import Domain.Customer (Customer)
import Domain.Portfolio (Portfolio)
import Risk.Policy (PolicyConfig)
import Risk.Rules.Policy (RulePolicy)

data PolicyPortfolioInput rule = PolicyPortfolioInput
  { inputRulePolicy :: RulePolicy rule
  , inputPortfolio :: Portfolio
  }

data SuitabilityInput = SuitabilityInput
  { suitabilityPolicy :: PolicyConfig
  , suitabilityCustomer :: Customer
  , suitabilityPortfolio :: Portfolio
  }