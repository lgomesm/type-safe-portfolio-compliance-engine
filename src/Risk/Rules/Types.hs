{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- Definindo o contrato comum das regras de compliance
module Risk.Rules.Types (RuleEvidence, RuleEvaluation (..), ComplianceRule (..), violationsOf, evaluateViolations) where

import Data.Kind (Type)

import Risk.Violation (Violation)

type family RuleEvidence (rule :: Type) :: Type

data RuleEvaluation rule = RuleEvaluation
  { evaluationEvidence :: RuleEvidence rule
  , evaluationViolations :: [Violation]
  }

violationsOf :: RuleEvaluation rule -> [Violation]
violationsOf = evaluationViolations

-- Cada regra define sua propria entrada pq nem tds precisam do mesmo contexto pra serem avaliadas
class ComplianceRule rule where
  type RuleInput rule :: Type

  evaluateRule :: rule -> RuleInput rule -> RuleEvaluation rule

evaluateViolations :: ComplianceRule rule => rule -> RuleInput rule -> [Violation]
evaluateViolations rule input = violationsOf (evaluateRule rule input)