{-# LANGUAGE TypeFamilies #-}

-- Cada regra declara sua propria representacao pra manter só os dados de config que ela realmente precisa
module Risk.Rules.Policy (RulePolicy) where

data family RulePolicy rule