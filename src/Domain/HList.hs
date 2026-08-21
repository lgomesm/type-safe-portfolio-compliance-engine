{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- A lista é generica pq sua responsabilidade e só manter essa informacao de tipos, sem preicsar depender das regras 
-- especificas do dominio que utiliza
module Domain.HList (HList (HNil, (:&))) where

import Data.Kind (Type)

data HList (xs :: [Type]) where
  HNil :: HList '[]
  (:&) :: x -> HList xs -> HList (x ': xs)

infixr 5 :&