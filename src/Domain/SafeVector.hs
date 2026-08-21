{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Domain.SafeVector (Nat (..), Vec (VNil, (:>)), Fin (FZ, FS), SNat (SZ, SS), SomeSNat (..), N1, N2, N3, sN0, sN1, 
  sN2, sN3, indexVec, vecToList, snatToInt, takeVec, someSNatFromInt) where

-- Representando os naturais no nivel de tipos pq eles sao usados pra expressar o tamanho de Vec e os limites de Fin
data Nat
  = Z
  | S Nat

-- O tamanho faz parte do tipo pra que ele continue conhecido dps que o vetor foi construido
data Vec (n :: Nat) a where
  VNil :: Vec 'Z a
  (:>) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :>

instance Eq a => Eq (Vec n a) where
  VNil == VNil = True
  (x :> xs) == (y :> ys) = x == y && xs == ys

instance Show a => Show (Vec n a) where
  showsPrec _ VNil = showString "VNil"
  showsPrec precedence (x :> xs) =
    showParen (precedence > 5) $
      showsPrec 6 x . showString " :> " . showsPrec 5 xs

type N1 = 'S 'Z

type N2 = 'S N1

type N3 = 'S N2

-- Preciso de uma representacao em runtime quando o tamanho conhecido pelo tipo tb precisa orientar a construcao
data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- O existencial permite que eu preserve um tamanho descoberto em runtime mesmo quando esse tamanho nao e conhecido pela 
-- assinatura do chamador propriamente
data SomeSNat = forall n. SomeSNat (SNat n)

sN0 :: SNat 'Z
sN0 = SZ

sN1 :: SNat N1
sN1 = SS sN0

sN2 :: SNat N2
sN2 = SS sN1

sN3 :: SNat N3
sN3 = SS sN2

-- O indice tb carrega o tamanho no tipo pra que só posicoes validas possam ser representadas
data Fin (n :: Nat) where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

-- Nao ha caso de falha pq Fin n so pode representar indices validos pra um Vec n a
indexVec :: Fin n -> Vec n a -> a
indexVec FZ (x :> _) = x
indexVec (FS index) (_ :> xs) = indexVec index xs

-- Adicionando a conversao pra interoperar com codigo que nao precisa preservar a garantia de tamanho no tipo
vecToList :: Vec n a -> [a]
vecToList VNil = []
vecToList (x :> xs) = x : vecToList xs

-- Convertendo a "testemunha" pra Int quando o tamanho tb é necessario em uma operacao de runtime
snatToInt :: SNat n -> Int
snatToInt SZ = 0
snatToInt (SS size) = 1 + snatToInt size

-- Preciso usar o Maybe pq uma lista comum pode nao ter elementos suficientes pro tamanho solicitado
takeVec :: SNat n -> [a] -> Maybe (Vec n a)
takeVec SZ _ = Just VNil
takeVec (SS _) [] = Nothing
takeVec (SS size) (x : xs) = (x :>) <$> takeVec size xs

-- O resultado é existencial pq o tamanho so é conhecido dps que o valor recebido em runtime é interpretado
someSNatFromInt :: Int -> Maybe SomeSNat
someSNatFromInt value
  | value < 0 = Nothing
someSNatFromInt 0 = Just (SomeSNat SZ)
someSNatFromInt value =
  case someSNatFromInt (value - 1) of
    Just (SomeSNat previous) -> Just (SomeSNat (SS previous))
    Nothing -> Nothing