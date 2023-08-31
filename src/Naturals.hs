{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Naturals
  (
  ) where

import Algebra                 ( Magma (..), Monoid (..), Semigroup (..) )
import Data.Kind               ( Type )
import Data.Singletons.Base.TH
import GHC.Base                ( undefined )
import Prelude                 ( Eq )

$(singletons [d|
  data Nat :: Type where
    Zero :: Nat
    Succ :: Nat -> Nat
    deriving (Eq)

  (+) :: Nat -> Nat -> Nat
  Zero     + n = n
  (Succ n) + m = Succ (n + m)

  (*) :: Nat -> Nat -> Nat
  Zero     * _ = Zero
  (Succ n) * m = n * m + m

  antecessor :: Nat -> Nat
  antecessor Zero     = Zero
  antecessor (Succ n) = n
  |])

addSucc :: SNat a -> SNat b -> a + 'Succ b :~: 'Succ (a + b)
addSucc SZero _ = Refl
addSucc (SSucc m) n = case addSucc m n of
  Refl -> Refl

addCommutative :: SNat a -> SNat b -> (a + b) :~: (b + a)
addCommutative = undefined

----------------------------------------------------------
---------- Algebraic Structures in the naturals ----------
----------------------------------------------------------

---------- Magma on Addition ----------

instance Magma Nat where
  type a <> b = a + b
  (%<>)       = (%+)

---------- Semigroup on Addition ----------

additionAssoc :: SNat a -> SNat b -> SNat c -> (a + (b + c)) :~: ((a + b) + c)
additionAssoc SZero     _ _ = Refl
additionAssoc (SSucc a) b c = case additionAssoc a b c of
  Refl -> Refl

instance Magma Nat => Semigroup Nat where
  assoc = additionAssoc

---------- Monoid on Addition ----------

neutralElemAdditionL :: SNat a -> 'Zero + a :~: a
neutralElemAdditionL _ = Refl

neutralElemAdditionR :: SNat a -> a + 'Zero :~: a
neutralElemAdditionR SZero     = Refl
neutralElemAdditionR (SSucc n) = case neutralElemAdditionR n of
  Refl -> Refl

instance Semigroup Nat => Monoid Nat where
  type Neutral Nat = 'Zero
  leftNeutral  = neutralElemAdditionL
  rightNeutral = neutralElemAdditionR
