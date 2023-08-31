{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType      #-}

module Naturals () where

import           Algebra                 (Magma (..), Semigroup (..))
import           Data.Kind               (Type)
import           Data.Singletons.Base.TH
import           Prelude                 (Eq)

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

----------------------------------------------------------
---------- Algebraic Structures in the naturals ----------
----------------------------------------------------------

---------- Magma on Addition ----------

additionAssoc :: SNat a -> SNat b -> SNat c -> (a + (b + c)) :~: ((a + b) + c)
additionAssoc SZero     _ _ = Refl
additionAssoc (SSucc a) b c = case additionAssoc a b c of
  Refl -> Refl

instance Magma Nat where
  type a <> b = a + b
  (%<>) = (%+)

---------- Semigroup on Addition ----------

instance Magma Nat => Semigroup Nat where
  assoc = additionAssoc

