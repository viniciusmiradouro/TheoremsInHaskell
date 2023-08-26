{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Naturals () where

import           Data.Kind               (Type)
import           Data.Singletons.Base.TH
import           Prelude                 (Eq, undefined)

$(singletons [d|
  data Nat :: Type where
    Zero :: Nat
    Succ :: Nat -> Nat
    deriving (Eq)

  (+) :: Nat -> Nat -> Nat
  Zero     + n = n
  (Succ n) + m = Succ (n + m)
  |])

additionAssoc :: SNat a -> SNat b -> SNat c -> ((a + b) + c) :~: (a + (b + c))
additionAssoc SZero     _ _ = Refl
additionAssoc (SSucc a) b c = case additionAssoc a b c of
  Refl -> Refl

additionCommutative :: SNat a -> SNat b -> (a + b) :~: (b + a)
additionCommutative = undefined
