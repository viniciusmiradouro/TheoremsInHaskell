{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Algebra () where

import           Data.Type.Equality ((:~:))
import           Prelude.Singletons (Sing)

------------------------------------------------
------ Structures with 1 binary operation ------
------------------------------------------------

class Magma a where
  type (x :: a) <> (y :: a) :: a
  (%<>) :: Sing (x :: a) -> Sing (y :: a) -> Sing (z :: a)

class Magma a => Semigroup a where
  assoc :: Sing (x :: a) -> Sing (y :: a) -> Sing (z :: a) -> (x <> (y <> z)) :~: ((x <> y) <> z)

--------------------------------------------------------------
------ Structures with 1 binary operation and 1 element ------
--------------------------------------------------------------

class Semigroup a => Monoid a where
  type Neutral a :: a
  leftNeutral :: Sing (x :: a) -> (Neutral a <> x) :~: x
  rightNeutral :: Sing (x :: a) -> x :~: (Neutral a <> x)

---------------------------------------------------------------------------------
------ Structures with 1 binary operation, 1 unary operation and 1 element ------
---------------------------------------------------------------------------------

class Monoid a => Group a where
  type Inverse (x :: a) :: a
  inverse :: Sing (x :: a) -> Sing (Inverse x)
  leftCancelation :: Sing (x :: a) -> (Inverse x <> x) :~: Neutral a
  rightCancelation :: Sing (x :: a) -> (x <> Inverse x) :~: Neutral a
