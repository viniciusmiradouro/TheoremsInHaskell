{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Naturals () where

import           Data.Singletons.Base.Enum (PEnum (Succ))
import           Data.Singletons.Base.TH
import           GHC.TypeLits.Singletons
import           Prelude.Singletons

theorem_1 :: SNat a -> SNat b -> Succ a + Succ b :~: Succ (Succ (a + b))
theorem_1  = undefined
