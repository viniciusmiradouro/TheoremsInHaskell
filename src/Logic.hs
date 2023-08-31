{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Logic
  (
  ) where

import Data.Type.Equality
import Data.Void
import GHC.Stack          ( HasCallStack )
import Prelude            ( Either (..), fst, snd, undefined, ($), (.) )

--------------------------------
------ Rules of Inference ------
--------------------------------

axiom :: forall a. HasCallStack => a
axiom = undefined

iffIntro :: (a -> b, b -> a) -> a :~: b
iffIntro = axiom -- Can't seem to construct it otherwise

fromIff :: a :~: b -> (a -> b, b -> a)
fromIff = axiom -- Can't seem to construct it otherwise

iffElimL :: a :~: b -> (a -> b)
iffElimL = fst . fromIff

iffElimR :: a :~: b -> (b -> a)
iffElimR = snd . fromIff

iffConcretization :: a :~: b -> Either a b -> (a, b)
iffConcretization ab (Left  a) = (a, iffElimL ab a)
iffConcretization ab (Right b) = (iffElimR ab b, b)

orIntroL :: a -> Either a b
orIntroL = Left

orIntroR :: b -> Either a b
orIntroR = Right

andIntro :: a -> b -> (a, b)
andIntro a b = (a, b)

modusPonens :: (a -> b) -> a -> b
modusPonens ab = ab

modusTollens :: (a -> b) -> (b -> Void) -> (a -> Void)
modusTollens ab nb = nb . ab

hypotheticalSyllogism :: (a -> b) -> (b -> c) -> (a -> c)
hypotheticalSyllogism ab bc = bc . ab

proofByCases :: (a -> c) -> (b -> c) -> Either a b -> c
proofByCases ac __ (Left  a) = ac a
proofByCases __ bc (Right b) = bc b

constructiveDillema :: (a -> b) -> (c -> d) -> Either a c -> Either b d
constructiveDillema ab __ (Left  a) = Left  (ab a)
constructiveDillema __ cd (Right c) = Right (cd c)

destructiveDillema :: (a -> b) -> (c -> d) -> Either (b -> Void) (d -> Void) -> Either (a -> Void) (c -> Void)
destructiveDillema ab __ (Left  nb) = Left  $ modusTollens ab nb
destructiveDillema __ cd (Right nd) = Right $ modusTollens cd nd

------------------------------------
------ Conjunction Properties ------
------------------------------------

andCommutative1 :: (a, b) -> (b, a)
andCommutative1 (a, b) = (b, a)

andCommutative2 :: (b, a) -> (a, b)
andCommutative2 (b, a) = (a, b)

andCommutative :: (a, b) :~: (b, a)
andCommutative = iffIntro $ andIntro andCommutative2 andCommutative1

andAssociative1 :: (a, (b,  c)) -> ((a,  b),  c)
andAssociative1 (a, (b, c)) = ((a, b), c)

andAssociative2 :: ((a,  b),  c) -> (a, (b,  c))
andAssociative2 ((a, b), c) = (a, (b, c))

andAssociative :: (a, (b,  c)) :~: ((a,  b),  c)
andAssociative = iffIntro $ andIntro andAssociative1 andAssociative2

diagonal :: a -> (a, a)
diagonal a = (a, a)

simplificationL :: (a, b) -> a
simplificationL (a, _) = a

simplificationR :: (a, b) -> b
simplificationR (_, b) = b

------------------------------------
------ Disjunction Properties ------
------------------------------------

orCommutative1 :: Either a b -> Either b a
orCommutative1 (Left  a) = Right a
orCommutative1 (Right b) = Left  b

orCommutative2 :: Either b a -> Either a b
orCommutative2 (Left  b) = Right b
orCommutative2 (Right a) = Left  a

orCommutative :: Either a b :~: Either b a
orCommutative = iffIntro $ andIntro orCommutative1 orCommutative2

orAssociative1 :: Either a (Either b c) -> Either (Either a b) c
orAssociative1 (Right (Right c)) = Right c
orAssociative1 (Right (Left  b)) = Left  (Right b)
orAssociative1 (Left  a)         = Left  (Left a)

orAssociative2 :: Either (Either a b) c -> Either a (Either b c)
orAssociative2 (Left  (Right b)) = Right (Left b)
orAssociative2 (Left  (Left  a)) = Left  a
orAssociative2 (Right c)         = Right (Right c)

orAssociative :: Either a (Either b c) :~: Either (Either a b) c
orAssociative = iffIntro $ andIntro orAssociative1 orAssociative2

---------------------------
------ DeMorgan Laws ------
---------------------------

deMorgan1 :: Either a b -> ((a -> Void, b -> Void) -> Void)
deMorgan1 = undefined -- Couldn't find a way to construct this. Maybe it depends on excluded middle?

deMorgan2 :: (a, b) -> (Either (a -> Void) (b -> Void) -> Void)
deMorgan2 = undefined -- Couldn't find a way to construct this. Maybe it depends on excluded middle?

deMorgan3 :: (Either (a -> Void) (b -> Void) -> Void) -> (a, b)
deMorgan3 = undefined -- Couldn't find a way to construct this. Maybe it depends on excluded middle?

deMorgan4 :: ((a -> Void, b -> Void) -> Void) -> Either a b
deMorgan4 = undefined -- Couldn't find a way to construct this. Maybe it depends on excluded middle?

-------------------------------------------------------
------ Properties of Disjunction and Conjunction ------
-------------------------------------------------------

andDistributesOverOr1 :: (a, Either b c) -> Either (a, b) (a, c)
andDistributesOverOr1 (a, Left  b) = Left  (a, b)
andDistributesOverOr1 (a, Right c) = Right (a, c)

andDistributesOverOr2 :: Either (a, b) (a, c) -> (a, Either b c)
andDistributesOverOr2 (Left  (a, b)) = (a, Left  b)
andDistributesOverOr2 (Right (a, c)) = (a, Right c)

andDistributesOverOr :: (a, Either b c) :~: Either (a, b) (a, c)
andDistributesOverOr = iffIntro $ andIntro andDistributesOverOr1 andDistributesOverOr2

orDistributesOverAnd1 :: Either a (b, c) -> (Either a b, Either a c)
orDistributesOverAnd1 (Right (b, c)) = (Right b,  Right c)
orDistributesOverAnd1 (Left a)       = (Left  a,  Left  a)

orDistributesOverAnd2 :: (Either a b, Either a c) -> Either a (b, c)
orDistributesOverAnd2 (Right b, Right c) = Right (b, c)
orDistributesOverAnd2 (Left  a, _______) = Left a
orDistributesOverAnd2 (_______, Left  a) = Left a

orDistributesOverAnd :: Either a (b, c) :~: (Either a b, Either a c)
orDistributesOverAnd = iffIntro $ andIntro orDistributesOverAnd1 orDistributesOverAnd2
