{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators, StandaloneDeriving, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Applicative
import Data.Typeable
import Data.Dynamic

data a :-> b where
  (:->) :: a -> b -> (a :-> b)
  S :: (x :-> a :-> b) -> (x :-> a) -> (x :-> b)
  K :: a -> (x :-> a)
  I :: (a :-> a)

infixr 1 :->

data Inst a where
  Inst :: a -> Inst a
  Pair :: Inst a -> Inst a -> Inst a
  (:*) :: (Inst a :-> Inst b) -> Inst a -> Inst b

(|*|) :: (Applicative f) => f (a -> b) -> Inst (f a) -> f b
f |*| Inst x = f <*> x
f |*| Pair x y = (f |*| x) *> (f |*| y)
--f |*| ((a :-> b) :* x) = 
--S :: (x :-> a :-> b) -> (x :-> a) -> (x :-> b)
--K :: a -> (x :-> a)
--I :: (a :-> a)

main = print "hello"
