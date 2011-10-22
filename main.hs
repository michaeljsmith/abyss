{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators, StandaloneDeriving, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Applicative
import Data.Typeable
import Data.Dynamic

data a :-> b where
  (:->) :: a -> b -> (a :-> b)

data Inst a where
  Inst :: a -> Inst a
  Pair :: Inst a -> Inst a -> Inst a
  (:*) :: (Inst a :-> Inst b) -> Inst a -> Inst b

infixr 1 :->

arg :: a :-> b -> a
arg (a :-> b) = a

subst :: (Inst x :-> Inst a :-> Inst b) -> (Inst x :-> Inst a) -> (Inst x :-> Inst b)
subst f g = Pair (arg f) (arg g) :-> ((f :* x1) :* (g :* x0))
  where
    x0 = Pair x (arg f)
    x1 = Pair x (arg g)

main = print "hello"

