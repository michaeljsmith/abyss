{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators, StandaloneDeriving, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Applicative
import Data.Typeable
import Data.Dynamic

data Inst a where
  Inst :: a -> Inst a
  Pair :: Inst a -> Inst a -> Inst a

subst :: Inst (x -> a -> b) -> Inst (x -> a) -> Inst (x -> b)
subst f g x = f x0 (g x1) where
  x0 = Pair x (arg g)
  x1 = Pair x (arg f)

main = print "hello"

