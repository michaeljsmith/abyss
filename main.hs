{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators, StandaloneDeriving, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Applicative
import Data.Typeable
import Data.Dynamic

data Inst a where
  (:->) :: Inst a -> Inst b -> Inst (a -> b)
  S :: Inst (x -> a -> b) -> Inst (x -> a) -> Inst (x -> b)
  K :: Inst a -> Inst (x -> a)
  I :: Inst (a -> a)

  Inst :: a -> Inst a
  Pair :: Inst a -> Inst a -> Inst a

(|*|) :: Inst (a -> b) -> Inst a -> Inst b

infixr 1 :->

main = print "hello"
