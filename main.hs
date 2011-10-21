{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators, StandaloneDeriving, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Applicative
import Data.Typeable
import Data.Dynamic

class Compound c where
  arg :: c -> Type a

data Type a where
  Prim :: a -> Type b
  Sbst :: Type (x -> a -> b) -> Type (x -> a) -> Type (x -> b)
  Cnst :: Type a -> Type (x -> a)
  Appl :: Type (x -> a) -> Type x -> Type a
  Pair :: Type a -> Type a -> Type a
  Null :: Type a

typeArg :: Type (x -> a) -> Type x
typeArg (Prim x) = Prim (arg x)
typeArg (Sbst f g) = Pair (typeArg f) (typeArg g)
typeArg (Cnst x) = Null
typeArg (Appl f x) = x

main = print "hello"

