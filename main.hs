{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators, StandaloneDeriving, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Applicative
import Data.Monoid
import Data.Typeable
import Data.Dynamic

data a :-> b where
  Relation :: a -> (a -> b) -> (a :-> b)

infixr 1 :->

refs :: (a :-> b) -> a
refs (Relation a b) = a

gen :: (a :-> b) -> a -> b
gen (Relation a b) = b

(|*|) :: (a :-> b) -> a -> b
f |*| x = gen f x

sbst :: Monoid x => (x :-> a :-> b) -> (x :-> a) -> (x :-> b)
sbst f g = Relation newRefs genOutput
  where
    newRefs = refs f `mappend` refs g
    genOutput xs = subf xs |*| subg xs
    subf xs = (gen f) (refs g `mappend` xs)
    subg xs = (gen g) (refs f `mappend` xs)

cnst :: Monoid x => a -> (x :-> a)
cnst a = Relation mempty (\x -> a)

data a :~> b where
  Relation' :: (a, b -> a, b) -> (a :~> b)

infixr 1 :~>

bind (Relation' f) = f

type Obj a = (a -> a)

(|%|) :: (a :~> b) -> a -> Obj b
f |%| x = 

infixl 8 |%|

main = print "hello"
