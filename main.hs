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

type Obj a = a -> a

compose :: (Obj b -> Obj c) -> (Obj a -> Obj b) -> (Obj a -> Obj c)
compose f g = \x -> f (g x)

cnst' :: Obj a -> (Obj x -> Obj a)
cnst' x = \y -> x

sbst' :: (Obj x -> Obj a -> Obj b) -> (Obj x -> Obj a) -> (Obj x -> Obj b)
sbst' f g x = \b ->
  f xf (g xg) b
    where
      xf y = x y

main = print "hello"
