{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators, StandaloneDeriving, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Applicative
import Data.Typeable
import Data.Dynamic

data a :-> b where
  (:->) :: [a] -> ([a] -> b) -> (a :-> b)

infixr 1 :->

refs :: (a :-> b) -> [a]
refs (a :-> b) = a

gen :: (a :-> b) -> [a] -> b
gen (a :-> b) = b

(|*|) :: (a :-> b) -> a -> b
f |*| x = gen f [x]

sbst :: (x :-> a :-> b) -> (x :-> a) -> (x :-> b)
sbst f g = newRefs :-> genOutput
  where
    newRefs = refs f ++ refs g
    genOutput = \xs -> subf xs |*| subg xs
    subf xs = (gen f) (refs g ++ xs)
    subg xs = (gen g) (refs f ++ xs)

cnst :: a -> (x :-> a)
cnst a = [] :-> (\x -> a)

main = print "hello"
