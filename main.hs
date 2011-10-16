{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators, StandaloneDeriving, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Applicative
import Data.Typeable
import Data.Dynamic

checked_cast :: (Typeable a, Typeable b) => a -> b
checked_cast = (flip fromDyn undefined) . toDyn

data Expr a where
  Cnst :: a -> Expr a
  Var :: Integer -> Expr a
  (:*) :: (Typeable a, Typeable f) => Expr (a -> f) -> Expr a -> Expr f
  S :: (Typeable x, Typeable a, Typeable g) => Expr ((x -> a -> g) -> (x -> a) -> x -> g)
  K :: (Typeable a, Typeable h) => Expr (a -> h -> a)
  Lmb :: (Typeable a, Typeable b) => Integer -> Expr a -> Expr (b -> a)
  deriving Typeable

instance Show (Expr a) where
  show (Cnst a) = "<const>"
  show (Var n) = "Var " ++ show n
  show (f :* g) = show f ++ " " ++ show g
  show S = "S"
  show K = "K"
  show (Lmb n x) = "\\" ++ show n ++ " " ++ show x

--data Term a where
--  Term :: a -> Term a
--  S :: Term ((x -> a -> b) -> (x -> a) -> x -> b)
--  K :: Term (a -> b -> a)
--  (:*) :: Term (a -> b) -> Term a -> Term b
--  deriving Typeable

--instance Show (Term a) where
--  show (Term a) = "<const>"
--  show S = "S"
--  show K = "K"
--  show (f :* g) = show f ++ " " ++ show g

i :: forall d. Typeable d => Expr (d -> d)
i = S :* K :* (K :: Expr (d -> d -> d))

elim :: Expr a -> Expr a
elim (Cnst x) = Cnst x
elim (Var n) = undefined
elim (f :* x) = elim f :* elim x
elim (Lmb m v@(Var n)) = if m == n then checked_cast (iof v) else undefined
  where iof :: forall e. Typeable e => Expr e -> Expr (e -> e)
        iof v = i :: Expr (e -> e)
elim (Lmb m (Lmb n x)) = elim (Lmb m (elim (Lmb n x)))
elim (Lmb n (x :* y)) = S :* elim (Lmb n x) :* elim (Lmb n y)
elim (Lmb n x) = K :* elim x

main = print "hello"
