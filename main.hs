{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators, StandaloneDeriving, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Applicative
import Data.Typeable
import Data.Dynamic

checked_cast :: (Typeable a, Typeable b) => a -> b
checked_cast = (flip fromDyn undefined) . toDyn

data Expr a where
  Cnst :: a -> Expr a
  Var :: Integer -> Expr a
  (:*:) :: (Typeable a, Typeable f) => Expr (a -> f) -> Expr a -> Expr f
  S' :: (Typeable x, Typeable a, Typeable g) => Expr (x -> a -> g) -> Expr (x -> a) -> Expr (x -> g)
  K' :: (Typeable a, Typeable h) => Expr a -> Expr (h -> a)
  I' :: Typeable i => Expr (i -> i)
  Lmb :: (Typeable a, Typeable b) => Integer -> Expr a -> Expr (b -> a)
  deriving Typeable

instance Show (Expr a) where
  show (Cnst a) = "<const>"
  show (Var n) = "Var " ++ show n
  show (f :*: g) = "(" ++ show f ++ " " ++ show g ++ ")"
  show (S' x y) = "S" ++ show x ++ show y
  show (K' x) = "K" ++ show x
  show (Lmb n x) = "\\" ++ show n ++ " " ++ show x

data Term a where
  Term :: a -> Term a
  S :: Term (x -> a -> b) -> Term (x -> a) -> Term (x -> b)
  K :: Term a -> Term (b -> a)
  I :: Term (a -> a)
  deriving Typeable

instance Show (Term a) where
  show (Term a) = "<const>"
  show (S x y) = "S" ++ show x ++ show y
  show (K x) = "K" ++ show x
  show I = "I"

elim :: Expr a -> Expr a
elim (Cnst x) = Cnst x
elim (Var n) = undefined
elim (f :*: x) = elim f :*: elim x
elim (S' x y) = S' x y
elim (K' x) = K' x
elim I' = I'
elim (Lmb m v@(Var n)) =
  if m == n
    then checked_cast (iof v)
    else K' v
  where iof :: forall e. Typeable e => Expr e -> Expr (e -> e)
        iof v = I' :: Expr (e -> e)
elim (Lmb m (Lmb n x)) = elim (Lmb m (elim (Lmb n x)))
elim (Lmb n (f :*: x)) = S' (elim (Lmb n f)) (elim (Lmb n x))
elim (Lmb n x) = K' (elim x)

reduce :: Expr a -> Term a
reduce (Cnst a) = Term a
reduce (Var n) = undefined
reduce (f :*: x) = undefined
reduce (S' x y) = S (reduce x) (reduce y)
reduce (K' x) = K (reduce x)
reduce I' = I
reduce l@(Lmb n x) = reduce (elim l)

eval :: Term a -> a
eval (Term a) = a
eval (S f g) = \ x -> (eval f) x ((eval g) x)
eval (K x) = \y -> (eval x)
eval I = \x -> x

--omap :: Applicative b => (a -> (b o)) -> Term a -> (b o)
--omap f (Term a) = a
--omap f S = \h g x -> h x (g x)
--omap f K = \x y -> x
--omap f I = \x -> x
--omap f (h :* x) = omap h (omap x)

foo :: Expr (Integer -> Integer -> Integer)
--foo = (Lmb 1 $ Lmb 2 $ (Cnst (*)) :*: (Var 2) :*: (Var 1)) :*: (Cnst (3 :: Integer)) :*: (Cnst (5 :: Integer))
foo = (Lmb 1 $ Lmb 2 $ (Cnst (-)) :*: (Var 1) :*: (Var 2))

bar = reduce foo

baz = eval bar 3 4

main = do
  print foo
  print bar
  print baz
