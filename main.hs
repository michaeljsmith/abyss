{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators, StandaloneDeriving, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Applicative
import Data.Typeable
import Data.Dynamic

checked_cast :: (Typeable a, Typeable b) => a -> b
checked_cast = (flip fromDyn undefined) . toDyn

data Expr a where
  Cnst :: a -> Expr a
  Var :: Integer -> Expr a
  (:*:) :: (Typeable a, Typeable b) => Expr (a -> b) -> Expr a -> Expr b
  Lmb :: (Typeable a, Typeable b) => Integer -> Expr a -> Expr (b -> a)
  deriving Typeable

data Term a where
  Term :: a -> Term a
  S :: Term ((x -> a -> b) -> (x -> a) -> x -> b)
  K :: Term (a -> b -> a)
  (:*) :: Term (a -> b) -> Term a -> Term b
  deriving Typeable

instance Show (Term a) where
  show (Term a) = "<const>"
  show S = "S"
  show K = "K"
  show (f :* g) = show f ++ " " ++ show g

s = S
k = K

i :: Term (d -> d)
i = S :* K :* K

elim :: Expr a -> Term a
elim (Cnst x) = Term x
elim (f :*: x) = elim f :* elim x
elim (Lmb m v@(Var n)) = checked_cast (iof v)
  where iof :: Expr e -> Term (e -> e)
        iof v = i :: Term (e -> e)
elim (Lmb n x) = K :* elim x

--T[Î»x.E] => (K T[E]) (if x does not occur free in E)
--T[Î»x.x] => I
--T[Î»x.Î»y.E] => T[Î»x.T[Î»y.E]] (if x occurs free in E)
--T[Î»x.(Eâ Eâ)] => (S T[Î»x.Eâ] T[Î»x.Eâ])

eval :: Term a -> a
eval (Term a) = a
eval S = \f g x -> f x (g x)
eval K = \x y -> x
eval (x :* y) = (eval x) (eval y)

--foo = typeOf (undefined :: Integer) == typeOf (undefined :: Integer)

main = print "hello"
