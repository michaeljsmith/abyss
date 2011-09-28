{-#LANGUAGE GADTs, EmptyDataDecls #-}

import Control.Applicative

data Type a where
  Constant :: a -> Type a
  Apply :: Type (b -> a) -> Type b -> Type a

instance Functor Type where
  fmap f g = Apply (Constant f) g

instance Applicative Type where
  pure = Constant
  (<*>) = Apply

evalType :: Type a -> a
evalType (Constant a) = a
evalType (Apply f g) = (evalType f) (evalType g)

data View x = View x deriving Show
view = fmap View

data Variable a = Variable a deriving Show
variable :: a -> Type (Variable a)
variable x = pure (Variable x)

app = view (variable 3)

evalApp = evalType app

main =
  putStrLn (show evalApp)
