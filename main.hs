{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances #-}

import Data.Monoid
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

instance Applicative (State a) where
  pure  = return
  (<*>) = ap

instance Monoid a => Applicative (Writer a) where
  pure  = return
  (<*>) = ap

type Cpp a = State [Label] a
newtype Label = Label {labelString :: String}

newLabel :: Cpp Label
newLabel = gets head <* (modify tail)

newtype Expression a = Expression String deriving Show

data Show a => Literal a = Literal a
literalExpr :: (Show a) => Literal a -> Expression a
literalExpr (Literal a) = Expression (show a)

type Block a = Writer String a

withVar :: (Show a) => Literal a -> Cpp (Block (Expression a -> b) -> Block b)
withVar x = (withVarBlock x) <$> newLabel where
  withVarBlock :: (Show a) => Literal a -> Label -> Block (Expression a -> b) -> Block b
  withVarBlock x label tpl =
    tell ("var " ++ (labelString label) ++ " = " ++ ((show . literalExpr) x)) *> (tpl <*> pure (Expression (labelString label)))

main =
  putStrLn "hello"
