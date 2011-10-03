{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances #-}

import Data.Monoid
import Control.Applicative
import Control.Monad.State

instance Applicative (State a) where
  pure  = return
  (<*>) = ap

type Cpp a = State [Label] a
newtype Label = Label {labelString :: String}

newLabel :: Cpp Label
newLabel = gets head <* (modify tail)

newtype Literal a = Literal a
newtype Expression a = Expression String
newtype Block = Block String

blockplus :: Block -> Block -> Block
blockplus (Block s0) (Block s1) = Block (s0 ++ s1)

withVar :: Literal a -> Cpp (Expression a -> Block) -> Cpp Block
withVar (Literal x) blockTpl =
  (\f x -> Block ("var " ++ x ++ "\n") `blockplus` f x) <$>
    (pure (.) <*> blockTpl <*> pure Expression) <*>
    (labelString <$> newLabel)

main =
  putStrLn "hello"
