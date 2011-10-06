{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances #-}

import Data.Monoid
import Control.Applicative
import Control.Monad.State

instance Applicative (State a) where
  pure  = return
  (<*>) = ap

--local :: Block (Expression a -> (a -> b) -> b)
--local = pure (.) <*> (bsplit <*> newLabel <*>
--  pure (\x -> code ("var " ++ labelText x ++ "\n")))

dup :: (a -> a -> b) -> a -> b
dup f x = f x x

fdup :: Applicative f => f ((a -> a -> b) -> a -> b)
fdup = pure dup

finter :: Applicative f => f ((a -> b) -> a -> a)
finter = pure (.) <*> fdup <*> (pure (.) <*> pure (const id))

main =
  putStrLn "hello"
