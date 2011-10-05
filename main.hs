{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances #-}

import Data.Monoid
import Control.Applicative
import Control.Monad.State

instance Applicative (State a) where
  pure  = return
  (<*>) = ap

main =
  putStrLn "hello"
