{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators, StandaloneDeriving, FlexibleContexts, DeriveDataTypeable, ScopedTypeVariables #-}

import Control.Applicative
import Data.Typeable
import Data.Dynamic

data a :-> b where
  (:->) :: a -> b -> (a :-> b)
  Sbst :: (x :-> a :-> b) -> (x :-> a) -> (x :-> b)
  Ignr :: b -> a -> (b :-> a)

data Object a where
  Object :: a -> Object a
  (:*) :: (Object a :-> Object b) -> Object a -> Object b
  ObjectPair :: Object a -> Object a -> Object a

infixr 1 :->

type Variable = IORef String

memberO s m = do
  ref <- newIORef s

--data Value a = Value {valueExpression :: a}
--textOf (Value a) = show a

--constant x = Value x

--data TextDisplay = TextDisplay (Value String)
--display (TextDisplay v) = print (textOf v)

--textDisplay v = v :-> TextDisplay v

--app = textDisplay (constant "Hello, world!")

--main = display app
main = print "hello"

