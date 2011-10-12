{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators #-}

import Data.Monoid
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

newtype Expression a = Expression {expressionString :: String}

type Block a = StateT [String] (Writer String) a

class Type t where
  typeName :: t -> String

data IntegerT = IntegerT

instance Type IntegerT where
  typeName = const "int"

blockText :: Block a -> String
blockText block = execWriter $ runStateT block labels
  where
    labels = ["l" ++ show x | x <- [1..]]

newLabel :: Block String
newLabel = do {x <- gets head; modify tail; return x}

code :: String -> Block ()
code s = tell s

variableRef :: String -> Expression a
variableRef name = Expression name

local :: Type a => a -> Block (Expression a)
local t = do
  label <- newLabel
  code $ typeName t ++ " " ++ label ++ ";\n"
  return $ Expression label

function :: Type a => a -> String -> Block b -> Block ()
function t name body = do
  code $ typeName t ++ " " ++ name ++ "() {\n"
  body
  code $ "}\n"

eval :: Expression a -> Block ()
eval x = do
  code $ expressionString x ++ ";\n"

mainDecl = function IntegerT "main" $ do
  v <- local IntegerT
  eval v
  local IntegerT

text = blockText mainDecl

main = do
  putStrLn text
