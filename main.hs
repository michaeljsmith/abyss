{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, FlexibleContexts #-}

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

data Function t = Type t => Function {funType :: t, funName :: String, funBlock :: Block ()}

scope :: Block a -> Block ()
scope block = do
  code $ "{\n"
  block
  code $ "}\n"

function :: Type a => a -> String -> Block b -> Function a
function type_ name body = Function type_ name (scope body)

declareFun :: Type a => Function a -> Block ()
declareFun fn = do
  code $ typeName (funType fn) ++ " " ++ (funName fn) ++ "()"
  funBlock fn

call :: Function a -> Expression a
call fn = Expression $ funName fn ++ "()"

eval :: Expression a -> Block ()
eval x = do
  code $ expressionString x ++ ";\n"

foo = function IntegerT "foo" $ do
  v <- local IntegerT
  eval v
  local IntegerT

main_ = function IntegerT "main" $ do
  eval $ call foo

module_ = do
  declareFun foo
  declareFun main_

text = blockText $ module_

bar = do
  x <- readLn :: IO String
  do
    y <- readLn :: IO String
    putStrLn x
    return y
  return x

(+) x y = x

bat = 1 + 2

main = do
  putStrLn text
