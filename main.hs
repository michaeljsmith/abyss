{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances #-}

import Data.Monoid
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

newtype Expression a = Expression {expressionString :: String}

type Block a = StateT [String] (Writer String) a

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

variableDecl :: String -> Block (Expression a)
variableDecl name = do
  code ("var " ++ name ++ ";\n")
  return $ Expression name

local :: (Expression a -> Block b) -> Block b
local tpl = do
  label <- newLabel
  variableDecl label
  tpl (variableRef label)

function :: String -> Block a -> Block ()
function name body = do
  code $ "fun " ++ name ++ "{\n"
  body
  code $ "}\n"

block = do
  function "main" $
    local $ \v -> do
      code $ expressionString v ++ "\n"
      variableDecl "bar"

text = blockText block

main = do
  putStrLn text
