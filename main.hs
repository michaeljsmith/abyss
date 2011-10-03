{-#LANGUAGE GADTs, EmptyDataDecls #-}

import Control.Applicative
import Control.Monad.State

instance Applicative (State a) where
  pure  = return
  (<*>) = ap

type Block a = State [String] a
type Code = Block String

generateCode :: Block Code -> String
generateCode block =
  evalState block ["v" ++ show i | i <- [1..]]

blockplus :: Block Code -> Block Code -> Block Code
blockplus = liftA2 (++)

code :: String -> Block Code
code code = return code

statement :: String -> String
statement code = code ++ ";\n"

eval :: Block Code -> Block Code
eval expr = statement <$> expr

assign :: Block Variable -> Block Code -> Block Code
assign var expr = statement <$> (setValue <$> var <*> expr)

newtype Label = Label {getLabelString :: String}

newLabel :: Block Label
newLabel = Label <$> (gets head <* (modify tail))

class Gettable g where
  getValue :: g -> String

class Settable s where
  setValue :: s -> String -> String

data Variable = Variable Label

instance Gettable Variable where
  getValue = readVar

instance Settable Variable where
  setValue = writeVar

variable :: Block Variable
variable = Variable <$> newLabel

withVar :: (Block Variable -> Block a) -> Block a
withVar fn = fn variable

readVar :: Variable -> String
readVar (Variable name) = getLabelString name

writeVar :: Variable -> String -> String
writeVar (Variable name) expr = (getLabelString name) ++ " = " ++ expr

prompt :: (Gettable g) => Block g -> Block Code
prompt message = code "print(" `blockplus` (getValue <$> message) `blockplus` code ")"

readInput :: Block Code
readInput = code "read()"

foo :: Variable -> String -> String
foo = setValue

appBlock :: Block Code
appBlock =
  eval (prompt variable) `blockplus`
  assign variable readInput

appCode = generateCode appBlock

main =
  putStr appCode
