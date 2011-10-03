{-#LANGUAGE GADTs, EmptyDataDecls #-}

import Control.Applicative
import Control.Monad.State

instance Applicative (State a) where
  pure  = return
  (<*>) = ap

type Block a = State [String] a
type Code = Block String

generateCode :: Code -> String
generateCode block =
  evalState block ["v" ++ show i | i <- [1..]]

blockplus :: Code -> Code -> Code
blockplus = liftA2 (++)

code :: String -> Code
code code = return code

statement :: String -> String
statement code = code ++ ";\n"

eval :: Code -> Code
eval expr = statement <$> expr

assign :: Variable -> Code -> Code
assign var expr = statement <$> (setValue <$> var <*> expr)

newtype Label = Label {getLabelString :: String}

newLabel :: Block Label
newLabel = Label <$> (gets head <* (modify tail))

class Gettable g where
  getValue :: g -> String

class Settable s where
  setValue :: s -> String -> String

data Variable_ = Variable_ Label
type Variable = Block Variable_

instance Gettable Variable_ where
  getValue = readVar

instance Settable Variable_ where
  setValue = writeVar

variable :: Variable
variable = Variable_ <$> newLabel

withVar :: (Variable -> Block a) -> Block a
withVar fn = fn variable

readVar :: Variable_ -> String
readVar (Variable_ name) = getLabelString name

writeVar :: Variable_ -> String -> String
writeVar (Variable_ name) expr = (getLabelString name) ++ " = " ++ expr

prompt :: (Gettable g) => Block g -> Code
prompt message = code "print(" `blockplus` (getValue <$> message) `blockplus` code ")"

readInput :: Code
readInput = code "read()"

appBlock :: Code
appBlock =
  eval (prompt variable) `blockplus`
  assign variable readInput

appCode = generateCode appBlock

main =
  putStr appCode
