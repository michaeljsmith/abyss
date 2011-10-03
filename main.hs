{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances #-}

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
assign var expr = statement <$> (setValue var expr)

newtype Label = Label {getLabelString :: String}

newLabel :: Block Label
newLabel = Label <$> (gets head <* (modify tail))

class Gettable g where
  getValue :: g -> Code

class Settable s where
  setValue :: s -> Code -> Code

data Variable_ = Variable_ {getVarName_ :: Label}
type Variable = Block Variable_

instance Gettable Variable where
  getValue = liftA readVar where
    readVar (Variable_ name) = getLabelString name

instance Settable Variable where
  setValue var expr = varName var `blockplus` code " = " `blockplus` expr

varName :: Variable -> Code
varName var = getLabelString <$> (getVarName_ <$> var)

variable :: Variable
variable = Variable_ <$> newLabel

withVar :: (Variable -> Block a) -> Block a
withVar fn = fn variable

prompt :: (Gettable g) => g -> Code
prompt message = code "print(" `blockplus` (getValue message) `blockplus` code ")"

readInput :: Code
readInput = code "read()"

appBlock :: Code
appBlock =
  eval (prompt variable) `blockplus`
  assign variable readInput

appCode = generateCode appBlock

main =
  putStr appCode
