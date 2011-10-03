{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances #-}

import Data.Monoid
import Control.Applicative
import Control.Monad.State

instance Applicative (State a) where
  pure  = return
  (<*>) = ap

type Block a = State [String] a
type Code = Block String

instance Monoid Code where
  mappend = liftA2 (++)
  mempty = code ""

generateCode :: Code -> String
generateCode block =
  evalState block ["v" ++ show i | i <- [1..]]

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
  getValue var = getLabelString <$> (getVarName_ <$> var)

instance Settable Variable where
  setValue var expr = getValue var `mappend` code " = " `mappend` expr

variable :: Variable
variable = Variable_ <$> newLabel

withVar :: (Variable -> Block a) -> Block a
withVar fn = fn variable

prompt :: (Gettable g) => g -> Code
prompt message = code "print(" `mappend` (getValue message) `mappend` code ")"

readInput :: Code
readInput = code "read()"

appBlock :: Code
appBlock =
  eval (prompt variable) `mappend`
  assign variable readInput

appCode = generateCode appBlock

main =
  putStr appCode
