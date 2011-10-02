{-#LANGUAGE GADTs, EmptyDataDecls #-}

import Control.Applicative
import Control.Monad.State

instance Applicative (State a) where
  pure  = return
  (<*>) = ap

type Block a = State [String] a
type Code = String

generateCode :: Block Code -> Code
generateCode block =
  evalState block ["v" ++ show i | i <- [1..]]

blockplus :: Block Code -> Block Code -> Block Code
blockplus = liftA2 (++)

class Gettable g where
  getValue :: g -> Code

class Settable s where
  setValue :: s -> Code -> Code

newtype Label = Label {getLabelString :: String}
data Variable = Variable Label

instance Gettable Variable where
  getValue = readVar

instance Settable Variable where
  setValue = writeVar

code :: Code -> Block Code
code code = return code

statement :: Code -> Code
statement code = code ++ ";\n"

eval :: Block Code -> Block Code
eval expr = statement <$> expr

assign :: Block Variable -> Block Code -> Block Code
assign var expr = statement <$> (setValue <$> var <*> expr)

newLabel :: Block Label
newLabel = (Label . head) <$> (get <* (modify tail))

variable :: Block Variable
variable = Variable <$> newLabel

readVar :: Variable -> Code
readVar (Variable name) = getLabelString name

writeVar :: Variable -> Code -> Code
writeVar (Variable name) expr = (getLabelString name) ++ " = " ++ expr

prompt :: (Gettable g) => Block g -> Block Code
prompt message = code "print(" `blockplus` (getValue <$> message) `blockplus` code ")"

readInput :: Block Code
readInput = code "read()"

foo :: Variable -> Code -> Code
foo = setValue

appBlock :: Block Code
appBlock =
  eval (prompt variable) `blockplus`
  assign variable readInput

appCode = generateCode appBlock

main =
  putStr appCode
