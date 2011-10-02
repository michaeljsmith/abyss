{-#LANGUAGE GADTs, EmptyDataDecls #-}

import Control.Applicative
import Control.Monad.State

instance Applicative (State a) where
  pure  = return
  (<*>) = ap

type Block a = State [String] a
type Code = String

class Gettable g where
  getValue :: g -> Code

class Settable s where
  setValue :: s -> Code -> Code

data Variable = Variable String

instance Gettable Variable where
  getValue = readVar

instance Settable Variable where
  setValue = writeVar

variable :: Block Variable
variable = do
  (label:labels) <- get
  put labels
  return $ Variable label

readVar :: Variable -> Code
readVar (Variable name) = name

writeVar :: Variable -> Code -> Code
writeVar (Variable name) expr = name ++ " = " ++ expr

prompt :: (Gettable g) => Block g -> Block Code
prompt message = (++) <$> (return "print(") <*> (getValue <$> message)

readInput :: Block Code
readInput = return "read()"

statement :: Code -> Code
statement code = code ++ ";\n"

foo :: Variable -> Code -> Code
foo = setValue

block :: Block Code
block = (++) <$>
  (statement <$> (prompt variable)) <*>
  (statement <$> (setValue <$> variable <*> readInput))

(code, vars) = (runState block) ["v" ++ show i | i <- [1..]]

main =
  putStr code
