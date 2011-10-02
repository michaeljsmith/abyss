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

data Prompt a = Prompt a

displayPrompt :: (Gettable g) => Prompt g -> Code
displayPrompt (Prompt message) = "print(" ++ (getValue message) ++ ")"

readInput :: Block Code
readInput = return "read()"

statement :: Code -> Code
statement code = code ++ ";\n"

--app = Prompt (Variable "foo")

foo :: Variable -> Code -> Code
foo = setValue

code :: Block Code
code = (++) <$>
  (statement <$> (displayPrompt <$> (Prompt <$> variable))) <*>
  (statement <$> (setValue <$> variable <*> readInput))

main =
  --putStr code
  putStr "hello"
