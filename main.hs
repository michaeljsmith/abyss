{-#LANGUAGE GADTs, EmptyDataDecls, TypeSynonymInstances, TypeOperators #-}

import Control.Applicative

newtype Code a = Code {runCode :: String -> (a, String)}

instance Functor Code where
	fmap f x = Code $ \st -> let (a, s) = runCode x st in (f a, s)

instance Applicative Code where
  pure x = Code $ \s -> (x, s)
  sf <*> sv = Code (\st -> let (f, st1) = runCode sf st
                               (a, st2) = runCode sv st1
                           in (f a, st2))

class Type t where
  typeCode :: t -> Code ()

code :: String -> Code ()
code s = Code $ \s0 -> ((), s0 ++ s)

execCode :: Code a -> String
execCode c = snd $ runCode c ""

scope :: Code a -> Code ()
scope body = code "{\n" *> body *> code "\n}\n" 

newtype Function f = Function {runFunc :: (f, Code ())}
funDecl = snd . runFunc
funInvoke = fst . runFunc

function type_ name (invoke, body) = Function (code (name ++ "(") *> invoke *> code ")", typeCode type_ *> code " " *> code name *> body)

body type_ block = (pure type_, scope block)

data Int_ = Int_
int = Int_

instance Type Int_ where
  typeCode t = code "int"

fooDecl = function int "foo"$
  body int$
    code "return 1;"
foo = funInvoke fooDecl

barDecl = function int "bar"$
  body int$
    code "printf(\"%d\\n\", " *> foo *> code ");"
bar = funInvoke barDecl

program =
  funDecl fooDecl *>
  funDecl barDecl

main = putStrLn programText
  where
    programText = execCode program
