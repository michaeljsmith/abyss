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

code :: String -> Code ()
code s = Code $ \s0 -> ((), s0 ++ s)

execCode :: Code a -> String
execCode c = snd $ runCode c ""

program = code "foo"

main = putStrLn programText
  where
    programText = execCode program
