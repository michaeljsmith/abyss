{-#LANGUAGE GADTs, EmptyDataDecls #-}

data Declaration = Declaration String String
data TypeBuilder = TypeBuilder [String] [Declaration]

type Type a = TypeBuilder -> (a, TypeBuilder)

fmap' :: (a -> b) -> Type a -> Type b
fmap' f m = \st -> let (a, s) = m st in (f a, s)

pure' :: a -> Type a
pure' a = \st -> (a, st)

(<@>) :: Type (a -> b) -> Type a -> Type b
sf <@> sv = \st -> let (f, st1) = sf st
                       (a, st2) = sv st1
                    in (f a, st2)

return' :: a -> Type a
return' a = pure' a

bind :: Type a -> (a -> Type b) -> Type b
m `bind` f = \st -> let (a, st1) = m st
                        (b, st2) = f a st1
                     in (b, st2)

readLabel :: Type String
readLabel (TypeBuilder ls ds) = ((head ls), (TypeBuilder (tail ls) ds))

data Variable = Variable String Int

declareVariable :: Type (String -> Variable)
declareVariable (TypeBuilder ls ds) = ((Variable s 3), (TypeBuilder ls (Declaration "int" s : ds)))

variable = declareVariable <@> readLabel

data Class t = Class String (Type t)
--printClass (Class n t) =

main =
  putStrLn "hello"
