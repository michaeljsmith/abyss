{-#LANGUAGE GADTs, EmptyDataDecls #-}

data Object a where
  Prim :: a -> Object a
  Apply :: Object (b -> a) -> Object b -> Object a
  Dup :: Object (b -> b -> a) -> Object b -> Object a
  Flip :: Object (b -> c -> a) -> Object c -> Object b -> Object a
  Const :: Object a -> Object b -> Object a
  Comp :: Object (b -> a) -> Object (c -> b) -> Object (c -> a)

eval :: Object a -> a
eval (Prim a) = a
eval (Apply f x) = (eval f) (eval x)
eval (Dup f x) = (eval f) x' x' where x' = (eval x)
eval (Flip f x y) = (eval f) (eval y) (eval x)
eval (Const x y) = (eval x)
eval (Comp f g) = (eval f) . (eval g)

main =
  putStrLn "hello"
