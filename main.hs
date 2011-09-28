{-#LANGUAGE GADTs, EmptyDataDecls #-}

data Reference a where
  DirectReference :: a -> Reference a
  BranchReference :: (Reference b) -> (Reference c) -> Reference a

data Type a where
  Constant :: (Reference a -> b) -> Type a
  Apply :: Type (b -> a) -> Type b -> Type a

(<*>) :: Type (a -> b) -> Type a -> Type b
(<*>) = Apply

data ViewElement

data View a = View (Reference a) (Reference ViewElement)
view = Constant View

data Variable a = Variable (Reference a)
variable = Constant Variable

app = view <*> variable

main =
  putStrLn "hello"

