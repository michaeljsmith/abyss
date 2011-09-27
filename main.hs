import Control.Monad.State

data RealType = NullType | RealType [String] RealType
  deriving Show

type Type = RealType -> State [String] RealType

newLabel :: State [String] String
newLabel = do
  (label:rest) <- get
  put rest
  return label

variable :: Type
variable s0 = do
  label <- newLabel
  return (RealType [label] s0)

view :: Type -> Type
view var s0 =
  return (RealType [] s0)

app = view variable

realApp = evalState (app NullType) ["m" ++ show i | i <- [1..]]

main =
  putStrLn (show realApp)
