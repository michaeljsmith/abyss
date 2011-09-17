import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative

data StatementAst = VariableAst String String
data MemberAst = MemberVariableAst String String | MemberFunctionAst String String [StatementAst]
data ClassAst = ClassAst String [MemberAst]

class_ name = ClassAst name []
addToClassAst x (ClassAst name ms) = ClassAst name (x:ms)

indent = do
  indent <- ask
  tell indent

prettyPrintMemberAst (MemberVariableAst t n) = do
  indent
  tell t
  tell " "
  tell n
  tell ";\n"

prettyPrintClassAst (ClassAst n ms) = do
  indent
  tell "class "
  tell n
  tell " {\n"
  local (++ "  ") (do
    mapM_ prettyPrintMemberAst ms)
  indent
  tell "};\n"

data Class a = Class (ClassAst -> (a, ClassAst))

runClass classAst0 (Class class_) = class_ classAst0

instance Monad Class where
  Class c1 >>= fc2 =
    Class (\classAst0 ->
      let (r, classAst1) = c1 classAst0
          Class c2 = fc2 r in
            c2 classAst1)

  return k =  Class (\classAst -> (k, classAst))

memberVariable type_ name = Class (\classAst0 -> ((), addToClassAst (MemberVariableAst type_ name) classAst0))

runFormatter classAst = runWriter (runReaderT (prettyPrintClassAst classAst) "")

testClass = do
  memberVariable "int" "hello"
  memberVariable "char" "world"

main = putStr text where
  ((), text) = runFormatter classAst
  (result, classAst) = runClass (class_ "asdf") testClass
