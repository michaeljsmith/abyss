import Control.Monad.Writer

data MemberAst = MemberVariable String String
data ClassAst = ClassAst String [MemberAst]

addAst x (ClassAst name ms) = ClassAst name (x:ms)

prettyPrintMemberAst (MemberVariable t n) = do
  tell t
  tell " "
  tell n
  tell ";\n"

prettyPrintClassAst (ClassAst n ms) = do
  tell "class "
  tell n
  tell " {\n"
  mapM_ prettyPrintMemberAst ms
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

addMemberVariable type_ name = Class (\classAst0 -> ((), addAst (MemberVariable type_ name) classAst0))

class_ = do addMemberVariable "int" "hello"
            addMemberVariable "char" "world"

main = putStr text where
  ((), text) = (runWriter (prettyPrintClassAst classAst))
  (result, classAst) = runClass (ClassAst "asdf" []) class_
