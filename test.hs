data MemberAst = MemberVariable String String
data ClassAst = ClassAst String [MemberAst]

addAst x (ClassAst name ms) = ClassAst name (x:ms)

formatMemberAst (MemberVariable type_ name) =
  type_ ++ " " ++ name ++ ";\n"
formatClassAst (ClassAst name ms) =
  "class " ++ name ++ " {\n" ++ memberString ++ "}\n" where
    memberString = foldl (++) "" (map formatMemberAst ms)

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

main = putStr (formatClassAst classAst) where
  (result, classAst) = runClass (ClassAst "asdf" []) class_
