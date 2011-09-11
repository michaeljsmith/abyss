data Expression = Identifier String | Call Expression [Expression]
data Statement = Statement Expression
data TypeExpression = TypeName String
data VariableDeclaration = VariableDeclaration TypeExpression String
data Member = MethodDeclaration TypeExpression String [Statement] | MemberVariableDeclaration VariableDeclaration
data ClassDeclaration = ClassDeclaration String [Member]
data TopStatement = ClassDeclarationStatement ClassDeclaration
data TranslationUnit = TranslationUnit [TopStatement]

int = TypeName "int"

identifier string = Identifier string
call function arguments = Call function arguments 
statement expression = Statement expression
classDeclaration name members = ClassDeclaration name members
classDeclarationStatement classDeclaration = ClassDeclarationStatement classDeclaration
translationUnit statements = TranslationUnit statements
method type_ name statements = MethodDeclaration type_ name statements
variableDeclaration type_ name = VariableDeclaration type_ name
memberVariableDeclaration declaration = MemberVariableDeclaration declaration

formatTypeExpression (TypeName string) = string

formatArguments [] = ""
formatArguments (x:[]) = (formatExpression x)
formatArguments (x:xs) = (formatExpression x) ++ ", " ++ (formatArguments xs)

formatExpression (Identifier string) = string
formatExpression (Call function arguments) =
  (formatExpression function) ++ "(" ++ (formatArguments arguments) ++ ")"

formatStatement (Statement expression) = (formatExpression expression) ++ ";"

formatStatements [] = ""
formatStatements (x:xs) = (formatStatement x) ++ "\n" ++ (formatStatements xs)

formatVariableDeclaration (VariableDeclaration type_ name) =
  formatTypeExpression type_ ++ " " ++ name ++ ";"

formatMember (MethodDeclaration type_ name statements) =
  formatTypeExpression type_ ++ " " ++ name ++ "() {\n" ++
  (formatStatements statements) ++
  "}"

formatMember (MemberVariableDeclaration variableDeclaration) =
  formatVariableDeclaration variableDeclaration

formatMembers [] = ""
formatMembers (x:xs) = formatMember x ++ "\n" ++ formatMembers xs

formatClassDeclaration (ClassDeclaration name members) =
  "class " ++ name ++ " {\n" ++
  (formatMembers members) ++
  "};"

formatTopStatement (ClassDeclarationStatement classDeclaration) =
  formatClassDeclaration classDeclaration

formatTopStatements [] = ""
formatTopStatements (x:xs) = formatTopStatement x ++ "\n" ++ formatTopStatements xs

formatTranslationUnit (TranslationUnit tu) = formatTopStatements tu

class Composable a where
  vars :: a -> [VariableDeclaration]

data IntVariable = IntVariable Int
integer n = IntVariable n

instance Composable IntVariable where
  vars (IntVariable n) = [variableDeclaration (TypeName "int") "silly"]

data Counter n = Counter n
counter n = Counter n

instance (Composable n) => Composable (Counter n) where
  vars (Counter n) = vars n

root = counter (integer 3)
members = vars root

displayTranslationUnit =
  translationUnit [
    classDeclarationStatement (classDeclaration "Display" (map memberVariableDeclaration members))]

main = putStr $ formatTranslationUnit displayTranslationUnit
