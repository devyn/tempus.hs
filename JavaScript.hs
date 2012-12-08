module JavaScript (compiler) where
import Data.List
import AST

compiler :: [Definition] -> String
compiler program = intercalate "\n" (map definition program)

definition :: Definition -> String
definition (Value name value) = "var " ++ name ++ ";\n"
definition (Function name args value) = "function " ++ name ++"(" ++ intercalate ", " args ++ ") { return " ++ expression value ++ " }"

{-
definition :: Definition -> String
definition (Value names values) =
  "var " ++ intercalate ", " names ++ ";\n" ++
  "(function($) {\n" ++ indent
    (concat (zipWith (++) (map (++ " = $[") names) (map ((++ "];\n") . show) [0..]))) ++
  "})("++ "[" ++
    intercalate ", " (map expression values) ++
  "]" ++ ");\n"
definition (Function name arguments value) =
  "var " ++ name ++ " = function(" ++ intercalate ", " arguments ++ ") {\n" ++ indent
    ("return " ++ expression value) ++ -- "[" ++ intercalate ", " (map expression values) ++ "];") ++
  "};"
-}

expression :: Expression -> String
expression (Reference name) = name
expression (Application expr args) = expression expr ++ "(" ++ intercalate ", " (map expression args) ++ ")"
--expression (Member owner name []) = (expression owner) ++ "." ++ name
--expression (Member owner name args) = (expression owner) ++ "." ++ name ++ "(" ++ intercalate ", " (map expression args) ++ ")"
expression (Infix oper left right) = term left ++ " " ++ oper ++ " " ++ term right where
expression (Prefix oper expr) = oper ++ term expr
{-expression (Object definitions) = let names = concatMap definitionNames definitions in
  "(function() {\n" ++ indent
    (compiler definitions) ++
    "return { " ++ intercalate ", " (zipWith (++) (map (++ ": ") names) names) ++ " }" ++
  "})()"-}

term t@(Infix _ _ _) = "(" ++ expression t ++ ")"
--term t@(Conditional _) = "(" ++ expression t ++ ")"
term t = expression t

{-
definitionNames :: Definition -> [String]
definitionNames (Value names _) = names
definitionNames (Function name _ _) = [name]
-}

indent :: String -> String
indent = unlines . map ("  " ++) . lines