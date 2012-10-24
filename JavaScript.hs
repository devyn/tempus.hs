module JavaScript where
import Data.List
import AST

compiler :: [Definition] -> String
compiler program = intercalate "\n" (map definition program)

definition :: Definition -> String
definition (Value names values) =
  "var " ++ intercalate ", " names ++ ";\n" ++
  "(function($) {\n" ++ indent
    (concat (zipWith (++) (map (++ " = $[") names) (map ((++ "];\n") . show) [0..]))) ++
  "})("++ {-(if length values > 1 then "[" else "")-}"[" ++
    intercalate ", " (map expression values) ++
  {-(if length values > 1 then "]" else "")-}"]" ++ ");\n"
definition (Function name arguments values) =
  "var " ++ name ++ " = function(" ++ intercalate ", " arguments ++ ") {\n" ++ indent
    ("return " ++
{-    (if length values == 1
      then
        expression (head values) ++ ";"
      else-}
        "[" ++ intercalate ", " (map expression values) ++ "];") ++
  "};"

expression :: Expression -> String
expression (Reference name []) = name
expression (Reference name args) = name ++ "(" ++ intercalate ", " (map expression args) ++ ")"
expression (Member owner name []) = (expression owner) ++ "." ++ name
expression (Member owner name args) = (expression owner) ++ "." ++ name ++ "(" ++ intercalate ", " (map expression args) ++ ")"
expression (Infix oper left right) = term left ++ " " ++ oper ++ " " ++ term right where
expression (Prefix oper expr) = oper ++ term expr
expression (Object definitions) = let names = concatMap definitionNames definitions in
  "(function() {\n" ++ indent
    (compiler definitions) ++
    "return { " ++ intercalate ", " (zipWith (++) (map (++ ": ") names) names) ++ " }" ++
  "})()"

term t@(Infix _ _ _) = "(" ++ expression t ++ ")"
term t@(Conditional _) = "(" ++ expression t ++ ")"
term t = expression t

definitionNames :: Definition -> [String]
definitionNames (Value names _) = names
definitionNames (Function name _ _) = [name]

indent :: String -> String
indent = unlines . map ("  " ++) . lines