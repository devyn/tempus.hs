module JavaScript where
import Data.List
import AST

compiler :: [Definition] -> String
compiler program = intercalate "\n\n" (map definition program)

definition :: Definition -> String
definition (Value names values) = if length names == 1 && length values == 1
  then
    "var " ++ head names ++ " = " ++ expression (head values) ++ ";"
  else
    "var " ++ intercalate ", " names ++ ";\n" ++
    "(function($) {\n" ++ indent
      (intercalate "\n" (zipWith (++) (map (++ " = $[") names) (map ((++ "];") . show) [0..]))) ++
    "})([" ++ intercalate ", " (map expression values) ++ "]);"
definition (Function name arguments values) = name ++ " = "

expression :: Expression -> String
expression (Reference name []) = name
expression (Reference name args) = name ++ "(" ++ intercalate ", " (map expression args) ++ ")"
expression (Member owner name []) = (expression owner) ++ "." ++ name
expression (Member owner name args) = (expression owner) ++ "." ++ name ++ "(" ++ intercalate ", " (map expression args) ++ ")"

indent :: String -> String
indent = unlines . map ("  " ++) . lines