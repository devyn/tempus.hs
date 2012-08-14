module AST where

data Definition = Function String [String] [Expression]
                | Value   [String]         [Expression]  deriving (Show)
data Expression = Name String
                | Lookup Expression String
                | Call String [Expression]
                | Object [Definition]
                | Conditional [(Expression, Expression)] deriving (Show)