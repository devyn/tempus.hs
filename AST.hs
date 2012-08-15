module AST where

data Definition = Function String [String] [Expression]
                | Value   [String]         [Expression]  deriving (Show)
data Expression = Reference String [Expression]
                | Member Expression String [Expression]
                | Object [Definition]
                | Conditional [(Expression, Expression)] deriving (Show)