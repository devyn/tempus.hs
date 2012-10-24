module AST where

data Definition = Value   [String]         [Expression]
                | Function String [String] [Expression] deriving (Show)
data Expression = Reference String [Expression]
                | Member Expression String [Expression]
                | Infix String Expression Expression
                | Object [Definition]
                | Conditional [(Expression, Expression)] deriving (Show)