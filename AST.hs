module AST where

data Definition = Value   [String]         [Expression]
                | Function String [String] [Expression] deriving (Show)
data Expression = Reference String [Expression]
                | Infix String Expression Expression
                | Member Expression String [Expression]
                | Object [Definition]
                | Conditional [(Expression, Expression)] deriving (Show)