module AST where

data Definition = Definition [String] [String] [Expression]
data Expression = Reference String [Expression]
                | Object [Definition]
                | Conditional [(Expression, Expression)]