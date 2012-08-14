module AST where

data Definition = Function String [String] [Expression]
                | Value   [String]         [Expression]
data Expression = Reference String [Expression]
                | Object [Definition]
                | Conditional [(Expression, Expression)]