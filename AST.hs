module AST where

data Definition = Definition String Expression deriving (Show)
data Expression = Number Double
                | String String
                | Reference String
                | Lambda [String] Expression
                | Prefix String Expression
                | Infix String Expression Expression
                | Application Expression [Expression] deriving (Show)