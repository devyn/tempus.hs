module AST where

data Definition = Value    String          Expression
                | Function String [String] Expression deriving (Show)
data Expression = Number Double
                | String String
                | Reference String
                | Application Expression [Expression]
                | Infix String Expression Expression
                | Prefix String Expression deriving (Show)