module AST where

data Program = Program [InterfaceDeclaration] [Definition]

data InterfaceDeclaration = Import String
                          | Export String deriving (Show, Eq)

data Definition = Definition String Expression deriving (Show)
data Expression = Number Double
                | String String
                | Reference String
                | Lambda [String] Expression
                | Prefix String Expression
                | Infix String Expression Expression
                | Application Expression [Expression] deriving (Show)
