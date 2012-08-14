module Parser where
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import AST

program :: Parser [Definition]
program = scope <* eof

scope :: Parser [Definition] 
scope = many (definition <* optional (lexeme ";"))

definition :: Parser Definition
definition = try (Function <$> name <* lexeme "("
                           <*> sepBy1 name (try (lexeme ",")) <* lexeme ")" <* lexeme "="
                           <*> sepBy1 expression (lexeme ","))
         <|>     (Value    <$> sepBy1 name (lexeme ",") <* lexeme "="
                           <*> sepBy1 expression (lexeme ","))

expression :: Parser Expression
expression = try (Object <$> (lexeme "{" *> scope <* lexeme "}"))
         <|> try (Call   <$> name <* lexeme "("
                         <*> sepBy1 expression (lexeme ","))
         <|>     (Name   <$> name)

name :: Parser String
name = spaces *> many1 (noneOf " \n\t{}(),;") <* spaces

lexeme :: String -> Parser String
lexeme s = spaces *> string s <* spaces