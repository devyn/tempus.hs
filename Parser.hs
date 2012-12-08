module Parser (parser) where
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import AST

parser :: String -> String -> Either String [Definition]
parser filename s = case parse program filename s of
  Left  err -> Left $ show err
  Right ast -> Right ast

program :: Parser [Definition]
program = many (definition <* optional (lexeme ";")) <* spaces <* eof

definition :: Parser Definition
definition = try (Function <$> name
                           <*> bracket "(" ")" (sepBy1 name (try (lexeme ","))) <* lexeme "="
                           <*> expression)
             <|> (Value    <$> name <* lexeme "="
                           <*> expression)

expression :: Parser Expression
expression = parseInfix infixes where
  parseInfix :: [[String]] -> Parser Expression
  parseInfix []           = term
  parseInfix (low:higher) = chainl1 (parseInfix higher)
                                    ((\ o l r -> Infix o l r) <$> choice (map lexeme low))

term :: Parser Expression
term = --try (foldl Member <$> value <*> many (lexeme "." *> name)) <|>
       (Prefix <$> choice (map lexeme prefixes) <*> term)

value :: Parser Expression
value = try (bracket "(" ")" expression)
    <|> try number
    <|> try str
    <|> reference

reference :: Parser Expression
reference = Reference <$> name

name :: Parser String
name = spaces *> many1 (noneOf " \n\t{}(),;.")

number :: Parser Expression
number = (Number . read) <$> ((++) <$> many1 digit <*> (try (lexeme "." *> many1 digit)) <|> return "")

str :: Parser Expression
str = String <$> (spaces *> char '"' *> many (noneOf "\"") <* char '"')

infixes :: [[String]]
infixes = [ [ "+", "-" ]
          , [ "*", "/" ] ]

prefixes :: [String]
prefixes = [ "-" ]

lexeme :: String -> Parser String
lexeme s = spaces *> string s

bracket :: String -> String -> Parser a -> Parser a
bracket l r p = between (lexeme l) (lexeme r) p