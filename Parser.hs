module Parser where
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import AST

program :: Parser [Definition]
program = scope <* eof

scope :: Parser [Definition] 
scope = many (definition <* optional (lexeme ";"))

definition :: Parser Definition
definition = try (Function <$> name
                           <*> bracket "(" ")" (sepBy1 name (try (lexeme ","))) <* lexeme "="
                           <*> sepBy1 expression (lexeme ","))
             <|> (Value    <$> sepBy1 name (lexeme ",") <* lexeme "="
                           <*> sepBy1 expression (lexeme ","))

expression :: Parser Expression
expression = parseInfix infixes
  where parseInfix :: [[String]] -> Parser Expression
        parseInfix []           = term
        parseInfix (low:higher) = chainl1 (parseInfix higher)
                                          ((\ o l r -> Reference o [l, r]) <$> choice (map lexeme low))

term :: Parser Expression
term = try (foldl (uncurry . Member) <$> value <*> many (lexeme "." *> reference))
       <|> (Reference <$> choice (map lexeme prefixes) <*> ((:[]) <$> term))

value :: Parser Expression
value = try (bracket "(" ")" expression)
    <|> try (Object <$> bracket "{" "}" scope)
    <|> (uncurry Reference <$> reference)

reference :: Parser (String, [Expression])
reference = (,) <$> name
                <*> (try (bracket "(" ")" (sepBy1 expression (lexeme ","))
                     <|> return []))

name :: Parser String
name = spaces *> many1 (noneOf " \n\t{}(),;.") <* spaces

infixes :: [[String]]
infixes = [ [ "+", "-" ]
          , [ "*", "/" ] ]

prefixes :: [String]
prefixes = [ "-" ]

lexeme :: String -> Parser String
lexeme s = spaces *> string s <* spaces

bracket :: String -> String -> Parser a -> Parser a
bracket l r p = between (lexeme l) (lexeme r) p