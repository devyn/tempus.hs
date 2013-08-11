module Parser where--(parser) where
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import AST

parser :: String -> String -> Either String Program
parser filename s = case parse program filename s of
  Left  err -> Left $ show err
  Right ast -> Right ast

program :: Parser Program
program = Program <$> many (try interfaceDeclaration <* optional (try $ lexeme ";")) <* spaces
                  <*> many (try definition <* optional (try $ lexeme ";")) <* spaces <* eof

interfaceDeclaration :: Parser InterfaceDeclaration
interfaceDeclaration = Import <$> try (keyword "import" *> name)
                   <|> Export <$> try (keyword "export" *> name)

definition :: Parser Definition
definition = Definition <$> name
                        <*> (try (Lambda <$> bracket "(" ")" (sepBy1 name (lexeme ",")) <* lexeme "="
                                     <*> expression)
                             <|> (lexeme "=" *> expression))

expression :: Parser Expression
expression = parseInfix infixes where
  parseInfix :: [[String]] -> Parser Expression
  parseInfix []           = term
  parseInfix (low:higher) = chainl1 (try $ parseInfix higher)
                                    (Infix <$> choice (map (try . lexeme) low) <?> "operator")

term :: Parser Expression
term = try (Application <$> value <* lexeme "(" <*> sepBy1 (try expression) (try (lexeme ",")) <* lexeme ")")
   <|> try (Prefix <$> choice (map lexeme prefixes) <*> term)
   <|> value

value :: Parser Expression
value = try (bracket "(" ")" expression)
    <|> try number
    <|> try str
    <|> reference
    <?> "value"

reference :: Parser Expression
reference = Reference <$> name

name :: Parser String
name = spaces *> many1 (noneOf (" \n\t{}(),;.\"0123456789" ++ (concat.concat) infixes))

number :: Parser Expression
number = spaces *> ((Number . read) <$> ((++) <$> many1 digit <*> (try ((:) <$> char '.' <*> many1 digit) <|> return "")))

str :: Parser Expression
str = String <$> (spaces *> char '"' *> many (noneOf "\"") <* char '"')

infixes :: [[String]]
infixes = [ [ "+", "-" ]
          , [ "*", "/" ] ]

prefixes :: [String]
prefixes = [ "-" ]

lexeme :: String -> Parser String
lexeme s = spaces *> string s

keyword :: String -> Parser String
keyword s = spaces *> string s <* many1 (oneOf " \n\t")

bracket :: String -> String -> Parser a -> Parser a
bracket l r p = between (lexeme l) (lexeme r) p
