module Main where
import AST
import Parser
import JavaScript
import System.Environment

main :: IO ()
main = do
    (inFile:outFile:_) <- getArgs
    program <- readFile inFile
    case parser inFile program of
        Left err -> putStrLn err
        Right program -> writeFile outFile (compiler program)