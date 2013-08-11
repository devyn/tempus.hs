module Main where
import AST
import Parser
import Network
import JavaScript
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then
      putStrLn "Expected input and output files."
    else do
      let (inFile:outFile:_) = args
      program <- readFile inFile
      case parser inFile program of
        Left err -> putStrLn err
        Right program ->
          writeFile outFile (programToJavaScript program)
