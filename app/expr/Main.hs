module Main where

import           Combinators (runParser, Result (..))
import           Expr        (parseExpr)
import           Text.Printf (printf)

run :: String -> IO ()
run input = do
  case runParser parseExpr input of
    Success "" tree ->
      putStrLn $ printf "Simplified version: %s\n" (show tree)
    Success rest tree ->
      putStrLn $ printf "Simplified version: %s\nCould not parse: %s\n" (show tree) rest
    Failure e -> putStrLn $ "Parsing failed:\n" ++ show e

main :: IO ()
main = do
  input <- getLine
  run input
