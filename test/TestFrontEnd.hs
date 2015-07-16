module Main where

-- import AnsiParser.FrontEnd.Lex
import AnsiParser.FrontEnd.Parse

main :: IO ()
main = do
  let s = "Hello World"
  let tokens = scanTokens s
  let e = expr tokens
  print s
  print tokens
  print e
