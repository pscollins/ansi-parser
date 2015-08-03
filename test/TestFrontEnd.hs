module Main where

import Test.Hspec

import AnsiParser.Types
import AnsiParser.FrontEnd.Lex (scanTokens)
import AnsiParser.FrontEnd.Parse

import Debug.Trace

parse :: String -> [Expr]
parse s = trace (show res) res
  where Right res = trace (show $ parseExpr s) $ parseExpr s

parsesToEs :: String -> [Expr] -> Expectation
parsesToEs s r = parse s `shouldBe` r

parsesTo :: String -> Expr -> Expectation
parsesTo s r = s `parsesToEs` [r]

main :: IO ()
main = hspec $ do
  describe "plain text" $ do
    it "parses the empty string" $
      "" `parsesToEs` []
    it "does not split words" $
      "Hello World" `parsesTo` Plain "Hello World"
  describe "SGR" $ do
    it "parses reset" $
      "\x27[0;m" `parsesTo` Cmd (SGR [DefaultColor])
  describe "C1" $ do
    it "parses index" $
      "\x27\&D" `parsesTo` Cmd (C1 Index)
