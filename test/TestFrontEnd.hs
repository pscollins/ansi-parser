module Main where

import Test.Hspec

import AnsiParser.Types
import AnsiParser.FrontEnd.Lex
import AnsiParser.FrontEnd.Parse

import Control.Monad

import Debug.Trace

parse :: String -> [Expr]
parse s = trace (show res) res
  where Right res = trace (show $ parseExpr s) $ parseExpr s

lexStr :: String -> Either String [Token]
lexStr = alexScanTokens

lexTo :: String -> [Token] -> Expectation
lexTo inp outp = lexStr inp `shouldBe` Right outp

noLex :: String -> String -> Expectation
noLex inp outp = lexStr inp `shouldBe` Left outp

-- foldingLexer :: Alex [Token]
-- foldingLexer = ((liftM (:[]) scanTokens))

-- doscanTokens :: String -> [Token]
-- doscanTokens s = alexScanTokens s

-- lexStrs :: [String] -> [Either

parsesToEs :: String -> [Expr] -> Expectation
parsesToEs s r = parse s `shouldBe` r

parsesTo :: String -> Expr -> Expectation
parsesTo s r = s `parsesToEs` [r]

main :: IO ()
main = hspec $ do
  describe "lexer" $ do
    it "lexes the empty string" $
      "" `lexTo` [TokenEOF]
    it "lexes an escape char" $
      "\x27" `lexTo` [TokenEsc, TokenEOF]
    it "lexes an esc with other stuff" $
      "\x27\&15;" `lexTo` [TokenEsc, TokenNum 15, TokenSep, TokenEOF]
    it "lexes a color command" $
      "\x27[\&15;8m" `lexTo` [ TokenEsc, TokenLBracket, TokenNum 15
                             , TokenSep, TokenNum 8, TokenEndColorCmd
                             , TokenEOF ]

    it "lexes a single char function" $
      "\x27\&D" `lexTo` [TokenEsc, TokenCharFunc 'D', TokenEOF]
    it "lexes a nonprinting char" $
      "\x7" `lexTo` [TokenNonPrint '\x7', TokenEOF]
    it "lexes an ANSI set command" $
      "\x27 F" `lexTo` [TokenEsc, TokenSP, TokenAnsiSet 'F', TokenEOF]
    it "lexes a DEC line command" $
      "\x27#3" `lexTo` [TokenEsc, TokenHash, TokenDECLine 3, TokenEOF]
    it "lexes a simple char set command" $
      "\x27%@" `lexTo` [ TokenEsc, TokenPercent, TokenSimpleCharSet '@'
                       , TokenEOF]
    it "lexes a char set command" $
      "\x27(0" `lexTo` [TokenEsc, TokenLParen, TokenCharSet "0", TokenEOF]
    it "lexes an extended char set command" $
      "\x27/A" `lexTo` [TokenEsc, TokenDiv, TokenCharSet "A", TokenEOF]
    it "lexes a cursor command" $
      "\x27\&6" `lexTo` [TokenEsc, TokenCursorCmd '6', TokenEOF]
    it "lexes an invoke char set" $
      "\x27n" `lexTo` [TokenEsc, TokenInvokeCharSet 'n', TokenEOF]
    it "lexes a user defined key command" $
      "\x27P10;11|foo\x27\\" `lexTo` [ TokenDCS, TokenNum 10, TokenSep
                                     , TokenNum 11 , TokenPipe
                                     , TokenStringParam "foo"
                                     , TokenStringTerm, TokenEOF]




  -- describe "plain text" $ do
  --   it "parses the empty string" $
  --     "" `parsesToEs` []
  --   it "does not split words" $
  --     "Hello World" `parsesTo` Plain "Hello World"
  -- describe "SGR" $ do
  --   it "parses reset" $
  --     "\x27[0;m" `parsesTo` Cmd (SGR [DefaultColor])
  -- describe "C1" $ do
  --   it "parses index" $
  --     "\x27\&D" `parsesTo` Cmd (C1 Index)
