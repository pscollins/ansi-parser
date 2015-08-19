module Main where

import Test.Hspec

import AnsiParser.Types
import AnsiParser.Classes
import AnsiParser.FrontEnd.Lex
import AnsiParser.FrontEnd.Parse

import Control.Monad

import Debug.Trace

parse :: String -> Either String [Expr]
parse s = trace ("Parsing: " ++ show res) res
  where res = parseExpr s

lexStr :: String -> Either String [Token]
lexStr = alexScanTokens

lexTo :: String -> [Token] -> Expectation
lexTo inp outp = lexStr inp `shouldBe` Right outp

noLex :: String -> String -> Expectation
noLex inp outp = lexStr inp `shouldBe` Left outp

parseToEs :: String -> [Expr] -> Expectation
parseToEs s r = parse s `shouldBe` Right r

parseTo :: String -> Expr -> Expectation
parseTo s r = s `parseToEs` [r]

mkSimpleTest :: ToTerminal a => (a -> Expr) -> a -> SpecWith ()
mkSimpleTest toExpr expected =
  it ("parses " ++ show toParse) $
  toParse `parseTo`  toExpr expected
  where toParse = toTerminal expected


main :: IO ()
main = hspec $ do
  describe "lexer" $ do
    it "lexes the empty string" $
      "" `lexTo` [TokenEOF]
    it "lexes an escape char" $
      "\x1b" `lexTo` [TokenEsc, TokenEOF]
    it "lexes an esc with other stuff" $
      "\x1b\&15;" `lexTo` [TokenEsc, TokenNum 15, TokenSep, TokenEOF]
    it "lexes a color command" $
      "\x1b[\&15;8m" `lexTo` [ TokenEsc, TokenLBracket, TokenNum 15
                             , TokenSep, TokenNum 8, TokenEndCSI 'm' Nothing
                             , TokenEOF ]

    it "lexes a single char function" $
      "\x1b\&D" `lexTo` [TokenEsc, TokenCharFunc 'D', TokenEOF]
    it "lexes a nonprinting char" $
      "\x7" `lexTo` [TokenNonPrint '\x7', TokenEOF]
    it "lexes an ANSI set command" $
      "\x1b F" `lexTo` [TokenEsc, TokenSP, TokenAnsiSet 'F', TokenEOF]
    it "lexes a DEC line command" $
      "\x1b#3" `lexTo` [TokenEsc, TokenHash, TokenDECLine 3, TokenEOF]
    it "lexes a simple char set command" $
      "\x1b%@" `lexTo` [ TokenEsc, TokenPercent, TokenSimpleCharSet '@'
                       , TokenEOF]
    it "lexes a char set command" $
      "\x1b(0" `lexTo` [TokenEsc, TokenLParen, TokenCharSet "0", TokenEOF]
    it "lexes an extended char set command" $
      "\x1b/A" `lexTo` [TokenEsc, TokenDiv, TokenCharSet "A", TokenEOF]
    it "lexes a cursor command" $
      "\x1b\&6" `lexTo` [TokenEsc, TokenCursorCmd '6', TokenEOF]
    it "lexes an invoke char set" $
      "\x1bn" `lexTo` [TokenEsc, TokenInvokeCharSet 'n', TokenEOF]
    it "lexes a user defined key command" $
      "\x1bP10;11|foo\x1b\&\\" `lexTo` [ TokenEsc, TokenDCS, TokenNum 10
                                       , TokenSep, TokenNum 11 , TokenPipe
                                       , TokenStringParam "foo"
                                       , TokenStringTerm, TokenEOF]
    it "lexes a request status string" $
      "\x1BP$qr\x1b\&\\" `lexTo` [ TokenEsc, TokenDCS, TokenDollar
                                 , TokenStringParam "qr", TokenStringTerm
                                 , TokenEOF ]
    it "lexes a get/set termcap string" $
      "\x1bP+pFOO\x1b\&\\" `lexTo` [ TokenEsc, TokenDCS, TokenPlus
                                   , TokenStringParam "pFOO"
                                   , TokenStringTerm, TokenEOF ]
    it "lexes a CSI command" $
      "\x1b[10@" `lexTo` [ TokenEsc, TokenLBracket, TokenNum 10
                           , TokenEndCSI '@' Nothing, TokenEOF ]
    it "lexes plain text" $
      "Hello World" `lexTo` [TokenPlain "Hello World", TokenEOF]




  describe "plain text" $ do
    it "parses the empty string" $
      "" `parseToEs` []
    it "parses a non-empty string" $
      "Hello World" `parseTo` Plain "Hello World"
  describe "C1 commands" $
    mapM_ (mkSimpleTest (Cmd . C1)) [Index .. APC]
  describe "nonprinting characters" $
    mapM_ (mkSimpleTest (Cmd . NonPrint)) [Bell .. VerticalTab]
  describe "cmd" $ do
    it "parses a nonprinting character" $
      "\x7" `parseTo` Cmd (NonPrint Bell)
  describe "SGR" $ do
    it "parses reset" $
      "\x1b[0;m" `parseTo` Cmd (SGR [DefaultColor])
  describe "C1" $ do
    it "parses index" $
      "\x1b\&D" `parseTo` Cmd (C1 Index)
