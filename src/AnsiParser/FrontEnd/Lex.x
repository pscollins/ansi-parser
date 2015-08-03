{
module AnsiParser.FrontEnd.Lex where

import Debug.Trace
}

-- Following http://invisible-island.net/xterm/ctlseqs/ctlseqs.html

%wrapper "monad"

$digit = 0-9
$esc = \x027
$sep = \;


tokens :-
  $esc { mkT' TokenEsc `andBegin` ctrl }
  <ctrl> $sep { mkT' TokenSep }
  <ctrl> $digit+ { mkT $ TokenNum . read }
  <ctrl> m { mkT' TokenEndColorCmd `andBegin` 0 }
  <ctrl> [DEHMNOPVWXZ\^_] { mkT $ TokenChar . read }
  [^$esc]+ { mkT TokenPlain } -- FIXME: play better with controlcha r
{
data Token
  = TokenEsc
  | TokenSep
  | TokenLBracket
  | TokenRBracket
  | TokenChar Char
  | TokenPlain String
  | TokenNum Int
  | TokenEndColorCmd
  | TokenEOF
  deriving (Show)

scanTokens ::  Alex Token
scanTokens = alexMonadScan

lexWrap :: (Token -> Alex a) -> Alex a
lexWrap = (alexMonadScan >>=)

mkT :: (AlexInput -> Token) -> AlexInput -> Int -> Alex Token
mkT tokFn toTok _ _ = return $ tokFn toTok

mkT' :: Token -> AlexInput -> Int -> Alex Token
mkT' tok = mkT (const tok)

alexEOF :: Alex Token
alexEOF = return TokenEOF
}
