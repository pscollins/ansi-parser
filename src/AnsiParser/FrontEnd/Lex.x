{
module AnsiParser.FrontEnd.Lex where

import Control.Monad

import Debug.Trace
}

-- Following http://invisible-island.net/xterm/ctlseqs/ctlseqs.html

%wrapper "monad"

$esc = \x027
$sep = \;
$sp = \ -- a literal space character
$lp = \(
$rp = \)

@digit = [0-9]
@nonprint = [\x0-\x1f]
@print = [\ -\~]
@singlecharfunc = [DEHMNOVWXZ\^_]
@ansiset = [FGLMN]
@charsetcmd = [0\<\>AB4CRQKY`E65f9ZH7=]|(\%(5|6))



tokens :-
  $esc { mkT' TokenEsc `andBegin` ctrl }
  @nonprint { mkTChar TokenNonPrint }

  <ctrl>  @singlecharfunc {mkTChar TokenCharFunc `andBegin` 0}

  <ctrl> $sp { mkT' TokenSP  `andBegin` ansiset }
  <ansiset> @ansiset { mkTChar TokenAnsiSet  `andBegin` 0 }

  <ctrl> \# {mkT' TokenHash `andBegin` decline}
  <decline> [34568] {mkTRead TokenDECLine `andBegin` 0}

  <ctrl> \% {mkT' TokenPercent `andBegin` simplecharset}
  <simplecharset> [\@G] {mkTChar TokenSimpleCharSet `andBegin` 0}

  <ctrl> $lp {mkT' TokenLParen `andBegin` charset}
  <ctrl> $rp {mkT' TokenRParen `andBegin` charset}
  <ctrl> \* {mkT' TokenStar `andBegin` charset}
  <ctrl> \+ {mkT' TokenPlus `andBegin` charset}
  <ctrl> \- {mkT' TokenMinus `andBegin` charset}
  <ctrl> \. {mkT' TokenDot `andBegin` charset}
  <ctrl> \/ {mkT' TokenDiv `andBegin` largecharset}

  <charset, largecharset> @charsetcmd {mkT TokenCharSet `andBegin` 0}
  <largecharset> A {mkT TokenCharSet `andBegin` 0}

  <ctrl> [6789=>Fclm] {mkTChar TokenCursorCmd `andBegin` 0}
  <ctrl> [no\|\}\~] {mkTChar TokenInvokeCharSet `andBegin` 0}

  <ctrl> \[ { mkT' TokenLBracket `andBegin` csi }
  <csi> m { mkT' TokenEndColorCmd `andBegin` 0 }

  <0> \x27P {mkT' TokenDCS `andBegin` dcs}

  <dcs> \| {mkT' TokenPipe `andBegin` string}

  <string> @print+ {mkT TokenStringParam `andBegin` 0}
  <string> \x27\\ {mkT' TokenStringTerm `andBegin` 0}

  <ctrl,csi,dcs> $sep { mkT' TokenSep }
  <ctrl,csi,dcs> @digit+ { mkT $ TokenNum . read }

  <0> [^$esc]+ { mkT TokenPlain }
{
data Token
  = TokenEsc
  | TokenSep
  | TokenLBracket
  | TokenRBracket
  | TokenCharFunc Char
  | TokenNonPrint Char
  | TokenAnsiSet Char
  | TokenSimpleCharSet Char
  | TokenCharSet String
  | TokenPlain String
  | TokenNum Int
  | TokenSP
  | TokenPercent
  | TokenHash
  | TokenLParen
  | TokenRParen
  | TokenStar
  | TokenPlus
  | TokenMinus
  | TokenDot
  | TokenDiv
  | TokenDECLine Int
  | TokenCursorCmd Char
  | TokenInvokeCharSet Char
  | TokenEndColorCmd
  | TokenStringParam String
  | TokenPipe
  | TokenStringTerm
  | TokenDCS
  | TokenEOF
  deriving (Show, Eq)

scanToken ::  Alex Token
scanToken = alexMonadScan

alexScanTokens :: String -> Either String [Token]
alexScanTokens inp = runAlex inp gather
  where
  gather = alexMonadScan >>= collate
  collate TokenEOF = return [TokenEOF]
  collate t = (t:) `liftM` gather

lexWrap :: (Token -> Alex a) -> Alex a
lexWrap = (alexMonadScan >>=)

traceThrough :: Show a => a -> a
traceThrough x = trace ("Processing: " ++ show x) x

tokenContents :: AlexInput -> Int -> String
tokenContents (_, _, _, str) len = traceThrough $ take len str

mkT :: (String -> Token) -> AlexInput -> Int -> Alex Token
mkT tokFn toTok len = return $ tokFn tokContents
  where tokContents = tokenContents toTok len

mkT' :: Token -> AlexInput -> Int -> Alex Token
mkT' tok = mkT (const tok)

mkTRead :: Read a => (a -> Token) -> AlexInput -> Int -> Alex Token
mkTRead = mkT . (. read)

mkTChar :: (Char -> Token) -> AlexInput -> Int -> Alex Token
mkTChar = mkT . (. head)


alexEOF :: Alex Token
alexEOF = return TokenEOF
}
