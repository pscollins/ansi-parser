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


@digit = [0-9]
@nonprint = [\x0-\x1f]
@singlecharfunc = [DEHMNOPVWXZ\^_]
@ansiset = [FGLMN]



tokens :-
  $esc { mkT' TokenEsc `andBegin` ctrl }
  @nonprint { mkTChar TokenNonPrint }

  <ctrl>  @singlecharfunc { mkTChar TokenCharFunc }

  <ctrl> $sp { mkT' TokenSP  `andBegin` ansiset }
  <ansiset> @ansiset { mkTChar TokenAnsiSet  `andBegin` 0 }

  <ctrl> \# {mkT' TokenHash `andBegin` decline}
  <decline> [34568] {mkTRead TokenAnsiSet `andBegin` 0}

  <ctrl> \% {mkT' TokenPercent `andBegin` simplecharset}
  <simplecharset> [\@G] {mkTChar TokenCharSet `andBegin` 0}

  <ctrl> \[ { mkT' TokenLBracket `andBegin` csi }
  <csi> m { mkT' TokenEndColorCmd `andBegin` 0 }

  <ctrl,csi> $sep { mkT' TokenSep }
  <ctrl,csi> @digit+ { mkT $ TokenNum . read }

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
  | TokenCharSet Char
  | TokenPlain String
  | TokenNum Int
  | TokenSP
  | TokenPercent
  | TokenHash
  | TokenEndColorCmd
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

mkTRead :: (String -> Token) -> AlexInput -> Int -> Alex Token
mkTRead = mkT . (. read)

mkTChar :: (Char -> Token) -> AlexInput -> Int -> Alex Token
mkTChar = mkT . (. head)


alexEOF :: Alex Token
alexEOF = return TokenEOF
}
