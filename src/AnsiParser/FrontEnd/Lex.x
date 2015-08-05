{
module AnsiParser.FrontEnd.Lex where

import Control.Monad

import Debug.Trace
}

-- Following http://invisible-island.net/xterm/ctlseqs/ctlseqs.html

%wrapper "monad"

$esc = \x027
$sep = \;


@digit = [0-9]
@nonprint = [\x0-\x1f]
@singlecharfunc = [DEHMNOPVWXZ\^_]


tokens :-
  $esc { mkT' TokenEsc `andBegin` ctrl }

  @nonprint { mkT $ TokenNonPrint . head }

  <ctrl> \[ { mkT' TokenLBracket `andBegin` csi }


  <ctrl,csi> $sep { mkT' TokenSep }
  <ctrl,csi> @digit+ { mkT $ TokenNum . read }

  <csi> m { mkT' TokenEndColorCmd `andBegin` 0 }

  <ctrl>  @singlecharfunc { mkT $ TokenChar . head }

  <0> [^$esc]+ { mkT TokenPlain } -- FIXME: play better with controlcha r
{
data Token
  = TokenEsc
  | TokenSep
  | TokenLBracket
  | TokenRBracket
  | TokenChar Char
  | TokenNonPrint Char
  | TokenPlain String
  | TokenNum Int
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

alexEOF :: Alex Token
alexEOF = return TokenEOF
}
