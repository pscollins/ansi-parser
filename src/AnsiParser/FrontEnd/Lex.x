{
module AnsiParser.FrontEnd.Lex where

import Control.Monad

import Debug.Trace
}

-- Following http://invisible-island.net/xterm/ctlseqs/ctlseqs.html

%wrapper "monad"

$esc = \x1b
$sep = \;
$sp = \ -- a literal space character
$lp = \(
$rp = \)
$nonprint = \x0-\x1f


@digit = [0-9]


@singlecharfunc = [DEHMNOVWXZ\^_]
@ansiset = [FGLMN]
@charsetcmd = [0\<\>AB4CRQKY`E65f9ZH7=]|(\%(5|6))
@endcsichar = [\@ABCDEFGHJKLMPSTXZ`abcdefghilmnpqrstux]
@startcsichar = [\?>!]

@enddollarcsi = [prtvxz]
@enddquotecsi = [pq]
@endspcsi = [qtu]
@endsquotecsi = [wz\{\|\}\~]
@endstarcsi = [xy]

@endcsipair = (\$@enddollarcsi)|(\"@enddquotecsi)|( @endspcsi)|(\'@endsquotecsi)(\*@endstarcsi)
-- " sigh



tokens :-
  <0> $esc { mkT' TokenEsc `andBegin` ctrl }
  $nonprint { mkTChar TokenNonPrint }

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

  <ctrl> P {mkT' TokenDCS `andBegin` dcs}

  <dcs> \| {mkT' TokenPipe `andBegin` string}
  <dcs> \$ {mkT' TokenDollar `andBegin` string}
  <dcs> \+ {mkT' TokenPlus `andBegin` string}

  <string> $printable+ {mkT TokenStringParam}
  <string> $esc\\ {mkT' TokenStringTerm `andBegin` 0}

  <ctrl> \[ { mkT' TokenLBracket `andBegin` csistart }

  -- CSI body-starters
  <csistart> @startcsichar {mkTChar TokenStartCSI `andBegin` csi}
  <csistart> @digit+ {mkTRead TokenNum `andBegin` csi}

  -- CSI can only be ended from within the body
  <csi> @endcsichar|@endcsipair {mkT tokenEndCSI `andBegin` 0}

  <ctrl,csi,dcs> $sep { mkT' TokenSep }
  <ctrl,csi,dcs> @digit+ { mkT $ TokenNum . read }

  <0> [^$esc]+ { mkT TokenPlain }
{
-- we'd probably be better off simplifying out all of these single-char ones
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
  | TokenEndCSI Char (Maybe Char)
  | TokenStartCSI Char
  | TokenStringParam String
  | TokenPipe
  | TokenStringTerm
  | TokenDollar
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

-- tokenEndCSI' :: Char -> Token
-- tokenEndCSI' c = TokenEndCSI c Nothing

tokenEndCSI :: String -> Token
tokenEndCSI (c:d:[]) = TokenEndCSI c $ Just d
tokenEndCSI (c:[]) = TokenEndCSI c Nothing
tokenEndCSI _ = undefined -- error

alexEOF :: Alex Token
alexEOF = return TokenEOF
}
