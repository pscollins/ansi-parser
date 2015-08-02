{
module AnsiParser.FrontEnd.Lex where
}

-- Following http://invisible-island.net/xterm/ctlseqs/ctlseqs.html

%wrapper "monad"

$digit = 0-9
$esc = \x027
$sep = \;


tokens :-
  $esc { \s -> TokenEsc }
  $sep { \s -> TokenSep }
  [^$esc]+ { \s -> TokenPlain s } -- FIXME: play better with controlchar
  $digit+ { \s -> TokenNum $ read s }
  m { \s -> TokenEndColorCmd }
  [DEHMNOPVWXZ\^_] { \s -> TokenChar $ read s }

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

scanTokens :: Alex Token
scanTokens = alexMonadScan

lexWrap :: (Token -> Alex a) -> Alex a
lexWrap = (alexMonadScan >>=)

alexEOF :: Alex Token
alexEOF = return TokenEOF
}
