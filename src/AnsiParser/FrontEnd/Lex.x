{
module Main (main) where
}

-- Following http://invisible-island.net/xterm/ctlseqs/ctlseqs.html

%wrapper "basic"

$digit = 0-9
$esc = \x027
$sep = ;


tokens :-
  $esc { \s -> TokenEsc }
  $sep { \s -> TokenSep }
  [^$esc]+ { \s -> TokenPlain s }
  $digit+ { \s -> TokenNum $ read s }



data Token
  = TokenEsc
  | TokenSep
  | TokenPlain String
  | TokenNum Int

-- Actually I'm not sure we can do this at the lexer level
