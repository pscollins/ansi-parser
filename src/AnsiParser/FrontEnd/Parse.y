{
module AnsiParser.FrontEnd.Parse where

import AnsiParser.Types
import AnsiParser.FrontEnd.Lex
}

%name expr
%tokentype { Token }
%error { parseError }

%token
  '\x27' { TokenEsc }
  ';' { TokenSep }
  '[' { TokenLBracket }
  ']' { TokenRBracket }
  PLAIN { TokenPlain $$ }
  NUM { TokenNum $$ }
  CHAR { TokenChar $$ }

%%

-- We should use left recursion for better performance, but it
-- confuses me
exprs :: { [Expr] }
exprs
  : {- empty -} { [] }
  | expr exprs { $1 : $2 }


expr :: { Expr }
expr
  : PLAIN { Word $1 }
  | '\x27' cmd { Cmd $2 }



cmd :: { Cmd }
cmd
  : '[' csi { undefined }
  | ']' osc { undefined }
  | CHAR { C1 $ parseChar $1 }


csi :: { Cmd }
csi : { undefined }

osc :: { Cmd }
osc : { undefined }


{
parseChar :: Char -> C1
parseChar 'D' = Index
parseChar 'E' = NextLine
parseChar 'H' = TabSet
parseChar  _ = undefined -- TODO

parseError = undefined
scanTokens = alexScanTokens
}
