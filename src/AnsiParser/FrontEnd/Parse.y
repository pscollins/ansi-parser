{
module AnsiParser.FrontEnd.Parse where

import AnsiParser.Types
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
  : PLAIN { Plain $1 }
  | '\x27' cmd { Cmd $1 }



cmd :: { Cmd }
cmd
  : '[' csi { undefined }
  | ']' osc { undefined }
  | CHAR { parseChar $1 }


csi :: { Cmd }
csi : { undefined }

osc :: { Cmd }
osc : { undefined }


{
parseChar :: TokenChar -> C1
parseChar 'D' = Index
parseChar 'E' = NextLine
parseChar 'H' = TabSet
parseChar  _ = undefined -- TODO

}
