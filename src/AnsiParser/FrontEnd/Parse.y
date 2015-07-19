{
module AnsiParser.FrontEnd.Parse where

import AnsiParser.Types
import AnsiParser.FrontEnd.Lex
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  '\x27' { TokenEsc }
  ';' { TokenSep }
  '[' { TokenLBracket }
  ']' { TokenRBracket }
  'm' { TokenEndColorCmd }
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
  | '\x27' cmd { Cmd $2 }

numparams :: { [Int] }
numparams
  : {- empty -} { [] }
  | NUM ';' numparams { $1 : $3 }

colorcmd :: { [ColorCmd] }
colorcmd
  : numparams 'm' { map parseColorParam $1 }

cmd :: { Cmd }
cmd
  : '[' csi { $2 }
  | CHAR { C1 $ parseChar $1 }
  -- | ']' osc { undefined }



csi :: { Cmd }
csi
  : colorcmd { SGR $1 }

osc :: { Cmd }
osc : { undefined }


{
parseChar :: Char -> C1
parseChar 'D' = Index
parseChar 'E' = NextLine
parseChar 'H' = TabSet
parseChar  _ = undefined -- TODO


parseColorParam :: Int -> ColorCmd
parseColorParam 0 = DefaultColor

parseError tokens = error ("Error!" ++ show tokens)


}
