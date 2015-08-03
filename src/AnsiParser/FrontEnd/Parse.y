{
module AnsiParser.FrontEnd.Parse where

import AnsiParser.Types
import AnsiParser.FrontEnd.Lex

import Debug.Trace
}

%name parseTokens
%tokentype { Token }
%lexer { lexWrap } { alexEOF }
%monad { Alex }
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
  : {- empty -} { trace "exprs 30" [] }
  | expr exprs { trace "exprs 31" $ $1 : $2 }


expr :: { Expr }
expr
  : '\x27'cmd { trace "expr 36" $ Cmd $2 }
  | PLAIN { trace "expr 37" $ Plain $1 }


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

parseError :: Token -> Alex a
parseError tokens = error ("Error!" ++ show tokens)

-- parseExpr :: String -> [Expr]
-- parseExpr = parseTokens . scanTokens

parseExpr :: String -> Either String [Expr]
parseExpr s = runAlex s parseTokens
}
