{
module Negatron.Parser.Generator where
import Negatron.Scanner.Generator
import Negatron.Ast
import Data.Text (pack)
import Prelude hiding (fst, snd)
import qualified Data.Map.Strict as Map
}

%name parse
%tokentype { Lexeme }
%error { parseError }

%token
  int    { LInt   $$ }
  id     { LId    $$ }
  ptype  { LType  $$ }
  string { LStrLit $$ }
  bool   { LBool  $$ }
  null   { LNull }
  return { LRet }
  '='    { LAssign }
  ','    { LComma }
  ';'    { LSemi }
  '('    { LPAREN }
  ')'    { RPAREN }
  '{'    { LBRACE }
  '}'    { RBRACE }
  while  { LWhile }
  if     { LIf }
  else   { LElse }
  '-'    { LSub }
  '*'    { LMul }
  '/'    { LDiv }
  '=='   { LEqual }
  '!='   { LNeq }
  '<'    { LLess }
  '<='   { LLeq }
  '>'    { LGreater }
  '>='   { LGeq }
  '&&'   { LAnd }
  '||'   { LOr  }
  '!'    { LNot }
  output { LOutput }
  input  { LInput }

%nonassoc NOELSE
%nonassoc else
%right '='
%left '||'
%left '&&'
%left '==' '!='
%left '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'
%right '!' NEG

%%

program:
  decls { Program (Map.fromList $ map (\x-> (name x, x)) $ reverse $ snd $1) (reverse $ fst $1)  }

decls :: { ([Decl], [Function]) }
decls:
   {- empty -} { ([], []) }
 | decls vdecl { (($2 : fst $1), (snd $1)) }
 | decls fdecl { ((fst $1), ($2 : snd $1)) }

fdecl :: { Function }
fdecl:
   typ id '(' formals_opt ')' '{' stmt_list '}'
     { Function { typ = $1,
         name = pack $2,
         formals = $4,
         locals = [],
         body = reverse $7 } }

formals_opt:
    {- empty -} { [] }
  | formal_list   { reverse $1 }

formal_list:
    typ id                   { [Decl $1 (pack $2)] }
  | formal_list ',' typ id { Decl $3 (pack $4) : $1 }

typ:
  ptype { $1 }

vdecl:
   typ id ';' { Decl $1 (pack $2) }

stmt_list:
    {- empty -}  { [] }
  | stmt_list stmt { $2 : $1 }

stmt :: { Statement }
stmt:
    expr ';' { Expr $1 }
  | return ';' { Return Noexpr }
  | return expr ';' { Return $2 }
  | '{' stmt_list '}' { Block (reverse $2) }
  | if '(' expr ')' stmt %prec NOELSE { If $3 $5 (Block []) }
  | if '(' expr ')' stmt else stmt    { If $3 $5 $7 }
  | while '(' expr ')' stmt { While $3 $5 }
  | vdecl { StmtDecl $1 }
  | output expr ';' { Output $2 }
  | input id ';'    { Input (pack $2) }

expr:
    int                    { IntLit $1 }
  | string                 { StrLit (pack $1) }
  | bool                   { BoolLit $1 }
  | null                   { Null }
  | id                     { Id (pack $1) }
  | expr '-'  expr         { Bin  Sub  $1 $3 }
  | expr '*'  expr         { Bin  Mult $1 $3 }
  | expr '/'  expr         { Bin  Div  $1 $3 }
  | expr '==' expr         { Bin  Eq $1 $3 }
  | expr '!=' expr         { Bin  Neq  $1 $3 }
  | expr '<'  expr         { Bin  Less $1 $3 }
  | expr '<=' expr         { Bin  Leq  $1 $3 }
  | expr '>'  expr         { Bin  Greater $1 $3 }
  | expr '>=' expr         { Bin  Geq  $1 $3 }
  | expr '&&' expr         { Bin  And  $1 $3 }
  | expr '||' expr         { Bin  Or   $1 $3 }
  | '-' expr %prec NEG     { Un Neg $2 }
  | '!' expr               { Un Not $2 }
  | id '=' expr            { Assign (pack $1) $3 }
  | id '(' actuals_opt ')' { Call (pack $1) $3 }
  | '(' expr ')'           { $2 }

actuals_opt:
    {- empty -} { [] }
  | actuals_list  { reverse $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list ',' expr { $3 : $1 }

{
parseError _ = error "Unable to parse tokens"
fst (a, _) = a
snd (_, b) = b
}
