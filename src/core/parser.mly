%{
open Elements
%}

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> STRING
%token SEMICOL COMMA ASSIGN
%token OP_PLUS OP_MINUS OP_TIMES OP_DIV
       OP_EQ OP_NEQ OP_LNOT OP_LAND OP_LOR
%token LPAREN RPAREN LCURLY RCURLY
%token <string> IDENT
%token EOL
%token KWD_ELSE KWD_FUNCTION KWD_IF KWD_NULL KWD_RETURN
       KWD_FOR

%left OP_LAND OP_LOR
%left OP_EQ OP_NEQ
%left OP_PLUS OP_MINUS
%left OP_TIMES OP_DIV
%nonassoc OP_UMINUS OP_LNOT

%start main
%type <Elements.stmt> main
%type <Elements.expr> expr
%%
main:
  stmt SEMICOL { $1 }
;

stmts:
  | { [] }
  | stmt SEMICOL { [ $1 ] }
  | stmt SEMICOL stmts { $1 :: $3 }
;

stmt:
  |       { StmtEmpty }
  | expr  { StmtExpr $1 }
  | IDENT ASSIGN expr { StmtAssign ( $1, $3 ) }
  | KWD_RETURN expr { StmtReturn $2 }
  | for_stmt { $1 }
;

for_stmt:
  | KWD_FOR LPAREN stmt SEMICOL SEMICOL stmt RPAREN for_body { 
      StmtFor {
        init_stmt = $3 ;
        cond = Bool true ;
        postexec_stmt = $6 ;
        body = $8 ;
      }
    }
  | KWD_FOR LPAREN stmt SEMICOL expr SEMICOL stmt RPAREN for_body { 
      StmtFor {
        init_stmt = $3 ;
        cond = $5 ;
        postexec_stmt = $7 ;
        body = $9 ;
      }
    }
;

for_body:
  | stmt { [ $1 ] }
  | LCURLY stmts RCURLY { $2 }
;

expr:
  | KWD_NULL                { Null }
  | BOOL                    { Bool $1 }
  | INT                     { Int $1 }
  | FLOAT                   { Float $1 }
  | STRING                  { String $1 }
  | IDENT                   { Ident $1 }
  | LPAREN expr RPAREN      { $2 }
  | arith                   { $1 }
  | logic                   { $1 }
  | func                    { $1 }
  | call                    { $1 }
  | if_expr                 { $1 }
;

arith:
  | expr OP_PLUS expr          { Plus ( $1 , $3 ) }
  | expr OP_MINUS expr         { Minus ( $1 , $3 ) }
  | expr OP_TIMES expr         { Times ( $1 , $3 ) }
  | expr OP_DIV expr           { Div ( $1 , $3 ) }
  | OP_MINUS expr %prec OP_UMINUS { UMinus $2 }
;

logic:
  | OP_LNOT expr                { Not ( $2 ) }
  | expr OP_EQ expr            { Eq ( $1 , $3 ) }
  | expr OP_NEQ expr           { Neq ( $1 , $3 ) }
  | expr OP_LAND expr          { LAnd ( $1 , $3 ) }
  | expr OP_LOR expr           { LOr ( $1 , $3 ) }
;

if_expr:
  | KWD_IF LPAREN expr RPAREN if_body {
      If {
        condition = $3 ;
        ifstmts = if List.length $5 == 0 then [StmtExpr Null] else $5 ;
        elsestmts = [StmtExpr Null];
      }
    }
  | KWD_IF LPAREN expr RPAREN if_body KWD_ELSE if_body {
      If {
        condition = $3 ;
        ifstmts = if List.length $5 == 0 then [StmtExpr Null] else $5 ;
        elsestmts = if List.length $7 == 0 then [StmtExpr Null] else $7 ;
      }
    }
;

if_body:
  | expr { [StmtExpr $1] }
  | LCURLY stmts RCURLY  { $2 }
;

call:
  | expr LPAREN params RPAREN { Call { expr = $1 ; params = $3 } }
;

params:
  |                   { [] }
  | expr              { [ $1 ] }
  | expr COMMA params { $1 :: $3 }
;

func:
  | KWD_FUNCTION       arg_list func_body { Fun { name = None; args = $2 ; body = $3 } }
  | KWD_FUNCTION IDENT arg_list func_body { Fun { name = Some $2 ; args = $3 ; body = $4 } }
;

arg_list:
  LPAREN args RPAREN  { $2 }
;

func_body:
  LCURLY stmts RCURLY { $2 }
;

args:
  |                   { [] }
  | IDENT             { [ $1 ] }
  | IDENT COMMA args  { $1 :: $3 }
;
