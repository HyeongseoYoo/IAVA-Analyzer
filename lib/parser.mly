%{
open Syntax
open Exp
%}

(* ===== tokens ===== *)

%token INIT MAIN HANDLER
%token ENABLE DISABLE UNIT
%token IF THEN ELSE
%token WHILE DO
%token LET IN
%token MALLOC

%token <int> INT
%token <string> ID

%token LBRACE RBRACE
%token LPAREN RPAREN
%token LBRACK RBRACK
%token COMMA
%token SEMI
%token COLONEQ
%token AMP
%token EQ LT GT NE LE GE
%token MINUS PLUS STAR

%token EOF

(* ===== precedence ===== *)

%right SEMI
%right COLONEQ
%left EQ LT GT NE LE GE
%left PLUS MINUS
%nonassoc STAR

(* ===== start symbol ===== *)


%start <Syntax.Program.t> prog
%type <Exp.t> exp
%%

(* ===== program ===== *)

prog:
  INIT LBRACE ib = init_body RBRACE
  MAIN LBRACE m = exp RBRACE
  EOF
    {
      let (globals, handlers) = ib in
      {
        Program.init = { Init.globals; handlers };
        main = m;
      }
    }
;

(* ===== init ===== *)

init_body:
/* empty */ { (Exp.Unit, []) }
| exp_seq handler_defs
    { ($1, $2) }
;

exp_seq:
  | exp { $1 }
  | exp SEMI exp_seq { Exp.Seq ($1, $3) }
  | exp SEMI { $1 }
;

handler_defs:
/* empty */  { [] }
| handler_def { [$1] }
| handler_def SEMI handler_defs { $1 :: $3 }
;

handler_def:
  HANDLER INT LBRACE exp RBRACE { Handler.{ iid = $2; body = $4 } }
;


(* ===== expressions ===== *)

exp:
    | exp SEMI exp { Exp.Seq ($1, $3) }
    | exp COLONEQ exp { Exp.Assign ($1, $3) }
    | IF exp THEN exp ELSE exp { Exp.If ($2, $4, $6) }
    | WHILE exp DO exp { Exp.While ($2, $4) }
    | LET ID EQ exp IN exp { Exp.Let ($2, $4, $6) }
    | exp PLUS exp { Exp.Bop (Exp.Plus, $1, $3) }
    | exp EQ exp { Exp.Bop (Exp.Eq, $1, $3) }
    | STAR exp LBRACK exp RBRACK { Exp.Deref ($2, $4) }
    | MALLOC LPAREN exp COMMA exp RPAREN { Exp.Malloc ($3, $5) }
    | atom { $1 }

(* ===== atoms ===== *)
atom:
    | UNIT { Unit }
    | ENABLE { Enable }
    | DISABLE { Disable }
    | INT { Int $1 }
    | ID { Var $1 }
    | LPAREN exp RPAREN { $2 } ;