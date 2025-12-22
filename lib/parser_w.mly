%{
open Syntax
open Exp
open Cmd

%}

%token <int> INT
%token <string> ID
%token COLONEQ AMP
%token READINT
%token REPEAT WHILE GOTO
%token IF THEN ELSE
%token EQ LT GT NE LE GE
%token PLUS MINUS AST
%token LPAREN RPAREN
%token SEMI
%token EOF

%right    SEMI
%nonassoc ELSE /* (if ... then ... else ...) */
%left     EQ LT GT NE LE GE
%left     PLUS MINUS
%left     AST
%nonassoc prec_unary

%start <Cmd.lbl_t> prog
%type <Exp.t> exp
%%

%inline mkcmd(symb): symb { Cmd.{lbl = 0; cmd = $1 } }

prog:
    | c = cmd; SEMI?; EOF { Cmd.relabel c }
cmd:
    | LPAREN; c = cmd; RPAREN
      { c }
    | mkcmd(AST; x = ID; COLONEQ; e = exp                           { Update_at (x, e) })
      { $1 }
    | mkcmd(x = ID; COLONEQ; e = exp                                { Assign (x, e) })
      { $1 }
    | mkcmd(c1 = cmd; SEMI; c2 = cmd                                { Seq (c1, c2) })
      { $1 }
    | mkcmd(IF; pred = exp; THEN; con = cmd; SEMI?; ELSE; alt = cmd { Cond (pred, con, alt) })
      { $1 }
    | mkcmd(REPEAT; c = cmd; SEMI?; WHILE; e = exp                  { Repeat (c, 0, e) })
      { $1 }
    | mkcmd(GOTO; e = exp                                           { Goto e })
      { $1 }
exp:
    | atom                               { $1 }
    | AMP; x = ID                        { Loc x }
    | AST; x = ID                        { At x }
    | op = uop; e = exp %prec prec_unary { Uop (op, e) }
    | e1 = exp; op = bop; e2 = exp       { Bop (op, e1, e2) }
%inline uop:
    | PLUS  { Uplus }
    | MINUS { Uminus }
%inline bop:
    | EQ    { Eq }
    | LT    { Lt }
    | GT    { Gt }
    | NE    { Ne }
    | LE    { Le }
    | GE    { Ge }
    | PLUS  { Plus }
    | MINUS { Minus }
    | AST   { Times }
atom:
    | n = INT                 { Int n }
    | x = ID                  { Var x }
    | READINT                 { Read_int }
    | LPAREN; e = exp; RPAREN { e }
