%{
open Syntax
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
%token OR AND
%token EQ LT GT NE LE GE
%token MINUS PLUS STAR

%token EOF

(* ===== precedence ===== *)

%right SEMI
%right COLONEQ
%left OR
%left AND
%left EQ LT GT NE LE GE
%left PLUS MINUS
%left STAR

(* ===== start symbol ===== *)


%start <Program.t> prog
%type  <Exp.lbl_t> exp
%type  <Exp.lbl_t * (Handler.t list)> init_body
%type  <Exp.lbl_t> exp_seq
%type  <Handler.t list> handler_defs
%type  <Handler.t> handler_def
%%

%inline mkexp(symb): symb { Exp.{lbl = Exp.Lbl.Init 0; exp = $1 } }

(* ===== program ===== *)

prog:
  INIT LBRACE ib = init_body RBRACE
  MAIN LBRACE m0 = exp RBRACE
  EOF
    {
      let (g0, h0) = ib in
      let g = Exp.relabel g0 (Exp.Lbl.Init 0) in
      let m = Exp.relabel m0 (Exp.Lbl.Main 0) in
      let hl =
      h0
      |> List.map (fun h ->
           let body' = Exp.relabel (Handler.get_body h) (Exp.Lbl.Handler (Handler.get_iid h, 0)) in
           { h with body = body' }) in
      {
        Program.global = g;
        Program.handler = hl;
        Program.main = m;
      }
    }
;

(* ===== init ===== *)


init_body:
/* empty */ { (Exp.{lbl = Exp.Lbl.Init 0; exp = Exp.Unit}, []) }
| exp_seq handler_defs
    { ($1, $2) }
;

exp_seq:
  | exp { $1 }
  | exp SEMI exp_seq { Exp.{ lbl = Exp.Lbl.Init 0; exp = Exp.Seq ($1, $3)} }
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
    | LPAREN e = exp RPAREN { e }
    (* atoms *)
    | mkexp(UNIT { Exp.Unit })
      { $1 }
    | mkexp(n = INT { Exp.Int n })
      { $1 }
    | mkexp(x = ID { Exp.Var x })
      { $1 }
    | mkexp(ENABLE { Exp.Enable })
      { $1 }
    | mkexp(DISABLE { Exp.Disable })
      { $1 }

    (* binary operators *)
    | mkexp(e1 = exp; EQ;   e2 = exp { Exp.Bop (Exp.Eq,   e1, e2) })
      { $1 }
    | mkexp(e1 = exp; LT;   e2 = exp { Exp.Bop (Exp.Lt,   e1, e2) })
      { $1 }
    | mkexp(e1 = exp; GT;   e2 = exp { Exp.Bop (Exp.Gt,   e1, e2) })
      { $1 }
    | mkexp(e1 = exp; NE;   e2 = exp { Exp.Bop (Exp.Ne,   e1, e2) })
      { $1 }
    | mkexp(e1 = exp; LE;   e2 = exp { Exp.Bop (Exp.Le,   e1, e2) })
      { $1 }
    | mkexp(e1 = exp; GE;   e2 = exp { Exp.Bop (Exp.Ge,   e1, e2) })
      { $1 }
    | mkexp(e1 = exp; PLUS; e2 = exp { Exp.Bop (Exp.Plus, e1, e2) })
      { $1 }
    | mkexp(e1 = exp; MINUS; e2 = exp { Exp.Bop (Exp.Minus, e1, e2) })
      { $1 }
    | mkexp(e1 = exp; STAR; e2 = exp { Exp.Bop (Exp.Times, e1, e2) })
      { $1 }
    | mkexp(e1 = exp; AND; e2 = exp { Exp.Bop (Exp.And, e1, e2) })
      { $1 }
    | mkexp(e1 = exp; OR; e2 = exp { Exp.Bop (Exp.Or, e1, e2) })
      { $1 }

    (* heap *)
    | mkexp(STAR; base = exp; LBRACK; idx = exp; RBRACK { Exp.Deref (base, idx) })
      { $1 }

    | mkexp(MALLOC; LPAREN; e1 = exp; COMMA; e2 = exp; RPAREN { Exp.Malloc (e1, e2) })
      { $1 }

    (* commands-as-expressions *)
    | mkexp(e1 = exp; COLONEQ; e2 = exp { Exp.Assign (e1, e2) })
      { $1 }

    | mkexp(e1 = exp; SEMI; e2 = exp { Exp.Seq (e1, e2) })
      { $1 }

    (* control *)
    | mkexp(IF; c = exp; THEN; t = exp; ELSE; f = exp { Exp.If (c, t, f) })
      { $1 }

    | mkexp(WHILE; c = exp; DO; LPAREN; body = exp  ; RPAREN { Exp.While (Exp.Lbl.Init 0, c ,body) })
      { $1 }

    (* let-binding *)
    // | mkexp(LET; x = ID; EQ; e1 = exp; IN; e2 = exp { Exp.Let (x, e1, e2) })
    //   { $1 }
