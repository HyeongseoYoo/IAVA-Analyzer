{
open Lexing
open Parser

exception SyntaxError of string

let comment_level = ref 0
let keywords =
  let tbl : (string, token) Hashtbl.t = Hashtbl.create 32 in
  let add_to_tbl (id, tok) = Hashtbl.add tbl id tok in
  List.iter add_to_tbl
    [
      ("init", INIT);
      ("main", MAIN);
      ("handler", HANDLER);
      ("enable", ENABLE);
      ("disable", DISABLE);
      ("unit", UNIT);
      ("if", IF);
      ("then", THEN);
      ("else", ELSE);
      ("while", WHILE);
      ("do", DO);
      ("let", LET);
      ("in", IN);
      ("malloc", MALLOC);
    ];
  tbl
}

let blank = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let whitespace = blank | newline
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let semi = ';' (whitespace ';')*

let digit = ['0'-'9']
let int = '-'? digit+

rule read =
  parse
  | blank     { read lexbuf }
  | newline   { new_line lexbuf; read lexbuf }
  | "(*"      { comment_level := 1; comment lexbuf; read lexbuf }
  | int as n  { INT (int_of_string n) }
  | id as s   { match Hashtbl.find_opt keywords s with Some s -> s | None -> ID s }
  | '='       { EQ }
  | '<'       { LT }
  | '>'       { GT }
  | "<>"      { NE }
  | "<="      { LE }
  | ">="      { GE }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { STAR }
  | ":="      { COLONEQ }
  | "&"       { AMP }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '{'       { LBRACE }
  | '}'       { RBRACE }
  | '['       { LBRACK }
  | ']'       { RBRACK }
  | ','       { COMMA }
  | semi      { SEMI }
  | eof       { EOF }
  | _         { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }

and comment =
  parse
  | "(*"    { incr comment_level; comment lexbuf }
  | "*)"    { decr comment_level; if !comment_level > 0 then comment lexbuf }
  | eof     { raise (SyntaxError "Unclosed comment") }
  | _       { comment lexbuf }

