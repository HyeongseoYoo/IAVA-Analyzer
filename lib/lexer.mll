{
open Lexing
open Parser

exception SyntaxError of string

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
      (* ("let", LET);
      ("in", IN); *)
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
  | "//"      { line_comment lexbuf }
  | "(*"      { block_comment 1 lexbuf }
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
  | "||"      { OR }
  | "&&"      { AND }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '{'       { LBRACE }
  | '}'       { RBRACE }
  | '['       { LBRACK }
  | ']'       { RBRACK }
  | ','       { COMMA }
  | '&'       { AMP }
  | semi      { SEMI }
  | eof       { EOF }
  | _         { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }

and line_comment =
  parse
  | newline { new_line lexbuf; read lexbuf }
  | eof     { EOF }
  | _       { line_comment lexbuf }

and block_comment depth =
  parse
  | "(*"    { block_comment (depth + 1) lexbuf }
  | "*)"    { if depth = 1 then read lexbuf else block_comment (depth - 1) lexbuf }
  | newline { new_line lexbuf; block_comment depth lexbuf }
  | eof     { raise (SyntaxError "Unterminated block comment") }
  | _       { block_comment depth lexbuf }

