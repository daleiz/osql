{
  open Printf
  open Lexing
  open Sql_parser

let error _ caller_id =
  prerr_endline (sprintf "Lexer error : %s" caller_id);
	raise Parsing.Parse_error

let pos lexbuf = (lexeme_start lexbuf, lexeme_end lexbuf)

let advance_line_pos pos =
  { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum; }

let advance_line lexbuf =
  lexbuf.lex_curr_p <- advance_line_pos lexbuf.lex_curr_p

let keyword_list = 
 [
   "create",CREATE;
   "stream",STREAM;
   "if",IF;
   "not",NOT;
   "exists",EXISTS;

   "insert",INSERT;
   "into",INTO;
   "values",VALUES;

   "select",SELECT;
   "from",FROM;
   "where",WHERE;
   "group",GROUP;
   "by",BY;
   "having",HAVING;

   "string",T_STRING;
   "int",T_INTEGER;
   "float",T_FLOAT;
   "bool",T_BOOLEAN;

   "true",TRUE;
   "false",FALSE;
  ]

module Keywords = Map.Make(String)

let keywords =
  let add map (k,v) =
    let k = String.lowercase_ascii k in
    if Keywords.mem k map then
      failwith (sprintf "Lexeme %s is already associated with keyword." k)
    else
      Keywords.add k v map
  in
  List.fold_left add Keywords.empty keyword_list

let get_ident str =
  let str = String.lowercase_ascii str in
  try Keywords.find str keywords with Not_found -> IDENT str

let ident str = IDENT (String.lowercase_ascii str)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha) (alpha | digit | '_' )*
let wsp = [' ' '\r' '\t']
let cmnt = "--" | "//" | "#"

rule token = parse
  | wsp   { token lexbuf }
  (* update line number *)
  | '\n'  { advance_line lexbuf; token lexbuf}

  | '('		{ LPAREN }
  | ')'		{ RPAREN }
  | ','   { COMMA }
  | '.'   { DOT }

  | cmnt { ignore (ruleComment "" lexbuf); token lexbuf }
  | "/*" { ignore (ruleCommentMulti "" lexbuf); token lexbuf }

  | "*" { ASTERISK }

  | "=" { EQUAL }
  | "<" { LESS }
  | ">" { GREATER }

  | '"' { STRING (ruleInQuotes "" lexbuf) }

  | ident as str {  get_ident str }
  | digit+ as str { INTEGER (int_of_string str) }
  | digit+ '.' digit+ as str { FLOAT (float_of_string str) }
  | eof		{ EOF }
  | _	{ error lexbuf "rule" }

and
ruleInQuotes acc = parse
  | '"'	        { acc }
  | eof	        { error lexbuf "no terminating quote" }
  | [^'"']+ as s { ruleInQuotes (acc^s) lexbuf }
  | _		{ error lexbuf "ruleInQuotes" }

and
ruleComment acc = parse
  | '\n'	{ advance_line lexbuf; acc }
  | eof	        { acc }
  | [^'\n']+    { let s = lexeme lexbuf in ruleComment (acc ^ s) lexbuf; }
  | _		{ error lexbuf "ruleComment"; }

and
ruleCommentMulti acc = parse
  | '\n'	{ advance_line lexbuf; ruleCommentMulti (acc ^ "\n") lexbuf }
  | "*/"	{ acc }
  | "*"
  | [^'\n' '*']+    { let s = lexeme lexbuf in ruleCommentMulti (acc ^ s) lexbuf }
  | _	        { error lexbuf "ruleCommentMulti" }

and
ruleTail acc = parse
  | eof { acc }
  | _* as str { ruleTail (acc ^ str) lexbuf }
