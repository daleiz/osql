(* Simple wrapper tying together parser and lexer *)

exception Error of exn * (int * int * string * string)

let parse_buf_exn lexbuf =
  try
    Sql_parser.input Sql_lexer.token lexbuf
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      let tail = Sql_lexer.ruleTail "" lexbuf in
			raise (Error (exn,(line,cnum,tok,tail)))
    end

let parse_buf lexbuf = try Some (parse_buf_exn lexbuf) with _ -> None

let parse_str str = parse_buf_exn (Lexing.from_string str)

