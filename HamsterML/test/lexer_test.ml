let lex code =
  let rec build_list lexbuf =
    match HamsterML.Lexer.read lexbuf with
    | EOF -> []
    | token -> token :: build_list lexbuf
  in
  build_list (Lexing.from_string code)
;;