let lex code =
  let rec build_list lexbuf =
    match HamsterML.Lexer.read lexbuf with
    | EOF -> []
    | token -> token :: build_list lexbuf
  in
  build_list (Lexing.from_string code)
;;

let%test _ = lex "\"rofl\"" = [ STRING "rofl" ]
let%test _ = lex "228" = [ INT 228 ]
let%test _ = lex "'a'" = [ CHAR 'a' ]
let%test _ = lex "228.337" = [ FLOAT 228.337 ]
let%test _ = lex "true" = [ BOOL true ]
let%test _ = lex "false" = [ BOOL false ]
let%test _ = lex "add" = [ IDENTIFIER "add" ]

let%test _ =
  lex
    "let rec sum list =\n\
    \   match list with\n\
    \   | [] -> 0\n\
    \   | head :: tail -> head + sum tail"
  = [ LET
    ; REC
    ; IDENTIFIER "sum"
    ; IDENTIFIER "list"
    ; EQUAL
    ; MATCH
    ; IDENTIFIER "list"
    ; WITH
    ; BAR
    ; LEFT_SQ_BRACKET
    ; RIGHT_SQ_BRACKET
    ; ARROW
    ; INT 0
    ; BAR
    ; IDENTIFIER "head"
    ; DOUBLE_COLON
    ; IDENTIFIER "tail"
    ; ARROW
    ; IDENTIFIER "head"
    ; PLUS
    ; IDENTIFIER "sum"
    ; IDENTIFIER "tail"
    ]
;;