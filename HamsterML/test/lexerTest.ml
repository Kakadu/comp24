let lex code =
  let rec build_list lexbuf =
    match HamsterML.Lexer.read lexbuf with
    | EOF -> []
    | token -> token :: build_list lexbuf
  in
  build_list (Lexing.from_string code)
;;

let%test _ = lex "\"rofl\"" = [ TYPE_STRING "rofl" ]
let%test _ = lex "228" = [ TYPE_INT 228 ]
let%test _ = lex "+228" = [ PLUS ; TYPE_INT 228 ]
let%test _ = lex "-228" = [ MINUS; TYPE_INT 228 ]
let%test _ = lex "'a'" = [ TYPE_CHAR 'a' ]
let%test _ = lex "228.337" = [ TYPE_FLOAT 228.337 ]
let%test _ = lex "+228.337" = [ PLUS; TYPE_FLOAT 228.337 ]
let%test _ = lex "-228.337" = [ MINUS; TYPE_FLOAT 228.337 ]
let%test _ = lex "1 -1 +2" = [TYPE_INT 1; MINUS; TYPE_INT 1; PLUS; TYPE_INT 2 ]
let%test _ = lex "true" = [ TYPE_BOOL true ]
let%test _ = lex "false" = [ TYPE_BOOL false ]
let%test _ = lex "add" = [ IDENTIFIER "add" ]
let%test _ = lex "()" = [ TYPE_UNIT ]
let%test _ = lex "( )" = [ TYPE_UNIT ]
let%test _ = lex "(* comment!!! *)" = []
let%test _ = lex "(  - )" = [ OP_IDENTIFIER '-' ]
let%test _ = lex "( + )" = [ OP_IDENTIFIER '+' ]
let%test _ = lex "(*)" = [ OP_IDENTIFIER '*' ]

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
    ; TYPE_INT 0
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
