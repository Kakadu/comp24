let get_tokens code =
  let rec build_list lexbuf =
    match Lexer.read lexbuf with
    | Parser.EOF -> []
    | token -> token :: build_list lexbuf
  in
  build_list (Lexing.from_string code)
;;

let%test _ = get_tokens "\"rofl\"" = [ Parser.STRING "rofl" ]
let%test _ = get_tokens "228" = [ Parser.INT 228 ]
let%test _ = get_tokens "'a'" = [ Parser.CHAR 'a' ]
let%test _ = get_tokens "228.337" = [ Parser.FLOAT 228.337 ]
let%test _ = get_tokens "true" = [ Parser.BOOL true ]
let%test _ = get_tokens "false" = [ Parser.BOOL false ]
let%test _ = get_tokens "add" = [ Parser.IDENTIFIER "add" ]

let%test _ =
  get_tokens
    "let rec sum list =\n\
    \   match list with\n\
    \   | [] -> 0\n\
    \   | head :: tail -> head + sum tail"
  = [ Parser.LET
    ; Parser.REC
    ; Parser.IDENTIFIER "sum"
    ; Parser.IDENTIFIER "list"
    ; Parser.EQUAL
    ; Parser.MATCH
    ; Parser.IDENTIFIER "list"
    ; Parser.WITH
    ; Parser.BAR
    ; Parser.LEFT_SQ_BRACKET
    ; Parser.RIGHT_SQ_BRACKET
    ; Parser.ARROW
    ; Parser.INT 0
    ; Parser.BAR
    ; Parser.IDENTIFIER "head"
    ; Parser.DOUBLE_COLON
    ; Parser.IDENTIFIER "tail"
    ; Parser.ARROW
    ; Parser.IDENTIFIER "head"
    ; Parser.PLUS
    ; Parser.IDENTIFIER "sum"
    ; Parser.IDENTIFIER "tail"
    ]
;;
