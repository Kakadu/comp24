open Lexer_test
open Parser_test

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


let%test _ = parse "1 + 2" = BinOp (ADD, Value (Const (Int 1)), Value (Const (Int 2)))
let%test _ = parse "1 - 2" = BinOp (SUB, Value (Const (Int 1)), Value (Const (Int 2)))

let%test _ =
  parse "1 + 2 - 3"
  = BinOp
      ( SUB
      , BinOp (ADD, Value (Const (Int 1)), Value (Const (Int 2)))
      , Value (Const (Int 3)) )
;;

let%test _ = parse "[1; 2]" = Value (List [ Const (Int 1); Const (Int 2) ])
let%test _ = parse "1::[2]" = Value (ListConcat (Const (Int 1), List [ Const (Int 2) ]))
let%test _ = parse "let a = 1" = BinOp (ASSIGN, Value (VarId "a"), Value (Const (Int 1)))