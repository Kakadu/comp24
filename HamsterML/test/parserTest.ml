open HamsterML.Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = HamsterML.Parser.prog HamsterML.Lexer.read lexbuf in
  ast
;;

(* let%test _ =
   parse "let a = 1 and b = 2"
   = LetIn
   ( [ BinOp (ASSIGN, Value (VarId "a"), Value (Const (Int 1)))
        ; BinOp (ASSIGN, Value (VarId "b"), Value (Const (Int 2)))
        ]
   , None )
   ;; *)

(* Data Type tests *)
let%test _ = parse "+228" = parse "228"
let%test _ = parse "228" = Value (Const (Int 228))
let%test _ = parse "-228" = Value (Const (Int (-228)))
let%test _ = parse "-228.337" = Value (Const (Float (-228.337)))
let%test _ = parse "+228.337" = Value (Const (Float 228.337))
let%test _ = parse "228.337" = Value (Const (Float 228.337))
let%test _ = parse "true" = Value (Const (Bool true))
let%test _ = parse "false" = Value (Const (Bool false))
let%test _ = parse "'1'" = Value (Const (Char '1'))
let%test _ = parse "\"Nike pro\"" = Value (Const (String "Nike pro"))

(* Values *)
let%test _ = parse "[1; 2]" = Value (List [ Const (Int 1); Const (Int 2) ])
let%test _ = parse "1::[2]" = Value (ListConcat (Const (Int 1), List [ Const (Int 2) ]))
let%test _ = parse "let a = 1" = BinOp (ASSIGN, Value (VarId "a"), Value (Const (Int 1)))
let%test _ = parse "1 + 2" = BinOp (ADD, Value (Const (Int 1)), Value (Const (Int 2)))
let%test _ = parse "1 - 2" = BinOp (SUB, Value (Const (Int 1)), Value (Const (Int 2)))

let%test _ =
  parse "1 + 2 - 3"
  = BinOp
      ( SUB
      , BinOp (ADD, Value (Const (Int 1)), Value (Const (Int 2)))
      , Value (Const (Int 3)) )
;;

let%test _ =
  parse "1 + (2 - 3)"
  = BinOp
      ( ADD
      , Value (Const (Int 1))
      , BinOp (SUB, Value (Const (Int 2)), Value (Const (Int 3))) )
;;
