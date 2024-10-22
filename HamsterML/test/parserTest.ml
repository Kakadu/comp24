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

let%test _ = parse "[1; 2]" = Value (List [ Const (Int 1); Const (Int 2) ])
let%test _ = parse "1::[2]" = Value (ListConcat (Const (Int 1), List [ Const (Int 2) ]))
let%test _ = parse "let a = 1" = BinOp (ASSIGN, Value (VarId "a"), Value (Const (Int 1)))
