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
