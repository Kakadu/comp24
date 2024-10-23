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
(* Variables *)
let%test _ = parse "sigma_nike_pro_228" = Value (VarId "sigma_nike_pro_228")
let%test _ = parse "pick_me:int" = Value (TypedVarID ("pick_me", PInt))
let%test _ = parse "sigma : float" = Value (TypedVarID ("sigma", PFloat))
let%test _ = parse "kfc_boss: string" = Value (TypedVarID ("kfc_boss", PString))
let%test _ = parse "roblox : char" = Value (TypedVarID ("roblox", PChar))
let%test _ = parse "( roblox : bool )" = Value (TypedVarID ("roblox", PBool))

(* Patterns *)
let%test _ =
  parse "[1; 2; 3; 4]"
  = Value (List [ Const (Int 1); Const (Int 2); Const (Int 3); Const (Int 4) ])
;;

let%test _ =
  parse "1::[2; 3; 4]"
  = Value
      (ListConcat (Const (Int 1), List [ Const (Int 2); Const (Int 3); Const (Int 4) ]))
;;

let%test _ =
  parse "('a', 'b', 'c', 'd')"
  = Value
      (Tuple [ Const (Char 'a'); Const (Char 'b'); Const (Char 'c'); Const (Char 'd') ])
;;

let%test _ = parse "(_)" = Value Wildcard

(* Expr *)
(* bop *)
let%test _ = parse "1 + 2" = BinOp (ADD, Value (Const (Int 1)), Value (Const (Int 2)))
let%test _ = parse "1 - 2" = BinOp (SUB, Value (Const (Int 1)), Value (Const (Int 2)))

let%test _ =
  parse "1 + 2 - -3"
  = BinOp
      ( SUB
      , BinOp (ADD, Value (Const (Int 1)), Value (Const (Int 2)))
      , Value (Const (Int (-3))) )
;;

let%test _ =
  parse "1 + (2 - 3)"
  = BinOp
      ( ADD
      , Value (Const (Int 1))
      , BinOp (SUB, Value (Const (Int 2)), Value (Const (Int 3))) )
;;

(* uop *)
let%test _ = parse "not true" = UnOp (NOT, Value (Const (Bool true)))

(* fun *)
let%test _ =
  parse "fun x y z -> true"
  = Fun ([ VarId "x"; VarId "y"; VarId "z" ], Value (Const (Bool true)))
;;

(* if *)
let%test _ =
  parse "if x = y then \"nike_pro\" else \"sigma\""
  = If
      ( BinOp (EQ, Value (VarId "x"), Value (VarId "y"))
      , Value (Const (String "nike_pro"))
      , Some (Value (Const (String "sigma"))) )
;;

let%test _ =
  parse "if x = y then \"nike_pro\""
  = If
      ( BinOp (EQ, Value (VarId "x"), Value (VarId "y"))
      , Value (Const (String "nike_pro"))
      , None )
;;

(* match *)
let%test _ =
  parse "match rofl with \n  | 228 -> true\n  | 337 -> true\n  | _ -> false"
  = Match
      ( Value (VarId "rofl")
      , [ Const (Int 228), Value (Const (Bool true))
        ; Const (Int 337), Value (Const (Bool true))
        ; Wildcard, Value (Const (Bool false))
        ] )
;;

(* let *)
let%test _ = parse "let a = 1" = Let (Nonrecursive, "a", [], Value (Const (Int 1)))

let%test _ =
  parse "let rec f a b = true"
  = Let (Recursive, "f", [ VarId "a"; VarId "b" ], Value (Const (Bool true)))
;;

let%test _ =
  parse "let f a b = 10 and g c d = 20 and e = 30 in nike_pro"
  = LetIn
      ( [ Let (Nonrecursive, "f", [ VarId "a"; VarId "b" ], Value (Const (Int 10)))
        ; Let (Nonrecursive, "g", [ VarId "c"; VarId "d" ], Value (Const (Int 20)))
        ; Let (Nonrecursive, "e", [], Value (Const (Int 30)))
        ]
      , Value (VarId "nike_pro") )
;;

let%test _ =
  parse "let a = 10 and b = 20 in let c = 30 in nike_pro"
  = LetIn
      ( [ Let (Nonrecursive, "a", [], Value (Const (Int 10)))
        ; Let (Nonrecursive, "b", [], Value (Const (Int 20)))
        ]
      , LetIn
          ( [ Let (Nonrecursive, "c", [], Value (Const (Int 30))) ]
          , Value (VarId "nike_pro") ) )
;;
