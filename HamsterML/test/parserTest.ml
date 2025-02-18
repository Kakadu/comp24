open HamsterML.Ast

let parse_expr (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = HamsterML.Parser.prog_expr HamsterML.Lexer.read lexbuf in
  ast
;;

let parse_pattern (s : string) : pattern =
  let lexbuf = Lexing.from_string s in
  let ast = HamsterML.Parser.prog_pattern HamsterML.Lexer.read lexbuf in
  ast
;;

let parse_prog (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = HamsterML.Parser.prog HamsterML.Lexer.read lexbuf in
  ast
;;

(* --- EXPRESSIONS --- *)

(* Lists and Tuples *)

let%test _ =
  parse_expr "[1; 2; 3]" = EList [ EConst (Int 1); EConst (Int 2); EConst (Int 3) ]
;;

let%test _ = parse_expr "[]" = EList []
let%test _ = parse_expr "(1,2)" = ETuple [ EConst (Int 1); EConst (Int 2) ]

let%test _ =
  parse_expr "(1,2,3)" = ETuple [ EConst (Int 1); EConst (Int 2); EConst (Int 3) ]
;;

let%test _ = parse_expr "1,2" = parse_expr "(1,2)"
let%test _ = parse_expr "1,2,3,4,5" = parse_expr "(1,2,3,4,5)"

let%test _ =
  parse_expr "([1], [2])" = ETuple [ EList [ EConst (Int 1) ]; EList [ EConst (Int 2) ] ]
;;

let%test _ = parse_expr "[(1,2)]" = EList [ ETuple [ EConst (Int 1); EConst (Int 2) ] ]

let%test _ =
  parse_expr "[1,2,3]"
  = EList [ ETuple [ EConst (Int 1); EConst (Int 2); EConst (Int 3) ] ]
;;

let%test _ =
  parse_expr "(1+2, 3+4)"
  = ETuple
      [ Application (Application (EOperation (Binary ADD), EConst (Int 1)), EConst (Int 2))
      ; Application (Application (EOperation (Binary ADD), EConst (Int 3)), EConst (Int 4))
      ]
;;

let%test _ =
  parse_expr "((+),(+))" = ETuple [ EOperation (Binary ADD); EOperation (Binary ADD) ]
;;

let%test _ =
  parse_expr "[1+2; 3+4]"
  = EList
      [ Application (Application (EOperation (Binary ADD), EConst (Int 1)), EConst (Int 2))
      ; Application (Application (EOperation (Binary ADD), EConst (Int 3)), EConst (Int 4))
      ]
;;

let%test _ =
  parse_expr "[(+);(+)]" = EList [ EOperation (Binary ADD); EOperation (Binary ADD) ]
;;

let%test _ =
  parse_expr "1,2,3,(4,5)"
  = ETuple
      [ EConst (Int 1)
      ; EConst (Int 2)
      ; EConst (Int 3)
      ; ETuple [ EConst (Int 4); EConst (Int 5) ]
      ]
;;

let%test _ =
  parse_expr "(1,2,3,(1,2))"
  = ETuple
      [ EConst (Int 1)
      ; EConst (Int 2)
      ; EConst (Int 3)
      ; ETuple [ EConst (Int 1); EConst (Int 2) ]
      ]
;;

let%test _ =
  parse_expr "1,2,3" = ETuple [ EConst (Int 1); EConst (Int 2); EConst (Int 3) ]
;;

let%test _ =
  parse_expr "(1,2,3)" = ETuple [ EConst (Int 1); EConst (Int 2); EConst (Int 3) ]
;;

let%test _ = parse_expr "(1,2,3)" = parse_expr "1,2,3"

(* Operations *)

let%test _ = parse_expr "(+)" = EOperation (Binary ADD)
let%test _ = parse_expr "(+)" = EOperation (Binary ADD)

let%test _ =
  parse_expr "(+) 4 5"
  = Application (Application (EOperation (Binary ADD), EConst (Int 4)), EConst (Int 5))
;;

let%test _ = parse_expr "-1" = Application (EOperation (Unary UMINUS), EConst (Int 1))

let%test _ =
  parse_expr "1+1"
  = Application (Application (EOperation (Binary ADD), EConst (Int 1)), EConst (Int 1))
;;

let%test _ = parse_expr "1+1" = parse_expr "1 + 1"

let%test _ =
  parse_expr "1+1"
  = Application (Application (EOperation (Binary ADD), EConst (Int 1)), EConst (Int 1))
;;

let%test _ = parse_expr "1-1" = parse_expr "1 - 1"
let%test _ = parse_expr "((1+2)+3)" = parse_expr "1 + 2 + 3"
let%test _ = parse_expr "(1+2)*3" != parse_expr "1 + 2 * 3"

(* Application *)

let%test _ = parse_expr "f x" = Application (EVar "f", EVar "x")
let%test _ = parse_expr "f x y" = Application (Application (EVar "f", EVar "x"), EVar "y")

let%test _ =
  parse_expr "f x y z"
  = Application (Application (Application (EVar "f", EVar "x"), EVar "y"), EVar "z")
;;

let%test _ =
  parse_expr "f (g x)" = Application (EVar "f", Application (EVar "g", EVar "x"))
;;

let%test _ =
  parse_expr "f x, g y"
  = ETuple [ Application (EVar "f", EVar "x"); Application (EVar "g", EVar "y") ]
;;

let%test _ =
  parse_expr "(f x, g y)"
  = ETuple [ Application (EVar "f", EVar "x"); Application (EVar "g", EVar "y") ]
;;

let%test _ = parse_expr "f x, g y" = parse_expr "(f x, g y)"

let%test _ =
  parse_expr "[f x y]"
  = EList [ Application (Application (EVar "f", EVar "x"), EVar "y") ]
;;

let%test _ =
  parse_expr "1 + a"
  = Application (Application (EOperation (Binary ADD), EConst (Int 1)), EVar "a")
;;

let%test _ =
  parse_expr "a + a"
  = Application (Application (EOperation (Binary ADD), EVar "a"), EVar "a")
;;

let%test _ =
  parse_expr "a + 1"
  = Application (Application (EOperation (Binary ADD), EVar "a"), EConst (Int 1))
;;

let%test _ =
  parse_expr "1 + f x"
  = Application
      ( Application (EOperation (Binary ADD), EConst (Int 1))
      , Application (EVar "f", EVar "x") )
;;

let%test _ =
  parse_expr "f (1+1) x"
  = Application
      ( Application
          ( EVar "f"
          , Application
              (Application (EOperation (Binary ADD), EConst (Int 1)), EConst (Int 1)) )
      , EVar "x" )
;;

(* Concat *)

let%test _ = parse_expr "1::[]" = EListConcat (EConst (Int 1), EList [])

let%test _ =
  parse_expr "1::2::[]"
  = EListConcat (EConst (Int 1), EListConcat (EConst (Int 2), EList []))
;;

let%test _ =
  parse_expr "1::2::[3;4]"
  = EListConcat
      ( EConst (Int 1)
      , EListConcat (EConst (Int 2), EList [ EConst (Int 3); EConst (Int 4) ]) )
;;

(* Type constraints *)

let%test _ = parse_expr "(1 : int)" = EConstraint (EConst (Int 1), PInt)

let%test _ =
  parse_expr "(1 + 1 : int)"
  = EConstraint
      ( Application (Application (EOperation (Binary ADD), EConst (Int 1)), EConst (Int 1))
      , PInt )
;;

let%test _ =
  parse_expr "f (x : int)" = Application (EVar "f", EConstraint (EVar "x", PInt))
;;

let%test _ =
  parse_expr "((f x) : string)" = EConstraint (Application (EVar "f", EVar "x"), PString)
;;

let%test _ =
  parse_expr "let f (x:int) y = x + y"
  = Let
      ( Nonrecursive
      , [ ( Var "f"
          , [ Constraint (Var "x", PInt); Var "y" ]
          , Application (Application (EOperation (Binary ADD), EVar "x"), EVar "y") )
        ]
      , None )
;;

let%test _ =
  parse_expr "fun f g x -> (f (x: string) (g x: bool) : int)"
  = Fun
      ( [ Var "f"; Var "g"; Var "x" ]
      , EConstraint
          ( Application
              ( Application (EVar "f", EConstraint (EVar "x", PString))
              , EConstraint (Application (EVar "g", EVar "x"), PBool) )
          , PInt ) )
;;

let%test _ =
  parse_expr "fun x y -> (if 1 = 1 then x else y : int)"
  = Fun
      ( [ Var "x"; Var "y" ]
      , EConstraint
          ( If
              ( Application
                  (Application (EOperation (Binary EQ), EConst (Int 1)), EConst (Int 1))
              , EVar "x"
              , Some (EVar "y") )
          , PInt ) )
;;

let%test _ =
  parse_expr "fun (x:int) (y:bool) (z:string) -> 1"
  = Fun
      ( [ Constraint (Var "x", PInt)
        ; Constraint (Var "y", PBool)
        ; Constraint (Var "z", PString)
        ]
      , EConst (Int 1) )
;;

let%test _ =
  parse_expr "[(x:int); y; z]"
  = EList [ EConstraint (EVar "x", PInt); EVar "y"; EVar "z" ]
;;

let%test _ =
  parse_expr "((x:int), y, z)"
  = ETuple [ EConstraint (EVar "x", PInt); EVar "y"; EVar "z" ]
;;

let%test _ =
  parse_expr "(x: int) :: [x]"
  = EListConcat (EConstraint (EVar "x", PInt), EList [ EVar "x" ])
;;

(* Pattern matching *)

let%test _ =
  parse_expr "match x with 10 -> true"
  = Match (EVar "x", [ Const (Int 10), EConst (Bool true) ])
;;

let%test _ = parse_expr "match x with | 10 -> true" = parse_expr "match x with 10 -> true"

let%test _ =
  parse_expr "match x with | 1 -> 10 | _ -> 20"
  = Match (EVar "x", [ Const (Int 1), EConst (Int 10); Wildcard, EConst (Int 20) ])
;;

let%test _ =
  parse_expr "match xs with\n  | [] -> 0\n  | h::tl -> 1 + length tl"
  = Match
      ( EVar "xs"
      , [ List [], EConst (Int 0)
        ; ( ListConcat (Var "h", Var "tl")
          , Application
              ( Application (EOperation (Binary ADD), EConst (Int 1))
              , Application (EVar "length", EVar "tl") ) )
        ] )
;;

let%test _ =
  parse_expr "match xs with\n  | [] -> acc\n  | h::tl -> helper (acc + 1) tl"
  = Match
      ( EVar "xs"
      , [ List [], EVar "acc"
        ; ( ListConcat (Var "h", Var "tl")
          , Application
              ( Application
                  ( EVar "helper"
                  , Application
                      (Application (EOperation (Binary ADD), EVar "acc"), EConst (Int 1))
                  )
              , EVar "tl" ) )
        ] )
;;

let%test _ =
  parse_expr
    "match xs with\n\
    \  | [] -> []\n\
    \  | a::[] -> [f a]\n\
    \  | a::b::[] -> [f a; f b]\n\
    \  | a::b::c::[] -> [f a; f b; f c]\n\
    \  | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl"
  = Match
      ( EVar "xs"
      , [ List [], EList []
        ; ListConcat (Var "a", List []), EList [ Application (EVar "f", EVar "a") ]
        ; ( ListConcat (Var "a", ListConcat (Var "b", List []))
          , EList [ Application (EVar "f", EVar "a"); Application (EVar "f", EVar "b") ] )
        ; ( ListConcat (Var "a", ListConcat (Var "b", ListConcat (Var "c", List [])))
          , EList
              [ Application (EVar "f", EVar "a")
              ; Application (EVar "f", EVar "b")
              ; Application (EVar "f", EVar "c")
              ] )
        ; ( ListConcat
              ( Var "a"
              , ListConcat (Var "b", ListConcat (Var "c", ListConcat (Var "d", Var "tl")))
              )
          , EListConcat
              ( Application (EVar "f", EVar "a")
              , EListConcat
                  ( Application (EVar "f", EVar "b")
                  , EListConcat
                      ( Application (EVar "f", EVar "c")
                      , EListConcat
                          ( Application (EVar "f", EVar "d")
                          , Application (Application (EVar "map", EVar "f"), EVar "tl") )
                      ) ) ) )
        ] )
;;

let%test _ =
  parse_expr "match xs with\n    | [] -> []\n    | h::tl -> append h (helper tl)"
  = Match
      ( EVar "xs"
      , [ List [], EList []
        ; ( ListConcat (Var "h", Var "tl")
          , Application
              ( Application (EVar "append", EVar "h")
              , Application (EVar "helper", EVar "tl") ) )
        ] )
;;

let%test _ =
  parse_expr " match xs with [] -> () | h::tl -> let () = f h"
  = Match
      ( EVar "xs"
      , [ List [], EConst Unit
        ; ( ListConcat (Var "h", Var "tl")
          , Let (Nonrecursive, [ Const Unit, [], Application (EVar "f", EVar "h") ], None)
          )
        ] )
;;

let%test _ =
  parse_expr
    "match xs with\n\
    \  | [] -> []\n\
    \  | h::tl -> append (map (fun a -> (h,a)) ys) (cartesian tl ys)"
  = Match
      ( EVar "xs"
      , [ List [], EList []
        ; ( ListConcat (Var "h", Var "tl")
          , Application
              ( Application
                  ( EVar "append"
                  , Application
                      ( Application
                          (EVar "map", Fun ([ Var "a" ], ETuple [ EVar "h"; EVar "a" ]))
                      , EVar "ys" ) )
              , Application (Application (EVar "cartesian", EVar "tl"), EVar "ys") ) )
        ] )
;;

(* --- PATTERNS --- *)

(* simple patterns *)

let%test _ = parse_pattern "10" = Const (Int 10)
let%test _ = parse_pattern "nike_pro" = Var "nike_pro"
let%test _ = parse_pattern "_" = Wildcard
let%test _ = parse_pattern "(_)" = parse_pattern "_"
let%test _ = parse_pattern "(nike_pro)" = parse_pattern "nike_pro"
let%test _ = parse_pattern "(10)" = parse_pattern "10"

(* Tuples *)

let%test _ = parse_pattern "(1,2)" = Tuple [ Const (Int 1); Const (Int 2) ]

let%test _ =
  parse_pattern "(1,2,3)" = Tuple [ Const (Int 1); Const (Int 2); Const (Int 3) ]
;;

let%test _ = parse_pattern "1,2" = parse_pattern "(1,2)"
let%test _ = parse_pattern "1,2,3,4,5" = parse_pattern "(1,2,3,4,5)"

let%test _ =
  parse_pattern "1,2,3,(4,5)"
  = Tuple
      [ Const (Int 1)
      ; Const (Int 2)
      ; Const (Int 3)
      ; Tuple [ Const (Int 4); Const (Int 5) ]
      ]
;;

let%test _ =
  parse_pattern "((+),(+))" = Tuple [ Operation (Binary ADD); Operation (Binary ADD) ]
;;

let%test _ =
  parse_pattern "([1], [2])" = Tuple [ List [ Const (Int 1) ]; List [ Const (Int 2) ] ]
;;

(* Concat *)

let%test _ = parse_pattern "1::[]" = ListConcat (Const (Int 1), List [])

let%test _ =
  parse_pattern "1::2::[]"
  = ListConcat (Const (Int 1), ListConcat (Const (Int 2), List []))
;;

let%test _ =
  parse_pattern "1::2::[3;4]"
  = ListConcat
      (Const (Int 1), ListConcat (Const (Int 2), List [ Const (Int 3); Const (Int 4) ]))
;;

(* Constraints *)

let%test _ =
  parse_pattern "[(x:int); y; z]" = List [ Constraint (Var "x", PInt); Var "y"; Var "z" ]
;;

let%test _ =
  parse_pattern "((x:int), y, z)" = Tuple [ Constraint (Var "x", PInt); Var "y"; Var "z" ]
;;

let%test _ =
  parse_pattern "(x: int) :: [x]"
  = ListConcat (Constraint (Var "x", PInt), List [ Var "x" ])
;;
