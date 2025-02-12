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

(* let%test _ =
  parse_expr "let addi = fun f g x -> (f x (g x: bool) : int)"
  = Let
      ( Nonrecursive
      , [ ( Var "addi"
          , []
          , Fun
              ( [ Var "f"; Var "g"; Var "x" ]
              , EConstraint
                  ( Application
                      ( Application (EVar "f", EVar "x")
                      , EConstraint (Application (EVar "g", EVar "x"), PInt) )
                  , PBool ) ) )
        ]
      , None )
;; *)

let%test _ =
  parse_expr "f (x : int)" = Application (EVar "f", EConstraint (EVar "x", PInt))
;;

let%test _ =
  parse_expr "(f x) : string" = EConstraint (Application (EVar "f", EVar "x"), PString)
;;

let%test _ = parse_expr "f x : int" != EConstraint (Application (EVar "f", EVar "x"), PInt)

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
      ( [ Var "x"; Var "y"; Var "z" ]
      , Application
          ( Application (EVar "f", EConstraint (EVar "x", PString))
          , EConstraint (Application (EVar "g", EVar "x"), PBool) ) )
;;

let%test _ =
  parse_expr
    "let main =\n\
    \  let () = print_int (addi (fun x: int b -> if b then x+1 else x*2) (fun _start: \
     int -> _start/2 = 0) 4) in\n\
    \  0"
  = Let
      ( Nonrecursive
      , [ ( Var "main"
          , []
          , Let
              ( Nonrecursive
              , [ ( Const Unit
                  , []
                  , Application
                      ( EVar "print_int"
                      , Application
                          ( Application
                              ( Application
                                  ( EVar "addi"
                                  , Fun
                                      ( [ Constraint (Var "x", PInt); Var "b" ]
                                      , If
                                          ( EVar "b"
                                          , Application
                                              ( Application
                                                  (EOperation (Binary ADD), EVar "x")
                                              , EConst (Int 1) )
                                          , Some
                                              (Application
                                                 ( Application
                                                     (EOperation (Binary MUL), EVar "x")
                                                 , EConst (Int 2) )) ) ) )
                              , Fun
                                  ( [ Constraint (Var "_start", PInt) ]
                                  , Application
                                      ( Application
                                          ( EOperation (Binary EQ)
                                          , Application
                                              ( Application
                                                  (EOperation (Binary DIV), EVar "_start")
                                              , EConst (Int 2) ) )
                                      , EConst (Int 0) ) ) )
                          , EConst (Int 4) ) ) )
                ]
              , Some (EConst (Int 0)) ) )
        ]
      , None )
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
(*
   (* Data Type tests *)
let%test _ = parse "let a = +228" = parse "let a = 228"

let%test _ =
  parse "let a = 228" = [ Let (Nonrecursive, VarId "a", [], Pattern (Const (Int 228))) ]
;;

let%test _ =
  parse "let a = -228"
  = [ Let (Nonrecursive, VarId "a", [], Pattern (Const (Int (-228)))) ]
;;

let%test _ =
  parse "let a = -228.337"
  = [ Let (Nonrecursive, VarId "a", [], Pattern (Const (Float (-228.337)))) ]
;;

let%test _ =
  parse "let a = +228.337"
  = [ Let (Nonrecursive, VarId "a", [], Pattern (Const (Float 228.337))) ]
;;

let%test _ =
  parse "let a = 228.337"
  = [ Let (Nonrecursive, VarId "a", [], Pattern (Const (Float 228.337))) ]
;;

let%test _ =
  parse "let a = true"
  = [ Let (Nonrecursive, VarId "a", [], Pattern (Const (Bool true))) ]
;;

let%test _ =
  parse "let a = false"
  = [ Let (Nonrecursive, VarId "a", [], Pattern (Const (Bool false))) ]
;;

let%test _ =
  parse "let b = '1'" = [ Let (Nonrecursive, VarId "b", [], Pattern (Const (Char '1'))) ]
;;

let%test _ =
  parse "let a = \"Nike pro\""
  = [ Let (Nonrecursive, VarId "a", [], Pattern (Const (String "Nike pro"))) ]
;;

(* Variables *)
let%test _ =
  parse "let sigma_nike_pro_228 = a"
  = [ Let (Nonrecursive, VarId "sigma_nike_pro_228", [], Pattern (VarId "a")) ]
;;

let%test _ =
  parse "let (pick_me:int) = 1"
  = [ Let (Nonrecursive, TypedVarID ("pick_me", PInt), [], Pattern (Const (Int 1))) ]
;;

let%test _ =
  parse "let sigma : float = 1.2"
  = [ Let (Nonrecursive, TypedVarID ("sigma", PFloat), [], Pattern (Const (Float 1.2))) ]
;;

let%test _ =
  parse "let kfc_boss: string = \"kfc_boss\""
  = [ Let
        ( Nonrecursive
        , TypedVarID ("kfc_boss", PString)
        , []
        , Pattern (Const (String "kfc_boss")) )
    ]
;;

let%test _ =
  parse "let roblox : char = 'r'"
  = [ Let (Nonrecursive, TypedVarID ("roblox", PChar), [], Pattern (Const (Char 'r'))) ]
;;

let%test _ =
  parse "let ( roblox : bool ) = false"
  = [ Let (Nonrecursive, TypedVarID ("roblox", PBool), [], Pattern (Const (Bool false))) ]
;;

(* Patterns *)
let%test _ =
  parse "let l = [1; 2; 3; 4]"
  = [ Let
        ( Nonrecursive
        , VarId "l"
        , []
        , Pattern
            (List
               [ Pattern (Const (Int 1))
               ; Pattern (Const (Int 2))
               ; Pattern (Const (Int 3))
               ; Pattern (Const (Int 4))
               ]) )
    ]
;;

let%test _ = parse "let l = []" = [ Let (Nonrecursive, VarId "l", [], Pattern (List [])) ]

let%test _ =
  parse "let l = 1::[2; 3; 4]"
  = [ Let
        ( Nonrecursive
        , VarId "l"
        , []
        , Pattern
            (ListConcat
               ( Pattern (Const (Int 1))
               , List
                   [ Pattern (Const (Int 2))
                   ; Pattern (Const (Int 3))
                   ; Pattern (Const (Int 4))
                   ] )) )
    ]
;;

let%test _ =
  parse "let t = ('a', 'b', 'c', 'd')"
  = [ Let
        ( Nonrecursive
        , VarId "t"
        , []
        , Pattern
            (Tuple
               [ Pattern (Const (Char 'a'))
               ; Pattern (Const (Char 'b'))
               ; Pattern (Const (Char 'c'))
               ; Pattern (Const (Char 'd'))
               ]) )
    ]
;;

let%test _ =
  parse "let ( ) = ()" = [ Let (Nonrecursive, Const Unit, [], Pattern (Const Unit)) ]
;;

let%test _ = parse "let ( ) = ()" = parse "let () = ( )"

let%test _ =
  parse "let (_) = 1" = [ Let (Nonrecursive, Wildcard, [], Pattern (Const (Int 1))) ]
;;

(* Expr *)
(* bop *)
let%test _ =
  parse "let a = 1 + 2"
  = [ Let
        ( Nonrecursive
        , VarId "a"
        , []
        , BinOp (ADD, Pattern (Const (Int 1)), Pattern (Const (Int 2))) )
    ]
;;

let%test _ =
  parse "let b = 1 - 2"
  = [ Let
        ( Nonrecursive
        , VarId "b"
        , []
        , BinOp (SUB, Pattern (Const (Int 1)), Pattern (Const (Int 2))) )
    ]
;;

let%test _ =
  parse "let a = 1 + 2 - -3"
  = [ Let
        ( Nonrecursive
        , VarId "a"
        , []
        , BinOp
            ( SUB
            , BinOp (ADD, Pattern (Const (Int 1)), Pattern (Const (Int 2)))
            , Pattern (Const (Int (-3))) ) )
    ]
;;

let%test _ =
  parse "let a = 1 + (2 - 3)"
  = [ Let
        ( Nonrecursive
        , VarId "a"
        , []
        , BinOp
            ( ADD
            , Pattern (Const (Int 1))
            , BinOp (SUB, Pattern (Const (Int 2)), Pattern (Const (Int 3))) ) )
    ]
;;

(* uop *)
let%test _ =
  parse "let a = not true"
  = [ Let (Nonrecursive, VarId "a", [], UnOp (NOT, Pattern (Const (Bool true)))) ]
;;

(* fun *)
let%test _ =
  parse "let f x y z = fun x y z -> true"
  = [ Let
        ( Nonrecursive
        , VarId "f"
        , [ VarId "x"; VarId "y"; VarId "z" ]
        , Fun ([ VarId "x"; VarId "y"; VarId "z" ], Pattern (Const (Bool true))) )
    ]
;;

(* if *)
let%test _ =
  parse "let f x y = if x = y then \"nike_pro\" else \"sigma\""
  = [ Let
        ( Nonrecursive
        , VarId "f"
        , [ VarId "x"; VarId "y" ]
        , If
            ( BinOp (EQ, Pattern (VarId "x"), Pattern (VarId "y"))
            , Pattern (Const (String "nike_pro"))
            , Some (Pattern (Const (String "sigma"))) ) )
    ]
;;

let%test _ =
  parse "let f x y = if x = y then \"nike_pro\""
  = [ Let
        ( Nonrecursive
        , VarId "f"
        , [ VarId "x"; VarId "y" ]
        , If
            ( BinOp (EQ, Pattern (VarId "x"), Pattern (VarId "y"))
            , Pattern (Const (String "nike_pro"))
            , None ) )
    ]
;;

(* match *)
let%test _ =
  parse "let f rofl = match rofl with\n | 228 -> true\n | 337 -> true\n | _ -> false"
  = [ Let
        ( Nonrecursive
        , VarId "f"
        , [ VarId "rofl" ]
        , Match
            ( Pattern (VarId "rofl")
            , [ Const (Int 228), Pattern (Const (Bool true))
              ; Const (Int 337), Pattern (Const (Bool true))
              ; Wildcard, Pattern (Const (Bool false))
              ] ) )
    ]
;;

let%test _ =
  parse "let f rofl = match rofl with \n  | [] -> 0\n  | hd::tl -> 10"
  = [ Let
        ( Nonrecursive
        , VarId "f"
        , [ VarId "rofl" ]
        , Match
            ( Pattern (VarId "rofl")
            , [ List [], Pattern (Const (Int 0))
              ; ListConcat (Pattern (VarId "hd"), VarId "tl"), Pattern (Const (Int 10))
              ] ) )
    ]
;;

let%test _ =
  parse "let f x = match x with | 1 -> 10 | 2 -> 20"
  = [ Let
        ( Nonrecursive
        , VarId "f"
        , [ VarId "x" ]
        , Match
            ( Pattern (VarId "x")
            , [ Const (Int 1), Pattern (Const (Int 10))
              ; Const (Int 2), Pattern (Const (Int 20))
              ] ) )
    ]
;;

let%test _ =
  parse "let f x = match x with | 1 -> 10 | 2 -> 20"
  = parse "let f x = match x with 1 -> 10 | 2 -> 20"
;;

(* let declarations *)
let%test _ =
  parse "let a = 1" = [ Let (Nonrecursive, VarId "a", [], Pattern (Const (Int 1))) ]
;;

let%test _ =
  parse "let rec f a b = true"
  = [ Let (Recursive, VarId "f", [ VarId "a"; VarId "b" ], Pattern (Const (Bool true))) ]
;;

let%test _ =
  parse "let f a b = 10 and g c d = 20 and e = 30 in nike_pro"
  = [ LetAndIn
        ( [ Let
              (Nonrecursive, VarId "f", [ VarId "a"; VarId "b" ], Pattern (Const (Int 10)))
          ; Let
              (Nonrecursive, VarId "g", [ VarId "c"; VarId "d" ], Pattern (Const (Int 20)))
          ; Let (Nonrecursive, VarId "e", [], Pattern (Const (Int 30)))
          ]
        , Some (Pattern (VarId "nike_pro")) )
    ]
;;

let%test _ =
  parse "let a = 10 and b = 20 in let c = 30 in nike_pro"
  = [ LetAndIn
        ( [ Let (Nonrecursive, VarId "a", [], Pattern (Const (Int 10)))
          ; Let (Nonrecursive, VarId "b", [], Pattern (Const (Int 20)))
          ]
        , Some
            (LetAndIn
               ( [ Let (Nonrecursive, VarId "c", [], Pattern (Const (Int 30))) ]
               , Some (Pattern (VarId "nike_pro")) )) )
    ]
;;

let%test _ =
  parse "let a = 10 and b = 20"
  = [ LetAndIn
        ( [ Let (Nonrecursive, VarId "a", [], Pattern (Const (Int 10)))
          ; Let (Nonrecursive, VarId "b", [], Pattern (Const (Int 20)))
          ]
        , None )
    ]
;;

(* application *)

let%test _ =
  parse "let a = (*123*) f x y (*123*)"
  = [ Let
        ( Nonrecursive
        , VarId "a"
        , []
        , Application
            (Pattern (VarId "f"), Application (Pattern (VarId "x"), Pattern (VarId "y")))
        )
    ]
;;

(* nested recursive let and *)

let%test _ =
  parse "let rec a = 10 and b = 20 in let c = 30 in sigma"
  = [ LetAndIn
        ( [ Let (Recursive, VarId "a", [], Pattern (Const (Int 10)))
          ; Let (Nonrecursive, VarId "b", [], Pattern (Const (Int 20)))
          ]
        , Some
            (LetAndIn
               ( [ Let (Nonrecursive, VarId "c", [], Pattern (Const (Int 30))) ]
               , Some (Pattern (VarId "sigma")) )) )
    ]
;;

(* Unit *)
let%test _ =
  parse "let () = ()" = [ Let (Nonrecursive, Const Unit, [], Pattern (Const Unit)) ]
;;

let%test _ =
  parse "let () = () and () = print_int 1"
  = [ LetAndIn
        ( [ Let (Nonrecursive, Const Unit, [], Pattern (Const Unit))
          ; Let
              ( Nonrecursive
              , Const Unit
              , []
              , Application (Pattern (VarId "print_int"), Pattern (Const (Int 1))) )
          ]
        , None )
    ]
;;

(* Operation override *)

let%test _ =
  parse "let ( + ) x y = x - y"
  = [ Let
        ( Nonrecursive
        , VarId "+"
        , [ VarId "x"; VarId "y" ]
        , BinOp (SUB, Pattern (VarId "x"), Pattern (VarId "y")) )
    ]
;;

(* Patterns in functions' names *)

let%test _ =
  parse "let f x = let (k, j) = x in j in f (1, 2)"
  = [ LetAndIn
        ( [ Let
              ( Nonrecursive
              , VarId "f"
              , [ VarId "x" ]
              , LetAndIn
                  ( [ Let
                        ( Nonrecursive
                        , Tuple [ Pattern (VarId "k"); Pattern (VarId "j") ]
                        , []
                        , Pattern (VarId "x") )
                    ]
                  , Some (Pattern (VarId "j")) ) )
          ]
        , Some
            (Application
               ( Pattern (VarId "f")
               , Pattern (Tuple [ Pattern (Const (Int 1)); Pattern (Const (Int 2)) ]) ))
        )
    ]
;;

let%test _ =
  parse "let (a,b) = (1,2) in b"
  = [ LetAndIn
        ( [ Let
              ( Nonrecursive
              , Tuple [ Pattern (VarId "a"); Pattern (VarId "b") ]
              , []
              , Pattern (Tuple [ Pattern (Const (Int 1)); Pattern (Const (Int 2)) ]) )
          ]
        , Some (Pattern (VarId "b")) )
    ]
;; *)
