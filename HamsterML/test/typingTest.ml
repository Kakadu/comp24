open HamsterML.Typing
open HamsterML.Ast
open ParserTest

let parse_expr (s : string) = List.hd (parse s)

let typecheck f x =
  let res = R.run (f TypeEnv.empty x) in
  match res with
  | Ok (_, t) -> t
  | Error e -> failwith (show_error e)
;;

let typecheck_expr x =
  let res = R.run (Infer.infer_expr TypeEnv.empty x) in
  match res with
  | Ok (s, t) -> Subst.apply t s
  | Error e -> failwith (show_error e)
;;

(* -- Let *)

let%test _ = typecheck_expr (parse_expr "let x = true") = TBool
let%test _ = typecheck_expr (parse_expr "let (a, b) = (1, 2)") = TTuple [ TInt; TInt ]

let%test _ =
  typecheck_expr (parse_expr "let f x y = x + y") = TArrow (TInt, TArrow (TInt, TInt))
;;

let%test _ =
  typecheck_expr (parse_expr "let f = fun x y -> x + y")
  = typecheck_expr (parse_expr "let f x y = x + y")
;;

let%test _ =
  typecheck_expr (parse_expr "let f x y = if x = y && x = 10 then 1 else 2")
  = TArrow (TInt, TArrow (TInt, TInt))
;;

let%test _ =
  typecheck_expr (parse_expr "let f x = if x = 10 then true else false")
  = TArrow (TInt, TBool)
;;

let%test _ =
  typecheck_expr (parse_expr "let f x = if x = 10 then true else false")
  = TArrow (TInt, TBool)
;;

let%test _ = typecheck_expr (parse_expr "let x () () = 10")
 = TArrow (TUnit, TArrow (TUnit, TInt))

(* --- Data types *)
let%test _ = typecheck_expr (Pattern (Const (Int 228))) = TInt
let%test _ = typecheck_expr (Pattern (Const (Float 228.337))) = TFloat
let%test _ = typecheck_expr (Pattern (Const (String "kfc boss"))) = TString
let%test _ = typecheck_expr (Pattern (Const (Bool true))) = TBool
let%test _ = typecheck_expr (Pattern (Const (Char '1'))) = TChar

(* --- Binary operations *)
let%test _ =
  typecheck_expr (BinOp (ADD, Pattern (Const (Int 1)), Pattern (Const (Int 1)))) = TInt
;;

let%test _ =
  typecheck_expr
    (BinOp (CONCAT, Pattern (Const (String "nike")), Pattern (Const (String "pro"))))
  = TString
;;

let%test _ =
  typecheck_expr (BinOp (EQ, Pattern (Const (Int 1)), Pattern (Const (Int 20)))) = TBool
;;

(* fun [10; 10] = () => int list -> unit *)
let%test _ =
  typecheck_expr (Fun ([ List [ Const (Int 10); Const (Int 20) ] ], Pattern (Const Unit)))
  = TArrow (TList TInt, TUnit)
;;

(* --- Patterns *)

(* (1, 2, 3) => int * int * int  *)
let%test _ =
  typecheck Infer.infer_pattern (Tuple [ Const (Int 1); Const (Int 2); Const (Int 3) ])
  = TTuple [ TInt; TInt; TInt ]
;;

(* (1, "2", 3) => int * string * int  *)
let%test _ =
  typecheck
    Infer.infer_pattern
    (Tuple [ Const (Int 1); Const (String "2"); Const (Int 3) ])
  = TTuple [ TInt; TString; TInt ]
;;

(* (1, 2, 3) => int * int * int  *)
let%test _ =
  typecheck Infer.infer_pattern (Tuple [ Const (Int 1); Const (Int 2); Const (Int 1) ])
  = TTuple [ TInt; TInt; TInt ]
;;

(* ([1; 2; 3], "123", 123) => int list * string * int *)
let%test _ =
  typecheck
    Infer.infer_pattern
    (Tuple
       [ List [ Const (Int 1); Const (Int 2); Const (Int 3) ]
       ; Const (String "123")
       ; Const (Int 123)
       ])
  = TTuple [ TList TInt; TString; TInt ]
;;

(* 1 :: [2; 3] => int list  *)

let%test _ =
  typecheck
    Infer.infer_pattern
    (ListConcat (Const (Int 1), List [ Const (Int 2); Const (Int 3) ]))
  = TList TInt
;;