open HamsterML.Typing
open ParserTest

let infer_pattern (s : string) : inf_type =
  let inf_res = R.run (Infer.infer_pattern TypeEnv.default (parse_pattern s)) in
  match inf_res with
  | Ok (_, t) -> t
  | Error e -> failwith (show_error e)
;;

let infer_expr (s : string) : (inf_type) =
  let inf_res = R.run (Infer.infer_expr TypeEnv.default (parse_expr s)) in
  match inf_res with
  | Ok (s, t) -> Subst.apply t s
  | Error e -> failwith (show_error e)
;;

(* --- Patterns --- *)

let%test _ = infer_pattern "(1,2,3)" = TTuple [ TInt; TInt; TInt ]
let%test _ = infer_pattern "(1, true, 2)" = TTuple [ TInt; TBool; TInt ]
let%test _ = infer_pattern "()" = TUnit
let%test _ = infer_pattern "([1; 2; 3], true, 10)" = TTuple [ TList TInt; TBool; TInt ]

(* 1 :: [2; 3] => int list  *)

let%test _ = infer_pattern "1 :: [2; 3]" = TList TInt

(* --- Expressions ---  *)

let%test _ = infer_expr "fun (x: int) -> x" = TArrow (TInt, TInt)
let%test _ = infer_expr "fun x -> (x: int)" = TArrow (TInt, TInt)
let%test _ = infer_expr "fun x -> [(x: int)]" = TArrow (TInt, TList TInt)
let%test _ = infer_expr "fun x y z -> [x; y; (z: int)]" = TArrow (TInt, TArrow (TInt, TArrow (TInt, TList TInt)))
let%test _ = infer_expr "fun x y z -> [(x: int); y; z]" = TArrow (TInt, TArrow (TInt, TArrow (TInt, TList TInt)))
let%test _ = infer_expr "fun x y z -> [x; y; (z: int)]" = TArrow (TInt, TArrow (TInt, TArrow (TInt, TList TInt)))
let%test _ = infer_expr "fun x (y: int) z -> [x; y; z]" = TArrow (TInt, TArrow (TInt, TArrow (TInt, TList TInt)))


(* let%test _ = typecheck_expr (parse_expr "let x = true") = TBool
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
;; *)
