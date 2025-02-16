open HamsterML.Typing
open ParserTest

let infer_pattern (s : string) : inf_type =
  let inf_res = R.run (Infer.infer_pattern TypeEnv.default (parse_pattern s)) in
  match inf_res with
  | Ok (_, t) -> t
  | Error e -> failwith (show_error e)
;;

let infer_expr (s : string) : inf_type =
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
let%test _ = infer_expr "fun [(a: int); b] -> b" = TArrow (TList (TPVar 1), TInt)

let%test _ =
  infer_expr "fun (a, b) -> (1, 2)"
  = TArrow (TTuple [ TPVar 0; TPVar 1 ], TTuple [ TInt; TInt ])
;;

(* lists *)

let%test _ = infer_expr "fun x -> [(x: int)]" = TArrow (TInt, TList TInt)

let%test _ =
  infer_expr "fun x y z -> [x; y; (z: int)]"
  = TArrow (TInt, TArrow (TInt, TArrow (TInt, TList TInt)))
;;

let%test _ =
  infer_expr "fun x y z -> [(x: int); y; z]"
  = TArrow (TInt, TArrow (TInt, TArrow (TInt, TList TInt)))
;;

let%test _ =
  infer_expr "fun x y z -> [x; y; (z: int)]"
  = TArrow (TInt, TArrow (TInt, TArrow (TInt, TList TInt)))
;;

let%test _ =
  infer_expr "fun x (y: int) z -> [x; y; z]"
  = TArrow (TInt, TArrow (TInt, TArrow (TInt, TList TInt)))
;;

(* tuples *)

let%test _ =
  infer_expr "fun x -> ((x : int), x, x, x, x)"
  = TArrow (TInt, TTuple [ TInt; TInt; TInt; TInt; TInt ])
;;

let%test _ =
  infer_expr "fun (x: string) -> (x, x, x, x, x)"
  = TArrow (TString, TTuple [ TString; TString; TString; TString; TString ])
;;

let%test _ =
  infer_expr "fun x -> (x, x, x, x, (x: bool))"
  = TArrow (TBool, TTuple [ TBool; TBool; TBool; TBool; TBool ])
;;

(* concat *)
let%test _ =
  infer_expr "fun x y -> x :: y"
  = TArrow (TPVar 0, TArrow (TList (TPVar 0), TList (TPVar 0)))
;;

let%test _ =
  infer_expr "fun x y -> (x: int) :: y" = TArrow (TInt, TArrow (TList TInt, TList TInt))
;;

let%test _ =
  infer_expr "fun x y -> (x: bool) :: [(y: bool)]"
  = TArrow (TBool, TArrow (TBool, TList TBool))
;;

let%test _ =
  infer_expr "fun (x: string) y -> x :: [y]"
  = TArrow (TString, TArrow (TString, TList TString))
;;

(* if *)
let%test _ = infer_expr "fun x -> if x then 10 else 11" = TArrow (TBool, TInt)
let%test _ = infer_expr "fun x -> if x then 10 else 11" = TArrow (TBool, TInt)

let%test _ =
  infer_expr "fun x y z -> if x then (y: string) else z"
  = TArrow (TBool, TArrow (TString, TArrow (TString, TString)))
;;

let%test _ =
  infer_expr "fun x y z -> if x then y else (z: string)"
  = TArrow (TBool, TArrow (TString, TArrow (TString, TString)))
;;

let%test _ =
  infer_expr "fun x y -> if x then (y: string) else y"
  = TArrow (TBool, TArrow (TString, TString))
;;

(* application *)

let%test _ = infer_expr "(fun x y -> x) 1" = TArrow (TPVar 1, TInt)
let%test _ = infer_expr "(fun x y -> x) 1 2" = TInt
let%test _ = infer_expr "(fun f x y -> f x y) (+) 1 2" = TInt
let%test _ = infer_expr "(fun f x y -> f x y) (+) 1" = TArrow (TInt, TInt)
let%test _ = infer_expr "(fun f x y -> f x y) (+)" = TArrow (TInt, TArrow (TInt, TInt))
let%test _ = infer_expr "(fun f x y -> f x y) (fun a b -> a + b) 1 2" = TInt

let%test _ =
  infer_expr "(fun v x -> (fun (y: bool) -> (x: int))) 1"
  = TArrow (TInt, TArrow (TBool, TInt))
;;

(* match *)

let%test _ = infer_expr "fun x -> match x with 10 -> 10 | _ -> 0" = TArrow (TInt, TInt)

let%test _ =
  infer_expr "fun xs -> match xs with [] -> 10 | h::tl -> h" = TArrow (TList TInt, TInt)
;;

let%test _ =
  infer_expr
    "fun f xs -> match xs with \n\
    \      | [] -> [] \n\
    \      | (a: int)::[] -> [(f a: bool)] \n\
    \      | a::b::[] -> [f a; f b] \n\
    \      | a::b::c::[] -> [f a; f b; f c] \n\
    \      | a::b::c::d::tl -> f a :: f b :: f c :: f d :: []"
  = TArrow (TArrow (TInt, TBool), TArrow (TList TInt, TList TBool))
;;

(* Let *)

let%test _ = infer_expr "let x = true" = TBool
let%test _ = infer_expr "let (a, b) = (1, 2)" = TTuple [ TInt; TInt ]
let%test _ = infer_expr "let a, b = 1, 2" = TTuple [ TInt; TInt ]
let%test _ = infer_expr "let f x y = x + y" = TArrow (TInt, TArrow (TInt, TInt))
let%test _ = infer_expr "let f = fun x y -> x + y" = infer_expr "let f x y = x + y"

let%test _ =
  infer_expr "let f x y = if x = y && x = 10 then 1 else 2"
  = TArrow (TInt, TArrow (TInt, TInt))
;;

let%test _ = infer_expr "let x () () = 10" = TArrow (TUnit, TArrow (TUnit, TInt))
let%test _ = infer_expr "let x () = ()" = TArrow (TUnit, TUnit)
