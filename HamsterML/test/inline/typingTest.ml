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

let infer_prog (s : string) =
  let inf_res = R.run (Infer.infer_prog TypeEnv.default (parse_prog s)) in
  match inf_res with
  | Ok env -> TypeEnv.pp Format.std_formatter env
  | Error e -> failwith (show_error e)
;;

(* --- Patterns --- *)

let%test _ = infer_pattern "(1,2,3)" = TTuple [ TInt; TInt; TInt ]
let%test _ = infer_pattern "(1, true, 2)" = TTuple [ TInt; TBool; TInt ]
let%test _ = infer_pattern "()" = TUnit
let%test _ = infer_pattern "([1; 2; 3], true, 10)" = TTuple [ TList TInt; TBool; TInt ]

(* 1 :: [2; 3] => int list *)

let%test _ = infer_pattern "1 :: [2; 3]" = TList TInt

(* --- Expressions --- *)

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

let%test _ = infer_expr "fun x -> if x = 10 then true else false" = TArrow (TInt, TBool)

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
let%test _ = infer_expr "let f x y = x + y in 1" = TInt
let%test _ = infer_expr "let f x = x in f" = TArrow (TPVar 2, TPVar 2)
let%test _ = infer_expr "let f x = x in f 1" = TInt
let%test _ = infer_expr "let f x y = x + y in f 1 2" = TInt
let%test _ = infer_expr "let f x = (x: int) in f" = TArrow (TInt, TInt)

let%test _ =
  infer_expr "fun f (x: int) (y: string) -> (f x y : bool)"
  = TArrow (TArrow (TInt, TArrow (TString, TBool)), TArrow (TInt, TArrow (TString, TBool)))
;;

let%test _ =
  infer_expr "fun x, y -> let a, b = (x: int), (y: bool)"
  = TArrow (TTuple [ TInt; TBool ], TTuple [ TInt; TBool ])
;;

let%test _ =
  infer_expr "fun (x: int), (y: bool) -> let a, b = x, y"
  = TArrow (TTuple [ TInt; TBool ], TTuple [ TInt; TBool ])
;;

let%test _ =
  infer_expr "fun (x: bool), y -> let a, b = x, (y: int) in a"
  = TArrow (TTuple [ TBool; TInt ], TBool)
;;

let%test _ =
  infer_expr "fun (a: int),(b: bool) -> let a,b = a, b in a"
  = TArrow (TTuple [ TInt; TBool ], TInt)
;;

let%test _ =
  infer_expr "let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)"
  = TArrow (TBool, TArrow (TInt, TInt))
;;

let%test _ = infer_expr "fun y -> let f x = x in f (y: bool)" = TArrow (TBool, TBool)
let%test _ = infer_expr "fun x -> let f x = x in f (x: bool)" = TArrow (TBool, TBool)
let%test _ = infer_expr "fun (x: bool) -> let f x = x in f x" = TArrow (TBool, TBool)
let%test _ = infer_expr "let (+) = (&&)" = TArrow (TBool, TArrow (TBool, TBool))
let%test _ = infer_expr "let (+) x y = (&&) x y" = TArrow (TBool, TArrow (TBool, TBool))
let%test _ = infer_expr "let f x = x in let g x y = x + y in g (f 1) (f 2)" = TInt
let%test _ = infer_expr "let a = 10 and b = 20 and c = 30 in a + b + c" = TInt

let%test _ =
  infer_expr "let f x = x and g x y = x, y in g (f 1) (f true)" = TTuple [ TInt; TBool ]
;;

let%test _ =
  infer_expr {|let f x y = (x, y) in let a = f 1 "hello" in let b = f true 3 in (a, b) |}
  = TTuple [ TTuple [ TInt; TString ]; TTuple [ TBool; TInt ] ]
;;

let%test _ =
  infer_expr "let temp = let f = fun x -> x in (f 1, f true)" = TTuple [ TInt; TBool ]
;;

let%test _ =
  infer_expr "fun tpl -> let (x, y) = tpl in x - y" = TArrow (TTuple [ TInt; TInt ], TInt)
;;

let%test _ =
  infer_expr "let fodd p n = let (e, o) = p in if n == 0 then 0 else e (n - 1)"
  = TArrow (TTuple [ TArrow (TInt, TInt); TPVar 3 ], TArrow (TInt, TInt))
;;

(* Let rec *)
let%test _ = infer_expr "let rec x = 10 :: x" = TList TInt

let%test _ =
  infer_expr "let rec fac n = if n <= 1 then 1 else fac (n-1) * n" = TArrow (TInt, TInt)
;;

let%test _ =
  infer_expr "let rec fac_cps n k = if n=1 then k 1 else fac_cps (n-1) (fun p -> k (p*n))"
  = TArrow (TInt, TArrow (TArrow (TInt, TPVar 13), TPVar 13))
;;

let%test _ =
  infer_expr "let rec fib n = if n<2 then n else fib (n - 1) + fib (n - 2)"
  = TArrow (TInt, TInt)
;;

let%test _ =
  infer_expr
    "let rec fib_acc a b n = if n=1 then b else let n1 = n-1 in let ab = a+b in fib_acc \
     b ab n1"
  = TArrow (TInt, TArrow (TInt, TArrow (TInt, TInt)))
;;

let%test _ =
  infer_expr "let rec fix f = f (fix f : int)" = TArrow (TArrow (TInt, TInt), TInt)
;;

let%test _ =
  infer_expr "let rec fix f x = f (fix f) x"
  = TArrow
      ( TArrow (TArrow (TPVar 2, TPVar 5), TArrow (TPVar 2, TPVar 5))
      , TArrow (TPVar 2, TPVar 5) )
;;

let%test _ =
  infer_expr
    "let rec map f xs =\n\
    \  match xs with\n\
    \  | [] -> []\n\
    \  | a::[] -> [f a]\n\
    \  | a::b::[] -> [f a; f b]\n\
    \  | a::b::c::[] -> [f a; f b; f c]\n\
    \  | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl"
  = TArrow (TArrow (TPVar 24, TPVar 8), TArrow (TList (TPVar 24), TList (TPVar 8)))
;;

let%test _ =
  infer_expr "let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)"
  = TArrow (TList (TPVar 5), TArrow (TList (TPVar 5), TList (TPVar 5)))
;;

let%test _ =
  infer_expr
    "let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter f tl"
  = TArrow (TArrow (TPVar 5, TUnit), TArrow (TList (TPVar 5), TUnit))
;;

(* Let and *)

let%test _ =
  infer_expr
    "let rec meven n = if n = 0 then 1 else modd (n - 1) and modd n = if n = 0 then 1 \
     else meven (n - 1) in meven 10"
  = TInt
;;

(* Constraints *)

let%test _ =
  infer_expr "let foo (f: 'a -> 'b -> 'c * 'c list) = 10"
  = TArrow (TArrow (TPVar 1, TArrow (TPVar 2, TTuple [ TPVar 3; TList (TPVar 3) ])), TInt)
;;
