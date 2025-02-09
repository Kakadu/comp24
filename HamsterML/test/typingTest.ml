(* open HamsterML.Typing

let typecheck f x =
  let res = R.run (f TypeEnv.empty x) in
  match res with
  | Ok (_, t) -> t
  | Error e -> failwith (show_error e)
;;

let typecheck_expr = typecheck Infer.infer_expr

(* --- data types *)
let%test _ = typecheck_expr (Pattern (Const (Int 228))) = TInt
let%test _ = typecheck_expr (Pattern (Const (Float 228.337))) = TFloat
let%test _ = typecheck_expr (Pattern (Const (String "kfc boss"))) = TString
let%test _ = typecheck_expr (Pattern (Const (Bool true))) = TBool
let%test _ = typecheck_expr (Pattern (Const (Char '1'))) = TChar

(* --- binary operations *)
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

(* --- patterns *)

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
    (Fun
       ( [ VarId "x"; Const Unit; VarId "y"; Const Unit; VarId "z"; Const Unit ]
       , BinOp
           ( ADD
           , BinOp (ADD, Pattern (VarId "x"), Pattern (VarId "y"))
           , Pattern (VarId "z") ) ))
  = TArrow
      ( TInt
      , TArrow (TUnit, TArrow (TInt, TArrow (TUnit, TArrow (TInt, TArrow (TUnit, TInt)))))
      )
;; *)
