open HamsterML.Typing

let typecheck expr =
  match infer expr with
  | Ok (_, t) -> t
  | Error e -> failwith (show_error e)
;;

(* data types *)
let%test _ = typecheck (Pattern (Const (Int 228))) = TInt
let%test _ = typecheck (Pattern (Const (Float 228.337))) = TFloat
let%test _ = typecheck (Pattern (Const (String "kfc boss"))) = TString
let%test _ = typecheck (Pattern (Const (Bool true))) = TBool
let%test _ = typecheck (Pattern (Const (Char '1'))) = TChar

(* binary operations *)
let%test _ =
  typecheck (BinOp (ADD, Pattern (Const (Int 1)), Pattern (Const (Int 1)))) = TInt
;;

let%test _ =
  typecheck
    (BinOp (CONCAT, Pattern (Const (String "nike")), Pattern (Const (String "pro"))))
  = TString
;;

let%test _ = typecheck (BinOp (EQ, Value (Const (Int 1)), Value (Const (Int 20)))) = TBool

(* fun *)

let%test _ =
  typecheck
    (Fun
       ( [ VarId "x"; Const Unit; VarId "y"; Const Unit; VarId "z"; Const Unit ]
       , BinOp (ADD, BinOp (ADD, Value (VarId "x"), Value (VarId "y")), Value (VarId "z"))
       ))
  = TArrow
      ( TInt
      , TArrow (TUnit, TArrow (TInt, TArrow (TUnit, TArrow (TInt, TArrow (TUnit, TInt)))))
      )
;;
