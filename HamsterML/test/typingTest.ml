open HamsterML.Typing

let typecheck expr =
  match infer expr with
  | Ok (_, t) -> t
  | Error e -> failwith (show_error e)
;;

(* data types *)
let%test _ = typecheck (Value (Const (Int 228))) = TInt
let%test _ = typecheck (Value (Const (Float 228.337))) = TFloat
let%test _ = typecheck (Value (Const (String "kfc boss"))) = TString
let%test _ = typecheck (Value (Const (Bool true))) = TBool
let%test _ = typecheck (Value (Const (Char '1'))) = TChar

(* binary operations *)
let%test _ = typecheck (BinOp (ADD, Value (Const (Int 1)), Value (Const (Int 1)))) = TInt

let%test _ =
  typecheck (BinOp (CONCAT, Value (Const (String "nike")), Value (Const (String "pro"))))
  = TString
;;

let%test _ = typecheck (BinOp (EQ, Value (Const (Int 1)), Value (Const (Int 20)))) = TBool
