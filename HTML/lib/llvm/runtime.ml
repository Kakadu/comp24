open AstLib.Ast

let not_supported = [ "GET_HEAD"; "GET_TALE"; "GET_NTH" ]

let codegen_funs =
  [ ( "create_closure"
    , TArr (TGround GInt, TArr (TGround GInt, TArr (TGround GInt, TGround GInt))) )
  ; ( "apply_args_to_closure"
    , TArr (TGround GInt, TArr (TGround GInt, TArr (TGround GInt, TGround GInt))) )
  ]
;;

let is_binop = function
  | "+" | "-" | "*" | "/" -> true
  | _ -> false
;;

let runtime_members =
  List.filter
    (fun (name, _) -> (not @@ List.mem name not_supported) && (not @@ is_binop name))
    Common.Stdlib.stdlib_typed
  @ codegen_funs
;;
