open AstLib.Ast

let not_supported = [ "GET_HEAD"; "GET_TALE"; "GET_NTH" ]

let codegen_funs =
  [ ( "create_closure"
    , TArr (TGround GInt, TArr (TGround GInt, TArr (TGround GInt, TGround GInt))) )
  ; ( "apply_args_to_closure"
    , TArr (TGround GInt, TArr (TGround GInt, TArr (TGround GInt, TVar "'vararg"))) )
  ]
;;

let is_optimized_binop = function
  | "+" | "-" | "*" | "/" -> true
  | _ -> false
;;

let runtime_members =
  let open Llvm_utils in
  Common.Stdlib.stdlib_typed
  |> List.filter (fun (name, _) ->
    not (List.mem name not_supported))
  |> List.map (fun (name, typ) -> map_ident_to_runtime name, typ)
  |> ( @ ) codegen_funs
;;
