open Base
open Anf_ast
open Cf_ast

let rec anf_expr expr cont =
  match expr with
  | CFConst c ->
    (match c with
     | Ast.CInt n -> cont (ImmInt n)
     | Ast.CBool b -> cont (ImmBool b)
     | _ -> failwith "Not implemented")
  | CFVar x -> cont (ImmId x)
  | CFApp (fun_expr, arg_expr) -> failwith "Not implemented"
  | CFIfElse (condition, true_branch, false_branch) -> failwith "Not implemented"
  | CFLetIn (def, expr) -> failwith "Not implemented"
  | _ -> failwith "Not implemented"
;;
