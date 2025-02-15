open Types
open Utils
open Subst
open Parser.Ast

let last_expr_id = VarId.create 0
let start_var_id = VarId.create 1
let fresh_var = fresh >>| fun n -> TVar n
let init_env = TypeEnv.empty

let infer_expression env expr =
  let rec helper env = function
    | Exp_constant c ->
      let const_to_type = function
        | Const_int _ -> TInt
        | Const_char _ -> TChar
        | Const_string _ -> TString
      in
      return (Subst.empty, TBase (const_to_type c))
    | _ -> raise (Unimplemented "infer_expr")
  in
  helper env expr
;;

let infer_structure_item prog =
  let rec helper env prog types =
    match prog with
    | [] -> return types
    | h :: tl ->
      (match h with
       | Str_eval e ->
         let* _, t = infer_expression env e in
         let types = types @ [ last_expr_id, t ] in
         helper env tl types
       | Str_value _ -> raise (Unimplemented "str_value infer"))
  in
  helper init_env prog []
;;

let error_to_string = function
  | Occurs_check -> "occurs check"
  | Unification_failed (val1, val2) ->
    "failed unification of types " ^ show_type_val val1 ^ " and " ^ show_type_val val2
;;

let infer_program prog =
  match run (infer_structure_item prog) start_var_id with
  | Ok _ as x -> x
  | Error e -> Error ("Type infering error: " ^ error_to_string e)
;;
