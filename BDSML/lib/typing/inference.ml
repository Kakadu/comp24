open Types
open Monads
open Helpers
open Parser.Ast

let last_expr_id = VarId.create 0
let start_var_id = VarId.create 1
let fresh_var = fresh >>| fun n -> TVar n
let init_env = TypeEnv.empty

let rec infer_base_type c =
  let const_to_type = function
    | Const_int _ -> TInt
    | Const_char _ -> TChar
    | Const_string _ -> TString
    | Const_bool _ -> TBool
  in
  return (Subst.empty, TBase (const_to_type c))

and infer_if env cond bthen belse =
  let bool_type = TBase TBool in
  let* s1, t1 = infer_expression env cond in
  let* s2, t2 = infer_expression env bthen in
  let* s3 = Subst.unify t1 bool_type in
  let* else_branch_subs, res_type =
    match belse with
    | Some exp ->
      let* s4, t3 = infer_expression env exp in
      let+ s5 = Subst.unify t2 t3 in
      [ s4; s5 ], Subst.apply s5 t2
    | None -> return ([], t2)
  in
  let+ united_sub = Subst.compose_all @@ else_branch_subs @ [ s3; s2; s1 ] in
  united_sub, res_type

and infer_expression env expr =
  let rec helper env = function
    | Exp_constant c -> infer_base_type c
    | Exp_if (cond, bthen, belse) -> infer_if env cond bthen belse
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
