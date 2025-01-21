open Base
open Ast
open Tast
open Types
open Vars

let is_simple = function
  | PIdent _ | PWild -> true
  | _ -> false
;;

let eliminate_arg_patterns body pattern_list =
  let generate_unique_arg_name used_names =
    let rec find_unique cnt =
      let current_name = "`arg_" ^ string_of_int cnt in
      match Set.find used_names ~f:(String.equal current_name) with
      | None -> current_name
      | _ -> find_unique (cnt + 1)
    in
    find_unique (Set.length used_names)
  in
  let rec helper used_names current_expr args_list = function
    | head :: tail ->
      (match head with
       | PIdent id -> helper (Set.add used_names id) current_expr (id :: args_list) tail
       | _ ->
         let arg_name = generate_unique_arg_name used_names in
         helper
           (Set.add used_names arg_name)
           (te_match
              (texpr_type current_expr)
              (te_var dummy_ty arg_name)
              [ head, current_expr ])
           (arg_name :: args_list)
           tail)
    | _ -> List.rev args_list, current_expr
  in
  let used_names = free_vars_texpr body in
  let ids, exp = helper used_names body [] pattern_list in
  List.map ids ~f:p_ident, exp
;;

let remove_patterns =
  let rec helper_expr = function
    | TEConst _ as c -> c
    | TEVar _ as v -> v
    | TEApp (t, l, r) -> te_app t (helper_expr l) (helper_expr r)
    | TEIfElse (ty, i, t, e) ->
      te_if_else ty (helper_expr i) (helper_expr t) (helper_expr e)
    | TEFun (t, p, e) ->
      let p, e = eliminate_arg_patterns e p in
      te_fun t p e
    | TELetIn (t, def, body) ->
      (* TODO: for complex patterns, transform
         let pattern = expr in body
         into
         match expr with pattern -> body
      *)
      let body = helper_expr body in
      (match def with
       | TDLet (_, _, pat, exp) ->
         if is_simple pat
         then te_let_in t (helper_def def) body
         else te_match t (helper_expr exp) [ pat, body ])
    | TETuple (t, xs) -> te_tuple t (List.map xs ~f:helper_expr)
    | TEList (t, xs) -> te_list t (List.map xs ~f:helper_expr)
    | TEMatch (t, e, c) ->
      te_match t (helper_expr e) (List.map c ~f:(fun (p, e) -> p, helper_expr e))
  and helper_def = function
    | TDLet (t, r, p, e) ->
      (* TODO:
         for tuple/list patterns, bind to temp var and bind inner vars from it
         from here, a pattern in definition should be a single id (?)
      *)
      assert (is_simple p);
      td_let_flag r t p (helper_expr e)
  in
  List.map ~f:helper_def
;;
