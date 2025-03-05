open Base
open Ast

(* Remove all the variables that are bound by a single pattern from free variables set *)
let rec remove_bound_vars_from_pattern free (pat : pattern) =
  match pat with
  | Wildcard | Operation _ | Const _ -> free
  | Var id -> Set.remove free id
  | ListConcat (p1, p2) -> remove_bound_vars_from_patterns free [ p1; p2 ]
  | Tuple (p1, p2, p_list) ->
    let p = p1 :: p2 :: p_list in
    remove_bound_vars_from_patterns free p
  | List pats -> remove_bound_vars_from_patterns free pats
  | Constraint (p, _) -> remove_bound_vars_from_pattern free p

and remove_bound_vars_from_patterns free (pats : pattern list) =
  List.fold ~init:free pats ~f:(fun acc pat -> remove_bound_vars_from_pattern acc pat)
;;

(* Collect the bound variables that a pattern binds, e.g. `match x with ...` *)
let rec bound_vars_in_pattern (pat : pattern) =
  match pat with
  | Wildcard | Operation _ | Const _ -> Set.empty (module String)
  | Var id -> Set.singleton (module String) id
  | ListConcat (p1, p2) -> Set.union (bound_vars_in_pattern p1) (bound_vars_in_pattern p2)
  | Tuple (p1, p2, p_list) ->
    let pats = p1 :: p2 :: p_list in
    List.fold
      pats
      ~init:(Set.empty (module String))
      ~f:(fun acc p -> Set.union acc (bound_vars_in_pattern p))
  | List pats ->
    List.fold
      pats
      ~init:(Set.empty (module String))
      ~f:(fun acc p -> Set.union acc (bound_vars_in_pattern p))
  | Constraint (p, _) -> bound_vars_in_pattern p
;;

(* Find free variables in an expression *)
let rec free_vars_expr (exp : expr) =
  match exp with
  | EConst _ | EOperation _ -> Set.empty (module String)
  | EVar id -> Set.singleton (module String) id
  | ETuple (e1, e2, e_list) ->
    let exps = e1 :: e2 :: e_list in
    List.fold
      exps
      ~init:(Set.empty (module String))
      ~f:(fun acc e -> Set.union acc (free_vars_expr e))
  | EList exps ->
    List.fold
      exps
      ~init:(Set.empty (module String))
      ~f:(fun acc e -> Set.union acc (free_vars_expr e))
  | EListConcat (e1, e2) | Application (e1, e2) ->
    Set.union (free_vars_expr e1) (free_vars_expr e2)
  | If (e1, e2, e3_opt) ->
    let base = Set.union (free_vars_expr e1) (free_vars_expr e2) in
    (match e3_opt with
     | Some e -> Set.union base (free_vars_expr e)
     | None -> base)
  | Match (e, cases) ->
    let free_in_cases =
      List.fold
        cases
        ~init:(Set.empty (module String))
        ~f:(fun acc (pat, body) ->
          let body_free = free_vars_expr body in
          let body_free = remove_bound_vars_from_pattern body_free pat in
          Set.union acc body_free)
    in
    Set.union (free_vars_expr e) free_in_cases
  | Let (fun_type, binds, in_expr_opt) ->
    (* Determine whether the let is recursive *)
    let is_rec =
      match fun_type with
      | Recursive -> true
      | Nonrecursive -> false
    in
    (* The names bound by the let (from the patterns of each binding) *)
    let bound_names =
      List.fold
        binds
        ~init:(Set.empty (module String))
        ~f:(fun acc (pat, _args, _body) -> Set.union acc (bound_vars_in_pattern pat))
    in
    (* Compute free variables from each binding's definition *)
    let free_in_binds =
      List.fold
        binds
        ~init:(Set.empty (module String))
        ~f:(fun acc (pat, args, body) ->
          let fvs = free_vars_expr body in
          (* Remove the variables bound by the function arguments *)
          let fvs =
            List.fold args ~init:fvs ~f:(fun acc arg ->
              remove_bound_vars_from_pattern acc arg)
          in
          (* For recursive lets, remove the binding pattern from the binding's body *)
          let fvs = if is_rec then remove_bound_vars_from_pattern fvs pat else fvs in
          (* In recursive lets, also remove all let-bound names *)
          let fvs = if is_rec then Set.diff fvs bound_names else fvs in
          Set.union acc fvs)
    in
    let free_in_in_expr =
      match in_expr_opt with
      | Some e -> Set.diff (free_vars_expr e) bound_names
      | None -> Set.empty (module String)
    in
    Set.union free_in_binds free_in_in_expr
  | EConstraint (e, _) -> free_vars_expr e
  | Fun (args, body) ->
    let free = free_vars_expr body in
    List.fold args ~init:free ~f:(fun acc arg -> remove_bound_vars_from_pattern acc arg)
;;

(* Generate fresh variable names *)
let fresh_var_counter = ref 0

let fresh_var prefix =
  let id = !fresh_var_counter in
  fresh_var_counter := id + 1;
  prefix ^ Int.to_string id
;;

(* Closure conversion function *)
let rec cc_expr = function
  | Let (fun_type, binds, in_expr_opt) ->
    let cc_binds =
      List.map binds ~f:(fun (pat, args, expr) ->
        match expr with
        | Application (Fun ([ Var _ ], body), EVar _)
        | Application (Fun ([ Var _ ], body), EConst _) ->
          (* when String.equal param_name arg_name -> *)
          (match cc_expr body with
           | Fun (inner_args, inner_body) -> pat, args @ inner_args, cc_expr inner_body
           | inner_body -> pat, args, cc_expr inner_body)
        | Fun (fun_args, fun_body) ->
          let rec collect_nested_funs acc_args = function
            | Fun (inner_args, inner_body) ->
              collect_nested_funs (acc_args @ inner_args) inner_body
            | body -> acc_args, body
          in
          let all_args, innermost_body = collect_nested_funs fun_args fun_body in
          pat, args @ all_args, cc_expr innermost_body
        | _ -> pat, args, cc_expr expr)
    in
    let cc_in_expr = Option.map in_expr_opt ~f:cc_expr in
    Let (fun_type, cc_binds, cc_in_expr)
  | Fun (args, expr) -> Fun (args, cc_expr expr)
  | EVar _ as e -> e
  | EConst _ as e -> e
  | EOperation _ as e -> e
  | ETuple (e1, e2, es) -> ETuple (cc_expr e1, cc_expr e2, List.map es ~f:cc_expr)
  | EList es -> EList (List.map es ~f:cc_expr)
  | EListConcat (e1, e2) -> EListConcat (cc_expr e1, cc_expr e2)
  | Application (e1, e2) -> Application (cc_expr e1, cc_expr e2)
  | If (cond, then_expr, else_expr) ->
    If (cc_expr cond, cc_expr then_expr, Option.map else_expr ~f:cc_expr)
  | Match (expr, cases) ->
    Match (cc_expr expr, List.map cases ~f:(fun (pat, body) -> pat, cc_expr body))
  | EConstraint (expr, ty) -> EConstraint (cc_expr expr, ty)
;;

(*
   let f = fun x -> fun y -> x + y                =>    let f x y = x + y
   let f = fyn x y -> x + y                       =>    let f x y = x + y
   let f = fun x y -> fun a b -> a b x y          =>    let f x y a b = a b x y
   let a = 1 in let b = 2 in let f = a + b in f   =>    let a = 1 in let b = 2 in let f arg1 arg2 = arg1 arg2 in f a b
   let f = (fun x -> x + 1) 2                     =>    let f x = x + 1 in f 2
   let a, b = 1, 2                                =>    let a = 1 in let b = 2 in a, b
*)
