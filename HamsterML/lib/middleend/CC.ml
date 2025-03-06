open Base
open Ast

let stdlib_names = Set.of_list (module String) [ "print_int"; "print_endline" ]

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
  let remove_stdlib_names s = Set.diff s stdlib_names in
  match exp with
  | EConst _ | EOperation _ -> Set.empty (module String)
  | EVar id -> Set.singleton (module String) id
  | ETuple (e1, e2, e_list) ->
    let exps = e1 :: e2 :: e_list in
    let free =
      List.fold
        exps
        ~init:(Set.empty (module String))
        ~f:(fun acc e -> Set.union acc (free_vars_expr e))
    in
    remove_stdlib_names free
  | EList exps ->
    let free =
      List.fold
        exps
        ~init:(Set.empty (module String))
        ~f:(fun acc e -> Set.union acc (free_vars_expr e))
    in
    remove_stdlib_names free
  | EListConcat (e1, e2) | Application (e1, e2) ->
    let free = Set.union (free_vars_expr e1) (free_vars_expr e2) in
    remove_stdlib_names free
  | If (e1, e2, e3_opt) ->
    let base = Set.union (free_vars_expr e1) (free_vars_expr e2) in
    let free =
      match e3_opt with
      | Some e -> Set.union base (free_vars_expr e)
      | None -> base
    in
    remove_stdlib_names free
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
    let free = Set.union (free_vars_expr e) free_in_cases in
    remove_stdlib_names free
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
    let free = Set.union free_in_binds free_in_in_expr in
    remove_stdlib_names free
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

let rec cc_expr =
  let pattern_list_to_vars patterns = List.map patterns ~f:(fun x -> Var x) in
  let args_list_to_id_set args =
    List.fold
      args
      ~init:(Set.empty (module String))
      ~f:(fun acc arg ->
        match arg with
        | Var x -> Set.add acc x
        | _ -> acc)
  in
  let extract_fun_args expr =
    let rec loop e acc =
      match e with
      | Fun (fun_args, fun_body) -> loop fun_body (acc @ fun_args)
      | _ -> acc, e
    in
    loop expr []
  in
  let add_free_vars args expr =
    let unbound = free_vars_expr expr in
    let bound = args_list_to_id_set args in
    let free_vars = Set.diff unbound bound in
    args @ pattern_list_to_vars (Set.to_list free_vars)
  in
  function
  | EVar _ as e -> e
  | EConst _ as e -> e
  | EOperation _ as e -> e
  | ETuple (e1, e2, es) -> ETuple (cc_expr e1, cc_expr e2, List.map es ~f:cc_expr)
  | EList es -> EList (List.map es ~f:cc_expr)
  | EListConcat (e1, e2) -> EListConcat (cc_expr e1, cc_expr e2)
  | If (cond, then_expr, else_expr) ->
    If (cc_expr cond, cc_expr then_expr, Option.map else_expr ~f:cc_expr)
  | Match (expr, cases) ->
    Match (cc_expr expr, List.map cases ~f:(fun (pat, body) -> pat, cc_expr body))
  | EConstraint (expr, ty) -> EConstraint (cc_expr expr, ty)
  | Application (e1, e2) -> Application (cc_expr e1, cc_expr e2)
  | Fun (args, expr) ->
    (* Process based on whether the expression to the right of the arrow is a `fun` expression *)
    (match expr with
     | Fun _ ->
       let all_args, final_body = extract_fun_args (Fun (args, expr)) in
       let new_args = add_free_vars all_args final_body in
       Fun (new_args, cc_expr final_body)
     | _ ->
       let new_args = add_free_vars args expr in
       Fun (new_args, cc_expr expr))
  | Let (fun_type, binds, in_expr_opt) ->
    let cc_binds =
      List.map binds ~f:(fun (pat, args, expr) ->
        let all_args, body = extract_fun_args expr in
        let combined_args = all_args @ args in
        let final_args =
          match fun_type with
          | Recursive ->
            let unbound = free_vars_expr body in
            let bound = args_list_to_id_set combined_args in
            let fun_names = bound_vars_in_pattern pat in
            let free_vars = Set.diff (Set.diff unbound bound) fun_names in
            combined_args @ pattern_list_to_vars (Set.to_list free_vars)
          | Nonrecursive -> add_free_vars combined_args body
        in
        pat, final_args, cc_expr body)
    in
    let new_in_expr_opt = Option.map in_expr_opt ~f:cc_expr in
    Let (fun_type, cc_binds, new_in_expr_opt)
;;
;;
