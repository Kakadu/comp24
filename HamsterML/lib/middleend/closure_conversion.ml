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

let unbound_variables (exp : expr) = free_vars_expr exp

let cc = function
  | Fun (args, expr) ->
    let unbound = unbound_variables expr in
    let new_expr =
      Let
        ( Nonrecursive
        , [ ( Var "anon"
            , List.append args (Set.to_list unbound |> List.map ~f:(fun x -> Var x))
            , expr )
          ]
        , None )
    in
    new_expr
  | _ -> failwith "not yet implemented"
;;
