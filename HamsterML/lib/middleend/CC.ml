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

let cc_expr =
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
      | Fun (fun_args, fun_body) -> loop fun_body (fun_args @ acc)
      | _ -> acc, e
    in
    loop expr []
  in
  let add_free_vars args expr =
    let unbound = free_vars_expr expr in
    let bound = args_list_to_id_set args in
    let free_vars = Set.diff unbound bound in
    pattern_list_to_vars (Set.to_list free_vars) @ args
  in
  let rec process env = function
    | EVar id ->
      (match Map.find env id with
       | Some captured_vars ->
         (* Apply the function to its captured variables *)
         List.fold captured_vars ~init:(EVar id) ~f:(fun acc var ->
           Application (acc, EVar var))
       | None -> EVar id)
    | EConst _ as e -> e
    | EOperation _ as e -> e
    | ETuple (e1, e2, es) ->
      ETuple (process env e1, process env e2, List.map es ~f:(process env))
    | EList es -> EList (List.map es ~f:(process env))
    | EListConcat (e1, e2) -> EListConcat (process env e1, process env e2)
    | If (cond, then_expr, else_expr) ->
      If (process env cond, process env then_expr, Option.map else_expr ~f:(process env))
    | Match (expr, cases) ->
      Match
        (process env expr, List.map cases ~f:(fun (pat, body) -> pat, process env body))
    | EConstraint (expr, ty) -> EConstraint (process env expr, ty)
    | Application (e1, e2) -> Application (process env e1, process env e2)
    | Fun (args, expr) ->
      (* Process based on whether the expression to the right of the arrow is a `fun` expression *)
      (match expr with
       | Fun _ ->
         let all_args, final_body = extract_fun_args (Fun (args, expr)) in
         let new_args = add_free_vars all_args final_body in
         let bound_vars = args_list_to_id_set new_args in
         (* Create new environment where we remove bound parameters *)
         let new_env = Map.filter_keys env ~f:(fun k -> not (Set.mem bound_vars k)) in
         Fun (new_args, process new_env final_body)
       | _ ->
         let new_args = add_free_vars args expr in
         let bound_vars = args_list_to_id_set new_args in
         let new_env = Map.filter_keys env ~f:(fun k -> not (Set.mem bound_vars k)) in
         let final_fun_expr = Fun (new_args, process new_env expr) in
         let added_args =
           Set.diff (args_list_to_id_set new_args) (args_list_to_id_set args)
         in
         if Set.is_empty added_args
         then final_fun_expr
         else
           List.fold (Set.to_list added_args) ~init:final_fun_expr ~f:(fun acc v ->
             Application (acc, EVar v)))
    | Let (fun_type, binds, in_expr_opt) ->
      let new_env_ref = ref env in
      let cc_binds =
        List.map binds ~f:(fun (pat, args, expr) ->
          let all_args, body = extract_fun_args expr in
          let combined_args = args @ all_args in
          let final_args =
            match pat with
            | Const Unit -> [] (* Don't add free variables to `let ()` *)
            | _ ->
              (match fun_type with
               | Recursive ->
                 let unbound = free_vars_expr body in
                 let bound = args_list_to_id_set combined_args in
                 let fun_names = bound_vars_in_pattern pat in
                 let free_vars = Set.diff (Set.diff unbound bound) fun_names in
                 pattern_list_to_vars (Set.to_list free_vars) @ combined_args
               | Nonrecursive ->
                 (* Don't add free variables from nested `let ()` expressions *)
                 (match expr with
                  | Let (_, inner_binds, _)
                    when List.exists inner_binds ~f:(fun (p, _, _) ->
                           match p with
                           | Const Unit -> true
                           | _ -> false) -> combined_args
                  | _ -> add_free_vars combined_args body))
          in
          (* Update environment with bound functions and their free variables *)
          (match pat with
           | Var id ->
             let captured_vars =
               List.filter_map final_args ~f:(function
                 | Var v
                   when not
                          (List.exists combined_args ~f:(function
                            | Var v' -> String.equal v v'
                            | _ -> false)) -> Some v
                 | _ -> None)
             in
             new_env_ref := Map.set !new_env_ref ~key:id ~data:captured_vars
           | _ -> ());
          let bound_vars = args_list_to_id_set final_args in
          let body_env = Map.filter_keys env ~f:(fun k -> not (Set.mem bound_vars k)) in
          pat, final_args, process body_env body)
      in
      let new_env = !new_env_ref in
      let new_in_expr_opt = Option.map in_expr_opt ~f:(process new_env) in
      Let (fun_type, cc_binds, new_in_expr_opt)
  in
  process (Map.empty (module String))
;;

let cc_prog (prog : prog) =
  let rec helper = function
    | hd :: tl ->
      let new_hd = cc_expr hd in
      new_hd :: helper tl
    | [] -> []
  in
  helper prog
;;
