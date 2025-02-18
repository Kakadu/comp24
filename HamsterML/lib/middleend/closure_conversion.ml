(* closure conversion *)

open Base
open Ast

(* get free variables from an expression *)
let unbound_variables exp =
  let rec exclude_bind_pattern pat base_set =
    let rec exclude_bind_pattern_list acc = function
      | hd :: tl ->
        (match hd with
         | Wildcard | Operation _ -> exclude_bind_pattern_list acc tl
         | ListConcat (pat1, pat2) ->
           let new_acc = exclude_bind_pattern_list acc (pat1 :: [ pat2 ]) in
           exclude_bind_pattern_list new_acc tl
         | Var id -> exclude_bind_pattern_list (Set.remove acc id) tl
         | Tuple pat_list | List pat_list ->
           let new_acc = exclude_bind_pattern_list acc pat_list in
           exclude_bind_pattern_list new_acc tl
         | Const _ -> exclude_bind_pattern_list acc tl
         | Constraint (pat, _) ->
           let new_acc = exclude_bind_pattern pat base_set in
           exclude_bind_pattern_list new_acc tl)
      | [] -> acc
    in
    match pat with
    | Wildcard | Operation _ | Const _ -> base_set
    | ListConcat (pat1, pat2) -> exclude_bind_pattern_list base_set (pat1 :: [ pat2 ])
    | Var id -> Set.remove base_set id
    | Tuple pat_list | List pat_list -> exclude_bind_pattern_list base_set pat_list
    | Constraint (pat, _) -> exclude_bind_pattern_list base_set [ pat ]
  in
  let rec helper_expr exp =
    let rec helper_pattern pat =
      match pat with
      | Const _ | Operation _ | Wildcard -> Set.empty (module String)
      | Var id -> Set.add (Set.empty (module String)) id
      | Tuple pats | List pats ->
        List.fold
          pats
          ~init:(Set.empty (module String))
          ~f:(fun acc h -> Set.union acc (helper_pattern h))
      | ListConcat (pat1, pat2) -> Set.union (helper_pattern pat1) (helper_pattern pat2)
      | Constraint (pat, _) -> helper_pattern pat
    in
    match exp with
    | EConst _ | EOperation _ -> Set.empty (module String)
    | EVar id -> Set.add (Set.empty (module String)) id
    | ETuple exps | EList exps ->
      List.fold
        exps
        ~init:(Set.empty (module String))
        ~f:(fun acc e -> Set.union acc (helper_expr e))
    | EListConcat (expr1, expr2) | Application (expr1, expr2) ->
      Set.union (helper_expr expr1) (helper_expr expr2)
    | If (e1, e2, e3) ->
      let if_then = Set.union (helper_expr e1) (helper_expr e2) in
      (match e3 with
       | Some e -> Set.union if_then (helper_expr e)
       | None -> if_then)
    | Match (e, cases) ->
      let unbound_in_cases =
        List.fold
          cases
          ~init:(Set.empty (module String))
          ~f:(fun acc (pat, expr) ->
            Set.union acc (exclude_bind_pattern pat (helper_expr expr)))
      in
      Set.union unbound_in_cases (helper_expr e)
    | Let (_, binds, in_expr) ->
      let binds_set =
        List.fold
          binds
          ~init:(Set.empty (module String))
          ~f:(fun acc (pat, arg_list, exp) ->
            let pattern_set = helper_pattern pat in
            let arg_list_set =
              List.fold
                arg_list
                ~init:(Set.empty (module String))
                ~f:(fun acc2 h -> Set.union acc2 (helper_pattern h))
            in
            let exp_set = helper_expr exp in
            let bind_item = Set.union pattern_set (Set.union arg_list_set exp_set) in
            Set.union acc bind_item)
      in
      let in_expr_set =
        match in_expr with
        | Some e -> helper_expr e
        | None -> Set.empty (module String)
      in
      Set.union binds_set in_expr_set
    | EConstraint (expr, _) -> helper_expr expr
    | Fun (args, expr) ->
      let free_vars = helper_expr expr in
      List.fold args ~init:free_vars ~f:(fun acc arg -> exclude_bind_pattern arg acc)
  in
  helper_expr exp
;;

(* let close_function = failwith "not yet implemented"
let convert = failwith "not yet implemented"
let convert_ast = failwith "not yet implemented" *)
