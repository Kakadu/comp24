(* closure conversion *)

open Base
open Ast

(*
   let f x y = x - y                        |> free_vars: {}
   fun x -> x                               |> free_vars: {}
   match x with | (a,b) -> 1 | _ -> 2       |> free_vars: {}
   let f x = let (k,j) = x in j in f (1,2)  |> free_vars: {}
*)

(** get free variables from an expression *)
let unbound_variables exp =
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
      let base_set = helper_expr e in
      let cases_set =
        List.fold
          cases
          ~init:(Set.empty (module String))
          ~f:(fun acc (pat, expr) ->
            Set.union acc (Set.union (helper_pattern pat) (helper_expr expr)))
      in
      Set.union base_set cases_set
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
      let args_set =
        List.fold
          args
          ~init:(Set.empty (module String))
          ~f:(fun acc pat -> Set.union acc (helper_pattern pat))
      in
      let expr_set = helper_expr expr in
      Set.union args_set expr_set
  in
  helper_expr exp
;;

(* let close_function = failwith "not yet implemented"
let convert = failwith "not yet implemented"
let convert_ast = failwith "not yet implemented" *)
