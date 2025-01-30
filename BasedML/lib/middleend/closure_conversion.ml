(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Base

(* Find unbound variables within an expression *)
let unbound_identifiers exp =
  let rec helper =
    (* Takes set of unbound identifiers and binds*)
    let rec bind_pattern pat base_set =
      let rec bind_pattern_list acc = function
        | h :: tl ->
          (match h with
           | PWildCard -> bind_pattern_list acc tl
           | PCons (pat1, pat2) ->
             let new_acc = bind_pattern_list acc (pat1 :: [ pat2 ]) in
             bind_pattern_list new_acc tl
           | PIdentifier id -> bind_pattern_list (Set.remove acc id) tl
           | PTuple patterns ->
             let new_acc = bind_pattern_list acc patterns in
             bind_pattern_list new_acc tl
           | PConstant _ -> bind_pattern_list acc tl
           | PConstraint (pat, _) ->
             let new_acc = bind_pattern pat base_set in
             bind_pattern_list new_acc tl)
        | [] -> acc
      in
      match pat with
      | PWildCard | PConstant _ -> base_set
      | PCons (pat1, pat2) -> bind_pattern_list base_set (pat1 :: [ pat2 ])
      | PIdentifier x -> Set.remove base_set x
      | PTuple pats -> bind_pattern_list base_set pats
      | PConstraint (pat, _) -> bind_pattern_list base_set [ pat ]
    in
    function
    | EConstant _ -> (module String) |> Set.empty
    | EIdentifier id -> Set.add ((module String) |> Set.empty) id
    | EFunction (pat, exp) ->
      let unbound_in_fun = helper exp in
      bind_pattern pat unbound_in_fun
    | EApplication (left_exp, right_exp) ->
      let unbound_in_left = helper left_exp in
      let unbound_in_right = helper right_exp in
      Set.union unbound_in_left unbound_in_right
    | EIfThenElse (guard_expr, if_expr, else_expr) ->
      let unbound_in_guard = helper guard_expr in
      let unbound_in_if = helper if_expr in
      let unbound_in_else = helper else_expr in
      Set.union (Set.union unbound_in_guard unbound_in_if) unbound_in_else
    | ELetIn (_, pat, outer_exp, inner_exp) ->
      let rec set_of_pat = function
        | PWildCard | PConstant _ -> (module String) |> Set.empty
        | PIdentifier id -> Set.add ((module String) |> Set.empty) id
        | PCons (left_pat, right_pat) ->
          Set.union (set_of_pat left_pat) (set_of_pat right_pat)
        | PTuple pats ->
          let initial_set = Set.empty (module String) in
          List.fold_left
            ~init:initial_set
            ~f:(fun acc h -> Set.union acc (set_of_pat h))
            pats
        | PConstraint (pat, _) -> set_of_pat pat
      in
      let rec collect_binds acc = function
        | EFunction (pat, next) -> collect_binds (Set.union acc (set_of_pat pat)) next
        | _ -> acc
      in
      let unbound_in_outer = helper outer_exp in
      let unbound_in_inner = helper inner_exp in
      let binds = collect_binds ((module String) |> Set.empty) outer_exp in
      let unbound_in_outer_without_pat = bind_pattern pat unbound_in_outer in
      let unbound_in_inner_without_pat = bind_pattern pat unbound_in_inner in
      let unbound_in_inner_final = Set.diff unbound_in_inner_without_pat binds in
      let unbound_in_outer_final = Set.diff unbound_in_outer_without_pat binds in
      Set.union unbound_in_outer_final unbound_in_inner_final
    | ETuple exps ->
      List.fold
        exps
        ~init:((module String) |> Set.empty)
        ~f:(fun acc h -> Set.union acc (helper h))
    | EMatch (pat, branches) ->
      let unbound_in_braches =
        List.fold
          branches
          ~init:((module String) |> Set.empty)
          ~f:(fun acc (pat, exp) -> Set.union acc (bind_pattern pat (helper exp)))
      in
      bind_pattern pat unbound_in_braches
    | EConstraint (expr, _) -> helper expr
  in
  helper exp
;;

let rec close_function lts local_ctx global_ctx convert = function
  | EFunction (pat, body) ->
    EFunction (pat, close_function lts local_ctx global_ctx convert body)
  | expr -> convert lts local_ctx global_ctx expr
;;

let rec get_global_names = function
  | PIdentifier id -> Set.add ((module String) |> Set.empty) id
  | PWildCard | PConstant _ -> (module String) |> Set.empty
  | PConstraint (pat, _) -> get_global_names pat
  | PCons (pat1, pat2) ->
    List.fold_left
      [ pat1; pat2 ]
      ~f:(fun acc h -> Set.union acc (get_global_names h))
      ~init:((module String) |> Set.empty)
  | PTuple pats ->
    List.fold_left
      pats
      ~f:(fun acc h -> Set.union acc (get_global_names h))
      ~init:((module String) |> Set.empty)
;;

let infix_ops_set =
  Set.of_list
    (module String)
    [ "( + )"; "( :: )"; "( * )"; "( - )"; "( == )"; "( = )"; "( / )" ]
;;

let convert global_ctx declaration =
  let rec helper lts local_ctx global_ctx = function
    | EConstant const -> EConstant const
    | EIdentifier id ->
      (match Map.find local_ctx id with
       | Some free ->
         let ids = List.map (Set.to_list free) ~f:(fun x -> EIdentifier x) in
         List.fold_left ids ~f:(fun f arg -> EApplication (f, arg)) ~init:(EIdentifier id)
       | None -> EIdentifier id)
    | EFunction (pat, body) ->
      let unbound_names = unbound_identifiers (EFunction (pat, body)) in
      let unbound_names_without_global =
        Set.diff (Set.diff unbound_names global_ctx) infix_ops_set
      in
      let unbound_ids_patterns =
        List.map (Set.to_list unbound_names_without_global) ~f:(fun x -> PIdentifier x)
      in
      let unbound_ids_exps =
        List.map (Set.to_list unbound_names_without_global) ~f:(fun x -> EIdentifier x)
      in
      let closed_fun =
        close_function
          lts
          local_ctx
          (Set.diff global_ctx infix_ops_set)
          helper
          (EFunction (pat, body))
      in
      let new_fun =
        List.fold_right
          ~f:(fun pat exp -> EFunction (pat, exp))
          unbound_ids_patterns
          ~init:closed_fun
      in
      List.fold_left
        unbound_ids_exps
        ~f:(fun f arg -> EApplication (f, arg))
        ~init:new_fun
    | EApplication (left, right) ->
      EApplication
        ( helper lts local_ctx (Set.diff (Set.diff global_ctx lts) infix_ops_set) left
        , helper lts local_ctx (Set.diff (Set.diff global_ctx lts) infix_ops_set) right )
    | EIfThenElse (guard, then_branch, else_branch) ->
      let global_ctx = Set.diff global_ctx infix_ops_set in
      EIfThenElse
        ( helper lts local_ctx (Set.diff global_ctx infix_ops_set) guard
        , helper lts local_ctx (Set.diff global_ctx infix_ops_set) then_branch
        , helper lts local_ctx (Set.diff global_ctx infix_ops_set) else_branch )
    | ELetIn (rec_flag, pat, outer, inner) ->
      (match pat, outer with
       (* Inner fun *)
       | PIdentifier id, EFunction (_, _) ->
         let updated_lts = Set.add lts id in
         let updated_global_env =
           match rec_flag with
           | Rec -> Set.add (Set.diff global_ctx infix_ops_set) id
           | NotRec -> Set.add (Set.diff global_ctx infix_ops_set) id
         in
         let unbound_names = unbound_identifiers (ELetIn (rec_flag, pat, outer, inner)) in
         let unbound_names_without_global =
           Set.diff (Set.diff unbound_names updated_global_env) infix_ops_set
         in
         let closed_fun = close_function lts local_ctx updated_global_env helper outer in
         let unbound_ids_without_global =
           List.map (Set.to_list unbound_names_without_global) ~f:(fun x -> PIdentifier x)
         in
         let closed_outer =
           List.fold_right
             unbound_ids_without_global
             ~f:(fun pat exp -> EFunction (pat, exp))
             ~init:closed_fun
         in
         let updated_local_env =
           Map.set local_ctx ~key:id ~data:unbound_names_without_global
         in
         let closed_inner =
           helper
             updated_lts
             updated_local_env
             (Set.diff (Set.add global_ctx id) infix_ops_set)
             inner
         in
         let updated_outer =
           helper
             updated_lts
             updated_local_env
             (Set.diff (Set.add global_ctx id) infix_ops_set)
             closed_outer
         in
         ELetIn (rec_flag, PIdentifier id, updated_outer, closed_inner)
       | _ ->
         ELetIn
           ( rec_flag
           , pat
           , helper lts local_ctx (Set.diff global_ctx infix_ops_set) outer
           , helper lts local_ctx (Set.diff global_ctx infix_ops_set) inner ))
    | ETuple exps ->
      let new_exps =
        List.map exps ~f:(helper lts local_ctx (Set.diff global_ctx infix_ops_set))
      in
      ETuple new_exps
    | EMatch (pat, branches) ->
      let new_branches =
        List.map branches ~f:(fun (pat, exp) ->
          pat, helper lts local_ctx (Set.diff global_ctx infix_ops_set) exp)
      in
      EMatch (pat, new_branches)
    | EConstraint (exp, type_name) ->
      EConstraint (helper lts local_ctx (Set.diff global_ctx infix_ops_set) exp, type_name)
  in
  let close_declaration global_ctx = function
    | DSingleLet (flag, DLet (pat, exp)) ->
      DSingleLet
        ( flag
        , DLet
            ( pat
            , close_function
                ((module String) |> Set.empty)
                ((module String) |> Map.empty)
                (Set.diff global_ctx infix_ops_set)
                helper
                exp ) )
    | DMutualRecDecl (flag, decls) ->
      let rec handle_mutual_rec global_ctx = function
        | [] -> [], Set.diff global_ctx infix_ops_set
        | DLet (pat, exp) :: tl ->
          let closed_exp =
            close_function
              ((module String) |> Set.empty)
              (Map.empty (module String))
              (Set.diff global_ctx infix_ops_set)
              helper
              exp
          in
          let new_decl, new_env = handle_mutual_rec global_ctx tl in
          DLet (pat, closed_exp) :: new_decl, new_env
      in
      let new_decls, _ = handle_mutual_rec global_ctx decls in
      DMutualRecDecl (flag, new_decls)
  in
  close_declaration global_ctx declaration
;;

let convert_ast ast =
  let close ast =
    List.fold
      ast
      ~f:(fun (acc, ctx) -> 
        function
        | DSingleLet (flag, DLet (pat, body)) ->
          ( convert ctx (DSingleLet (flag, DLet (pat, body))) :: acc
          , Set.union ctx (get_global_names pat) )
        | DMutualRecDecl (flag, decls) ->
          convert ctx (DMutualRecDecl (flag, decls)) :: acc, ctx)
      ~init:([], (module String) |> Set.empty)
  in
  let converted_ast, _ = ast |> close in
  converted_ast |> List.rev
;;

let test_closure_convert ast =
  let converted = convert_ast ast in
  Stdlib.Format.printf "%s" (Ast.show_declarations converted)
;;
