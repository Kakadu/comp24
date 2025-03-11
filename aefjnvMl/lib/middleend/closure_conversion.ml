(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Common.Base_lib
open Common.Ast_construct

let std_lib_names =
  let open LibF in
  List.map get_name base_lib_decls
;;

let eliminate_funs =
  let rec helper patterns = function
    | Exp_function (p, e) -> helper (p :: patterns) e
    | expr -> patterns, expr
  in
  helper []
;;

let pattern_vars =
  let rec helper acc = function
    | Pat_var v -> v :: acc
    | Pat_cons (e1, e2) -> helper (helper acc e1) e2
    | Pat_tuple es -> List.fold_left helper acc es
    | _ -> acc
  in
  helper []
;;

let rec construct_from_pats expr = function
  | hd :: tl -> efun hd (construct_from_pats expr tl)
  | [] -> expr
;;

let empty_set = Base.Set.empty (module Base.String)
let set = Base.Set.of_list (module Base.String)

let expr_vars =
  let pat_set p =
    Base.List.fold_left
      (pattern_vars p)
      ~f:(fun acc p -> Base.Set.add acc p)
      ~init:empty_set
  in
  let rec helper acc = function
    | Exp_type (e, _) -> helper acc e
    | Exp_constant _ -> acc
    | Exp_ident id -> Base.Set.add acc id
    | Exp_tuple exps -> List.fold_left helper acc exps
    | Exp_function (p, e) -> Base.Set.diff (helper acc e) (pat_set p)
    | Exp_let (d, e) ->
      let acc = helper acc e in
      helper_decl acc d
    | Exp_match (e, cases) ->
      let acc =
        Base.List.fold_left
          cases
          ~f:(fun acc (p, e) -> Base.Set.diff (helper acc e) (pat_set p))
          ~init:acc
      in
      helper acc e
    | Exp_ifthenelse (e, e', e'') -> helper (helper (helper acc e) e') e''
    | Exp_apply (e, e') | Exp_list (e, e') -> helper (helper acc e) e'
  and helper_decl acc = function
    | Decl (Nonrecursive, [ { vb_pat; vb_expr } ]) ->
      Base.Set.diff (helper acc vb_expr) (pat_set vb_pat)
    | Decl (Recursive, bindings) ->
      let acc =
        Base.List.fold_left
          bindings
          ~f:(fun acc { vb_pat = _; vb_expr } -> helper acc vb_expr)
          ~init:acc
      in
      Base.List.fold_left
        bindings
        ~f:(fun acc { vb_pat; vb_expr = _ } -> Base.Set.diff acc (pat_set vb_pat))
        ~init:acc
    | _ -> acc
  in
  helper (set std_lib_names)
;;

let closure_conversion toplvl =
  let rec closure_expr toplvl env = function
    | Exp_type (e, t) -> etype e t
    | Exp_constant c -> econst c
    | Exp_ident id as node ->
      (match Base.Map.find env id with
       | None -> node
       | Some vars ->
         Base.Set.fold_right (* ?? *) vars ~f:(fun v acc -> eapp acc (eval v)) ~init:node)
    | Exp_tuple exps ->
      let items = Base.List.map exps ~f:(closure_expr toplvl env) in
      etuple items
    | Exp_function (p, e) ->
      let ps, inner = eliminate_funs e in
      let ps = List.rev ps in
      let p_vars = Base.List.concat_map (p :: ps) ~f:pattern_vars in
      let p_vars_set = set p_vars in
      let free_vars = Base.Set.diff (expr_vars inner) p_vars_set in
      let free_vars = Base.Set.diff free_vars (set toplvl) in
      let saturated =
        Base.Set.fold free_vars ~f:(fun acc v -> pvar v :: acc) ~init:(p :: ps)
      in
      let expr = closure_expr toplvl env inner in
      let expr = construct_from_pats expr saturated in
      Base.Set.fold_right free_vars ~f:(fun v acc -> eapp acc (eval v)) ~init:expr
    | Exp_let (decl, e) ->
      let decl, env = closure_decl toplvl env decl in
      elet decl (closure_expr toplvl env e)
    | Exp_match (e, cases) ->
      let e = closure_expr toplvl env e in
      let cases = Base.List.map cases ~f:(fun (p, e) -> p, closure_expr toplvl env e) in
      ematch e cases
    | Exp_ifthenelse (e, e', e'') ->
      let e = closure_expr toplvl env e in
      let e' = closure_expr toplvl env e' in
      let e'' = closure_expr toplvl env e'' in
      eite e e' e''
    | Exp_apply (e, e') ->
      let e = closure_expr toplvl env e in
      let e' = closure_expr toplvl env e' in
      eapp e e'
    | Exp_list (e, e') ->
      let e = closure_expr toplvl env e in
      let e' = closure_expr toplvl env e' in
      econs e e'
  and closure_decl toplvl env =
    let helper rec_flag env = function
      | { vb_pat = Pat_var v as pv; vb_expr } ->
        let ps, inner_expr = eliminate_funs vb_expr in
        let p_vars = Base.List.concat_map ps ~f:pattern_vars in
        let p_vars_set = set p_vars in
        let free_vars = Base.Set.diff (expr_vars inner_expr) p_vars_set in
        let free_vars = Base.Set.diff free_vars (set toplvl) in
        let free_vars =
          match rec_flag with
          | Recursive -> Base.Set.remove free_vars v
          | Nonrecursive -> free_vars
        in
        let free_vars =
          if Base.Set.length p_vars_set == 0 then empty_set else free_vars
        in
        let saturated =
          Base.Set.fold free_vars ~f:(fun acc v -> pvar v :: acc) ~init:(List.rev ps)
        in
        let env = Base.Map.set env ~key:v ~data:free_vars in
        let expr = closure_expr toplvl env inner_expr in
        let binding = { vb_pat = pv; vb_expr = construct_from_pats expr saturated } in
        binding, env
      | binding -> binding, env
    in
    function
    | Decl (Nonrecursive, [ binding ]) ->
      let binding, env = helper Nonrecursive env binding in
      edecl Nonrecursive [ binding ], env
    | Decl (Recursive, bindings) ->
      let ps, _ =
        Base.List.fold_left
          bindings
          ~init:([], [])
          ~f:(fun (ps, es) { vb_pat; vb_expr } -> vb_pat :: ps, vb_expr :: es)
      in
      let p_vars = Base.List.concat_map ps ~f:pattern_vars in
      let env =
        Base.List.fold_left p_vars ~init:env ~f:(fun acc pv ->
          Base.Map.set acc ~key:pv ~data:empty_set)
      in
      let env, bindings =
        Base.List.fold_left bindings ~init:(env, []) ~f:(fun (env, bindings) b ->
          let b, env = helper Recursive env b in
          env, b :: bindings)
      in
      let env', bindings' =
        Base.List.fold_left bindings ~init:(env, []) ~f:(fun (env, bindings) b ->
          let b, env = helper Recursive env b in
          env, b :: bindings)
      in
      edecl Recursive bindings', env'
    | decl -> decl, env
  in
  let empty_map = Base.Map.empty (module Base.String) in
  function
  | Str_eval e ->
    let e = closure_expr toplvl empty_map e in
    Str_eval e, toplvl
  | Str_value (Decl (rec_flag, bindings) as d) ->
    let ps, _ =
      Base.List.fold_left bindings ~init:([], []) ~f:(fun (ps, es) { vb_pat; vb_expr } ->
        vb_pat :: ps, vb_expr :: es)
    in
    let p_vars = Base.List.concat_map ps ~f:pattern_vars in
    let saturated = if rec_flag = Recursive then toplvl @ p_vars else toplvl in
    let decl, _ = closure_decl saturated empty_map d in
    let new_tpls = Base.List.concat_map bindings ~f:(fun b -> pattern_vars b.vb_pat) in
    Str_value decl, Base.List.append new_tpls toplvl
;;

let convert_program ast =
  let open Base.List in
  let decls, _ =
    fold_left ast ~init:([], std_lib_names) ~f:(fun (ss, tlvls) s ->
      let s, tlvls = closure_conversion tlvls s in
      s :: ss, tlvls)
  in
  rev decls
;;
