(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module VarMap = Stdlib.Map.Make (String)
module VarSet = Stdlib.Set.Make (String)

let std_var_set = 
  let init_var_set = VarSet.empty in 
  let init_var_set = VarSet.add "( * )" init_var_set in 
  let init_var_set = VarSet.add "( / )" init_var_set in 
  let init_var_set = VarSet.add "( + )" init_var_set in 
  let init_var_set = VarSet.add "( - )" init_var_set in 
  let init_var_set = VarSet.add "( = )" init_var_set in 
  let init_var_set = VarSet.add "( == )" init_var_set in 
  let init_var_set = VarSet.add "( <> )" init_var_set in 
  let init_var_set = VarSet.add "( != )" init_var_set in 
  let init_var_set = VarSet.add "( < )" init_var_set in 
  let init_var_set = VarSet.add "( <= )" init_var_set in 
  let init_var_set = VarSet.add "( > )" init_var_set in 
  let init_var_set = VarSet.add "( >= )" init_var_set in 
  let init_var_set = VarSet.add "( && )" init_var_set in 
  let init_var_set = VarSet.add "( || )" init_var_set in 
  let init_var_set = VarSet.add "print_int" init_var_set in 
  let init_var_set = VarSet.add "print_string" init_var_set in 
  let init_var_set = VarSet.add "( ~+ )" init_var_set in 
  let init_var_set = VarSet.add "( ~- )" init_var_set in
init_var_set

let get_vars_from_pattern pattern =
  let rec helper acc = function
    | Ast.PAny -> acc
    | Ast.PConst _ -> acc
    | Ast.PVar (Ast.Id var_name) -> VarSet.add var_name acc
    | Ast.PTuple pat_lst -> List.fold_left (fun acc pat -> helper acc pat) acc pat_lst
    | Ast.PCons (pat1, pat2) -> helper (helper acc pat1) pat2
    | Ast.PType (pat, _) -> helper acc pat
  in
  helper VarSet.empty pattern
;;

let get_vars_from_pattern_lst pattern_lst = 
  let rec helper acc = function
  | [] -> acc 
  | [ h ] -> VarSet.fold (fun new_var acc -> VarSet.add new_var acc) (get_vars_from_pattern h) acc
  | h :: tl -> helper (VarSet.fold (fun new_var acc -> VarSet.add new_var acc) (get_vars_from_pattern h) acc) tl
  in helper VarSet.empty pattern_lst

let get_vars_from_expr expr env =
  let rec helper acc env = function
    | Ast.EConst _ -> acc
    | Ast.EVar (Id var_name) ->
      (match VarSet.find_opt var_name env with
       | Some _ -> acc
       | None -> VarSet.add var_name acc)
    | Ast.ETuple exp_lst -> List.fold_left (fun acc exp -> helper acc env exp) acc exp_lst
    | Ast.EFun (pat_lst, expr) ->
      let vars_from_pattern = get_vars_from_pattern_lst pat_lst
      in
      let new_env =
        VarSet.fold (fun var_name acc -> VarSet.add var_name acc) vars_from_pattern env
      in
      helper acc new_env expr
    | Ast.ELet (Ast.Nonrecursive, (pat, expr1), expr2) ->
      let vars_from_pattern = get_vars_from_pattern pat in
      let new_acc = helper acc env expr1 in
      let new_env =
        VarSet.fold (fun var_name acc -> VarSet.add var_name acc) vars_from_pattern env 
      in
      helper new_acc new_env expr2
    | Ast.ELet (Ast.Recursive, (pat, expr1), expr2) ->
      let vars_from_pattern = get_vars_from_pattern pat in
      let new_env =
        VarSet.fold (fun var_name acc -> VarSet.add var_name acc) vars_from_pattern env
      in
      let new_acc = helper acc new_env expr1 in
      helper new_acc new_env expr2
    | Ast.EApp (expr1, expr2) -> helper (helper acc env expr1) env expr2
    | Ast.EMatch (expr, case_lst) ->
      let new_acc = helper acc env expr in
      List.fold_left
        (fun acc (pat, expr) ->
          let vars_from_pattern = get_vars_from_pattern pat in
          let new_env =
            VarSet.fold
              (fun var_name acc -> VarSet.add var_name acc)
              vars_from_pattern
              env
          in
          helper acc new_env expr)
        new_acc
        case_lst
    | Ast.EIf (expr1, expr2, expr3) ->
      helper (helper (helper acc env expr1) env expr2) env expr3
    | Ast.ECons (expr1, expr2) -> helper (helper acc env expr1) env expr2
    | Ast.EType (expr, _) -> helper acc env expr
  in
  helper VarSet.empty env expr
;;

let rec convert_expr env global_env = function
  | Ast.EConst _ as const -> const
  | Ast.EVar (Ast.Id var_name) as s ->
    (match VarMap.find_opt var_name env with
     | Some param_lst ->
       List.fold_left
         (fun exp param -> Ast.EApp (exp, Ast.EVar (Ast.Id param)))
         s
         param_lst
     | None -> s)
  | Ast.ETuple exp_lst ->
    let new_exp_lst =
      List.fold_left
        (fun lst exp ->
          let new_exp = convert_expr env global_env exp in
          new_exp :: lst)
        []
        exp_lst
    in
    Ast.ETuple (List.rev new_exp_lst)
  | Ast.EFun (pat_lst, expr) as f ->
    let vars_from_pats = VarSet.elements (get_vars_from_pattern_lst pat_lst) in
    let updated_env =
      List.fold_left (fun acc new_var -> VarMap.remove new_var acc) env vars_from_pats
    in
    let updated_expr = convert_expr updated_env global_env expr in
    let vars_to_add = VarSet.elements (get_vars_from_expr f global_env) in
    let pat_vars_to_add =
      List.fold_left
        (fun acc var_to_add -> Ast.PVar (Ast.Id var_to_add) :: acc)
        []
        vars_to_add
    in
    let all_vars = List.append pat_vars_to_add pat_lst in
    let new_efun = Ast.EFun (all_vars, updated_expr) in
    List.fold_left
      (fun acc var_to_add -> Ast.EApp (acc, Ast.EVar (Ast.Id var_to_add)))
      new_efun
      (List.rev vars_to_add)
  | Ast.ELet (Ast.Nonrecursive, (pat, expr1), expr2) ->
    let vars_to_add = VarSet.elements (get_vars_from_expr expr1 global_env) in
    let new_value_binding =
      match vars_to_add with
      | [] -> pat, convert_expr env global_env expr1
      | _ ->
        let rev_pat_to_add =
          List.fold_left
            (fun acc new_var ->
              let new_pat = Ast.PVar (Ast.Id new_var) in
              new_pat :: acc)
            []
            vars_to_add
        in
        let pat_to_add = List.rev rev_pat_to_add in
        (match expr1 with 
        | Ast.EFun (pat_lst, fexpr) ->
          let all_pats = List.append pat_to_add pat_lst in 
          pat, Ast.EFun (all_pats, convert_expr env global_env fexpr)
        | _ -> 
          let converted_expr1 = convert_expr env global_env expr1 in 
         pat, Ast.EFun (pat_to_add, converted_expr1))
    in
    let vars_from_pattern = VarSet.elements (get_vars_from_pattern pat) in
    let updated_env =
      List.fold_left
        (fun acc new_var -> VarMap.add new_var vars_to_add acc)
        env
        vars_from_pattern
    in
    let updated_global_env =
      List.fold_left
        (fun acc new_var -> VarSet.add new_var acc)
        global_env
        vars_from_pattern
    in
    let converted_expr2 = convert_expr updated_env updated_global_env expr2 in
    Ast.ELet (Ast.Nonrecursive, new_value_binding, converted_expr2)
  | Ast.ELet (Ast.Recursive, (pat, expr1), expr2) ->
    let vars_from_pattern = VarSet.elements (get_vars_from_pattern pat) in
    let updated_global_env =
      List.fold_left
        (fun acc new_var -> VarSet.add new_var acc)
        global_env
        vars_from_pattern
    in
    let vars_to_add = VarSet.elements (get_vars_from_expr expr1 updated_global_env) in
    let new_value_binding =
      match vars_to_add with
      | [] ->
        (match expr1 with 
        | Ast.EFun (pat_lst, fexpr) -> 
          pat, Ast.EFun (pat_lst, convert_expr env global_env fexpr)
        | _ -> pat, convert_expr env global_env expr1)
      | _ ->
        let rev_pat_to_add =
          List.fold_left
            (fun acc new_var ->
              let new_pat = Ast.PVar (Ast.Id new_var) in
              new_pat :: acc)
            []
            vars_to_add
        in
        let pat_to_add = List.rev rev_pat_to_add in
        (match expr1 with 
        | Ast.EFun (pat_lst, fexpr) ->
          let all_pats = List.append pat_to_add pat_lst in 
          pat, Ast.EFun (all_pats, convert_expr env global_env fexpr)
        | _ -> 
          let converted_expr1 = convert_expr env global_env expr1 in 
         pat, Ast.EFun (pat_to_add, converted_expr1))
    in
    let updated_env =
      List.fold_left
        (fun acc new_var -> VarMap.add new_var vars_to_add acc)
        env
        vars_from_pattern
    in
    let converted_expr2 = convert_expr updated_env updated_global_env expr2 in
    Ast.ELet (Ast.Recursive, new_value_binding, converted_expr2)
  | Ast.EApp (expr1, expr2) ->
    let new_expr1 = convert_expr env global_env expr1 in
    let new_expr2 = convert_expr env global_env expr2 in
    EApp (new_expr1, new_expr2)
  | Ast.EMatch (expr1, case_lst) ->
    let new_expr1 = convert_expr env global_env expr1 in
    let new_cases =
      List.fold_left
        (fun acc (pat, expr) ->
          let vars_from_pat = VarSet.elements (get_vars_from_pattern pat) in
          let updated_env =
            List.fold_left
              (fun acc new_var -> VarMap.remove new_var acc)
              env
              vars_from_pat
          in
          let new_case_expr = convert_expr updated_env global_env expr in
          (pat, new_case_expr) :: acc)
        []
        case_lst
    in
    Ast.EMatch (new_expr1, List.rev new_cases)
  | Ast.EIf (expr1, expr2, expr3) ->
    let new_expr1 = convert_expr env global_env expr1 in
    let new_expr2 = convert_expr env global_env expr2 in
    let new_expr3 = convert_expr env global_env expr3 in
    Ast.EIf (new_expr1, new_expr2, new_expr3)
  | Ast.ECons (expr1, expr2) ->
    let new_expr1 = convert_expr env global_env expr1 in
    let new_expr2 = convert_expr env global_env expr2 in
    Ast.ECons (new_expr1, new_expr2)
  | Ast.EType (expr, typ) ->
    let new_expr = convert_expr env global_env expr in
    Ast.EType (new_expr, typ)
;;

let get_pats_lst_from_value_binding_lst value_binding_lst = 
  List.fold_left
  (fun acc value_binding ->
    let pat, _ = value_binding in 
    pat :: acc)
  []
  value_binding_lst

let convert_structure_item global_env = function
  | Ast.SILet (Ast.Nonrecursive, value_binding_lst) ->
    let new_value_bindings =
      List.fold_left
        (fun acc (pat, expr) ->
          let new_expr = convert_expr VarMap.empty global_env expr in
          (pat, new_expr) :: acc)
        []
        value_binding_lst
    in
    Ast.SILet (Ast.Nonrecursive, List.rev new_value_bindings)
  | Ast.SILet (Ast.Recursive, value_binding_lst) ->
    let pat_lst = get_pats_lst_from_value_binding_lst value_binding_lst in
    let vars_from_pats = VarSet.elements (get_vars_from_pattern_lst pat_lst) in
    let updated_global_env =
      List.fold_left (fun acc new_var -> VarSet.add new_var acc) global_env vars_from_pats
    in
    let new_value_bindings =
      List.fold_left
        (fun acc (pat, expr) ->
          match expr with
          | Ast.EFun (fpat, expr) ->
            let new_expr = convert_expr VarMap.empty updated_global_env expr in
            (pat, Ast.EFun (fpat, new_expr)) :: acc
          | _ ->
            let new_expr = convert_expr VarMap.empty updated_global_env expr in
            (pat, new_expr) :: acc)
        []
        value_binding_lst
    in
    Ast.SILet (Ast.Recursive, List.rev new_value_bindings)
  | Ast.SIExpr expr ->
    let new_expr = convert_expr VarMap.empty global_env expr in
    Ast.SIExpr new_expr
;;

let convert_structure global_env structure =
  let new_structure, _ =
    List.fold_left
      (fun (new_structure, global_env) structure_item ->
        let new_structure_item = convert_structure_item global_env structure_item in
        let vars_from_pattern =
          match structure_item with
          | Ast.SILet (_, value_binding_lst) ->
            let pat_lst = get_pats_lst_from_value_binding_lst value_binding_lst in 
            VarSet.elements (get_vars_from_pattern_lst pat_lst)
          | Ast.SIExpr _ -> []
        in
        let new_global_env =
          List.fold_left
            (fun acc new_var -> VarSet.add new_var acc)
            global_env
            vars_from_pattern
        in
        new_structure_item :: new_structure, new_global_env)
      ([], global_env)
      structure
  in
  List.rev new_structure
;;

let run_expr_closure_conversion expr = convert_expr VarMap.empty std_var_set expr

let run_closure_conversion (structure : Ast.structure) =
  convert_structure std_var_set structure
;;
