(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(* | Simple_ast.SELet (Ast.Nonrecursive, (ident, expr1), expr2) ->
   let vars_to_add = VarSet.elements (get_vars_from_expr expr1 global_env) in
   let new_value_binding =
   match vars_to_add with
   | [] -> ident, convert_expr env global_env expr1
   | _ ->
   let rev_id_lst_to_add =
   List.fold_left
   (fun acc new_var ->
   let new_pat = Ast.Id new_var in
   new_pat :: acc)
   []
   vars_to_add
   in
   let id_lst_to_add = List.rev rev_id_lst_to_add in
   (match expr1 with
   | Simple_ast.SEFun (fident_lst, fexpr) ->
   let all_idents = List.append id_lst_to_add fident_lst in
   ident, Simple_ast.SEFun (all_idents, convert_expr env global_env fexpr)
   | _ ->
   let converted_expr1 = convert_expr env global_env expr1 in
   ident, Simple_ast.SEFun (id_lst_to_add, converted_expr1))
   in
   let var_name = get_var_name_from_id ident in
   let updated_env = VarMap.add var_name vars_to_add env in
   let updated_global_env = VarSet.add var_name global_env in
   let converted_expr2 = convert_expr updated_env updated_global_env expr2 in
   Simple_ast.SELet (Ast.Nonrecursive, new_value_binding, converted_expr2) *)

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
  let init_var_set = VarSet.add "#gen_matching_failed#" init_var_set in
  let init_var_set = VarSet.add "#gen_tuple_getter#" init_var_set in
  let init_var_set = VarSet.add "#gen_list_getter_head#" init_var_set in
  let init_var_set = VarSet.add "#gen_list_getter_tail#" init_var_set in
  let init_var_set = VarSet.add "#gen_list_getter_length#" init_var_set in
  init_var_set
;;

let get_var_name_from_id = function
  | Ast.Id var_name -> var_name
;;

let get_var_names_from_id_lst id_lst =
  let rec helper acc = function
    | [] -> acc
    (* | [ h ] -> h :: acc *)
    | h :: tl -> helper (VarSet.add (get_var_name_from_id h) acc) tl
  in
  helper VarSet.empty id_lst
;;

let get_id_lst_from_var_names_lst var_names_lst =
  let rev_id_lst_to_add =
    List.fold_left
      (fun acc new_var ->
        let new_id = Ast.Id new_var in
        new_id :: acc)
      []
      var_names_lst
  in
  List.rev rev_id_lst_to_add
;;

let get_vars_from_expr expr env =
  let rec helper acc env = function
    | Simple_ast.SEConst _ -> acc
    | Simple_ast.SEVar (Id var_name) ->
      (match VarSet.find_opt var_name env with
       | Some _ -> acc
       | None -> VarSet.add var_name acc)
    | Simple_ast.SETuple exp_lst ->
      List.fold_left (fun acc exp -> helper acc env exp) acc exp_lst
    | Simple_ast.SEFun (id_lst, expr) ->
      let vars_from_id_lst = get_var_names_from_id_lst id_lst in
      let new_env =
        VarSet.fold (fun var_name acc -> VarSet.add var_name acc) vars_from_id_lst env
      in
      helper acc new_env expr
    | Simple_ast.SELet (Ast.Nonrecursive, (ident, expr1), expr2) ->
      let var_name = get_var_name_from_id ident in
      let new_acc = helper acc env expr1 in
      let new_env = VarSet.add var_name env in
      helper new_acc new_env expr2
    | Simple_ast.SELet (Ast.Recursive, (ident, expr1), expr2) ->
      let var_name = get_var_name_from_id ident in
      let new_env = VarSet.add var_name env in
      let new_acc = helper acc new_env expr1 in
      helper new_acc new_env expr2
    | Simple_ast.SEApp (expr1, expr2) -> helper (helper acc env expr1) env expr2
    | Simple_ast.SEIf (expr1, expr2, expr3) ->
      helper (helper (helper acc env expr1) env expr2) env expr3
    | Simple_ast.SECons (expr1, expr2) -> helper (helper acc env expr1) env expr2
  in
  helper VarSet.empty env expr
;;

(*     (match VarMap.find_opt var_name env with
       | Some param_lst ->
       List.fold_left
       (fun exp param -> Simple_ast.SEApp (exp, Simple_ast.SEVar (Ast.Id param)))
       s
       param_lst
       | None -> s) *)

let recursively_add_params start_expr var_name env =
  let rec helper start_expr var_name =
    match VarMap.find_opt var_name env with
    | Some param_lst ->
      List.fold_left
        (fun exp param ->
          Simple_ast.SEApp (exp, helper (Simple_ast.SEVar (Ast.Id param)) param))
        start_expr
        param_lst
    | None -> start_expr
  in
  helper start_expr var_name
;;

let rec convert_fun_expr env global_env = function
  | Simple_ast.SEFun (id_lst, expr) as f ->
    let vars_from_id_lst = get_var_names_from_id_lst id_lst in
    let updated_env =
      VarSet.fold (fun new_var acc -> VarMap.remove new_var acc) vars_from_id_lst env
    in
    let vars_to_add = VarSet.elements (get_vars_from_expr f global_env) in
    let updated_env =
      List.fold_left
        (fun acc new_var -> VarMap.remove new_var acc)
        updated_env
        vars_to_add
    in
    let updated_expr = convert_expr updated_env global_env expr in
    let id_lst_to_add = get_id_lst_from_var_names_lst vars_to_add in
    let all_vars = List.append id_lst_to_add id_lst in
    vars_to_add, Simple_ast.SEFun (all_vars, updated_expr)
  | _ as expr -> [], expr

and convert_expr env global_env = function
  | Simple_ast.SEConst _ as const -> const
  | Simple_ast.SEVar (Ast.Id var_name) as s -> recursively_add_params s var_name env
  | Simple_ast.SETuple exp_lst ->
    let rev_new_exp_lst =
      List.fold_left
        (fun lst exp ->
          let new_exp = convert_expr env global_env exp in
          new_exp :: lst)
        []
        exp_lst
    in
    Simple_ast.SETuple (List.rev rev_new_exp_lst)
  | Simple_ast.SEFun _ as f ->
    let vars_to_add, new_efun = convert_fun_expr env global_env f in
    List.fold_left
      (fun acc var_to_add ->
        Simple_ast.SEApp
          ( acc
          , recursively_add_params (Simple_ast.SEVar (Ast.Id var_to_add)) var_to_add env
          ))
      new_efun
      vars_to_add
  | Simple_ast.SELet (Ast.Nonrecursive, (ident, expr1), expr2) ->
    (match expr1 with
     | Simple_ast.SEFun _ as f ->
       let vars_to_add, new_efun = convert_fun_expr env global_env f in
       let new_value_binding = ident, new_efun in
       let var_name = get_var_name_from_id ident in
       let updated_env = VarMap.add var_name vars_to_add env in
       let converted_expr2 = convert_expr updated_env global_env expr2 in
       Simple_ast.SELet (Ast.Nonrecursive, new_value_binding, converted_expr2)
     | _ ->
       Simple_ast.SELet
         ( Ast.Nonrecursive
         , (ident, convert_expr env global_env expr1)
         , convert_expr env global_env expr2 ))
  | Simple_ast.SELet (Ast.Recursive, (ident, expr1), expr2) ->
      let ident_var_name = get_var_name_from_id ident in
      (match expr1 with 
      | Simple_ast.SEFun (id_lst, fexpr) as f -> 
        let vars_from_id_lst = get_var_names_from_id_lst id_lst in
        let updated_env =
          VarSet.fold (fun new_var acc -> VarMap.remove new_var acc) vars_from_id_lst env
        in
        let vars_to_add = VarSet.elements (get_vars_from_expr f global_env) in
        let vars_to_add = List.filter (fun var_name -> var_name <> ident_var_name) vars_to_add in
        let updated_env =
          List.fold_left
          (fun acc new_var -> VarMap.remove new_var acc)
          updated_env
          vars_to_add
        in
        let updated_env = VarMap.add ident_var_name vars_to_add updated_env in
        let id_lst_to_add = get_id_lst_from_var_names_lst vars_to_add in
        let all_vars = List.append id_lst_to_add id_lst in 
        let converted_expr1 = Simple_ast.SEFun (all_vars, convert_expr updated_env global_env fexpr) in 
        let converted_value_binding = ident, converted_expr1 in
        let updated_env = VarMap.add ident_var_name vars_to_add env in
        let converted_expr2 = convert_expr updated_env global_env expr2 in
        Simple_ast.SELet (Ast.Recursive, converted_value_binding, converted_expr2)
      | _ -> 
        let updated_env = VarMap.remove ident_var_name env in 
        let converted_expr1 = convert_expr updated_env global_env expr1 in 
        let converted_expr2 = convert_expr updated_env global_env expr2 in
        Simple_ast.SELet (Ast.Recursive, (ident, converted_expr1), converted_expr2))
  | Simple_ast.SEApp (expr1, expr2) ->
    let new_expr1 = convert_expr env global_env expr1 in
    let new_expr2 = convert_expr env global_env expr2 in
    Simple_ast.SEApp (new_expr1, new_expr2)
  | Simple_ast.SEIf (expr1, expr2, expr3) ->
    let new_expr1 = convert_expr env global_env expr1 in
    let new_expr2 = convert_expr env global_env expr2 in
    let new_expr3 = convert_expr env global_env expr3 in
    Simple_ast.SEIf (new_expr1, new_expr2, new_expr3)
  | Simple_ast.SECons (expr1, expr2) ->
    let new_expr1 = convert_expr env global_env expr1 in
    let new_expr2 = convert_expr env global_env expr2 in
    Simple_ast.SECons (new_expr1, new_expr2)
;;

let get_var_names_from_value_binding_lst value_binding_lst =
  List.fold_left
    (fun acc value_binding ->
      let id, _ = value_binding in
      match id with
      | Ast.Id var_name -> var_name :: acc)
    []
    value_binding_lst
;;

let convert_value_binding_lst global_env value_binding_lst =
  let rev_value_binding_lst =
    List.fold_left
      (fun acc (id, expr) ->
        let new_expr = convert_expr VarMap.empty global_env expr in
        (id, new_expr) :: acc)
      []
      value_binding_lst
  in
  List.rev rev_value_binding_lst
;;

let convert_structure_item global_env = function
  | Simple_ast.SSILet (Ast.Nonrecursive, value_binding_lst) ->
    let new_value_bindings = convert_value_binding_lst global_env value_binding_lst in
    Simple_ast.SSILet (Ast.Nonrecursive, new_value_bindings)
  | Simple_ast.SSILet (Ast.Recursive, value_binding_lst) ->
    let var_names = get_var_names_from_value_binding_lst value_binding_lst in
    let updated_global_env =
      List.fold_left (fun acc new_var -> VarSet.add new_var acc) global_env var_names
    in
    let new_value_bindings =
      convert_value_binding_lst updated_global_env value_binding_lst
    in
    Simple_ast.SSILet (Ast.Recursive, new_value_bindings)
  | Simple_ast.SSIExpr expr ->
    let new_expr = convert_expr VarMap.empty global_env expr in
    Simple_ast.SSIExpr new_expr
;;

let convert_structure global_env (structure : Simple_ast.sstructure) =
  let rev_new_structure, _ =
    List.fold_left
      (fun (new_structure, global_env) structure_item ->
        let new_structure_item = convert_structure_item global_env structure_item in
        let vars_from_structure_item =
          match structure_item with
          | Simple_ast.SSILet (_, value_binding_lst) ->
            get_var_names_from_value_binding_lst value_binding_lst
          | Simple_ast.SSIExpr _ -> []
        in
        let new_global_env =
          List.fold_left
            (fun acc new_var -> VarSet.add new_var acc)
            global_env
            vars_from_structure_item
        in
        new_structure_item :: new_structure, new_global_env)
      ([], global_env)
      structure
  in
  List.rev rev_new_structure
;;

let run_expr_closure_conversion expr = convert_expr VarMap.empty std_var_set expr

let run_closure_conversion (structure : Simple_ast.sstructure) =
  convert_structure std_var_set structure
;;

(* it was on recursive let
   let new_value_bindings =
   List.fold_left
   (fun acc (ident, expr) ->
   match expr with
   | Simple_ast.SEFun (fids, expr) ->
   let new_expr = convert_expr VarMap.empty updated_global_env expr in
   (ident, Simple_ast.SEFun (fids, new_expr)) :: acc
   | _ ->
   let new_expr = convert_expr VarMap.empty updated_global_env expr in
   (ident, new_expr) :: acc)
   []
   value_binding_lst
   in
*)

(* | Simple_ast.SELet (Ast.Recursive, (ident, expr1), expr2) ->
    let var_name = get_var_name_from_id ident in
    let updated_global_env = VarSet.add var_name global_env in
    let vars_to_add = VarSet.elements (get_vars_from_expr expr1 updated_global_env) in
    let new_value_binding =
      match vars_to_add with
      | [] ->
        (match expr1 with
         | Simple_ast.SEFun (id_lst, fexpr) ->
           ident, Simple_ast.SEFun (id_lst, convert_expr env global_env fexpr)
         | _ -> ident, convert_expr env global_env expr1)
      | _ ->
        let rev_id_lst_to_add =
          List.fold_left
            (fun acc new_var ->
              let new_pat = Ast.Id new_var in
              new_pat :: acc)
            []
            vars_to_add
        in
        let id_lst_to_add = List.rev rev_id_lst_to_add in
        (match expr1 with
         | Simple_ast.SEFun (pat_lst, fexpr) ->
           let all_pats = List.append id_lst_to_add pat_lst in
           ident, Simple_ast.SEFun (all_pats, convert_expr env global_env fexpr)
         | _ ->
           let converted_expr1 = convert_expr env global_env expr1 in
           ident, Simple_ast.SEFun (id_lst_to_add, converted_expr1))
    in
    let updated_env = VarMap.add var_name vars_to_add env in
    let converted_expr2 = convert_expr updated_env updated_global_env expr2 in
    Simple_ast.SELet (Ast.Recursive, new_value_binding, converted_expr2) *)