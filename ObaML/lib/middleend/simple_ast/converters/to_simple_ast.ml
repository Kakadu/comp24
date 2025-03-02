(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

let gen_matching_failed_id = Ast.Id "#gen_matching_failed#"

let gen_matching_failed =
  Simple_ast.SEApp (Simple_ast.SEVar gen_matching_failed_id, Simple_ast.SEConst Ast.CUnit)
;;

let gen_pat_expr_name = "#gen_pat_expr#"
let gen_tuple_getter_id = Ast.Id "#gen_tuple_getter#"

let gen_tuple_getter_fun ind tuple =
  Simple_ast.SEApp
    ( Simple_ast.SEApp
        (Simple_ast.SEVar gen_tuple_getter_id, Simple_ast.SEConst (Ast.CInt ind))
    , tuple )
;;

let gen_list_getter_head_id = Ast.Id "#gen_list_getter_head#"

let gen_list_getter_head lst =
  Simple_ast.SEApp (Simple_ast.SEVar gen_list_getter_head_id, lst)
;;

let gen_list_getter_tail_id = Ast.Id "#gen_list_getter_tail#"

let gen_list_getter_tail lst =
  Simple_ast.SEApp (Simple_ast.SEVar gen_list_getter_tail_id, lst)
;;

let gen_list_getter_length_id = Ast.Id "#gen_list_getter_length#"

let gen_list_getter_length lst =
  Simple_ast.SEApp (Simple_ast.SEVar gen_list_getter_length_id, lst)
;;

let true_condition = (Simple_ast.SEConst (Ast.CBool true))

let construct_full_expr cases_conditions cases_expr =
  let rec helper = function
    | [ cond ], [ expr ] -> Simple_ast.SEIf (cond, expr, gen_matching_failed)
    | cond :: tl_cond, expr :: tl_expr ->
      (match cond with
       | Simple_ast.SEConst (Ast.CBool true) ->
         Simple_ast.SEIf (cond, expr, gen_matching_failed)
       | _ -> Simple_ast.SEIf (cond, expr, helper (tl_cond, tl_expr)))
    | _ -> Simple_ast.SEVar (Ast.Id "Hello proger")
  in
  helper (cases_conditions, cases_expr)
;;

(** @return
      (list_lenght, is_min_length)

      1 :: 2 :: [] -> (2, false)
      1 :: 2 :: a -> (2, true)
      1 :: 2 :: _ -> (2, true) *)
let get_list_length lst =
  let rec helper acc = function
    | Ast.PCons (_, (Ast.PCons _ as tl)) -> helper (acc + 1) tl
    | Ast.PCons (_, Ast.PConst Ast.CEmptyList) -> acc + 1, false
    | _ -> acc + 1, true
  in
  helper 0 lst
;;

let construct_lst_length_condition lst gen_decl_name =
  let lst_length, is_min_length = get_list_length lst in
  let op =
    match is_min_length with
    | true -> Simple_ast.SEVar (Ast.Id "( >= )")
    | false -> Simple_ast.SEVar (Ast.Id "( = )")
  in
  Simple_ast.SEApp
    ( Simple_ast.SEApp (op, gen_list_getter_length (Simple_ast.SEVar gen_decl_name))
    , Simple_ast.SEConst (Ast.CInt lst_length) )
;;

let concat_conditions curr_cond new_cond =
  match curr_cond with
  | Simple_ast.SEConst (Ast.CBool true) -> new_cond
  | _ ->
    Simple_ast.SEApp
      (Simple_ast.SEApp (Simple_ast.SEVar (Ast.Id "( && )"), curr_cond), new_cond)
;;

let construct_equality_condition gen_decl_name pat curr_cond =
  (** @param needs_to_check_cons_length = false only if we call helper for list tail *)
  let rec helper current_condition current_gen_expr needs_to_check_cons_length = function
    | Ast.PAny -> current_condition
    | Ast.PConst const ->
      let new_condition =
        Simple_ast.SEApp
          ( Simple_ast.SEApp (Simple_ast.SEVar (Ast.Id "( = )"), Simple_ast.SEConst const)
          , current_gen_expr )
      in
      concat_conditions current_condition new_condition
    | Ast.PVar _ -> current_condition
    | Ast.PTuple pat_lst ->
      let new_cond, _ =
        List.fold_left
          (fun acc pat ->
            let cond, ind = acc in
            let new_cond =
              helper cond (gen_tuple_getter_fun ind current_gen_expr) true pat
            in
            new_cond, ind + 1)
          (current_condition, 0)
          pat_lst
      in
      new_cond
    | Ast.PCons (pat1, pat2) as cons ->
      let current_condition =
        match needs_to_check_cons_length with
        | true ->
          let list_length_cond = construct_lst_length_condition cons gen_decl_name in
          concat_conditions current_condition list_length_cond
        | false -> current_condition
      in
      let new_cond =
        helper current_condition (gen_list_getter_head current_gen_expr) true pat1
      in
      helper new_cond (gen_list_getter_tail current_gen_expr) false pat2
    | Ast.PType (pat, _) -> helper current_condition current_gen_expr true pat
  in
  helper curr_cond (Simple_ast.SEVar gen_decl_name) true pat
;;

let construct_new_fun_expr pat gen_var_name curr_expr =
  let rec helper curr_gen_expr curr_expr = function
    | Ast.PAny -> curr_expr
    | Ast.PConst _ -> curr_expr
    | Ast.PVar var_name ->
      Simple_ast.SELet (Ast.Nonrecursive, (Simple_ast.SId var_name, curr_gen_expr), curr_expr)
    | Ast.PTuple tup_lst ->
      let new_expr, _ =
        List.fold_left
          (fun acc tup_pat ->
            let curr_expr, ind = acc in
            helper (gen_tuple_getter_fun ind curr_gen_expr) curr_expr tup_pat, ind - 1)
          (curr_expr, List.length tup_lst - 1)
          (List.rev tup_lst)
      in
      new_expr
    | Ast.PCons (pat1, pat2) ->
      let h1 = helper (gen_list_getter_tail curr_expr) curr_expr pat2 in
      helper (gen_list_getter_head curr_gen_expr) h1 pat1
    | Ast.PType (pat, _) -> helper curr_gen_expr curr_expr pat
  in
  helper (Simple_ast.SEVar gen_var_name) curr_expr pat
;;

let rec construct_case_expr gen_decl_name case =
  let pat, expr = case in
  let rec helper current_expr current_gen_expr = function
    | Ast.PAny -> current_expr
    | Ast.PConst _ -> current_expr
    | Ast.PVar var_name ->
      Simple_ast.SELet (Ast.Nonrecursive, (Simple_ast.SId var_name, current_gen_expr), current_expr)
    | Ast.PTuple pat_lst ->
      let new_expr, _ =
        List.fold_left
          (fun acc pat ->
            let current_expr, ind = acc in
            helper current_expr (gen_tuple_getter_fun ind current_gen_expr) pat, ind - 1)
          (current_expr, List.length pat_lst - 1)
          (List.rev pat_lst)
      in
      new_expr
    | Ast.PCons (pat1, pat2) ->
      let new_expr = helper current_expr (gen_list_getter_tail current_gen_expr) pat2 in
      helper new_expr (gen_list_getter_head current_gen_expr) pat1
    | Ast.PType (pat, _) -> helper current_expr current_gen_expr pat
  in
  helper (simplify_expr expr) (Simple_ast.SEVar gen_decl_name) pat

and simplify_expr expr =
  let rec helper = function
    | Ast.EConst const -> Simple_ast.SEConst const
    | Ast.EVar var -> Simple_ast.SEVar var
    | Ast.ETuple tup_lst ->
      let rev_new_tup_lst =
        List.fold_left
          (fun acc tup_expr ->
            let new_tup_expr = helper tup_expr in
            new_tup_expr :: acc)
          []
          tup_lst
      in
      Simple_ast.SETuple (List.rev rev_new_tup_lst)
    | Ast.EFun (pat_lst, expr) ->
      (* заменить все паттерны на переменные *)
      let rev_new_fun_args, _ =
        List.fold_left
          (fun acc pat ->
            let new_fun_args, num = acc in
            match pat with
            | Ast.PVar var_name -> var_name :: new_fun_args, num
            | _ ->
              let var_name = Ast.Id (gen_pat_expr_name ^ string_of_int num) in
              var_name :: new_fun_args, num + 1)
          ([], 0)
          pat_lst
      in
      (* сгенерировать новый expr на основе старого, добавив в него let объявления по паттернам *)
      let simplified_expr = helper expr in
      let new_expr =
        List.fold_left2
          (fun curr_expr pat gen_var_name ->
            match pat with
            | Ast.PVar _ -> curr_expr
            | _ -> construct_new_fun_expr pat gen_var_name curr_expr)
          simplified_expr
          (List.rev pat_lst)
          rev_new_fun_args
      in
      let cond = 
        List.fold_left2
        (fun curr_cond pat gen_var_name -> 
          match pat with 
          | Ast.PVar _ -> curr_cond
          | _ -> construct_equality_condition gen_var_name pat curr_cond)
        true_condition
        (List.rev pat_lst)
        rev_new_fun_args
      in
      let expr_with_cond = 
        (match cond with 
        | Simple_ast.SEConst (Ast.CBool true) -> new_expr 
        | _ -> Simple_ast.SEIf (cond, new_expr, gen_matching_failed))
      in
      let sp_new_fun_args = 
        List.fold_left
        (fun acc fun_arg -> 
          Simple_ast.SId fun_arg :: acc )
        []
        rev_new_fun_args
      in
      Simple_ast.SEFun (sp_new_fun_args, expr_with_cond)
    | Ast.ELet (Ast.Nonrecursive, value_binding, expr2) ->
      let pat, expr1 = value_binding in
      (match pat with
       | Ast.PVar var_name ->
         Simple_ast.SELet
           (Ast.Nonrecursive, (Simple_ast.SId var_name, simplify_expr expr1), simplify_expr expr2)
       | _ ->
         let new_gen_decl_name = gen_pat_expr_name ^ string_of_int 0 in
         let cond = construct_equality_condition (Ast.Id new_gen_decl_name) pat true_condition in 
         let rev_new_value_bindings =
           construct_new_nonrec_value_decl value_binding new_gen_decl_name cond
         in
         let new_expr2 =
           List.fold_left
             (fun exp value_binding ->
               let ident, value_binding_exp = value_binding in
               Simple_ast.SELet (Ast.Nonrecursive, (ident, value_binding_exp), exp))
             (helper expr2)
             rev_new_value_bindings
         in
         let _, expr1 = value_binding in
         Simple_ast.SELet
           (Ast.Nonrecursive, (Simple_ast.SId (Ast.Id new_gen_decl_name), simplify_expr expr1), new_expr2))
    | Ast.ELet (Ast.Recursive, value_binding, expr2) ->
      let ident, new_expr1 = construct_new_rec_value_binding value_binding in
      Simple_ast.SELet (Ast.Recursive, (ident, new_expr1), simplify_expr expr2)
    | Ast.EApp (expr1, expr2) ->
      let new_expr1 = helper expr1 in
      let new_expr2 = helper expr2 in
      Simple_ast.SEApp (new_expr1, new_expr2)
    | Ast.EMatch (expr, case_lst) ->
      (* достать декларацию для expr *)
      let new_gen_decl_name = Ast.Id (gen_pat_expr_name ^ string_of_int 0) in
      (* пройти по каждому кейсу и достать equality condition *)
      let rev_equality_conditions =
        List.fold_left
          (fun acc case -> let pat, _ = case in construct_equality_condition new_gen_decl_name pat true_condition :: acc)
          []
          case_lst
      in
      let equality_conditions = List.rev rev_equality_conditions in
      (* пройтись по каждому кейсу и сгенерировать код для присвоения переменным паттерна *)
      let rev_cases_expr =
        List.fold_left
          (fun acc case -> construct_case_expr new_gen_decl_name case :: acc)
          []
          case_lst
      in
      let cases_expr = List.rev rev_cases_expr in
      (* собрать все через if *)
      let full_expr = construct_full_expr equality_conditions cases_expr in
      Simple_ast.SELet
        (Ast.Nonrecursive, (Simple_ast.SId new_gen_decl_name, simplify_expr expr), full_expr)
    | Ast.EIf (expr1, expr2, expr3) ->
      let new_expr1 = helper expr1 in
      let new_expr2 = helper expr2 in
      let new_expr3 = helper expr3 in
      Simple_ast.SEIf (new_expr1, new_expr2, new_expr3)
    | Ast.ECons (expr1, expr2) ->
      let new_expr1 = helper expr1 in
      let new_expr2 = helper expr2 in
      Simple_ast.SECons (new_expr1, new_expr2)
    | Ast.EType (expr, _) -> helper expr
  in
  helper expr

and construct_new_nonrec_value_decl value_binding gen_decl_name cond =
  let pat, _ = value_binding in
  let rec helper acc curr_expr = function
    | Ast.PAny -> acc
    | Ast.PConst _ -> acc
    | Ast.PVar var_id -> (Simple_ast.SId var_id, curr_expr) :: acc
    | Ast.PTuple tup_lst ->
      let new_acc, _ =
        List.fold_left
          (fun acc elm ->
            let cur_value_bindings, ind = acc in
            helper cur_value_bindings (gen_tuple_getter_fun ind curr_expr) elm, ind + 1)
          (acc, 0)
          tup_lst
      in
      new_acc
    | Ast.PCons (pat1, pat2) ->
      let h1 = helper acc (gen_list_getter_head curr_expr) pat1 in
      helper h1 (gen_list_getter_tail curr_expr) pat2
    | Ast.PType (pat, _) -> helper acc curr_expr pat
  in
  let rev_new_value_bindings = helper [] (Simple_ast.SEVar (Ast.Id gen_decl_name)) pat in
  (* let vb_with_cond = (match new_value_bindings, cond with 
    | [], _ | _, Simple_ast.SEConst (Ast.CBool true) -> new_value_bindings
    | h :: tl, _ -> 
      let ident, expr = h in 
      (ident, Simple_ast.SEIf (cond, expr, gen_matching_failed)) :: tl)
    in
    List.rev vb_with_cond
  in *)
  let rev_new_vb_with_cond = 
    (match cond with 
    | Simple_ast.SEConst (Ast.CBool true) -> rev_new_value_bindings
    | _ ->
      let lst = List.fold_left
      (fun acc decl -> 
        let ident, expr = decl in 
        (ident, Simple_ast.SEIf (cond, expr, gen_matching_failed)) :: acc)
      []
      rev_new_value_bindings
      in List.rev lst) 
  in
  rev_new_vb_with_cond

and construct_new_rec_value_binding value_binding =
  let pat, expr = value_binding in
  let var_id =
    match pat with
    | Ast.PVar var_id -> var_id
    | _ -> Ast.Id "Hello infer"
  in
  Simple_ast.SId var_id, simplify_expr expr

and construct_new_rec_value_bindings value_binding_lst =
  let new_bindings_lst =
    List.fold_left
      (fun acc value_binding -> construct_new_rec_value_binding value_binding :: acc)
      []
      value_binding_lst
  in
  List.rev new_bindings_lst
;;

let simplify_structure_item structure_item =
  let helper = function
    | Ast.SILet (Ast.Nonrecursive, value_binding_lst) ->
      let rev_gen_decl_lst, rev_gen_decl_names, _ =
        List.fold_left
          (fun acc value_binding ->
            let gen_decl_lst, gen_decl_names, gen_decl_number = acc in
            let _, expr = value_binding in
            let new_gen_decl_name = gen_pat_expr_name ^ string_of_int gen_decl_number in
            let new_gen_decl =
              Simple_ast.SSILet
                (Ast.Nonrecursive, [ Simple_ast.SId (Ast.Id new_gen_decl_name), simplify_expr expr ])
            in
            ( new_gen_decl :: gen_decl_lst
            , new_gen_decl_name :: gen_decl_names
            , gen_decl_number + 1 ))
          ([], [], 0)
          value_binding_lst
      in
      let filtered_gen_decl_lst =
        List.fold_left2
          (fun acc gen_decl value_binding ->
            let pat, _ = value_binding in
            match pat with
            | Ast.PVar _ -> acc
            | _ -> gen_decl :: acc)
          []
          rev_gen_decl_lst
          (List.rev value_binding_lst)
      in
      let gen_decl_names = List.rev rev_gen_decl_names in
      let rev_updated_value_bindings =
        List.fold_left2
          (fun acc value_binding gen_decl_name ->
            let pat, expr = value_binding in
            match pat with
            | Ast.PVar var_name -> (Simple_ast.SId var_name, simplify_expr expr) :: acc
            | _ ->
              let cond = construct_equality_condition (Ast.Id gen_decl_name) pat true_condition in 
              let rev_new_value_bindings =
                construct_new_nonrec_value_decl value_binding gen_decl_name cond
              in
              List.append rev_new_value_bindings acc)
          []
          value_binding_lst
          gen_decl_names
      in
      List.append
        filtered_gen_decl_lst
        [ Simple_ast.SSILet (Ast.Nonrecursive, List.rev rev_updated_value_bindings) ]
    | Ast.SILet (Ast.Recursive, value_binding_lst) ->
      let new_rec_value_binding_lst =
        construct_new_rec_value_bindings value_binding_lst
      in
      [ Simple_ast.SSILet (Ast.Recursive, new_rec_value_binding_lst) ]
    | Ast.SIExpr expr -> [ Simple_ast.SSIExpr (simplify_expr expr) ]
  in
  helper structure_item
;;

let simplify_structure structure =
  List.fold_left
    (fun acc structure_item ->
      let new_structure_items = simplify_structure_item structure_item in
      List.append acc new_structure_items)
    []
    structure
;;

let convert (structure : Ast.structure) = simplify_structure structure
