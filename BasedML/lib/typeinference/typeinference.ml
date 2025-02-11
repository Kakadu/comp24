(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open StatementInfer
open Substitution
open StartState
open Help
open Generalise
open Ast

let infer_const : Ast.constant -> (state, Ast.type_name) t = function
  | Ast.CBool _ -> return Ast.TBool
  | Ast.CInt _ -> return Ast.TInt
  | Ast.CNil ->
    let* tv = fresh_tv in
    return (Ast.TList tv)
  | Ast.CUnit -> return Ast.TUnit
;;

type pattern_mode =
  | PMAdd
  | PMCheck

let infer_pattern : pattern_mode -> Ast.pattern -> (state, Ast.type_name) t =
  fun pm cpat ->
  let infer_add_pident name tp =
    let* var_tp = read_var_type name in
    match var_tp with
    | None -> write_flat_var_type name tp *> return tp
    | Some _ ->
      fail (Format.sprintf "Variable %s is bound several times in this matching" name)
  and infer_add_pconstant c tp =
    let* const_tp = infer_const c in
    write_subst tp const_tp *> return const_tp
  and infer_add_ptupple p_lst tp rec_fun =
    let* t_lst = map_list rec_fun p_lst in
    let tuple_tp = Ast.TTuple t_lst in
    write_subst tp tuple_tp *> return tuple_tp
  and infer_add_pcons (p_head, p_tail) tp rec_fun =
    let* head_tp = rec_fun p_head in
    let* tail_tp = rec_fun p_tail in
    write_subst tp tail_tp *> write_subst (Ast.TList head_tp) tail_tp *> return tail_tp
  in
  let rec help pat =
    let* tp = fresh_tv in
    match pat with
    | Ast.PIdentifier name -> infer_add_pident name tp
    | Ast.PConstant c -> infer_add_pconstant c tp
    | Ast.PTuple p_lst -> infer_add_ptupple p_lst tp help
    | Ast.PCons (p_head, p_tail) -> infer_add_pcons (p_head, p_tail) tp help
    | Ast.PWildCard -> return tp
    | Ast.PConstraint (pat, ctp) ->
      let* new_tp = help pat in
      write_subst new_tp ctp *> return ctp
  in
  let* glob_env = read_env in
  write_env MapString.empty
  *> let* tp = help cpat in
     let* loc_env = read_env in
     match pm with
     | PMAdd ->
       let new_env =
         MapString.merge
           (fun _ old_el -> function
             | Some x -> Some x
             | None -> old_el)
           glob_env
           loc_env
       in
       write_env new_env *> return tp
     | PMCheck ->
       let* _ = write_env glob_env in
       let loc_vars = MapString.bindings loc_env in
       map_list
         (fun (name, tp) ->
           match tp with
           | TFFlat tp ->
             let* opt_tp = read_var_type name in
             (match opt_tp with
              | Some x -> write_subst x tp
              | None -> fail (Format.sprintf "Unbound value %s" name))
           | TFSchem _ -> fail "Unreacheble error: getschem type in pattern")
         loc_vars
       *> return tp
;;

let rec infer_expr : Ast.expr -> (state, Ast.type_name) t =
  fun expr ->
  let help = function
    | Ast.EConstant c -> infer_const c
    | Ast.EIdentifier name -> infer_ident name
    | Ast.EFunction (arg, body) -> infer_func arg body
    | Ast.EApplication (fun_exp, arg_exp) -> infer_app fun_exp arg_exp
    | Ast.EIfThenElse (e1, e2, e3) -> infer_ifthenelse e1 e2 e3
    | Ast.ELetIn (rec_flag, pattern, expr_val, expr_in) ->
      let* _ = infer_let_common rec_flag pattern expr_val in
      infer_expr expr_in
    | Ast.ETuple exp_lst ->
      let* t_lst = map_list infer_expr exp_lst in
      return (Ast.TTuple t_lst)
    | Ast.EMatch (scrutin, pat_exp_lst) -> infer_match scrutin pat_exp_lst
    | Ast.EConstraint (exp, tp) ->
      let* exp_tp = infer_expr exp in
      write_subst exp_tp tp *> return exp_tp
  in
  let* env = read_env in
  help expr <* write_env env

and infer_ident : string -> (state, Ast.type_name) t =
  fun name ->
  let* vt_opt = read_var_type name in
  match vt_opt with
  | None -> fail (Format.sprintf "Unbound value: %s" name)
  | Some tp -> return tp

and infer_func : Ast.pattern -> Ast.expr -> (state, Ast.type_name) t =
  fun pat exp ->
  let* arg_tp = infer_pattern PMAdd pat in
  let* exp_tp = infer_expr exp in
  let fcn_tp = Ast.TFunction (arg_tp, exp_tp) in
  return fcn_tp

and infer_app : Ast.expr -> Ast.expr -> (state, Ast.type_name) t =
  fun func_exp args_exp ->
  let* self_tp = fresh_tv in
  let* func_tp = infer_expr func_exp in
  let* arg_tp = infer_expr args_exp in
  write_subst func_tp (Ast.TFunction (arg_tp, self_tp)) *> return self_tp

and infer_ifthenelse : Ast.expr -> Ast.expr -> Ast.expr -> (state, Ast.type_name) t =
  fun e1 e2 e3 ->
  let* tp = fresh_tv in
  let* t1 = infer_expr e1 in
  let* t2 = infer_expr e2 in
  let* t3 = infer_expr e3 in
  write_subst t1 Ast.TBool *> write_subst tp t2 *> write_subst tp t3 *> return tp

and infer_match : Ast.pattern -> (Ast.pattern * Ast.expr) list -> (state, Ast.type_name) t
  =
  fun scrutin pat_exp_lst ->
  let* tp = fresh_tv in
  let* scr_tp = infer_pattern PMCheck scrutin in
  let* cur_env = read_env in
  let help (pat, exp) =
    let* pat_tp = infer_pattern PMAdd pat in
    let* exp_tp = infer_expr exp in
    write_subst pat_tp scr_tp *> write_subst exp_tp tp *> write_env cur_env
  in
  map_list help pat_exp_lst *> return tp

and infer_let_common : Ast.rec_flag -> Ast.pattern -> Ast.expr -> (state, unit) t =
  fun rec_f pat exp ->
  let* prev_env = read_env in
  let* p_tp, exp_tp =
    match rec_f with
    | NotRec ->
      let* exp_tp = infer_expr exp in
      let* p_tp = infer_pattern PMAdd pat in
      return (p_tp, exp_tp)
    | Rec ->
      (match pat with
       | Ast.PIdentifier _ ->
         let* p_tp = infer_pattern PMAdd pat in
         let* exp_tp = infer_expr exp in
         return (p_tp, exp_tp)
       | _ -> fail " Only variables are allowed as left-hand side of `let rec'")
  in
  let* _ = write_subst p_tp exp_tp in
  let* external_tvs = get_tv_from_env prev_env in
  generalise external_tvs pat p_tp
;;

let infer_mr_let_only_pat : Ast.pattern -> (state, Ast.type_name) t = function
  | Ast.PIdentifier _ as pat -> infer_pattern PMAdd pat
  | _ -> fail " Only variables are allowed as left-hand side of `let rec'"
;;

let infer_let_decl : Ast.let_declaration -> (state, unit) t = function
  | Ast.DSingleLet (rec_f, DLet (pat, exp)) -> infer_let_common rec_f pat exp
  | Ast.DMutualRecDecl (Ast.NotRec, decl_lst) ->
    map_list
      (function
        | Ast.DLet (pat, exp) -> infer_let_common Ast.NotRec pat exp)
      decl_lst
    *> return ()
  | Ast.DMutualRecDecl (Ast.Rec, decl_lst) ->
    let* prev_env = read_env in
    let* p_tp_lst =
      map_list (fun (Ast.DLet (pat, _)) -> infer_mr_let_only_pat pat) decl_lst
    in
    let* exp_tp_lst = map_list (fun (Ast.DLet (_, exp)) -> infer_expr exp) decl_lst in
    let* _ =
      map_list
        (fun (p_tp, exp_tp) -> write_subst p_tp exp_tp)
        (List.combine p_tp_lst exp_tp_lst)
    in
    let* external_tvs = get_tv_from_env prev_env in
    let* _ =
      map_list
        (fun (Ast.DLet (pat, _), tp) -> generalise external_tvs pat tp)
        (List.combine decl_lst p_tp_lst)
    in
    return ()
;;

type res_map = Ast.type_name MapString.t [@@deriving show { with_path = false }]

let infer_declarations : Ast.declarations -> (state, res_map) t =
  fun dec_lst ->
  map_list infer_let_decl dec_lst
  *> let* env = read_env in
     let lst = MapString.bindings env in
     let* restored_lst =
       map_list
         (fun (name, _tp) ->
           let* var_tp = read_var_type name in
           match var_tp with
           | Some tp -> return (name, tp)
           | None -> fail "unreachable error: can not find name from env in env")
         lst
     in
     return (MapString.of_seq (List.to_seq restored_lst))
;;

let infer_prog decls =
  let _, res = run (init_used_type_names decls *> infer_declarations decls) start_state in
  res
;;

let test_infer_exp string_exp =
  let res = Parser.parse Parser.p_exp string_exp in
  match res with
  | Result.Ok exp ->
    let (_, substs, _, _), res =
      run (infer_expr exp >>= restore_type) (MapString.empty, [], 0, SetString.empty)
    in
    (match res with
     | Result.Ok tp ->
       Format.printf
         "res: %s@\n substs: %s"
         (Ast.show_type_name tp)
         (show_substitution_list substs)
     | Result.Error s -> Format.printf "Infer error: %s" s)
  | Result.Error e -> Format.printf "Parser error: %s" e
;;

let test_infer_prog_with_state s_state string_exp =
  let res = Parser.parse_program string_exp in
  match res with
  | Result.Ok prog ->
    let _, res = run (infer_declarations prog) s_state in
    (match res with
     | Result.Ok map -> Format.printf "%s@\n" (show_res_map map)
     | Result.Error s -> Format.printf "Infer error: %s" s)
  | Result.Error e -> Format.printf "Parser error: %s" e
;;

let test_infer_prog string_exp =
  let res = Parser.parse_program string_exp in
  match res with
  | Result.Ok prog ->
    let res = infer_prog prog in
    (match res with
     | Result.Ok map -> Format.printf "%s@\n" (show_res_map map)
     | Result.Error s -> Format.printf "Infer error: %s" s)
  | Result.Error e -> Format.printf "Parser error: %s" e
;;

module StringMap = Map.Make (String)

(* Helper function to compare two types with alpha equivalence *)
let rec alpha_equivalent
  (mapping_t1_to_t2 : string StringMap.t)
  (mapping_t2_to_t1 : string StringMap.t)
  (t1 : type_name)
  (t2 : type_name)
  : bool * string StringMap.t * string StringMap.t
  =
  match t1, t2 with
  | TUnit, TUnit | TInt, TInt | TBool, TBool -> true, mapping_t1_to_t2, mapping_t2_to_t1
  | TPoly var1, TPoly var2 ->
    let mapped_var2 = StringMap.find_opt var1 mapping_t1_to_t2 in
    let mapped_var1 = StringMap.find_opt var2 mapping_t2_to_t1 in
    (match mapped_var2, mapped_var1 with
     | Some mapped_var2', Some mapped_var1' ->
       if mapped_var2' = var2 && mapped_var1' = var1
       then true, mapping_t1_to_t2, mapping_t2_to_t1
       else false, mapping_t1_to_t2, mapping_t2_to_t1
     | None, None ->
       let updated_mapping_t1_to_t2 = StringMap.add var1 var2 mapping_t1_to_t2 in
       let updated_mapping_t2_to_t1 = StringMap.add var2 var1 mapping_t2_to_t1 in
       true, updated_mapping_t1_to_t2, updated_mapping_t2_to_t1
     | _ -> false, mapping_t1_to_t2, mapping_t2_to_t1)
  | TTuple ts1, TTuple ts2 ->
    if List.compare_lengths ts1 ts2 = 0
    then
      List.fold_right2
        (fun tp1 tp2 (res, map1, map2) ->
          if res then alpha_equivalent map1 map2 tp1 tp2 else false, map1, map2)
        ts1
        ts2
        (true, mapping_t1_to_t2, mapping_t2_to_t1)
    else false, mapping_t1_to_t2, mapping_t2_to_t1
  | TFunction (dom1, codom1), TFunction (dom2, codom2) ->
    let dom_equal, updated_mapping_t1_to_t2, updated_mapping_t2_to_t1 =
      alpha_equivalent mapping_t1_to_t2 mapping_t2_to_t1 dom1 dom2
    in
    if not dom_equal
    then false, updated_mapping_t1_to_t2, updated_mapping_t2_to_t1
    else alpha_equivalent updated_mapping_t1_to_t2 updated_mapping_t2_to_t1 codom1 codom2
  | TList t1, TList t2 -> alpha_equivalent mapping_t1_to_t2 mapping_t2_to_t1 t1 t2
  | _ -> false, mapping_t1_to_t2, mapping_t2_to_t1
;;

(* Public function to compare two types *)
let types_equal t1 t2 =
  let result, _, _ = alpha_equivalent StringMap.empty StringMap.empty t1 t2 in
  result
;;
