(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)
include StatementInfer

open Substitution

let const2type : Ast.constant -> Ast.typeName = function
  | Ast.CBool _ -> Ast.TBool
  | Ast.CInt _ -> Ast.TInt
;;

type patern_mode =
  | PMAdd
  | PMCheck

let infer_pattern : patern_mode -> Ast.pattern -> (state, Ast.typeName) t =
  fun pm cpat ->
  let get_pat_and_type = function
    | Ast.PNConstraint pat -> fresh_tv >>= fun t -> return (pat, t)
    | Ast.PConstraint (pat, tp) -> return (pat, tp)
  in
  let infer_add_pident name tp =
    let* var_tp = read_var_type name in
    match var_tp with
    | None -> write_flat_var_type name tp *> return tp
    | Some _ ->
      fail (Format.sprintf "Variable %s is bound several times in this matching" name)
  and infer_add_pconstant c tp =
    let const_tp = const2type c in
    write_subst tp const_tp *> return const_tp
  and infer_add_ptupple p_lst tp rec_fun =
    let* t_lst = map_list rec_fun p_lst in
    let tuple_tp = Ast.TTuple t_lst in
    write_subst tp tuple_tp *> return tuple_tp
  and infer_add_pcons (p_head, p_tail) tp rec_fun =
    let* head_tp = rec_fun p_head in
    let* tail_tp = rec_fun p_tail in
    write_subst tp tail_tp *> write_subst (Ast.TList head_tp) tail_tp *> return tail_tp
  and infer_add_pnil tp =
    let* tv = fresh_tv in
    let list_tp = Ast.TList tv in
    write_subst tp list_tp *> return list_tp
  in
  let rec help cp =
    let* pat, tp = get_pat_and_type cp in
    match pat with
    | Ast.PIdentifier name -> infer_add_pident name tp
    | Ast.PConstant c -> infer_add_pconstant c tp
    | Ast.PTuple p_lst -> infer_add_ptupple p_lst tp help_no_constraint
    | Ast.PCons (p_head, p_tail) -> infer_add_pcons (p_head, p_tail) tp help_no_constraint
    | Ast.PNil -> infer_add_pnil tp
    | Ast.PWildCard -> return tp
  and help_no_constraint ncp = help (PNConstraint ncp) in
  let* glob_env = read_env in
  write_env MapString.empty
  *> let* tp = help cpat in
     let* loc_env = read_env in
     match pm with
     | PMAdd ->
       let new_env =
         MapString.merge
           (fun _ old_el new_el ->
             match new_el with
             | Some x -> Some x
             | None -> old_el)
           glob_env
           loc_env
       in
       write_env new_env *> return tp
     | PMCheck ->
       let not_found =
         MapString.fold
           (fun name _ acc ->
             match acc with
             | None when not (MapString.mem name glob_env) -> Some name
             | x -> x)
           loc_env
           None
       in
       (match not_found with
        | None -> write_env glob_env *> return tp
        | Some x -> fail (Format.sprintf "Unbound value %s" x))
;;

let rec infer_expr : Ast.expr -> (state, Ast.typeName) t =
  fun expr ->
  let help = function
    | Ast.EConstant c -> return (const2type c)
    | Ast.EIdentifier name -> infer_ident name
    | Ast.ENil ->
      let* tv = fresh_tv in
      return (Ast.TList tv)
    | Ast.EFunction (arg, body) -> infer_func arg body
    | Ast.EApplication (fun_exp, arg_exp) -> infer_app fun_exp arg_exp
    | Ast.EIfThenElse (e1, e2, e3) -> infer_ifthenelse e1 e2 e3
    | Ast.ELetIn (_rec_flag, _pattern, _expr_val, _expr_in) ->
      fail "boba should be implemented"
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

and infer_ident : string -> (state, Ast.typeName) t =
  fun name ->
  let* vt_opt = read_var_type name in
  match vt_opt with
  | None -> fail (Format.sprintf "Unbound value: %s" name)
  | Some tp -> return tp

and infer_func : Ast.pattern -> Ast.expr -> (state, Ast.typeName) t =
  fun pat exp ->
  let* arg_tp = infer_pattern PMAdd pat in
  let* exp_tp = infer_expr exp in
  let fcn_tp = Ast.TFunction (arg_tp, exp_tp) in
  return fcn_tp

and infer_app : Ast.expr -> Ast.expr -> (state, Ast.typeName) t =
  fun func_exp args_exp ->
  let* self_tp = fresh_tv in
  let* func_tp = infer_expr func_exp in
  let* arg_tp = infer_expr args_exp in
  write_subst func_tp (Ast.TFunction (arg_tp, self_tp)) *> return self_tp

and infer_ifthenelse : Ast.expr -> Ast.expr -> Ast.expr -> (state, Ast.typeName) t =
  fun e1 e2 e3 ->
  let* tp = fresh_tv in
  let* t1 = infer_expr e1 in
  let* t2 = infer_expr e2 in
  let* t3 = infer_expr e3 in
  write_subst t1 Ast.TInt *> write_subst tp t2 *> write_subst tp t3 *> return tp

and infer_match : Ast.pattern -> (Ast.pattern * Ast.expr) list -> (state, Ast.typeName) t =
  fun scrutin pat_exp_lst ->
  let* tp = fresh_tv in
  let* scr_tp = infer_pattern PMCheck scrutin in
  let* cur_env = read_env in
  let help (pat, exp) =
    let* pat_tp = infer_pattern PMAdd pat in
    let* exp_tp = infer_expr exp in
    write_subst pat_tp scr_tp *> write_subst exp_tp tp *> return () <* write_env cur_env
  in
  map_list help pat_exp_lst *> return tp
;;

let test_infer_exp string_exp =
  let res = Parser.parse Parser.p_exp string_exp in
  match res with
  | Result.Ok exp ->
    let (_, substs, _), res = run (infer_expr exp) (MapString.empty, [], 0) in
    (match res with
     | Result.Ok tp ->
       Format.printf
         "res: %s@\n substs: %s"
         (Ast.show_typeName tp)
         (show_subs_state substs)
     | Result.Error s -> Format.printf "Infer error: %s" s)
  | Result.Error e -> Format.printf "Parser error: %s" e
;;

let%expect_test _ =
  test_infer_exp "fun ((x, y): (int*bool)) -> y";
  [%expect
    {|
    res: (TFunction ((TTuple [(TPoly "_p0"); (TPoly "_p1")]), (TPoly "_p1")))
     substs: [("_p1", TBool); ("_p0", TInt)] |}]
;;

let%expect_test _ =
  test_infer_exp "fun ((x::y): (int list)) -> y";
  [%expect
    {|
    res: (TFunction ((TPoly "_p1"), (TPoly "_p1")))
     substs: [("_p0", TInt); ("_p1", (TList TInt))] |}]
;;

let%expect_test _ =
  test_infer_exp {|fun f (list: int) -> match list with
  | [] -> list
  | h :: tl -> h|};
  [%expect
    {|
    res: (TFunction ((TPoly "_p0"), (TFunction (TInt, (TPoly "_p1")))))
     substs: [("_p4", TInt); ("_p6", TInt); ("_p7", (TList TInt)); ("_p5", (TList TInt));
      ("_p1", TInt); ("_p2", (TList TInt)); ("_p3", (TList TInt))] |}]
;;

let%expect_test _ =
  test_infer_exp {|fun f list -> match nolist with
  | [] -> list
  | h :: tl -> h|};
  [%expect {| Infer error: Unbound value nolist |}]
;;

let%expect_test _ =
  test_infer_exp {|(fun f x -> f)(fun f x -> f)|};
  [%expect
    {|
    res: (TPoly "_p0")
     substs: [("_p0",
      (TFunction ((TPoly "_p2"),
         (TFunction ((TPoly "_p3"), (TFunction ((TPoly "_p4"), (TPoly "_p3")))))
         )));
      ("_p1",
       (TFunction ((TPoly "_p3"), (TFunction ((TPoly "_p4"), (TPoly "_p3"))))))
      ] |}]
;;
