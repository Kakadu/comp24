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

let pat_remove_constr : Ast.pattern -> Ast.pattern_no_constraint = function
  | Ast.PConstraint (p, _) -> p
  | Ast.PNConstraint p -> p
;;

let restore_type : Ast.typeName -> (state, Ast.typeName) t =
  fun tp ->
  let* subs = read_subs in
  return (apply_substs subs tp)
;;

let rec get_tv_from_tp acc = function
  | Ast.TBool | Ast.TInt -> acc
  | Ast.TPoly x -> SetString.add x acc
  | Ast.TFunction (t1, t2) ->
    SetString.union (get_tv_from_tp acc t1) (get_tv_from_tp SetString.empty t2)
  | Ast.TList t1 -> get_tv_from_tp acc t1
  | Ast.TTuple t_lst -> List.fold_left get_tv_from_tp acc t_lst
;;

let get_tv_from_env : env_map -> SetString.t =
  fun env ->
  MapString.fold
    (fun _var_name tp acc ->
      match tp with
      | TFFlat tp -> get_tv_from_tp acc tp
      | TFSchem _ -> acc)
    env
    SetString.empty
;;

let rec write_scheme_for_pattern
  : SetString.t -> Ast.pattern_no_constraint -> Ast.typeName -> (state, unit) t
  =
  fun free_vars pattern tp ->
  let rec_call = write_scheme_for_pattern free_vars in
  match pattern, tp with
  | PWildCard, _ | PNil, _ | PConstant _, _ -> return ()
  | PIdentifier x, tp ->
    let used_tvs = get_tv_from_tp SetString.empty tp in
    let new_free_vars = SetString.inter used_tvs free_vars in
    write_var_type x (TFSchem (new_free_vars, tp))
  | PCons (p1, p2), TList t -> rec_call p1 t *> rec_call p2 tp
  | PTuple p_lst, TTuple t_lst ->
    map_list (fun (p, t) -> rec_call p t) (List.combine p_lst t_lst) *> return ()
  | _ -> fail "something strange during generealisetion"
;;

let generalise
  : SetString.t -> Ast.pattern_no_constraint -> Ast.typeName -> (state, unit) t
  =
  fun bound_vars pat tp ->
  let* tp = restore_type tp in
  let* subs = read_subs in
  let used_vars = get_tv_from_tp SetString.empty tp in
  let unbound_vars = SetString.diff used_vars bound_vars in
  let free_vars =
    SetString.filter (fun name -> not (List.mem_assoc name subs)) unbound_vars
  in
  write_scheme_for_pattern free_vars pat tp
;;

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
       | Ast.PConstraint (Ast.PIdentifier _, _) | Ast.PNConstraint (Ast.PIdentifier _) ->
         let* p_tp = infer_pattern PMAdd pat in
         let* exp_tp = infer_expr exp in
         return (p_tp, exp_tp)
       | _ -> fail " Only variables are allowed as left-hand side of `let rec'")
  in
  let* _ = write_subst p_tp exp_tp in
  let external_tvs = get_tv_from_env prev_env in
  generalise external_tvs (pat_remove_constr pat) p_tp
;;

let infer_mr_let_only_pat : Ast.pattern -> (state, Ast.typeName) t = function
  | (Ast.PConstraint (Ast.PIdentifier _, _) | Ast.PNConstraint (Ast.PIdentifier _)) as pat
    -> infer_pattern PMAdd pat
  | _ -> fail " Only variables are allowed as left-hand side of `let rec'"
;;

let infer_let_decl : Ast.let_declaration -> (state, unit) t = function
  | Ast.DSingleLet (DLet (rec_f, pat, exp)) -> infer_let_common rec_f pat exp
  | Ast.DMutualRecDecl decl_lst ->
    (match decl_lst with
     | [] -> fail "unreachable error: empty mutual rec?!!"
     | Ast.DLet (Ast.NotRec, _, _) :: _ ->
       map_list
         (fun dlet ->
           let (Ast.DLet (_rec_flag, pat, exp)) = dlet in
           infer_let_common Ast.NotRec pat exp)
         decl_lst
       *> return ()
     | Ast.DLet (Ast.Rec, _, _) :: _ ->
       let* prev_env = read_env in
       let* p_tp_lst =
         map_list (fun (Ast.DLet (_, pat, _)) -> infer_mr_let_only_pat pat) decl_lst
       in
       let* exp_tp_lst =
         map_list (fun (Ast.DLet (_, _, exp)) -> infer_expr exp) decl_lst
       in
       let* _ =
         map_list
           (fun (p_tp, exp_tp) -> write_subst p_tp exp_tp)
           (List.combine p_tp_lst exp_tp_lst)
       in
       let external_tvs = get_tv_from_env prev_env in
       let* _ =
         map_list
           (fun (Ast.DLet (_, pat, _), tp) ->
             generalise external_tvs (pat_remove_constr pat) tp)
           (List.combine decl_lst p_tp_lst)
       in
       return ())
;;

type res_map = Ast.typeName MapString.t [@@deriving show { with_path = false }]

let infer_declarations : Ast.declarations -> (state, res_map) t =
  fun dec_lst ->
  map_list infer_let_decl dec_lst
  *> let* env = read_env in
     let lst = MapString.bindings env in
     let* restored_lst =
       map_list
         (fun (name, _) ->
           let* var_tp = read_var_type name in
           match var_tp with
           | Some x -> restore_type x >>= fun tp -> return (name, tp)
           | None -> fail "unreachable error: can not find name from env in env")
         lst
     in
     return (MapString.of_seq (List.to_seq restored_lst))
;;

let infer_prog decls =
  let _, res = run (infer_declarations decls) start_state in
  res
;;

let test_infer_exp string_exp =
  let res = Parser.parse Parser.p_exp string_exp in
  match res with
  | Result.Ok exp ->
    let (_, substs, _), res =
      run (infer_expr exp >>= restore_type) (MapString.empty, [], 0)
    in
    (match res with
     | Result.Ok tp ->
       Format.printf
         "res: %s@\n substs: %s"
         (Ast.show_typeName tp)
         (show_subs_state substs)
     | Result.Error s -> Format.printf "Infer error: %s" s)
  | Result.Error e -> Format.printf "Parser error: %s" e
;;

let test_infer_prog string_exp =
  let res = Parser.parse_program string_exp in
  match res with
  | Result.Ok prog ->
    let _, res = run (infer_declarations prog) (MapString.empty, [], 0) in
    (match res with
     | Result.Ok map -> Format.printf "%s@\n" (show_res_map map)
     | Result.Error s -> Format.printf "Infer error: %s" s)
  | Result.Error e -> Format.printf "Parser error: %s" e
;;

let%expect_test _ =
  test_infer_exp "fun ((x, y): (int*bool)) -> y";
  [%expect
    {|
    res: (TFunction ((TTuple [TInt; TBool]), TBool))
     substs: [("_p1", TBool); ("_p0", TInt)] |}]
;;

let%expect_test _ =
  test_infer_exp "fun ((x::y): (int list)) -> y";
  [%expect
    {|
    res: (TFunction ((TList TInt), (TList TInt)))
     substs: [("_p0", TInt); ("_p1", (TList TInt))] |}]
;;

let%expect_test _ =
  test_infer_exp
    {|fun (tuper_var: int) -> match tuper_var with
  | ([]: 'a list) -> tuper_var
  | (h :: tl: 'a list) -> h|};
  [%expect {|
    Infer error: Can not unify `TInt` and `(TList (TPoly "_p2"))` |}]
;;

let%expect_test _ =
  test_infer_exp
    {|fun tuper_var -> match tuper_var with
  | ([]: 'a list) -> tuper_var
  | (h :: tl: 'a list) -> h|};
  [%expect
    {|
    Infer error: The type variable _p4 occurs inside (TList (TPoly "_p4")) |}]
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
    res: (TFunction ((TPoly "_p2"),
       (TFunction ((TPoly "_p3"), (TFunction ((TPoly "_p4"), (TPoly "_p3")))))))
     substs: [("_p0",
      (TFunction ((TPoly "_p2"),
         (TFunction ((TPoly "_p3"), (TFunction ((TPoly "_p4"), (TPoly "_p3")))))
         )));
      ("_p1",
       (TFunction ((TPoly "_p3"), (TFunction ((TPoly "_p4"), (TPoly "_p3"))))))
      ] |}]
;;

let%expect_test _ =
  test_infer_exp {|let x = 1 in x|};
  [%expect {|
    res: TInt
     substs: [("_p0", TInt)] |}]
;;

let%expect_test _ =
  test_infer_exp {|let id = fun x -> x in id|};
  [%expect
    {|
    res: (TFunction ((TPoly "_p2"), (TPoly "_p2")))
     substs: [("_p1", (TFunction ((TPoly "_p0"), (TPoly "_p0"))))] |}]
;;

let%expect_test _ =
  test_infer_exp
    {|let rec fiboCPS = fun n acc -> match n with
    | 0 -> acc 0
    | 1 -> acc 1
    | _ -> fiboCPS n (fun x -> fiboCPS n (fun y -> acc x))
      in fiboCPS 2 (fun x -> x)|};
  [%expect
    {|
    res: TInt
     substs: [("_p11", TInt); ("_p13", TInt); ("_p14", TInt);
      ("_p12", (TFunction ((TFunction (TInt, TInt)), TInt))); ("_p9", TInt);
      ("_pd", (TPoly "_pa")); ("_p10", (TPoly "_pa")); ("_pf", TInt);
      ("_pe", (TFunction ((TFunction (TInt, (TPoly "_pa"))), (TPoly "_pa"))));
      ("_p8", (TPoly "_pa")); ("_pc", TInt);
      ("_pb", (TFunction ((TFunction (TInt, (TPoly "_pa"))), (TPoly "_pa"))));
      ("_p0",
       (TFunction (TInt,
          (TFunction ((TFunction (TInt, (TPoly "_pa"))), (TPoly "_pa"))))));
      ("_p6", (TPoly "_pa")); ("_p7", TInt); ("_p3", (TPoly "_pa"));
      ("_p4", TInt); ("_p2", (TFunction (TInt, (TPoly "_pa")))); ("_p5", TInt);
      ("_p1", TInt)] |}]
;;

let%expect_test _ =
  test_infer_exp {|let id = fun x -> x in ((id 1), (id true))|};
  [%expect
    {|
    res: (TTuple [TInt; TBool])
     substs: [("_p4", TBool); ("_p5", TBool); ("_p2", TInt); ("_p3", TInt);
      ("_p1", (TFunction ((TPoly "_p0"), (TPoly "_p0"))))] |}]
;;

(* Declarations *)

let%expect_test _ =
  test_infer_prog {|let x = 1;;
    let y = 2;;|};
  [%expect {|
    [""x"": TInt,
     ""y"": TInt,
     ] |}]
;;

let%expect_test _ =
  test_infer_prog {|let id = fun x-> x;;|};
  [%expect {|
    [""id"": (TFunction ((TPoly "_p2"), (TPoly "_p2"))),
     ] |}]
;;

let%expect_test _ =
  test_infer_prog {|let id = fun x-> x;;
    let (x, y) = (id true, id 2);;|};
  [%expect
    {|
    [""id"": (TFunction ((TPoly "_p9"), (TPoly "_p9"))),
     ""x"": TBool,
     ""y"": TInt,
     ] |}]
;;

let%expect_test _ =
  test_infer_prog {|let rec f = fun x -> f;;|};
  [%expect
    {|
    Infer error: The type variable _p0 occurs inside (TFunction ((TPoly "_p1"), (TPoly "_p0"))) |}]
;;

let%expect_test _ =
  test_infer_prog {|let rec id = fun x -> x and dup = fun x y -> (id x, id y);;|};
  [%expect
    {|
    [""dup"": (TFunction ((TPoly "_p7"),
                 (TFunction ((TPoly "_p7"),
                    (TTuple [(TPoly "_p7"); (TPoly "_p7")])))
                 )),
     ""id"": (TFunction ((TPoly "_p8"), (TPoly "_p8"))),
     ] |}]
;;

let%expect_test _ =
  test_infer_prog
    {|let ((x, y) :('a * 'a)) = ((fun x-> x), (fun (x, y) -> (x, x)));;
  let (a, b) = ((x (1, 2)), (x (true, false)));;|};
  [%expect
    {|
    [""a"": (TTuple [TInt; TInt]),
     ""b"": (TTuple [TBool; TBool]),
     ""x"": (TFunction ((TTuple [(TPoly "_pd"); (TPoly "_pd")]),
               (TTuple [(TPoly "_pd"); (TPoly "_pd")]))),
     ""y"": (TFunction ((TTuple [(TPoly "_pe"); (TPoly "_pe")]),
               (TTuple [(TPoly "_pe"); (TPoly "_pe")]))),
     ] |}]
;;

let%expect_test _ =
  test_infer_prog
    {|let ((x, y) :('a * 'a)) = ((fun x-> x), (fun (x, y) -> (x, x)));;
  let (a, b) = ((x 1), (y (true, false)));;|};
  [%expect
    {|
    Infer error: Can not unify `TInt` and `(TTuple [(TPoly "_p7"); (TPoly "_p7")])` |}]
;;

let%expect_test _ =
  test_infer_prog
    {|
let rec even = fun n -> match n with
    | 0 -> true
    | x -> odd (x)
and odd = fun n -> match n with
    | 0 -> false
    | x -> even (x)
|};
  [%expect
    {|
    [""even"": (TFunction (TInt, TBool)),
     ""odd"": (TFunction (TInt, TBool)),
     ] |}]
;;

let%expect_test _ =
  test_infer_prog
    {|
let (-) = fun (a:int) (b:int)->  a;;

let rec even = fun n -> match n with
    | 0 -> true
    | x -> odd (x - 1)
and odd = fun n -> match n with
    | 0 -> false
    | x -> even (x - 1)
|};
  [%expect {| |}]
;;

let%expect_test _ =
  test_infer_prog
    {|
let (-) = fun (a:int) (b:int)->  a;;

let fibo n =
  let rec fiboCPS n acc =
    match n with
    | 0 -> acc 0
    | 1 -> acc 1
    | _ -> fiboCPS (n - 1) (fun x -> fiboCPS (n - 2) (fun y -> acc (x + y)))
  in
  fiboCPS n (fun x -> x)
;;
|};
  [%expect {|  |}]
;;
