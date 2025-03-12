(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf_ast
open Ll_conversion.Ll_ast
open Match_elimination.Me_ast

module Name_id = struct
  type t = string

  let compare = String.compare
end

module GlobalMap = Stdlib.Map.Make (Name_id)

open
  Common.Monads.GenericCounterMonad
    (struct
      type t = string scoped GlobalMap.t
    end)
    (String)

type bind = ABind of string id_t * cexpr

let new_me_name =
  let open Common.Naming in
  let+ num = fresh in
  Id_name (with_pref anf_prefix @@ Int.to_string num)
;;

let const_to_immexpr =
  let open Common.Ast in
  function
  | Const_int v -> Imm_int v
  | Const_bool v -> Imm_bool v
  | Const_nil -> Imm_nil
  | Const_unit -> Imm_unit
;;

let id_to_immexpr v = Imm_id v

let optimize_cons_to_rlist : ll_expr -> ll_expr -> ll_expr list =
  fun head_pt tail_pt ->
  let rec helper acc = function
    | LL_list (e1, e2) -> helper (e1 :: acc) e2
    | end_pt -> end_pt :: acc
  in
  helper [ head_pt ] tail_pt
;;

let check_rlist_invariant = function
  | lst :: pref -> return (lst, pref)
  | _ -> fail "At least one elem should be (nil)"
;;

let get_global = function
  | Id_unit -> return None
  | Id_name id ->
    let+ nm_space = read in
    GlobalMap.find_opt id nm_space
;;

let save_global nm to_global =
  get_global nm
  >>= function
  | Some _ -> fail "Impossible case"
  | None ->
    (match nm with
     | Id_name nm ->
       let* nm_space = read in
       save @@ GlobalMap.add nm (to_global nm) nm_space
     | Id_unit -> return ())
;;

let to_scoped_common builder = function
  | Id_unit -> Id_unit
  | Id_name v -> Id_name (builder v)
;;

let to_local = to_scoped_common (fun x -> Local_id x)
let gfunc x = Global_func x
let gvar x = Global_var x

let to_scoped id =
  let+ cond = get_global id in
  match cond with
  | Some id -> Id_name id
  | None -> to_local id
;;

let rec to_immexpr : ll_expr -> (bind list * immexpr) t = function
  | LL_const v -> return ([], const_to_immexpr v)
  | LL_ident id ->
    let+ id' = to_scoped (Id_name id) in
    [], id_to_immexpr id'
  | expr ->
    let* id = new_me_name in
    let+ binds, cexpr = to_cexpr expr in
    let new_bind = ABind (id, cexpr) in
    binds @ [ new_bind ], id_to_immexpr @@ to_local id

and to_cexpr : ll_expr -> (bind list * cexpr) t =
  let open Base in
  function
  | LL_const v -> return ([], C_immexpr (const_to_immexpr v))
  | LL_ident id ->
    let+ id' = to_scoped (Id_name id) in
    [], C_immexpr (id_to_immexpr id')
  | LL_let (id, expr, in') ->
    let* e_binds, cexpr = to_cexpr expr in
    let+ in_binds, in'' = to_cexpr in' in
    let let_bind = ABind (id, cexpr) in
    e_binds @ [ let_bind ] @ in_binds, in''
  | LL_ifthenelse (i, t, e) ->
    let* i_binds, i_cexpr = to_immexpr i in
    let* t_aexpr = to_aexpr t in
    let+ e_aexpr = to_aexpr e in
    i_binds, C_ifthenelse (i_cexpr, t_aexpr, e_aexpr)
  | LL_tuple tup'l ->
    let+ binds_n_vals = mapt tup'l to_immexpr in
    let binds'l, values'l = List.unzip binds_n_vals in
    let tup_binds = List.concat binds'l in
    tup_binds, C_tuple values'l
  | LL_list (h, tl) ->
    let rexpr'l = optimize_cons_to_rlist h tl in
    let* lst, pref'l = check_rlist_invariant rexpr'l in
    let* lst_bind'l, lst_immexpr = to_immexpr lst in
    let+ pref_binds_n_vals = mapt pref'l to_immexpr in
    let binds'l, values'l = List.unzip pref_binds_n_vals in
    let list_binds =
      let all_binds = lst_bind'l :: binds'l in
      List.concat all_binds
    in
    list_binds, C_rlist (lst_immexpr, values'l)
  | LL_apply (e_fun, e_arg) ->
    let optimize_apply f arg =
      (* TODO: chech args passing order correctness *)
      let rec helper fst_arg args = function
        | LL_apply (f', new_fst) -> helper new_fst (fst_arg :: args) f'
        | name_e -> name_e, fst_arg, args
      in
      helper arg [] f
    in
    let extract_name_from_imm = function
      | Imm_id (Id_name id) -> return id
      | _ ->
        fail "Apply left part expects 0 argument(s), but is applied here to 1 argument(s)"
    in
    let name_e, e_fst_arg, e_arg'l = optimize_apply e_fun e_arg in
    let* name_bind'l, imm_name = to_immexpr name_e in
    let* name = extract_name_from_imm imm_name in
    let* fst_bind'l, imm_fst = to_immexpr e_fst_arg in
    let+ binds_n_args = mapt e_arg'l to_immexpr in
    let arg_binds'l, arg'l = List.unzip binds_n_args in
    let arg_bind'l = List.concat (List.rev arg_binds'l) in
    let new_binds = name_bind'l @ arg_bind'l @ fst_bind'l in
    new_binds, C_apply (name, imm_fst, arg'l)

and to_aexpr : ll_expr -> aexpr t =
  let alet id cexpr aexpr = A_let (id, cexpr, aexpr) in
  fun expr ->
    let+ binds, cexpr = to_cexpr expr in
    let basic_aexpr = A_cexpr cexpr in
    let particial_aexpr =
      List.fold_left
        (fun acc (ABind (id, cvalue)) next -> acc @@ alet id cvalue next)
        (fun x -> x)
        binds
    in
    particial_aexpr basic_aexpr
;;

let to_func { lldec_name; lldec_args; lldec_body } =
  let+ adec_body = to_aexpr lldec_body in
  { adec_name = lldec_name; adec_args = lldec_args; adec_body }
;;

let to_anf_decl = function
  | LL_GlobalV (id, expr) ->
    let* aexpr = to_aexpr expr in
    let+ () = save_global id gvar in
    A_GlobalV (id, aexpr)
  | LL_Decl (r_flag, ll_fun, ll_fun'l) ->
    (match r_flag, ll_fun'l with
     | Nonrecursive, [] ->
       let* anf_fun = to_func ll_fun in
       let+ () = save_global (Id_name ll_fun.lldec_name) gfunc in
       A_NonrecDecl anf_fun
     | Nonrecursive, _ -> fail "With [and] supports only rec funcs"
     | Recursive, ll_fun'l ->
       let* _ =
         mapt (ll_fun :: ll_fun'l) (fun { lldec_name; _ } ->
           save_global (Id_name lldec_name) gfunc)
       in
       let* anf_fun = to_func ll_fun in
       let+ anf_fun'l = mapt ll_fun'l to_func in
       A_RecDecl (anf_fun, anf_fun'l))
;;

let to_anf_prog ll_prog = mapt ll_prog to_anf_decl

let convert_to_anf prog =
  let open Common.Errors in
  let open Common.Base_lib in
  let global_name_space =
    let global_by_default = List.map LibF.get_name base_lib_decls in
    let helper acc name = GlobalMap.add name (gfunc name) acc in
    List.fold_left helper GlobalMap.empty global_by_default
  in
  match run (to_anf_prog prog) 0 global_name_space with
  | _, Ok anf_decl'l -> Result.Ok anf_decl'l
  | _, Error msg -> Result.Error (illegal_state msg)
;;
