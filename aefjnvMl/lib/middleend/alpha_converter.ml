(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module StringAlphfaconverterMonad = struct
  type ident = string

  module Name_id = struct
    type t = ident

    let compare = compare
  end

  module Banned_Set = Stdlib.Set.Make (Name_id)
  module Bind_Map = Stdlib.Map.Make (Name_id)

  type fresh_id = int
  type bind_space = ident Bind_Map.t
  type ban_set = Banned_Set.t
  type reserved_re_prefs = Str.regexp list
  type ban_rules = reserved_re_prefs * ban_set
  type name_space = ban_rules * bind_space * fresh_id * ident

  include Common.Se_monad.Base_SE_Monad

  type 'a t = (name_space, 'a, string) Common.Se_monad.Base_SE_Monad.t

  let num_prefix current_prefix id = current_prefix ^ Int.to_string id
  let create_fresh_id id = id + 1

  let binding_scope : 'a t -> 'a t =
    fun f ->
    let* _, old_bindings, _, _ = read in
    let* f_res = f in
    let* b_rules, _, f_id, pref = read in
    let* () = save (b_rules, old_bindings, f_id, pref) in
    return f_res
  ;;

  let fresh_bind pre_formatter name =
    let apply_prefix pref name_ = pref ^ "_" ^ name_ in
    let get_uniq_name cur_pref new_name old_fresh_id b_set =
      let rec helper name_ id_ =
        match Banned_Set.find_opt name_ b_set with
        | None -> id_, name_
        | Some _ ->
          let supported_name = apply_prefix (num_prefix cur_pref id_) new_name in
          let fresh_id = create_fresh_id id_ in
          helper supported_name fresh_id
      in
      helper new_name old_fresh_id
    in
    let add_binding_in_ban_set orig_name new_name b_set =
      let b_set' = Banned_Set.add orig_name b_set in
      Banned_Set.add new_name b_set'
    in
    let* name', st' =
      let* (b_prefs, b_set), bindings, fresh_id, a_pref = read in
      let should_rename_via_prefs =
        Base.List.fold_left b_prefs ~init:false ~f:(fun acc pref ->
          match acc with
          | true -> true
          | false -> Str.string_match pref name 0)
      in
      let id', name'' =
        let start_name =
          let prepared_name = pre_formatter name in
          match should_rename_via_prefs with
          | true -> apply_prefix a_pref prepared_name
          | false -> prepared_name
        in
        get_uniq_name a_pref start_name fresh_id b_set
      in
      let bindings' = Bind_Map.add name name'' bindings in
      let b_set' = add_binding_in_ban_set name name'' b_set in
      let st'' = (b_prefs, b_set'), bindings', id', a_pref in
      return (name'', st'')
    in
    let* () = save st' in
    return name'
  ;;

  let get_bind name =
    let* _, binds, _, _ = read in
    match Bind_Map.find_opt name binds with
    | Some name' -> return name'
    | None -> fail ("=doesn't find binded value -- " ^ name)
  ;;
end

open StringAlphfaconverterMonad
open Common.Ast
open Common.Ast_construct

let fresh_bind = fresh_bind Common.Base_lib.converte_infix

type core_type_flag =
  | On
  | Off

let ct_flag = Off

let pt_with_ct_flag pt' ct =
  match ct_flag with
  | On -> return @@ pconstraint pt' ct
  | Off -> return pt'
;;

let e_with_ct_flag e' ct =
  match ct_flag with
  | On -> return @@ etype e' ct
  | Off -> return e'
;;

let rec aconvert_pattern = function
  | Pat_const c -> return @@ pconst c
  | Pat_cons (a, b) ->
    let* a' = aconvert_pattern a in
    let* b' = aconvert_pattern b in
    return @@ pcons a' b'
  | Pat_any -> return pany
  | Pat_tuple tup ->
    let* tup' =
      revt
      @@ fold_left_t tup ~init:(return []) ~f:(fun acc pt ->
        let* pt' = aconvert_pattern pt in
        return @@ (pt' :: acc))
    in
    return @@ ptuple tup'
  | Pat_constraint (pt, ct) ->
    let* pt' = aconvert_pattern pt in
    pt_with_ct_flag pt' ct
  | Pat_var name -> fresh_bind name >>| pvar
;;

let aconvert_decl_part aconv_expr = function
  | Decl (rf, vbl) ->
    (match rf with
     | Nonrecursive ->
       let* { vb_pat; vb_expr } =
         match vbl with
         | vb :: [] -> return vb
         | _ -> fail "Impossible case with nonrec flag"
       in
       let* vb_expr' = aconv_expr vb_expr in
       let* vb_pat' = aconvert_pattern vb_pat in
       return @@ edecl rf [ { vb_pat = vb_pat'; vb_expr = vb_expr' } ]
     | Recursive ->
       let vbl_fold_l f = fold_left_t vbl ~init:(return []) ~f in
       let* vb_ptl' =
         revt
         @@ vbl_fold_l (fun acc { vb_pat; _ } ->
           let* vb_pat' = aconvert_pattern vb_pat in
           return @@ (vb_pat' :: acc))
       in
       let* vb_exprl' =
         revt
         @@ vbl_fold_l (fun acc { vb_expr; _ } ->
           let* vb_expr' = aconv_expr vb_expr in
           return @@ (vb_expr' :: acc))
       in
       let vbl' = List.map2 evalue_binding vb_ptl' vb_exprl' in
       return @@ edecl rf vbl')
;;

let rec aconvert_expression = function
  | Exp_type (e, ct) ->
    let* e' = aconvert_expression e in
    e_with_ct_flag e' ct
  | Exp_ident name ->
    let* name' = get_bind name in
    return @@ eval name'
  | Exp_apply (a, b) ->
    let* a' = aconvert_expression a in
    let* b' = aconvert_expression b in
    return @@ eapp a' b'
  | Exp_list (a, b) ->
    let* a' = aconvert_expression a in
    let* b' = aconvert_expression b in
    return @@ econs a' b'
  | Exp_ifthenelse (b, t, e) ->
    let* b' = aconvert_expression b in
    let* t' = aconvert_expression t in
    let* e' = aconvert_expression e in
    return @@ eite b' t' e'
  | Exp_tuple etup ->
    let* tup' =
      revt
      @@ fold_left_t etup ~init:(return []) ~f:(fun acc e ->
        let* e' = aconvert_expression e in
        return @@ (e' :: acc))
    in
    return @@ etuple tup'
  | Exp_constant c -> return @@ econst c
  | Exp_function (pt, e) ->
    binding_scope
    @@
    let* pt' = aconvert_pattern pt in
    let* e' = aconvert_expression e in
    return @@ efun pt' e'
  | Exp_match (e, ptNel) ->
    let* e' = aconvert_expression e in
    let* ptNel' =
      revt
      @@ fold_left_t ptNel ~init:(return []) ~f:(fun acc (pt_, e_) ->
        binding_scope
        @@
        let* pt'_ = aconvert_pattern pt_ in
        let* e'_ = aconvert_expression e_ in
        return ((pt'_, e'_) :: acc))
    in
    return @@ ematch e' ptNel'
  | Exp_let (d, e) ->
    binding_scope
    @@
    let* d' = aconvert_decl_part aconvert_expression d in
    let* e' = aconvert_expression e in
    return @@ elet d' e'
;;

let aconvert_decl = aconvert_decl_part aconvert_expression

let aconvert_structure_item = function
  | Str_eval e -> aconvert_expression e >>| streval
  | Str_value d -> aconvert_decl d >>| strval
;;

let aconvert_program prog =
  revt
  @@ fold_left_t prog ~init:(return []) ~f:(fun acc sti ->
    let* sti' = aconvert_structure_item sti in
    return @@ (sti' :: acc))
;;

let rename_ast_with_uniq step_pref prog =
  let open Common.Naming in
  let open Common.Base_lib in
  let name_space =
    let ban_rules =
      let ban_set =
        let ban_names = forbidden_names @ std_lib_names in
        let helper acc name = Banned_Set.add name acc in
        List.fold_left helper Banned_Set.empty ban_names
      in
      let res_prefs = List.map Str.regexp reserved_prefs in
      res_prefs, ban_set
    in
    let bind_space =
      let helper acc name = Bind_Map.add name name acc in
      List.fold_left helper Bind_Map.empty std_lib_names
    in
    let fresh_id = 0 in
    ban_rules, bind_space, fresh_id, step_pref
  in
  let prog_alpha_converter = aconvert_program prog in
  let open Common.Errors in
  match run prog_alpha_converter name_space with
  | _, Ok prog' -> Result.Ok prog'
  | _, Error msg -> Result.Error (illegal_state msg)
;;
