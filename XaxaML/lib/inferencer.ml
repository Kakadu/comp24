(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Typedtree

type error =
  | Occurs_check
  | No_variable of string
  | Unification_failed of typ * typ
  | Multiple_bound of string
  | Recursive_binding
  | Impossible_state

let pp_error ppf : error -> unit = function
  | Occurs_check -> Format.fprintf ppf {|Occurs check failed|}
  | No_variable s -> Format.fprintf ppf {|Undefined variable %S|} s
  | Unification_failed (l, r) ->
    Format.fprintf ppf {|Unification failed on %a and %a|} pp_typ l pp_typ r
  | Multiple_bound s ->
    Format.fprintf ppf {|Variable %s is bound several times in matching|} s
  | Recursive_binding ->
    Format.printf {|Only variables are allowed as left-hand side of 'let rec'|}
  | Impossible_state -> Format.printf {|Inferencer run into impossible state|}
;;

open Common

module Type = struct
  let rec occurs_in v = function
    | T_var b -> b = v
    | T_arr (l, r) -> occurs_in v l || occurs_in v r
    | T_tuple (h, list) ->
      List.fold_left (fun occurs item -> occurs || occurs_in v item) (occurs_in v h) list
    | T_list t -> occurs_in v t
    | _ -> false
  ;;

  let type_vars =
    let rec helper acc = function
      | T_var b -> TypeVarSet.add b acc
      | T_arr (l, r) -> helper (helper acc l) r
      | T_tuple (h, list) -> List.fold_left helper (helper acc h) list
      | T_list t -> helper acc t
      | _ -> acc
    in
    helper TypeVarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : int -> typ -> (t, error) MonadCounterError.t
  val remove : t -> int -> t
  val apply : t -> typ -> typ
  val unify : typ -> typ -> (t, error) MonadCounterError.t
  val pp_subst : Format.formatter -> t -> unit
  val compose : t -> t -> (t, error) MonadCounterError.t
  val compose_all : t list -> (t, error) MonadCounterError.t
end = struct
  open MonadCounterError
  open Base

  type t = (int, typ, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let singleton k v =
    if Type.occurs_in k v
    then fail Occurs_check
    else return (Map.singleton (module Int) k v)
  ;;

  let find = Map.find
  let remove = Map.remove

  let apply sub =
    let rec helper = function
      | T_var tv as ty ->
        (match find sub tv with
         | None -> ty
         | Some x -> x)
      | T_arr (l, r) -> T_arr (helper l, helper r)
      | T_tuple (h, list) -> T_tuple (helper h, List.map list ~f:helper)
      | T_list t -> list_typ @@ helper t
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | T_prim l, T_prim r when String.equal l r -> return empty
    | T_var a, T_var b when Int.equal a b -> return empty
    | T_var b, t | t, T_var b -> singleton b t
    | T_arr (l1, r1), T_arr (l2, r2) ->
      let* sub1 = unify l1 l2 in
      let* sub2 = unify (apply sub1 r1) (apply sub1 r2) in
      compose sub1 sub2
    | T_tuple (h1, list1), T_tuple (h2, list2) ->
      let* unified =
        match List.map2 list1 list2 ~f:unify with
        | Unequal_lengths ->
          fail (Unification_failed (T_tuple (h1, list1), T_tuple (h2, list2)))
        | Ok res -> return res
      in
      List.fold_left unified ~init:(unify h1 h2) ~f:(fun acc s ->
        let* s = s in
        let* acc = acc in
        compose acc s)
    | T_list t1, T_list t2 -> unify t1 t2
    | _ -> fail (Unification_failed (l, r))

  and extend sub typ_var ty =
    match Map.find sub typ_var with
    | None ->
      let* new_sub = singleton typ_var (apply sub ty) in
      let upd ~key ~data acc =
        let* acc = acc in
        let ty = apply new_sub data in
        return (Map.update acc key ~f:(function _ -> ty))
      in
      Map.fold sub ~init:(return new_sub) ~f:upd
    | Some finded_type ->
      let* sub2 = unify ty finded_type in
      compose sub sub2

  and compose s1 s2 = RMap.fold s2 ~init:(return s1) ~f:extend

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose

  let pp_subst ppf sub =
    Base.Map.iteri sub ~f:(fun ~key ~data ->
      Stdlib.Format.fprintf ppf "[%d = %a] " key pp_typ_debug data)
  ;;
end

module Scheme = struct
  let free_vars (Scheme (bind_set, t)) = TypeVarSet.diff (Type.type_vars t) bind_set

  let apply sub (Scheme (bind_set, ty)) =
    let sub = TypeVarSet.fold (fun v s -> Subst.remove s v) bind_set sub in
    Scheme (bind_set, Subst.apply sub ty)
  ;;

  (* print scheme with binded vars *)
  let pp_scheme ppf (Scheme (binder_set, typ)) =
    if not (TypeVarSet.is_empty binder_set) then Format.fprintf ppf "forall";
    TypeVarSet.iter (fun n -> Format.fprintf ppf " %i" n) binder_set;
    if not (TypeVarSet.is_empty binder_set) then Format.fprintf ppf ". ";
    Format.fprintf ppf "%a" pp_typ_debug typ
  ;;
end

module TypeEnv : sig
  type t

  val empty : t
  val std : t
  val update : t -> string -> ?is_std:bool -> scheme -> t
  val remove : t -> string -> t
  val free_vars : t -> TypeVarSet.t
  val apply : Subst.t -> t -> t
  val find : t -> string -> scheme option
  val union : t -> t -> t
  val pp_env : Format.formatter -> t -> unit
  val pp_env_debug : Format.formatter -> t -> unit
end = struct
  open Base

  type t = (string, bool * scheme, String.comparator_witness) Map.t

  let update mp k ?(is_std = false) v = Map.update mp k ~f:(function _ -> is_std, v)
  let empty = Map.empty (module String)
  let add_to_std env name scheme = update env name ~is_std:true scheme

  (** Environment with supported functions and operators *)
  let std =
    let init_env = empty in
    let empty_bind = TypeVarSet.empty in
    let single_bind = TypeVarSet.singleton in
    let init_env =
      add_to_std init_env "print_int" (Scheme (empty_bind, int_typ @-> unit_typ))
    in
    let init_env =
      add_to_std init_env "print_bool" (Scheme (empty_bind, bool_typ @-> unit_typ))
    in
    let init_env = add_to_std init_env "~+" (Scheme (empty_bind, int_typ @-> int_typ)) in
    let init_env = add_to_std init_env "~-" (Scheme (empty_bind, int_typ @-> int_typ)) in
    let init_env =
      add_to_std init_env "*" (Scheme (empty_bind, int_typ @-> int_typ @-> int_typ))
    in
    let init_env =
      add_to_std init_env "/" (Scheme (empty_bind, int_typ @-> int_typ @-> int_typ))
    in
    let init_env =
      add_to_std init_env "+" (Scheme (empty_bind, int_typ @-> int_typ @-> int_typ))
    in
    let init_env =
      add_to_std init_env "-" (Scheme (empty_bind, int_typ @-> int_typ @-> int_typ))
    in
    let init_env =
      add_to_std init_env "&&" (Scheme (empty_bind, bool_typ @-> bool_typ @-> bool_typ))
    in
    let init_env =
      add_to_std init_env "||" (Scheme (empty_bind, bool_typ @-> bool_typ @-> bool_typ))
    in
    let init_env =
      add_to_std init_env ">" (Scheme (empty_bind, int_typ @-> int_typ @-> bool_typ))
    in
    let init_env =
      add_to_std init_env "<" (Scheme (empty_bind, int_typ @-> int_typ @-> bool_typ))
    in
    let init_env =
      add_to_std init_env "<=" (Scheme (empty_bind, int_typ @-> int_typ @-> bool_typ))
    in
    let init_env =
      add_to_std init_env ">=" (Scheme (empty_bind, int_typ @-> int_typ @-> bool_typ))
    in
    let init_env =
      add_to_std
        init_env
        "="
        (Scheme (single_bind 0, type_var 0 @-> type_var 0 @-> bool_typ))
    in
    let init_env =
      add_to_std
        init_env
        "<>"
        (Scheme (single_bind 1, type_var 1 @-> type_var 1 @-> bool_typ))
    in
    let init_env =
      add_to_std
        init_env
        "=="
        (Scheme (single_bind 2, type_var 2 @-> type_var 2 @-> bool_typ))
    in
    let init_env =
      add_to_std
        init_env
        "!="
        (Scheme (single_bind 3, type_var 3 @-> type_var 3 @-> bool_typ))
    in
    let init_env =
      add_to_std
        init_env
        "#list_hd"
        (Scheme (single_bind 4, list_typ (type_var 4) @-> type_var 4))
    in
    let init_env =
      add_to_std
        init_env
        "#list_tl"
        (Scheme (single_bind 5, list_typ (type_var 5) @-> list_typ (type_var 5)))
    in
    let init_env =
      add_to_std
        init_env
        "#list_length"
        (Scheme (single_bind 6, list_typ (type_var 6) @-> int_typ))
    in
    let init_env =
      (* Unfortunately, we cannot create more specific type for this function,
         because its actual type will depend on the number of elements in the tuple *)
      let type_var_set = TypeVarSet.add 8 (single_bind 7) in
      add_to_std
        init_env
        "#unpack_tuple"
        (Scheme (type_var_set, type_var 7 @-> int_typ @-> type_var 8))
    in
    let init_env =
      add_to_std init_env "#match_failure" (Scheme (single_bind 9, type_var 9))
    in
    init_env
  ;;

  let remove = Map.remove

  let find mp k =
    match Map.find mp k with
    | Some (_, scheme) -> Some scheme
    | None -> None
  ;;

  let free_vars env =
    Map.fold env ~init:TypeVarSet.empty ~f:(fun ~key:_ ~data acc ->
      TypeVarSet.union acc (Scheme.free_vars (snd data)))
  ;;

  let apply sub env = Map.map env ~f:(fun (flag, scheme) -> flag, Scheme.apply sub scheme)

  let union env1 env2 =
    Map.fold env2 ~init:env1 ~f:(fun ~key ~data:(is_std, scheme) acc ->
      update acc key ~is_std scheme)
  ;;

  let pp_env ppf env =
    let values_to_print = Map.filter env ~f:(fun (is_std, _) -> not is_std) in
    Map.iteri values_to_print ~f:(fun ~key ~data:(_, Scheme (_, typ)) ->
      Stdlib.Format.fprintf ppf "val %s : %a\n" key pp_typ typ)
  ;;

  let pp_env_debug ppf env =
    let values_to_print = Map.filter env ~f:(fun (is_std, _) -> not is_std) in
    Map.iteri values_to_print ~f:(fun ~key ~data:(_, s) ->
      Stdlib.Format.fprintf ppf "val %s : %a\n" key Scheme.pp_scheme s)
  ;;
end

open MonadCounterError

let fresh_var = fresh >>| fun n -> T_var n

let instantiate (Scheme (bind_set, ty)) =
  TypeVarSet.fold
    (fun cur_var acc_typ ->
      let* acc_typ = acc_typ in
      let* f1 = fresh_var in
      let* s = Subst.singleton cur_var f1 in
      return (Subst.apply s acc_typ))
    bind_set
    (return ty)
;;

(* creating a scheme out of a type *)
let generalize env ty =
  let free = TypeVarSet.diff (Type.type_vars ty) (TypeEnv.free_vars env) in
  Scheme (free, ty)
;;

(* creating a scheme out of a type for recursion functions *)
let generalize_rec env ty exept =
  let env = List.fold_left TypeEnv.remove env exept in
  let free = TypeVarSet.diff (Type.type_vars ty) (TypeEnv.free_vars env) in
  Scheme (free, ty)
;;

let convert_raw_typ raw_typ =
  let rec helper names = function
    | Ast.RT_prim str -> return (names, T_prim str)
    | Ast.RT_var str ->
      (match Base.Map.find names str with
       | Some typ -> return (names, typ)
       | None ->
         fresh_var
         >>| fun fresh ->
         let names = Base.Map.add_exn names ~key:str ~data:fresh in
         names, fresh)
    | Ast.RT_arr (l, r) ->
      let* names, l_typ = helper names l in
      let* names, r_typ = helper names r in
      return (names, T_arr (l_typ, r_typ))
    | Ast.RT_list t ->
      let* names, typ = helper names t in
      return (names, list_typ typ)
    | Ast.RT_tuple (h, tl) ->
      let rev_t = List.rev (h :: tl) in
      let last, other = List.hd rev_t, List.tl rev_t in
      let* init_names, init_typ = helper names last in
      RList.fold_right
        other
        ~init:(return (init_names, init_typ))
        ~f:(fun cur_t (names, typ1) ->
          let* names, typ2 = helper names cur_t in
          return (names, typ1 @-> typ2))
  in
  let* _, typ = helper (Base.Map.empty (module Base.String)) raw_typ in
  return typ
;;

let infer_const = function
  | Ast.C_int _ -> return int_typ
  | Ast.C_bool _ -> return bool_typ
  | Ast.C_empty_list ->
    let* fresh = fresh_var in
    return (list_typ fresh)
  | Ast.C_unit -> return unit_typ
;;

let rec infer_pattern env = function
  | Ast.P_typed (pat, fixed_typ) ->
    let* sub, typ, env = infer_pattern env pat in
    let* fixed_typ = convert_raw_typ fixed_typ in
    let* sub_uni = Subst.unify fixed_typ typ in
    let new_typ = Subst.apply sub_uni typ in
    let* final_sub = Subst.compose sub sub_uni in
    return (final_sub, new_typ, env)
  | Ast.P_val name ->
    if Base.String.equal name "()"
    then infer_pattern env (P_const C_unit)
    else
      let* fresh = fresh_var in
      (match TypeEnv.find env name with
       | Some _ -> fail (Multiple_bound name)
       | None ->
         let sheme = Scheme (TypeVarSet.empty, fresh) in
         let env = TypeEnv.update env name sheme in
         return (Subst.empty, fresh, env))
  | Ast.P_any ->
    let* fresh = fresh_var in
    return (Subst.empty, fresh, env)
  | Ast.P_const c ->
    let* const_typ = infer_const c in
    return (Subst.empty, const_typ, env)
  | Ast.P_cons_list (l1, r1) ->
    let* sub1, typ1, env1 = infer_pattern env l1 in
    let* sub2, typ2, env2 = infer_pattern (TypeEnv.apply sub1 env1) r1 in
    let* fresh = fresh_var in
    let* sub_uni = Subst.unify typ2 (list_typ fresh) in
    let typ2 = Subst.apply sub_uni typ2 in
    let* sub3 = Subst.unify (list_typ typ1) typ2 in
    let* final_sub = Subst.compose_all [ sub1; sub2; sub3; sub_uni ] in
    return (final_sub, Subst.apply sub3 typ2, env2)
  | Ast.P_tuple (h, list) ->
    let* sub1, typ1, env1 = infer_pattern env h in
    let f1 pat (sub_prev, l, env) =
      let* sub_cur, arg, env = infer_pattern env pat in
      let* sub = Subst.compose sub_prev sub_cur in
      return (sub, arg :: l, env)
    in
    let* sub, arg, env = RList.fold_right list ~init:(return (sub1, [], env1)) ~f:f1 in
    return (sub, T_tuple (typ1, arg), env)
;;

(* extend environment with values names from pattern *)
let rec generalize_vars_in_pattern typ env = function
  | Ast.P_typed (pat, _) -> generalize_vars_in_pattern typ env pat
  | Ast.P_any | Ast.P_const _ -> return env
  | Ast.P_val name ->
    if Base.String.equal name "()"
    then generalize_vars_in_pattern typ env (P_const C_unit)
    else (
      let gen_scheme = generalize env typ in
      return @@ TypeEnv.update env name gen_scheme)
  | P_tuple (l, r) ->
    (match typ with
     | T_tuple (typ_l, typ_list) ->
       let* env1 = generalize_vars_in_pattern typ_l env l in
       List.fold_left2
         (fun last_env cur_typ cur_pat ->
           let* last_env = last_env in
           let* cur_env = generalize_vars_in_pattern cur_typ last_env cur_pat in
           return cur_env)
         (return env1)
         typ_list
         r
     | _ -> fail Impossible_state)
  | P_cons_list (l, r) ->
    (match typ with
     | T_list inside_list ->
       let* env1 = generalize_vars_in_pattern inside_list env l in
       let* env2 = generalize_vars_in_pattern typ env r in
       return @@ TypeEnv.union env1 env2
     | _ -> fail Impossible_state)
;;

let check_typ fixed inferred =
  let* fixed_typ = convert_raw_typ fixed in
  let* sub_uni = Subst.unify inferred fixed_typ in
  let final_typ = Subst.apply sub_uni inferred in
  return (sub_uni, final_typ)
;;

let rec infer_expr env expr =
  let open Ast in
  match expr with
  | E_typed (expr, fixed_typ) ->
    let* sub, typ = infer_expr env expr in
    let* fixed_typ = convert_raw_typ fixed_typ in
    let* sub_uni = Subst.unify fixed_typ typ in
    let new_typ = Subst.apply sub_uni typ in
    let* final_sub = Subst.compose sub sub_uni in
    return (final_sub, new_typ)
  | E_const c ->
    let* ty = infer_const c in
    return (Subst.empty, ty)
  | E_ident name ->
    (match TypeEnv.find env name with
     | Some scheme ->
       let* ans = instantiate scheme in
       return (Subst.empty, ans)
     | None -> fail @@ No_variable name)
  | E_fun (first_pat, other_pats, e) ->
    let all_args = first_pat :: other_pats in
    let helper prev cur_pat =
      let* prev_sub, prev_typ_list, prev_env = prev in
      let* cur_sub, cur_typ, cur_env = infer_pattern TypeEnv.empty cur_pat in
      let next_env = TypeEnv.union prev_env cur_env in
      let* next_sub = Subst.compose prev_sub cur_sub in
      let next_typ_list = cur_typ :: prev_typ_list in
      return (next_sub, next_typ_list, next_env)
    in
    let* args_sub, args_types, args_env =
      List.fold_left helper (return (Subst.empty, [], TypeEnv.empty)) all_args
    in
    let args_types = List.rev args_types in
    let env = TypeEnv.union env args_env in
    let env = TypeEnv.apply args_sub env in
    let* expr_sub, expr_typ = infer_expr env e in
    let* final_sub = Subst.compose expr_sub args_sub in
    let args_types = List.map (Subst.apply expr_sub) args_types in
    let fun_type = List.fold_right arrow args_types expr_typ in
    return (final_sub, fun_type)
  | E_app (e1, e2) -> infer_app env e1 e2
  | E_let (Non_rec (pattern, fixed_typ, e1), e2) ->
    let* sub1, typ1 = infer_expr env e1 in
    let env = TypeEnv.apply sub1 env in
    let* sub_pattern, type_pattern, _ = infer_pattern TypeEnv.empty pattern in
    let* sub_uni = Subst.unify typ1 type_pattern in
    let* sub_uni = Subst.compose sub_pattern sub_uni in
    let env = TypeEnv.apply sub_uni env in
    let typ1 = Subst.apply sub_uni typ1 in
    let* env = generalize_vars_in_pattern typ1 env pattern in
    let* sub2, typ2 = infer_expr env e2 in
    let* final_sub = Subst.compose_all [ sub1; sub_uni; sub2 ] in
    (match fixed_typ with
     | Some fixed_typ ->
       let* sub_uni, final_typ = check_typ fixed_typ typ2 in
       let* final_sub = Subst.compose sub_uni final_sub in
       return (final_sub, final_typ)
     | None -> return (final_sub, typ2))
  | E_let (Rec decl_list, e2) ->
    let* sub1, env, _types = infer_rec env decl_list in
    let* sub2, typ2 = infer_expr env e2 in
    let* final_sub = Subst.compose sub1 sub2 in
    return (final_sub, typ2)
  | E_ite (e1, e2, e3) ->
    let* sub1, typ1 = infer_expr env e1 in
    let env = TypeEnv.apply sub1 env in
    let* sub2, typ2 = infer_expr env e2 in
    let env = TypeEnv.apply sub2 env in
    let* sub3, typ3 = infer_expr env e3 in
    let* sub_cond = Subst.unify typ1 bool_typ in
    let* sub_branches = Subst.unify typ2 typ3 in
    let* final_sub = Subst.compose_all [ sub1; sub2; sub3; sub_cond; sub_branches ] in
    return (final_sub, Subst.apply sub_branches typ2)
  | E_tuple (expr1, list) ->
    let* sub1, typ1 = infer_expr env expr1 in
    let env = TypeEnv.apply sub1 env in
    let f1 prev cur_expr =
      let* prev_env, prev_subst, prev_typ_list = prev in
      let* cur_subst, cur_typ = infer_expr prev_env cur_expr in
      let* next_subst = Subst.compose prev_subst cur_subst in
      let next_env = TypeEnv.apply next_subst prev_env in
      return (next_env, next_subst, cur_typ :: prev_typ_list)
    in
    let* _, final_sub, typ_list = List.fold_left f1 (return (env, sub1, [])) list in
    let typ_list =
      List.map (fun item -> Subst.apply final_sub item) (List.rev typ_list)
    in
    return (final_sub, T_tuple (Subst.apply final_sub typ1, typ_list))
  | E_cons_list (e1, e2) ->
    let* sub1, typ1 = infer_expr env e1 in
    let* sub2, typ2 = infer_expr (TypeEnv.apply sub1 env) e2 in
    let* sub3 = Subst.unify (list_typ typ1) typ2 in
    let* final_sub = Subst.compose_all [ sub1; sub2; sub3 ] in
    return (final_sub, Subst.apply sub3 typ2)
  | E_match (e, list) ->
    let* sub, arg_typ = infer_expr env e in
    let* ret_typ = fresh_var in
    let env = TypeEnv.apply sub env in
    let f1 (cur_pat, cur_expr) (last_sub, last_pat_typ, last_ret_typ) =
      let* last_sub = last_sub in
      let* cur_sub, cur_arg_typ, new_env = infer_pattern TypeEnv.empty cur_pat in
      let env = TypeEnv.union env new_env in
      let* sub_ret, cur_ret_typ = infer_expr (TypeEnv.apply cur_sub env) cur_expr in
      let cur_arg_typ = Subst.apply sub_ret cur_arg_typ in
      let* sub_uni1 = Subst.unify cur_arg_typ last_pat_typ in
      let* sub_uni2 = Subst.unify cur_ret_typ last_ret_typ in
      return
        ( Subst.compose_all [ last_sub; cur_sub; sub_ret; sub_uni1; sub_uni2 ]
        , Subst.apply sub_uni1 cur_arg_typ
        , Subst.apply sub_uni2 cur_ret_typ )
    in
    let* sub, _, ret_typ =
      RList.fold_right list ~init:(return (return sub, arg_typ, ret_typ)) ~f:f1
    in
    let* sub = sub in
    return (sub, Subst.apply sub ret_typ)

and infer_app env e1 e2 =
  let* return_type = fresh_var in
  let* sub1, typ1 = infer_expr env e1 in
  let* sub2, typ2 = infer_expr (TypeEnv.apply sub1 env) e2 in
  let typ1 = Subst.apply sub2 typ1 in
  let* sub3 = Subst.unify typ1 (typ2 @-> return_type) in
  let* final_sub = Subst.compose_all [ sub1; sub2; sub3 ] in
  return (final_sub, Subst.apply sub3 return_type)

and infer_rec env decl_list =
  let filtered =
    List.map
      (fun (pat, rt, e) ->
        match pat with
        | Ast.P_val name -> return (name, rt, e)
        | _ -> fail Recursive_binding)
      decl_list
  in
  let names =
    List.filter_map
      (fun (pat, _, _) ->
        match pat with
        | Ast.P_val name -> Some name
        | _ -> None)
      decl_list
  in
  (* Add names to environment *)
  let f1 (prev_types, prev_env) cur_decl =
    let* name, _, _ = cur_decl in
    let* ty = fresh_var in
    let cur_env = TypeEnv.update prev_env name (Scheme (TypeVarSet.empty, ty)) in
    return (ty :: prev_types, cur_env)
  in
  let* types, env = RList.fold_left filtered ~init:(return ([], env)) ~f:f1 in
  let types = List.rev types in
  let typed_decl =
    List.map2
      (fun decl typ ->
        let* _, _, e = decl in
        return (e, typ))
      filtered
      types
  in
  (* Infer expressions *)
  let f1 (prev_sub, prev_env, prev_types) cur_decl =
    let* cur_expr, typ = cur_decl in
    let* sub1, expr_type = infer_expr prev_env cur_expr in
    let* sub2 = Subst.unify expr_type typ in
    let* sub3 = Subst.compose sub1 sub2 in
    let cur_env = TypeEnv.apply sub3 prev_env in
    let* final_sub = Subst.compose sub3 prev_sub in
    return (final_sub, cur_env, typ :: prev_types)
  in
  let* sub, env, types =
    RList.fold_left typed_decl ~init:(return (Subst.empty, env, [])) ~f:f1
  in
  let types = List.rev types in
  let typed_names =
    List.map2
      (fun decl typ ->
        let* name, _, _ = decl in
        return (name, typ))
      filtered
      types
  in
  (* Apply substitution for types and generalize types *)
  let f1 prev_env cur_decl =
    let* name, cur_type = cur_decl in
    let cur_type = Subst.apply sub cur_type in
    let gen_scheme = generalize_rec prev_env cur_type names in
    let cur_env = TypeEnv.update prev_env name gen_scheme in
    return cur_env
  in
  let* env = RList.fold_left typed_names ~init:(return env) ~f:f1 in
  (* Check fixed types *)
  let fixed_inferred =
    List.map2
      (fun decl typ ->
        let* _, fixed, _ = decl in
        return (fixed, typ))
      filtered
      types
  in
  let f1 (prev_sub, prev_types) t =
    let* fixed, inferred = t in
    match fixed with
    | Some fixed ->
      let* cur_sub, cur_typ = check_typ fixed inferred in
      let* next_sub = Subst.compose prev_sub cur_sub in
      return (next_sub, cur_typ :: prev_types)
    | None -> return (prev_sub, inferred :: prev_types)
  in
  let* sub_uni, types =
    RList.fold_left fixed_inferred ~init:(return (Subst.empty, [])) ~f:f1
  in
  let types = List.rev types in
  let* final_sub = Subst.compose sub sub_uni in
  return (final_sub, env, types)

and infer_toplevel env = function
  | Ast.Let_decl (Non_rec (pattern, fixed_typ, expr)) ->
    let* sub, ty = infer_expr env expr in
    let env = TypeEnv.apply sub env in
    let* sub_pattern, pattern_type, _ = infer_pattern TypeEnv.empty pattern in
    let* sub_uni = Subst.unify ty pattern_type in
    let* sub_uni = Subst.compose sub_pattern sub_uni in
    let env = TypeEnv.apply sub_uni env in
    let ty = Subst.apply sub_uni ty in
    let* env = generalize_vars_in_pattern ty env pattern in
    (match fixed_typ with
     | Some fixed_typ ->
       let* sub_uni, _ = check_typ fixed_typ ty in
       let env = TypeEnv.apply sub_uni env in
       return env
     | None -> return env)
  | Ast.Let_decl (Rec decl_list) ->
    let* _, env, _types = infer_rec env decl_list in
    return env
  | Ast.Expr expr ->
    let* _ = infer_expr env expr in
    return env
;;

let infer_program prog =
  let rec helper env = function
    | [] -> return env
    | h :: tl ->
      let* new_env = infer_toplevel env h in
      let* env = helper new_env tl in
      return env
  in
  helper TypeEnv.std prog
;;

let run_infer_expr e =
  Result.map snd (run Std_names.type_var_count (infer_expr TypeEnv.std e))
;;

let run_infer_program p = run Std_names.type_var_count (infer_program p)
