(** Copyright 2024-2025, Dmitrii Kosarev, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** ref: https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml/inferencer.ml **)

open Ast
open Typing

module R = struct
  open Base.Result

  type 'a t = string -> string * ('a, error) Result.t

  let ( >>= ) monad f state =
    let last, result = monad state in
    match result with
    | Error e -> last, Error e
    | Ok value -> f value last
  ;;

  let fail error state = state, fail error
  let return value last = last, return value

  module Syntax = struct
    let ( let* ) x f = x >>= f
  end

  module RList = struct
    let fold_left map ~init ~f =
      Base.Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh last =
    let fresh_int = int_of_string last in
    string_of_int (fresh_int + 1), Ok last
  ;;

  let run monad = snd (monad "0")
end

type fresh = string

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArr (l, r) -> occurs_in v l || occurs_in v r
    | TTuple typ_list -> Base.List.exists typ_list ~f:(occurs_in v)
    | TList typ -> occurs_in v typ
    | TGround _ -> false
  ;;

  let free_vars =
    let empty = Base.Set.empty (module Base.String) in
    let rec helper acc = function
      | TVar n -> Base.Set.add acc n
      | TArr (left, right) -> helper (helper acc left) right
      | TTuple typ_list ->
        Base.List.fold_right
          typ_list
          ~f:(fun typ acc -> Base.Set.union (helper empty typ) acc)
          ~init:acc
      | TList typ -> helper acc typ
      | TGround _ -> acc
    in
    helper empty
  ;;
end

module Subst = struct
  open R
  open R.Syntax

  type t = (fresh, typ, Base.String.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.String)

  let mapping key value =
    if Type.occurs_in key value then fail OccursCheck else return (key, value)
  ;;

  let singleton key value =
    let* key, value = mapping key value in
    return @@ Base.Map.update empty key ~f:(fun _ -> value)
  ;;

  let find key subst = Base.Map.find subst key
  let remove subst key = Base.Map.remove subst key

  let apply s =
    let rec helper = function
      | TVar n ->
        (match find n s with
         | None -> tvar n
         | Some x -> x)
      | TArr (left, right) -> tarrow (helper left) (helper right)
      | TTuple typ_list -> ttuple @@ Base.List.map typ_list ~f:helper
      | TList typ -> tlist @@ helper typ
      | ground -> ground
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TGround l, TGround r when l = r -> return empty
    | TVar a, TVar b when a = b -> return empty
    | TVar b, typ | typ, TVar b -> singleton b typ
    | TArr (f1, s1), TArr (f2, s2) ->
      let* subst1 = unify f1 f2 in
      let* subst2 = unify (apply subst1 s1) (apply subst1 s2) in
      compose subst1 subst2
    | TTuple typ_list_l, TTuple typ_list_r ->
      (match Base.List.zip typ_list_l typ_list_r with
       | Base.List.Or_unequal_lengths.Unequal_lengths -> fail (UnificationFailed (l, r))
       | Base.List.Or_unequal_lengths.Ok zipped_list ->
         Base.List.fold_right
           zipped_list
           ~f:(fun (typ_l, typ_r) subst ->
             let* subst_pair = unify typ_l typ_r in
             let* subst = subst in
             compose subst_pair subst)
           ~init:(return empty))
    | TList typ1, TList typ2 -> unify typ1 typ2
    | _ -> fail (UnificationFailed (l, r))

  and extend key value subst =
    match find key subst with
    | None ->
      let value = apply subst value in
      let* s2 = singleton key value in
      RList.fold_left
        subst
        ~f:(fun key value acc ->
          let value = apply s2 value in
          let* key, value = mapping key value in
          return @@ Base.Map.update acc key ~f:(fun _ -> value))
        ~init:(return s2)
    | Some v2 ->
      let* s2 = unify value v2 in
      compose subst s2

  and compose s1 s2 = RList.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all substs =
    Base.List.fold_left substs ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose acc subst)
  ;;
end

type scheme = (type_variable_number, Base.String.comparator_witness) Base.Set.t * typ

module Scheme = struct
  type t = scheme

  let empty = Base.Set.empty (module Base.String)
  let occurs_in key (set, typ) = (not (Base.Set.mem set key)) && Type.occurs_in key typ
  let free_vars (set, typ) = Base.Set.diff set (Type.free_vars typ)

  let apply subst (set, typ) =
    let s2 = Base.Set.fold set ~init:subst ~f:(fun acc k -> Subst.remove acc k) in
    set, Subst.apply s2 typ
  ;;
end

module TypeEnv = struct
  type t = (ident, scheme, Base.String.comparator_witness) Base.Map.t

  let extend env id scheme = Base.Map.update env id ~f:(fun _ -> scheme)
  let empty = Base.Map.empty (module Base.String)

  let free_vars : t -> (type_variable_number, Base.String.comparator_witness) Base.Set.t =
    Base.Map.fold
      ~init:(Base.Set.empty (module Base.String))
      ~f:(fun ~key:_ ~data acc -> Base.Set.union acc (Scheme.free_vars data))
  ;;

  let apply s env = Base.Map.map env ~f:(Scheme.apply s)
end

open R
open R.Syntax

let unify = Subst.unify

let fresh_var =
  let* fresh = fresh in
  return @@ tvar fresh
;;

let instantiate =
  let fold_right set init f =
    Base.Set.fold_right set ~init ~f:(fun x acc ->
      let* acc = acc in
      f acc x)
  in
  fun (set, typ) ->
    fold_right set (return typ) (fun typ name ->
      let* fresh_var = fresh_var in
      let* subst = Subst.singleton name fresh_var in
      return @@ Subst.apply subst typ)
;;

let generalize env typ =
  let free = Base.Set.diff (Type.free_vars typ) (TypeEnv.free_vars env) in
  free, typ
;;

let lookup_env id map =
  match Base.Map.find map id with
  | None -> fail (UnboundValue id)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let rec convert_type = function
  | Ast.TGround g ->
    let ground =
      match g with
      | Ast.GInt -> tint
      | Ast.GBool -> tbool
      | Ast.GUnit -> tunit
    in
    return ground
  | Ast.TTuple tuple ->
    let rec helper acc = function
      | hd :: tl ->
        let* hd = convert_type hd in
        helper (hd :: acc) tl
      | [] -> return @@ ttuple (List.rev acc)
    in
    helper [] tuple
  | Ast.TArr (t1, t2) ->
    let* t1 = convert_type t1 in
    let* t2 = convert_type t2 in
    return @@ tarrow t1 t2
  | Ast.TList t ->
    let* t = convert_type t in
    return @@ tlist t
  | Ast.TVar var -> return @@ tvar var
;;

let unify_annotated_type typ t =
  match t with
  | None -> return Subst.empty
  | Some t ->
    let* t = convert_type t in
    unify typ t
;;

let infer_ptrn ?(env = TypeEnv.empty) (pat : pattern_typed) =
  let rec helper env = function
    | PConst x, ty ->
      let* typ =
        match x with
        | CInt _ -> return tint
        | CBool _ -> return tbool
        | CNil ->
          let* tv = fresh_var in
          return (tlist tv)
        | CUnit -> return tunit
      in
      let* sub = unify_annotated_type typ ty in
      return (env, Subst.apply sub typ)
    | PId x, ty ->
      let* tv = fresh_var in
      let* sub = unify_annotated_type tv ty in
      let typi = Subst.apply sub tv in
      let env = TypeEnv.extend env x (Scheme.empty, typi) in
      return (env, typi)
    | PList (hd, tl), ty ->
      let* env, typ_hd = helper env hd in
      let* env, typ_tl = helper env tl in
      let* subst1 = unify_annotated_type (tlist typ_hd) ty in
      let* subst2 = unify (tlist typ_hd) typ_tl in
      let* subst = Subst.compose subst1 subst2 in
      let env = TypeEnv.apply subst env in
      return (env, tlist (Subst.apply subst typ_hd))
    | PTuple tuple, ty ->
      let rec tuple_pat env acc = function
        | hd :: tl ->
          let* env, typ_hd = helper env hd in
          tuple_pat env (typ_hd :: acc) tl
        | [] ->
          let tuple_types = ttuple (List.rev acc) in
          let* subst = unify_annotated_type tuple_types ty in
          let new_ty = Subst.apply subst tuple_types in
          let env = TypeEnv.apply subst env in
          return (env, new_ty)
      in
      tuple_pat env [] tuple
  in
  helper env pat
;;

let rec get_return_type = function
  | TArr (_, ty) -> get_return_type ty
  | ty -> ty
;;

let rec infer env = function
  | EConst c ->
    let* typ =
      match c with
      | CInt _ -> return tint
      | CBool _ -> return tbool
      | CNil ->
        let* fresh_var = fresh_var in
        return (tlist fresh_var)
      | CUnit -> return tunit
    in
    return (Subst.empty, typ)
  | EId (IOBinOp ident | IOUnOp ident | IOIdent ident) ->
    (match ident with
     | "_" -> fail WildcardNotExpected
     (* todo: handle unary and binary "-" *)
     | _ -> lookup_env ident env)
  | EApp (e1, e2) ->
    let* subst_1, typ_1 = infer env e1 in
    let* subst_2, typ_2 = infer (TypeEnv.apply subst_1 env) e2 in
    let* type_variable = fresh_var in
    let* subst_3 = unify (tarrow typ_2 type_variable) (Subst.apply subst_2 typ_1) in
    let type_result = Subst.apply subst_3 type_variable in
    let* subst_result = Subst.compose_all [ subst_1; subst_2; subst_3 ] in
    return (subst_result, type_result)
  | EFun (pat, expr) ->
    let* env_pat, typ_pat = infer_ptrn pat ~env in
    let* subst_expr, typ_expr = infer env_pat expr in
    return (subst_expr, tarrow (Subst.apply subst_expr typ_pat) typ_expr)
  | EIf (cond, thn, els) ->
    let* subst_cond, typ_cond = infer env cond in
    let* subst_thn, typ_thn = infer env thn in
    let* subst_els, typ_els = infer env els in
    let* subst_1 = unify (Subst.apply subst_els typ_cond) tbool in
    let* subst_2 = unify typ_thn typ_els in
    let* subst_result =
      Subst.compose_all [ subst_cond; subst_thn; subst_els; subst_1; subst_2 ]
    in
    let type_result = Subst.apply subst_result typ_thn in
    return (subst_result, type_result)
  | EList (hd, tl) ->
    let* subst_hd, typ_hd = infer env hd in
    let typ_lhd = tlist typ_hd in
    let* subst_tl, typ_tl = infer env tl in
    let* s3 = unify typ_lhd typ_tl in
    let* subst_result = Subst.compose_all [ subst_hd; subst_tl; s3 ] in
    return (subst_result, Subst.apply subst_result typ_lhd)
  | ETuple x ->
    let* subst_result, typ_list =
      List.fold_left
        (fun acc expr ->
          let* subst, typ = infer env expr in
          let* subst_acc, typ_acc = acc in
          let* subst = Subst.compose subst subst_acc in
          return (subst, typ :: typ_acc))
        (return (Subst.empty, []))
        x
    in
    return (subst_result, ttuple @@ List.rev typ_list)
  | EClsr (decl, expr) ->
    let* subst_decl, env = infer_decl env decl in
    let* subst, typ_expr = infer env expr in
    let* subst_result = Subst.compose_all [ subst; subst_decl ] in
    return (subst_result, typ_expr)
  | EMatch (e, patmatch) ->
    let rec check_cases typ_res typ_e subst_e = function
      | (pat, expr_res) :: tl ->
        let* env_new, typ_pat = infer_ptrn pat ~env in
        let* subst_pat =
          match run (unify typ_pat typ_e) with
          | Ok subst_pat -> return subst_pat
          | Error _ -> fail @@ MismatchValues (typ_pat, typ_e)
        in
        let* subst_res_2, typ_res_2 = infer (TypeEnv.apply subst_pat env_new) expr_res in
        let* subst_u = unify typ_res_2 (Subst.apply subst_res_2 typ_res) in
        let* subst_e = Subst.compose_all [ subst_e; subst_u; subst_res_2; subst_pat ] in
        let typ_res_2 = Subst.apply subst_e typ_res_2 in
        (match tl with
         | [] -> return (subst_e, typ_res_2)
         | _ -> check_cases typ_res_2 (Subst.apply subst_e typ_pat) subst_e tl)
      | [] -> fail ParserAvoidedError
    in
    let* subst_e, typ_e = infer env e in
    let* type_variable = fresh_var in
    check_cases type_variable typ_e subst_e patmatch

and infer_common_decl env expr annotated_type =
  let* s1, typ = infer env expr in
  let* subst = unify_annotated_type (get_return_type typ) annotated_type in
  let* subst = Subst.compose subst s1 in
  return (typ, subst, env)

and infer_decl env = function
  | DLet (Not_recursive, id, expr, annotated_type) ->
    let* typ, subst, env = infer_common_decl env expr annotated_type in
    let scheme = generalize env typ in
    return (subst, TypeEnv.extend env id scheme)
  | DLet (Recursive, id, expr, annotated_type) ->
    let* type_variable = fresh_var in
    let env = TypeEnv.extend env id (Scheme.empty, type_variable) in
    let* _, subst, env = infer_common_decl env expr annotated_type in
    let typ = Subst.apply subst type_variable in
    let scheme = generalize env typ in
    return (subst, TypeEnv.extend env id scheme)
  | DLetMut (Not_recursive, let_list) ->
    let rec helper start_env acc_env substs = function
      | hd :: tl ->
        let ident, expr, annotated_type = hd in
        let* sub_infer, typ = infer start_env expr in
        let* sub_annotated = unify_annotated_type (get_return_type typ) annotated_type in
        let acc_env = TypeEnv.extend acc_env ident (generalize env typ) in
        let* sub_final = Subst.compose_all [ sub_infer; substs; sub_annotated ] in
        helper start_env acc_env sub_final tl
      | [] -> return (substs, acc_env)
    in
    (* todo: named arguments for helper function
       for some i'm lost in types :( *)
    let* substs, env = helper env env Subst.empty let_list in
    return (substs, env)
  | DLetMut (Recursive, let_list) ->
    let helper smth (ident, expr, annotated_type) =
      let* sub_init, env = smth in
      let* typ, sub_infer, env = infer_common_decl env expr annotated_type in
      let* sub =
        match Base.Map.find env ident with
        | Some (_, init_type) -> unify typ init_type
        | _ -> return Subst.empty
      in
      let env = TypeEnv.extend env ident (Scheme.empty, typ) in
      let* sub_final = Subst.compose_all [ sub_init; sub_infer; sub ] in
      return (sub_final, TypeEnv.apply sub_final env)
    in
    let* env =
      List.fold_left
        (fun env (id, _, _) ->
          let* env = env in
          let* fresh_var = fresh_var in
          return @@ TypeEnv.extend env id (Scheme.empty, fresh_var))
        (return env)
        let_list
    in
    let* sub, env = List.fold_left helper (return (Subst.empty, env)) let_list in
    return (sub, TypeEnv.apply sub env)
;;

(* todo: better env typess*)
let init_env =
  [ "print_int", tarrow tint tunit
  ; "print_newline", tarrow tunit tunit
  ; "-", tarrow tint @@ tarrow tint tint
  ; "+", tarrow tint @@ tarrow tint tint
  ; "/", tarrow tint @@ tarrow tint tint
  ; "*", tarrow tint @@ tarrow tint tint
  ; "<=", tarrow tint @@ tarrow tint tbool
  ; ">=", tarrow tint @@ tarrow tint tbool
  ; ">=", tarrow tint @@ tarrow tint tbool
  ; "||", tarrow tbool @@ tarrow tbool tbool
  ; "&&", tarrow tbool @@ tarrow tbool tbool
  ; ">", tarrow tint @@ tarrow tint tbool
  ; "=", tarrow tint @@ tarrow tint tbool
  ]
;;

let init_env =
  let bind env id typ = TypeEnv.extend env id (Scheme.empty, typ) in
  let env = TypeEnv.empty in
  List.fold_left (fun env (id, typ) -> bind env id typ) env init_env
;;

let run_inference prog =
  run
    (Base.List.fold_left prog ~init:(return init_env) ~f:(fun env decl ->
       let* env = env in
       let* _, env = infer_decl env decl in
       return env))
;;

let is_printable id typ =
  let is_in_init =
    match Base.Map.find init_env id with
    | None -> false
    | Some (_, typ_2) -> typ = typ_2
  in
  not (is_in_init || id = "_")
;;

let print_env env =
  let open Format in
  match env with
  | Ok env ->
    Base.Map.fold env ~init:() ~f:(fun ~key ~data _ ->
      let _, typ = data in
      if is_printable key typ
      then (
        let pp_res fmt (key, typ) =
          fprintf fmt "val %s : %a" key (print_typ ~carriage:true) typ
        in
        printf "%a" pp_res (key, typ))
      else ())
  | Error x -> print_type_error x
;;
