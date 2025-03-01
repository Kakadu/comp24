(** Copyright 2024-2025, Dmitrii Kosarev, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** ref: https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml/inferencer.ml **)

open AstLib.Ast
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
    let ( let* ) = ( >>= )
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
    | TTuple (t1, t2, typ_list) ->
      Base.List.exists (t1 :: t2 :: typ_list) ~f:(occurs_in v)
    | TList typ -> occurs_in v typ
    | TGround _ -> false
  ;;

  let free_vars =
    let empty = Base.Set.empty (module Base.String) in
    let rec helper acc = function
      | TVar n -> Base.Set.add acc n
      | TArr (left, right) -> helper (helper acc left) right
      | TTuple (t1, t2, typ_list) ->
        let typ_list = t1 :: t2 :: typ_list in
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
  let remove = Base.Map.remove

  let apply s =
    let rec helper = function
      | TVar n ->
        (match find n s with
         | None -> tvar n
         | Some x -> x)
      | TArr (left, right) -> tarrow (helper left) (helper right)
      | TTuple (t1, t2, typ_list) ->
        let t1 = helper t1 in
        let t2 = helper t2 in
        let typ_list = Base.List.map typ_list ~f:helper in
        ttuple t1 t2 typ_list
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
    | TTuple (t1_l, t2_l, typ_list_l), TTuple (t1_r, t2_r, typ_list_r) ->
      let typ_list_l = t1_l :: t2_l :: typ_list_l in
      let typ_list_r = t1_r :: t2_r :: typ_list_r in
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

  let compose_all (substs : t list) =
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
  let free_vars (set, typ) = Base.Set.diff (Type.free_vars typ) set

  let apply subst (set, typ) =
    let s2 = Base.Set.fold set ~init:subst ~f:Subst.remove in
    set, Subst.apply s2 typ
  ;;
end

module TypeEnv = struct
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  let extend env id scheme = Base.Map.update env id ~f:(fun _ -> scheme)
  let empty = Base.Map.empty (module Base.String)

  let free_vars : t -> (type_variable_number, Base.String.comparator_witness) Base.Set.t =
    Base.Map.fold
      ~init:(Base.Set.empty (module Base.String))
      ~f:(fun ~key:_ ~data acc -> Base.Set.union acc (Scheme.free_vars data))
  ;;

  let rec extend_by_pattern ((bs, ty) as scheme) acc pat =
    match pat, ty with
    | PConstraint (p, _), _ -> extend_by_pattern (bs, ty) acc p
    | PId v, _ -> extend acc v scheme
    | PList (h, tl), TList t ->
      let env = extend_by_pattern (bs, t) acc h in
      extend_by_pattern (bs, ty) env tl
    | PTuple (e1, e2, es), TTuple (t1, t2, ts) ->
      let es = e1 :: e2 :: es in
      let ts = t1 :: t2 :: ts in
      let new_env =
        Base.List.fold2 es ts ~init:acc ~f:(fun acc e t ->
          extend_by_pattern (bs, t) acc e)
      in
      (match new_env with
       | Ok env -> env
       | _ -> acc)
    | _ -> acc
  ;;

  let rec extend_by_pattern_or_op scheme acc = function
    | POpPat p -> extend_by_pattern scheme acc p
    | POpOp op -> extend acc op scheme
    | POrOpConstraint (p, _) -> extend_by_pattern_or_op scheme acc p
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

let instantiate : scheme -> typ R.t =
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

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env typ ->
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

let rec convert_type =
  let open AstLib in
  function
  | Ast.TGround g ->
    let ground =
      match g with
      | Ast.GInt -> tint
      | Ast.GBool -> tbool
      | Ast.GUnit -> tunit
    in
    return ground
  | Ast.TTuple (t1, t2, tlist) ->
    let* t1 = convert_type t1 in
    let* t2 = convert_type t2 in
    let rec helper acc = function
      | hd :: tl ->
        let* hd = convert_type hd in
        helper (hd :: acc) tl
      | [] -> return @@ ttuple t1 t2 (List.rev acc)
    in
    helper [] tlist
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

let has_annotated_type_pop = function
  | POrOpConstraint (_, ty) -> Some ty
  | _ -> None
;;

let infer_ptrn ?(env = TypeEnv.empty) (pat : pattern) =
  let rec helper env = function
    | PConstraint (x, ty) ->
      let* env, typ = helper env x in
      let* sub = unify_annotated_type typ (Some ty) in
      return (TypeEnv.apply sub env, Subst.apply sub typ)
    | PConst x ->
      let* typ =
        match x with
        | CInt _ -> return tint
        | CBool _ -> return tbool
        | CNil ->
          let* tv = fresh_var in
          return (tlist tv)
        | CUnit -> return tunit
      in
      return (env, typ)
    | PId x ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env x (Scheme.empty, tv) in
      return (env, tv)
    | PList (hd, tl) ->
      let* env, typ_hd = helper env hd in
      let* env, typ_tl = helper env tl in
      let* subst = unify (tlist typ_hd) typ_tl in
      let env = TypeEnv.apply subst env in
      return (env, tlist (Subst.apply subst typ_hd))
    | PTuple (t1, t2, tlist) ->
      let* env, t1 = helper env t1 in
      let* env, t2 = helper env t2 in
      let rec tuple_pat env acc = function
        | hd :: tl ->
          let* env, typ_hd = helper env hd in
          tuple_pat env (typ_hd :: acc) tl
        | [] ->
          let tuple_types = ttuple t1 t2 (List.rev acc) in
          return (env, tuple_types)
      in
      tuple_pat env [] tlist
  in
  helper env pat
;;

let infer_op env op =
  let* tv = fresh_var in
  let env = TypeEnv.extend env op (Scheme.empty, tv) in
  return (env, tv)
;;

let rec infer_ptrn_or_op env = function
  | POpPat pat -> infer_ptrn ~env pat
  | POpOp pat -> infer_op env pat
  | POrOpConstraint (pat, ty) ->
    let* env, typ = infer_ptrn_or_op env pat in
    let* sub = unify_annotated_type typ (Some ty) in
    return (TypeEnv.apply sub env, Subst.apply sub typ)
;;

let rec get_return_type = function
  | TArr (_, ty) -> get_return_type ty
  | ty -> ty
;;

let rec infer env = function
  | EConstraint (e, t) ->
    let* s1, typ = infer env e in
    let* subst = unify_annotated_type typ (Some t) in
    let* subst = Subst.compose s1 subst in
    return (subst, Subst.apply subst typ)
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
  | EId (IdentOfBaseOp Plus) -> lookup_env "base +" env
  | EId (IdentOfBaseOp Minus) -> lookup_env "base -" env
  | EId (IdentOfDefinable (IdentLetters ident) | IdentOfDefinable (IdentOp ident)) ->
    (match ident with
     | "_" -> fail WildcardNotExpected
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
    return (subst_expr, Subst.apply subst_expr (tarrow typ_pat typ_expr))
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
    let* s3 = unify typ_tl typ_lhd in
    let* subst_result = Subst.compose_all [ subst_hd; subst_tl; s3 ] in
    return (subst_result, Subst.apply subst_result typ_lhd)
  | ETuple (e1, e2, l) ->
    let* subst_e1, typ_e1 = infer env e1 in
    let* subst_e2, typ_e2 = infer env e2 in
    let* subst_result, typ_list =
      List.fold_left
        (fun acc expr ->
          let* subst, typ = infer env expr in
          let* subst_acc, typ_acc = acc in
          let* subst = Subst.compose subst subst_acc in
          return (subst, typ :: typ_acc))
        (return (Subst.empty, []))
        l
    in
    let* subst_result = Subst.compose_all [ subst_e1; subst_e2; subst_result ] in
    return (subst_result, ttuple typ_e1 typ_e2 (List.rev typ_list))
  | EClsr (decl, expr) ->
    let* subst_decl, env = infer_decl env decl in
    let* subst, typ_expr = infer (TypeEnv.apply subst_decl env) expr in
    let* subst_result = Subst.compose subst subst_decl in
    return (subst_result, typ_expr)
  | EMatch (e, branch, branch_list) ->
    let rec check_cases typ_res typ_e subst_e = function
      | (pat, expr_res) :: tl ->
        let* env_new, typ_pat = infer_ptrn pat ~env in
        let* subst_pat = unify typ_pat typ_e in
        let* subst_res_2, typ_res_2 = infer (TypeEnv.apply subst_pat env_new) expr_res in
        let* subst_u = unify typ_res_2 (Subst.apply subst_res_2 typ_res) in
        let* subst_e = Subst.compose_all [ subst_e; subst_u; subst_res_2; subst_pat ] in
        let typ_res_2 = Subst.apply subst_e typ_res_2 in
        check_cases typ_res_2 typ_e subst_e tl
      | [] -> return (subst_e, typ_res)
    in
    let* subst_e, typ_e = infer env e in
    let* type_variable = fresh_var in
    check_cases type_variable typ_e subst_e (branch :: branch_list)

and infer_decl env = function
  | DLet (Not_recursive, (pattern, expr_typed)) ->
    let* subst, t1 = infer env expr_typed in
    let scheme = generalize (TypeEnv.apply subst env) t1 in
    let* env, t2 = infer_ptrn_or_op env pattern in
    let env = TypeEnv.extend_by_pattern_or_op scheme env pattern in
    let* sub = unify (get_return_type t1) t2 in
    let* sub1 = Subst.compose subst sub in
    let env = TypeEnv.apply sub1 env in
    return (sub1, env)
  | DLet
      ( Recursive
      , ( (( POpPat (PId x)
           | POpOp x
           | POrOpConstraint (POpPat (PId x), _)
           | POrOpConstraint (POpOp x, _) ) as pattern)
        , expr_typed ) ) ->
    let* tv = fresh_var in
    let env = TypeEnv.extend env x (Scheme.empty, tv) in
    let ty_pat = has_annotated_type_pop pattern in
    let* s1 = unify_annotated_type tv ty_pat in
    let* subst, typ_expr = infer (TypeEnv.apply s1 env) expr_typed in
    let* s2 = unify typ_expr (Subst.apply subst tv) in
    let* sub = Subst.compose s2 subst in
    let env = TypeEnv.apply s2 env in
    let scheme = generalize env (Subst.apply sub tv) in
    return (subst, TypeEnv.extend env x scheme)
  | DLet (Recursive, _) -> fail UnexpectedRecursionLhs
  | DLetMut (Not_recursive, let_1, let_2, let_list) ->
    let let_list = let_1 :: let_2 :: let_list in
    let rec helper ~start_env ~acc_env ~substs = function
      | (pattern, expr) :: tl ->
        let* sub_infer, typ = infer start_env expr in
        let acc_env =
          TypeEnv.extend_by_pattern_or_op (generalize start_env typ) acc_env pattern
        in
        let* sub_final = Subst.compose_all [ sub_infer; substs ] in
        helper ~start_env ~acc_env ~substs:sub_final tl
      | [] -> return (substs, acc_env)
    in
    let* substs, env = helper ~start_env:env ~acc_env:env ~substs:Subst.empty let_list in
    return (substs, env)
  | DLetMut (Recursive, let_1, let_2, let_list) ->
    let let_list = let_1 :: let_2 :: let_list in
    let helper smth (pat, expr) =
      let* sub_init, env = smth in
      let* sub_infer, typ = infer env expr in
      match pat with
      | POpPat (PId x)
      | POpOp x
      | POrOpConstraint (POpPat (PId x), _)
      | POrOpConstraint (POpOp x, _) ->
        let* sub =
          match Base.Map.find env x with
          | Some (_, init_type) -> unify typ init_type
          | _ -> return Subst.empty
        in
        let env = TypeEnv.extend env x (Scheme.empty, typ) in
        let* sub_final = Subst.compose_all [ sub_init; sub_infer; sub ] in
        return (sub_final, TypeEnv.apply sub_final env)
      | _ -> fail UnexpectedRecursionLhs
    in
    let* env =
      List.fold_left
        (fun env (pattern, _) ->
          let* env = env in
          let* fresh_var = fresh_var in
          return @@ TypeEnv.extend_by_pattern_or_op (Scheme.empty, fresh_var) env pattern)
        (return env)
        let_list
    in
    let* sub, env = List.fold_left helper (return (Subst.empty, env)) let_list in
    return (sub, TypeEnv.apply sub env)
;;

let init_env =
  let un_op = tarrow in
  let bin_op a b res = tarrow a @@ tarrow b res in
  [ "print_int", tarrow tint tunit
  ; "print_newline", tarrow tunit tunit
  ; "base +", un_op tint tint
  ; "base -", un_op tint tint
  ; "+", bin_op tint tint tint
  ; "-", bin_op tint tint tint
  ; "*", bin_op tint tint tint
  ; "/", bin_op tint tint tint
  ; "<=", bin_op (tvar "_a") (tvar "_a") tbool
  ; "<", bin_op (tvar "_a") (tvar "_a") tbool
  ; ">=", bin_op (tvar "_a") (tvar "_a") tbool
  ; ">", bin_op (tvar "_a") (tvar "_a") tbool
  ; "=", bin_op (tvar "_a") (tvar "_a") tbool
  ; "==", bin_op (tvar "_a") (tvar "_a") tbool
  ; "!=", bin_op (tvar "_a") (tvar "_a") tbool
  ; "&&", bin_op tbool tbool tbool
  ; "||", bin_op tbool tbool tbool
  ; "( = )", bin_op (tvar "_a") (tvar "_a") tbool 
  ; "( != )", bin_op (tvar "_a") (tvar "_a") tbool  
  ; "( && )", bin_op tbool tbool tbool  
  (* todo ( + ) *)
  ; "RTE_ERROR_MATCH_FAILURE", tarrow tunit (tvar "_a");
  "GET_HEAD", tarrow (tlist @@ tvar "_a") (tvar "_a");
 "GET_TALE", tarrow (tlist @@ tvar "_a") (tlist @@ tvar "_a");
 "GET_NTH", tarrow (ttuple tint (tvar "_a") []) (tvar "_b") 


  ]
;;

let init_env =
  let bind env id typ = TypeEnv.extend env id (generalize env typ) in
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

let is_printable (id : string) typ =
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
