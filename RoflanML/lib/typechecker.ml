(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typing
open Roflanml_stdlib
open Common
open Common.Counter_Monad

type fresh = int

module VarSet = struct
  include Base.Set

  type t = (int, Base.Int.comparator_witness) Base.Set.t

  let empty = Base.Set.empty (module Base.Int)
end

module Type = struct
  let rec occurs_in v = function
    | TBase _ -> false
    | TVar b -> b = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TTuple (ty1, ty2, tys) -> Base.List.exists (ty1 :: ty2 :: tys) ~f:(occurs_in v)
    | TList ty -> occurs_in v ty
  ;;

  let free_vars : ty -> VarSet.t =
    let rec helper acc = function
      | TBase _ -> acc
      | TVar b -> VarSet.add acc b
      | TArrow (l, r) ->
        let lvars = helper acc l in
        helper lvars r
      | TTuple (ty1, ty2, tys) ->
        Base.List.fold_left
          (ty1 :: ty2 :: tys)
          ~f:(fun acc ty -> VarSet.union acc (helper VarSet.empty ty))
          ~init:acc
      | TList ty -> helper acc ty
    in
    helper VarSet.empty
  ;;

  let equal : ty -> ty -> bool =
    let rec helper l r =
      match l, r with
      | TBase l, TBase r -> l = r
      | TVar _, TVar _ -> true
      | TArrow (l1, r1), TArrow (l2, r2) -> helper l1 l2 && helper r1 r2
      | TTuple (l1, l2, ls), TTuple (r1, r2, rs) ->
        helper l1 r1
        && helper l2 r2
        &&
          (match
             Base.List.fold2 ls rs ~init:true ~f:(fun acc l r -> acc && helper l r)
           with
          | Ok r -> r
          | Unequal_lengths -> false)
      | TList l, TList r -> helper l r
      | _ -> false
    in
    helper
  ;;
end

let type_eq = Type.equal

module Subst = struct
  type t = (int, ty, Base.Int.comparator_witness) Base.Map.t

  let empty : t = Base.Map.empty (module Base.Int)

  let singleton (k, v) =
    if Type.occurs_in k v
    then fail (OccursCheckFailed (k, v))
    else return (Base.Map.singleton (module Base.Int) k v)
  ;;

  let apply (subst : t) =
    let rec helper = function
      | TBase b -> TBase b
      | TVar b ->
        (match Base.Map.find subst b with
         | Some v -> v
         | None -> TVar b)
      | TArrow (l, r) -> TArrow (helper l, helper r)
      | TTuple (ty1, ty2, tys) ->
        TTuple (helper ty1, helper ty2, Base.List.map tys ~f:helper)
      | TList ty -> TList (helper ty)
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TBase l, TBase r when l = r -> return empty
    | TBase _, TBase _ -> fail (UnificationFailed (l, r))
    | TVar l, TVar r when l = r -> return empty
    | TVar b, t | t, TVar b -> singleton (b, t)
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TTuple (l1, l2, ls), TTuple (r1, r2, rs) ->
      if List.compare_lengths ls rs <> 0
      then fail (UnificationFailed (l, r))
      else
        Base.List.fold_left
          (Base.List.zip_exn (l1 :: l2 :: ls) (r1 :: r2 :: rs))
          ~f:(fun s (l, r) ->
            let* shead = unify l r in
            let* s = s in
            compose s shead)
          ~init:(return empty)
    | TList ty1, TList ty2 -> unify ty1 ty2
    | _ -> fail (UnificationFailed (l, r))

  and extend (subst : t) (k, v) : (t, error) Counter_Monad.t =
    match Base.Map.find subst k with
    | Some v2 ->
      let* subst2 = unify v v2 in
      compose subst subst2
    | None ->
      let v = apply subst v in
      let* subst2 = singleton (k, v) in
      Base.Map.fold subst ~init:(return subst2) ~f:(fun ~key:k ~data:v acc ->
        let* acc = acc in
        let v = apply subst2 v in
        if Type.occurs_in k v
        then fail (OccursCheckFailed (k, v))
        else return (Base.Map.set acc ~key:k ~data:v))

  and compose subst1 subst2 =
    Base.Map.fold subst1 ~init:(return subst2) ~f:(fun ~key:k ~data:v acc ->
      let* acc = acc in
      extend acc (k, v))
  ;;

  let compose_all substs =
    Base.List.fold_left substs ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose acc subst)
  ;;
end

module Scheme = struct
  type t = S of VarSet.t * ty

  let free_vars : t -> VarSet.t = fun (S (s, ty)) -> VarSet.diff (Type.free_vars ty) s

  let apply (S (s, ty)) subst =
    let subst2 = VarSet.fold s ~init:subst ~f:Base.Map.remove in
    S (s, Subst.apply subst2 ty)
  ;;

  let equal (S (s1, ty1)) (S (s2, ty2)) = VarSet.equal s1 s2 && Type.equal ty1 ty2
end

module TypeEnv = struct
  include Base.Map

  type t = (id, Scheme.t, Base.String.comparator_witness) Base.Map.t

  let empty : t = Base.Map.empty (module Base.String)

  let free_vars : t -> VarSet.t =
    fun env ->
    Base.Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data:sch acc ->
      VarSet.union acc (Scheme.free_vars sch))
  ;;

  let apply : t -> Subst.t -> t =
    fun env subst -> Base.Map.map env ~f:(fun sch -> Scheme.apply sch subst)
  ;;

  let extend : t -> id * Scheme.t -> t =
    fun env (id, sch) -> Base.Map.set env ~key:id ~data:sch
  ;;

  let equal : t -> t -> bool = Base.Map.equal Scheme.equal

  (* Returns set of variable id's which occure only in one of two enviroments *)
  let vars_diff : t -> t -> id list =
    fun env1 env2 ->
    Base.Map.fold2 env1 env2 ~init:[] ~f:(fun ~key:id ~data:v acc ->
      match v with
      | `Left _ | `Right _ -> id :: acc
      | `Both _ -> acc)
  ;;

  let schemes_diff : t -> t -> (id * Scheme.t * Scheme.t) list =
    fun env1 env2 ->
    Base.Map.fold2 env1 env2 ~init:[] ~f:(fun ~key:id ~data:v acc ->
      match v with
      | `Both (l, r) when not (Scheme.equal l r) -> (id, l, r) :: acc
      | _ -> acc)
  ;;
end

let unify = Subst.unify
let fresh_var = fresh >>= fun x -> return (TVar x)

(* Create instance of type from scheme *)
let instantiate (Scheme.S (s, ty)) =
  VarSet.fold s ~init:(return ty) ~f:(fun ty name ->
    let* ty = ty in
    let* fv = fresh_var in
    let* subst = Subst.singleton (name, fv) in
    return (Subst.apply subst ty))
;;

(* Make scheme from type *)
let generalize : TypeEnv.t -> ty -> Scheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  Scheme.S (free, ty)
;;

let lookup_env : TypeEnv.t -> id -> (Subst.t * ty, error) Counter_Monad.t =
  fun env id ->
  match Base.Map.find env id with
  | Some sch ->
    let* ty = instantiate sch in
    return (Subst.empty, ty)
  | None -> fail (UndeclaredVariable id)
;;

let type_to_schema ty =
  let rec helper tv_mapping = function
    | TBase base -> return (TBase base, VarSet.empty, tv_mapping)
    | TVar x ->
      (match Base.Map.find tv_mapping x with
       | Some v -> return (TVar v, VarSet.empty, tv_mapping)
       | None ->
         let* fv = fresh in
         let tv_mapping = Base.Map.update tv_mapping x ~f:(fun _ -> fv) in
         return (TVar fv, VarSet.singleton (module Base.Int) fv, tv_mapping))
    | TArrow (l, r) ->
      let* lty, lvarset, tv_mapping = helper tv_mapping l in
      let* rty, rvarset, tv_mapping = helper tv_mapping r in
      return (TArrow (lty, rty), VarSet.union lvarset rvarset, tv_mapping)
    | TTuple (ty1, ty2, tys) ->
      let* ty1, varset1, tv_mapping = helper tv_mapping ty1 in
      let* ty2, varset2, tv_mapping = helper tv_mapping ty2 in
      let* tys, varset, tv_mapping =
        Base.List.fold_right
          tys
          ~init:(return ([], VarSet.empty, tv_mapping))
          ~f:(fun ty acc ->
            let* tys, varset, tv_mapping = acc in
            let* ty, varset1, tv_mapping = helper tv_mapping ty in
            return (ty :: tys, VarSet.union varset varset1, tv_mapping))
      in
      return
        ( TTuple (ty1, ty2, tys)
        , VarSet.union_list (module Base.Int) [ varset1; varset2; varset ]
        , tv_mapping )
    | TList ty ->
      let* ty, varset, tv_mapping = helper tv_mapping ty in
      return (TList ty, varset, tv_mapping)
  in
  let* ty, varset, _ = helper (Base.Map.empty (module Base.Int)) ty in
  return (Scheme.S (varset, ty))
;;

let create_base_env env =
  Base.Map.fold RoflanML_Stdlib.default ~init:(return env) ~f:(fun ~key ~data env ->
    let* env = env in
    let* sch = type_to_schema data in
    return (TypeEnv.extend env (key, sch)))
;;

let infer_pattern : pattern -> (TypeEnv.t * ty, error) Counter_Monad.t =
  let rec helper env = function
    | PWild ->
      let* tv = fresh_var in
      return (env, tv)
    | PEmpty ->
      let* tv = fresh_var in
      return (env, TList tv)
    | PConst c ->
      (match c with
       | CInt _ -> return (env, TBase BInt)
       | CBool _ -> return (env, TBase BBool)
       | CUnit -> return (env, TBase BUnit))
    | PVar x ->
      (match Base.Map.find env x with
       | None ->
         let* tv = fresh_var in
         let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
         return (env, tv)
       | Some (Scheme.S (_, ty)) -> return (env, ty))
    | PTuple (p1, p2, ps) ->
      let* env, ty1 = helper env p1 in
      let* env, ty2 = helper env p2 in
      let* env, tys =
        Base.List.fold_right
          ps
          ~init:(return (env, []))
          ~f:(fun p acc ->
            let* env, ty = helper env p in
            let* _, tys = acc in
            return (env, ty :: tys))
      in
      return (env, TTuple (ty1, ty2, tys))
    | PCons (p1, p2, ps) ->
      let p1, ps, plast =
        match List.rev ps with
        | [] -> p1, [], p2
        | h :: tl -> p1, p2 :: List.rev tl, h
      in
      let* env, ty1 = helper env p1 in
      let* env, ty =
        Base.List.fold_left
          ps
          ~init:(return (env, ty1))
          ~f:(fun acc p ->
            let* env, ty = acc in
            let* env, ty1 = helper env p in
            let* subst = unify ty ty1 in
            return (TypeEnv.apply env subst, Subst.apply subst ty))
      in
      let* env, ty_last = helper env plast in
      let* subst = unify (TList ty) ty_last in
      let ty_last = Subst.apply subst ty_last in
      let env = TypeEnv.apply env subst in
      return (TypeEnv.apply env subst, Subst.apply subst ty_last)
    | POr (p1, p2, ps) ->
      let* env1, ty1 = helper env p1 in
      let* env, ty =
        Base.List.fold_left
          (p2 :: ps)
          ~init:(return (env1, ty1))
          ~f:(fun acc p ->
            let* env1, ty1 = acc in
            let* env2, ty2 = helper env p in
            match TypeEnv.vars_diff env1 env2 with
            | h :: _ -> fail (OrPatternBoundsDiff h)
            | [] ->
              (match TypeEnv.schemes_diff env1 env2 with
               | (id, Scheme.S (_, ty1), Scheme.S (_, ty2)) :: _ ->
                 fail (OrPatternTypeDiff (id, ty1, ty2))
               | _ ->
                 let* subst = unify ty1 ty2 in
                 return (env1, Subst.apply subst ty1)))
      in
      return (env, ty)
  in
  helper TypeEnv.empty
;;

let infer env decl =
  let rec infer_expr env = function
    | EConst c ->
      (match c with
       | CInt _ -> return (Subst.empty, TBase BInt)
       | CBool _ -> return (Subst.empty, TBase BBool)
       | CUnit -> return (Subst.empty, TBase BUnit))
    | EVar x -> lookup_env env x
    | EFun ((id, arg_ty), e) ->
      let* tv = fresh_var in
      let* subst =
        match arg_ty with
        | None -> return Subst.empty
        | Some arg_ty ->
          let arg_ty = map_type arg_ty in
          unify tv arg_ty
      in
      let env2 = TypeEnv.extend env (id, S (VarSet.empty, Subst.apply subst tv)) in
      let* s, ty = infer_expr env2 e in
      let* s = Subst.compose s subst in
      let res_ty = TArrow (Subst.apply s tv, ty) in
      return (s, res_ty)
    | EApp (e1, e2) ->
      let* subst1, ty1 = infer_expr env e1 in
      let* subst2, ty2 = infer_expr (TypeEnv.apply env subst1) e2 in
      let* tv = fresh_var in
      let* subst3 = unify (Subst.apply subst2 ty1) (TArrow (ty2, tv)) in
      let res_ty = Subst.apply subst3 tv in
      let* final_subst = Subst.compose_all [ subst1; subst2; subst3 ] in
      return (final_subst, res_ty)
    | ETuple (e1, e2, es) ->
      let* subst1, ty1 = infer_expr env e1 in
      let* subst2, ty2 = infer_expr env e2 in
      let* substs, tys =
        Base.List.fold_right
          es
          ~init:(return ([], []))
          ~f:(fun e acc ->
            let* subst, ty = infer_expr env e in
            let* substs, tys = acc in
            return (subst :: substs, ty :: tys))
      in
      let* final_subst = Subst.compose_all (subst1 :: subst2 :: substs) in
      return (final_subst, TTuple (ty1, ty2, tys))
    | EList es ->
      (match es with
       | [] ->
         let* tv = fresh_var in
         return (Subst.empty, TList tv)
       | h :: tl ->
         let* final_subst, res_ty =
           Base.List.fold_left tl ~init:(infer_expr env h) ~f:(fun acc e ->
             let* subst, ty = acc in
             let* subst1, ty1 = infer_expr env e in
             let* subst2 = unify ty ty1 in
             let* final_subst = Subst.compose_all [ subst; subst1; subst2 ] in
             let res_ty = Subst.apply final_subst ty in
             return (final_subst, res_ty))
         in
         return (final_subst, TList res_ty))
    | EBranch (c, t, f) ->
      let* subst1, ty1 = infer_expr env c in
      let* subst2, ty2 = infer_expr env t in
      let* subst3, ty3 = infer_expr env f in
      let* subst4 = unify ty1 (TBase BBool) in
      let* subst5 = unify ty2 ty3 in
      let* final_subst = Subst.compose_all [ subst1; subst2; subst3; subst4; subst5 ] in
      return (final_subst, Subst.apply subst5 ty3)
    | ELetIn (NonRec, x, e1, e2) ->
      let* subst1, ty1 = infer_expr env e1 in
      let env2 = TypeEnv.apply env subst1 in
      let ty2 = generalize env2 ty1 in
      let env3 = TypeEnv.extend env2 (x, ty2) in
      let* subst2, ty3 = infer_expr env3 e2 in
      let* final_subst = Subst.compose subst1 subst2 in
      return (final_subst, ty3)
    | ELetIn (Rec, x, e1, e2) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* subst1, ty1 = infer_expr env e1 in
      let* subst2 = unify (Subst.apply subst1 tv) ty1 in
      let* subst = Subst.compose subst1 subst2 in
      let env = TypeEnv.apply env subst in
      let ty2 = generalize env (Subst.apply subst tv) in
      let* subst2, ty2 = infer_expr TypeEnv.(extend (apply env subst) (x, ty2)) e2 in
      let* final_subst = Subst.compose subst subst2 in
      return (final_subst, ty2)
    | EMatch (c, cases) ->
      let* c_subst, c_ty = infer_expr env c in
      let* tv = fresh_var in
      let* e_subst, e_ty =
        Base.List.fold_left
          cases
          ~init:(return (c_subst, tv))
          ~f:(fun acc (pat, e) ->
            let* subst, ty = acc in
            let* pat_env, pat_ty = infer_pattern pat in
            let pat_env =
              TypeEnv.fold pat_env ~init:env ~f:(fun ~key ~data acc ->
                TypeEnv.extend acc (key, data))
            in
            let* subst2 = unify c_ty pat_ty in
            let* subst3, e_ty = infer_expr pat_env e in
            let* subst4 = unify ty e_ty in
            let* final_subst = Subst.compose_all [ subst; subst2; subst3; subst4 ] in
            return (final_subst, Subst.apply final_subst ty))
      in
      let* final_subst = Subst.compose c_subst e_subst in
      return (final_subst, Subst.apply final_subst e_ty)
  in
  let infer_decl env = function
    | DLet (NonRec, _, e) ->
      let* subst, ty = infer_expr env e in
      return (subst, [ ty ])
    | DLet (Rec, x, e) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* subst1, ty1 = infer_expr env e in
      let* subst2 = unify (Subst.apply subst1 tv) ty1 in
      let* final_subst = Subst.compose subst1 subst2 in
      return (final_subst, [ Subst.apply final_subst tv ])
    | DMutualLet (_, decls) ->
      let* env =
        Base.List.fold decls ~init:(return env) ~f:(fun env (x, _) ->
          let* env = env in
          let* tv = fresh_var in
          return (TypeEnv.extend env (x, S (VarSet.empty, tv))))
      in
      let* subst, _, tys =
        Base.List.fold_right
          decls
          ~init:(return (Subst.empty, env, []))
          ~f:(fun (id, e) acc ->
            let* subst, env, tys = acc in
            let* subst1, ty1 = infer_expr env e in
            let (S (_, ty)) = Base.Map.find_exn env id in
            let* subst2 = unify (Subst.apply subst1 ty) ty1 in
            let* subst = Subst.compose_all [ subst; subst1; subst2 ] in
            let env = TypeEnv.apply env subst in
            let ty = Subst.apply subst ty in
            return (subst, env, ty :: tys))
      in
      return (subst, tys)
  in
  infer_decl env decl
;;

let run_infer ?(env = TypeEnv.empty) e =
  Result.map
    snd
    (run
       (let* env = create_base_env env in
        infer env e))
;;

let check_program env prog =
  let check_decl env decl =
    let* _, tys = infer env decl in
    match decl, tys with
    | DLet (_, id, _), [ ty ] ->
      let env = TypeEnv.extend env (id, generalize env ty) in
      return env
    | DMutualLet (_, decls), tys ->
      let env =
        Base.List.fold2_exn decls tys ~init:env ~f:(fun env (id, _) ty ->
          TypeEnv.extend env (id, generalize env ty))
      in
      return env
    | _ -> fail NotReachable
  in
  Base.List.fold_left prog ~init:(return env) ~f:(fun env e ->
    let* env = env in
    let* env = check_decl env e in
    return env)
;;

let typecheck ?(env = TypeEnv.empty) prog =
  run
    (let* env = create_base_env env in
     check_program env prog)
;;
