(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Typedtree
open Inf_errors

module R : sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val fail : error -> 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
end = struct
  type 'a t = int -> int * ('a, error) Result.t (* State and Result monad composition *)

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f s ->
    match m s with
    | s, Result.Error e -> s, Error e
    | s, Result.Ok v -> f v s
  ;;

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun m f s ->
    match m s with
    | s, Result.Error e -> s, Error e
    | s, Result.Ok v -> s, Base.Result.return @@ f v
  ;;

  let return v last = last, Base.Result.return v
  let fail e state = state, Base.Result.fail e
  let bind x ~f = x >>= f
  let fresh last = last + 1, Result.Ok last (* Get new state *)

  module Syntax = struct
    let ( let* ) x f = bind x ~f (* Syntax sugar for bind *)
  end

  module RMap = struct
    (* Classic map folding. *)
    let fold_left mp ~init ~f =
      let open Syntax in
      Base.Map.fold mp ~init ~f:(fun ~key ~data acc ->
        let* acc = acc in
        f key data acc)
    ;;
  end

  module RList = struct
    (* Classic list folding. *)
    let fold_left lt ~init ~f =
      let open Syntax in
      Base.List.fold_left lt ~init ~f:(fun acc item ->
        let* acc = acc in
        f acc item)
    ;;
  end

  (* Run and get the internal value. *)
  let run m = snd (m 0)
end

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TVar b -> b = v
    | TFunction (l, r) -> occurs_in v l || occurs_in v r
    | TList t -> occurs_in v t
    | TTuple ts -> List.fold_left (fun acc item -> acc || occurs_in v item) false ts
    | _ -> false
  ;;

  let type_vars =
    let rec helper acc = function
      | TVar b -> TVarSet.add b acc
      | TFunction (l, r) -> helper (helper acc l) r
      | TList t -> helper acc t
      | TTuple ts -> List.fold_left helper acc ts
      | TBool | TInt | TUnit -> acc
    in
    helper TVarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : int -> typ -> t R.t
  val remove : t -> int -> t
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
end = struct
  open R
  open R.Syntax

  (* A substitution is an map, where the first element of each list element is what needs to be replaced,
     the second is what it should be replaced with. *)
  type t = (int, typ, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)

  (* Creates a pair if no error occurs. *)
  let mapping k v =
    if Type.occurs_in k v then fail (`Occurs_check (k, v)) else return (k, v)
  ;;

  let singleton k v =
    let* k, v = mapping k v in
    return (Base.Map.singleton (module Base.Int) k v)
  ;;

  let find = Base.Map.find
  let remove = Base.Map.remove

  (* Replace all type variables in a type with values ​​from the substitution. *)
  let apply sub =
    let rec helper = function
      | TVar n as ty ->
        (match find sub n with
         | None -> ty
         | Some v -> v)
      | TFunction (l, r) -> tfunction (helper l) (helper r)
      | TList typ -> tlist (helper typ)
      | TTuple t_list -> ttuple (Base.List.map t_list ~f:helper)
      | other -> other
    in
    helper
  ;;

  (* Try to unify two types into a single type. *)
  let rec unify l r =
    match l, r with
    | TInt, TInt | TBool, TBool | TUnit, TUnit -> return empty
    | TVar a, TVar b when a = b -> return empty
    | TVar a, t | t, TVar a -> singleton a t
    | TFunction (left1, right1), TFunction (left2, right2) ->
      let* sub1 = unify left1 left2 in
      let* sub2 = unify (apply sub1 right1) (apply sub1 right2) in
      compose sub1 sub2
    | TList typ1, TList typ2 -> unify typ1 typ2
    | TTuple t_list1, TTuple t_list2 ->
      (match
         Base.List.fold2 t_list1 t_list2 ~init:(return empty) ~f:(fun acc it1 it2 ->
           let* sub1 = acc in
           let* sub2 = unify (apply sub1 it1) (apply sub1 it2) in
           compose sub1 sub2)
       with
       | Ok r -> r
       | _ -> fail (`Unification_failed (l, r)))
    | _ -> fail (`Unification_failed (l, r))

  (* Expanding the substitution with a new key-value. *)
  and extend k v sub =
    match find sub k with
    | None ->
      let v = apply sub v in
      let* new_sub = singleton k v in
      RMap.fold_left sub ~init:(return new_sub) ~f:(fun k v acc ->
        let v = apply new_sub v in
        let* k, v = mapping k v in
        return (Base.Map.update acc k ~f:(fun _ -> v)))
    | Some vl ->
      let* new_sub = unify v vl in
      compose sub new_sub

  (* Two substitution's composition. *)
  and compose sub1 sub2 = RMap.fold_left sub2 ~init:(return sub1) ~f:extend

  (* Composition of an arbitrary number of substitutions. *)
  let compose_all sub_list = RList.fold_left sub_list ~init:(return empty) ~f:compose
end

module Scheme = struct
  let free_vars = function
    | Scheme (bind_vars, typ) -> TVarSet.diff (Type.type_vars typ) bind_vars
  ;;

  let apply sub = function
    | Scheme (bind_vars, typ) ->
      let sub2 = TVarSet.fold (fun sub key -> Subst.remove key sub) bind_vars sub in
      Scheme (bind_vars, Subst.apply sub2 typ)
  ;;
end

module TypeEnv = struct
  (* A type enviroment is a map, the key of each element of which is a string,
     which is the name of the let-binding,
     and the key is the schema of the type of expression to which the name is bound. *)
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  let empty : t = Base.Map.empty (module Base.String)

  (* Free vars of a type environment is the set of all non-quantified
     type variables of all expressions in a given environment. *)
  let free_vars env =
    Base.Map.fold
      ~init:TVarSet.empty
      ~f:(fun ~key:_ ~data acc -> TVarSet.union acc (Scheme.free_vars data))
      env
  ;;

  (* Apply the substitution to each scheme from the enviroment. *)
  let apply env sub = Base.Map.map env ~f:(Scheme.apply sub)
  let extend env key schema = Base.Map.update ~f:(fun _ -> schema) env key
  let find = Base.Map.find

  let rec ext_by_pat env pat (Scheme (fvs, ty) as scheme) =
    match pat, ty with
    | PIdentifier id, _ -> extend env id scheme
    | PTuple ps, TTuple ts ->
      let env1 =
        Base.List.fold2 ps ts ~init:env ~f:(fun acc p t ->
          ext_by_pat acc p (Scheme (fvs, t)))
      in
      (match env1 with
       | Ok env -> env
       | _ -> env)
    | PCons (h, tl), TList t ->
      let env1 = ext_by_pat env h (Scheme (fvs, t)) in
      ext_by_pat env1 tl scheme
    | _ -> env
  ;;
end

open R
open R.Syntax

(* Take out a new state, which is a new “type variable”
   from the monad and wrap it in a type variable constructor. *)
let fresh_var = fresh >>| tvar

(* Create an expression type by using the altered scheme as follows:
   we take all the quantified variables in the type and replace them
   one by one with some type variable. *)
let instantiate (Scheme (bind_var, ty)) =
  TVarSet.fold
    (fun var_name acc ->
      let* acc = acc in
      let* fv = fresh_var in
      let* sub = Subst.singleton var_name fv in
      return (Subst.apply sub acc))
    bind_var
    (return ty)
;;

(* Reverse process: create a scheme by using a given type and environment. *)
let generalize : TypeEnv.t -> Type.t -> scheme =
  fun env ty ->
  let free = TVarSet.diff (Type.type_vars ty) (TypeEnv.free_vars env) in
  Scheme (free, ty)
;;

let generalize_rec env ty x =
  let env = Base.Map.remove env x in
  generalize env ty
;;

let lookup_env env name =
  (* If the passed name is defined in the enviroment,
     create its type according to the scheme and return it.
     Otherwise issue an error. *)
  match TypeEnv.find env name with
  | Some scheme ->
    let* ty = instantiate scheme in
    return (Subst.empty, ty)
    (* An empty substitution is needed here only for type matching. *)
  | None -> fail (`Unbound_variable name)
;;

let annotation_to_type =
  (* Convert a type annotation to a real type. *)
  let rec helper = function
    | AInt -> tint
    | ABool -> tbool
    | AUnit -> tunit
    | AFunction (l, r) -> tfunction (helper l) (helper r)
    | AList a -> tlist (helper a)
    | ATuple a -> ttuple @@ List.map (fun x -> helper x) a
  in
  helper
;;

let check_unique_vars =
  (* Checks that all variables in the pattern are unique.
     Used to detect severeal bound errors in tuple patterns,
     list constructor patterns. *)
  let rec helper var_set = function
    | PIdentifier v ->
      if VarSet.mem v var_set
      then
        (* If at least one variable is found twice, we raise an error. *)
        fail (`Several_bounds v)
      else return (VarSet.add v var_set)
    | PAny -> return var_set
    | PNill -> return var_set
    | PUnit -> return var_set
    | PConst _ -> return var_set
    | PTuple pattern_list -> RList.fold_left pattern_list ~init:(return var_set) ~f:helper
    | PCons (l, r) ->
      let* left_set = helper var_set l in
      helper left_set r
    | PConstraint (pat, _) -> helper var_set pat
  in
  helper VarSet.empty
;;

let check_unique_vars_list patterns =
  let rec helper var_set = function
    | [] -> return var_set
    | PIdentifier name :: rest ->
      if VarSet.mem name var_set
      then fail (`Several_bounds name)
      else helper (VarSet.add name var_set) rest
    | PAny :: rest -> helper var_set rest
    | PNill :: rest -> helper var_set rest
    | PUnit :: rest -> helper var_set rest
    | PConst _ :: rest -> helper var_set rest
    | PTuple ts :: rest -> helper var_set (ts @ rest)
    | PCons (hd, tl) :: rest ->
      let* var_set = helper var_set [ hd ] in
      helper var_set (tl :: rest)
    | PConstraint (pat, _) :: rest -> helper var_set (pat :: rest)
  in
  helper VarSet.empty patterns
;;

let infer_const c =
  let ty =
    match c with
    | CBool _ -> tbool
    | CInt _ -> tint
  in
  return (Subst.empty, ty)
;;

let infer_id env id =
  (* '_' - reserved for expressions whose result is not important to us. *)
  match id with
  | "_" ->
    let* fv = fresh_var in
    return (Subst.empty, fv)
  | _ -> lookup_env env id
;;

let infer_pattern =
  let rec helper env = function
    | PIdentifier id ->
      let* fv = fresh_var in
      let schema = Scheme (TVarSet.empty, fv) in
      let env = TypeEnv.extend env id schema in
      return (fv, env)
    | PAny ->
      let* fv = fresh_var in
      return (fv, env)
    | PNill ->
      let* fv = fresh_var in
      let ty = TList fv in
      return (ty, env)
    | PConst c ->
      let* _, ty = infer_const c in
      return (ty, env)
    | PUnit -> return (tunit, env)
    | PTuple pattern_list as tuple_p ->
      let* _ = check_unique_vars tuple_p in
      (* Check several bounds *)
      let* ty, env =
        (* Here is the list of types in reverse order. *)
        RList.fold_left
          pattern_list
          ~init:(return ([], env))
          ~f:(fun (acc, env) pattern ->
            let* ty1, env1 = helper env pattern in
            return (ty1 :: acc, env1))
      in
      let ty = ttuple (List.rev ty) in
      return (ty, env)
    | PCons (p1, p2) as cons_p ->
      let* _ = check_unique_vars cons_p in
      let* t1, env1 = helper env p1 in
      let* t2, env2 = helper env1 p2 in
      let* fv = fresh_var in
      let* sub1 = Subst.unify (tlist t1) fv in
      let* sub2 = Subst.unify t2 fv in
      let* sub3 = Subst.compose sub1 sub2 in
      let env3 = TypeEnv.apply env2 sub3 in
      let ty3 = Subst.apply sub3 fv in
      return (ty3, env3)
    | PConstraint (pat, an) ->
      let* ty, env1 = helper env pat in
      let* sub = Subst.unify ty (annotation_to_type an) in
      let env2 = TypeEnv.apply env1 sub in
      let ty2 = Subst.apply sub ty in
      return (ty2, env2)
  in
  helper
;;

let infer_expr =
  let rec helper env = function
    | EConst c -> infer_const c
    | EUnit -> return (Subst.empty, tunit)
    | EIdentifier id -> infer_id env id
    | ENill ->
      let* fv = fresh_var in
      return (Subst.empty, tlist fv)
    | EFun (pat, expr) ->
      let* ty1, env1 = infer_pattern env pat in
      let* sub, ty2 = helper env1 expr in
      let ty = tfunction ty1 ty2 in
      let result = Subst.apply sub ty in
      return (sub, result)
    | EApplication (f, arg) ->
      let* fv = fresh_var in
      let* sub1, f_ty = helper env f in
      let* sub2, arg_ty = helper (TypeEnv.apply env sub1) arg in
      let* sub = Subst.unify (tfunction arg_ty fv) (Subst.apply sub2 f_ty) in
      let* final_sub = Subst.compose_all [ sub1; sub2; sub ] in
      let ty = Subst.apply final_sub fv in
      return (final_sub, ty)
    | ECons (t1, t2) ->
      let* sub1, ty1 = helper env t1 in
      let env1 = TypeEnv.apply env sub1 in
      let* sub2, ty2 = helper env1 t2 in
      let* fv = fresh_var in
      let* sub3 = Subst.unify (tlist ty1) fv in
      let* sub4 = Subst.unify ty2 fv in
      let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
      let ty = Subst.apply sub fv in
      return (sub, ty)
    | EIf (cond, b1, b2) ->
      let* sub_cond, ty_cond = helper env cond in
      let* sub_b1, ty_b1 = helper env b1 in
      let* sub_b2, ty_b2 = helper env b2 in
      let* sub = Subst.unify ty_cond tbool in
      let* fv = fresh_var in
      let* sub1 = Subst.unify fv ty_b1 in
      let* sub2 = Subst.unify fv ty_b2 in
      let* subs = Subst.compose_all [ sub_cond; sub_b1; sub_b2; sub; sub1; sub2 ] in
      let ty = Subst.apply subs ty_b1 in
      return (subs, ty)
    | ETuple exprs ->
      let rec infer_tuple acc = function
        | [] -> return acc
        | hd :: tl ->
          let* sub1, ty1 = helper env hd in
          let acc_sub, acc_ty = acc in
          let* sub2 = Subst.compose sub1 acc_sub in
          let new_acc = sub2, ty1 :: acc_ty in
          infer_tuple new_acc tl
      in
      let acc = Subst.empty, [] in
      let* sub, ty = infer_tuple acc exprs in
      let ty_list = List.rev_map (Subst.apply sub) ty in
      let ty = ttuple ty_list in
      return (sub, ty)
    | EMatch (expr, cases) ->
      let* sub1, ty1 = helper env expr in
      let env1 = TypeEnv.apply env sub1 in
      let* fv = fresh_var in
      let f acc case =
        let acc_sub, acc_ty = acc in
        let pat, expr = case in
        let* pat_ty, pat_env = infer_pattern env1 pat in
        let* sub2 = Subst.unify ty1 pat_ty in
        let env2 = TypeEnv.apply pat_env sub2 in
        let* expr_sub, expr_ty = helper env2 expr in
        let* sub3 = Subst.unify expr_ty acc_ty in
        let* sub = Subst.compose_all [ acc_sub; expr_sub; sub2; sub3 ] in
        let ty = Subst.apply sub acc_ty in
        return (sub, ty)
      in
      RList.fold_left cases ~init:(return (sub1, fv)) ~f
    | ELetIn (NoRec, pat, e1, e2) ->
      let* s1, t1 = helper env e1 in
      let env = TypeEnv.apply env s1 in
      let s = generalize env t1 in
      let* t2, env1 = infer_pattern env pat in
      let env2 = TypeEnv.ext_by_pat env1 pat s in
      let* sub = Subst.unify t1 t2 in
      let* sub1 = Subst.compose sub s1 in
      let env3 = TypeEnv.apply env2 sub1 in
      let* s2, t2 = helper env3 e2 in
      let* s = Subst.compose sub1 s2 in
      return (s, t2)
    | ELetIn (Rec, pat, e1, e2) ->
      (match pat with
       | PIdentifier x ->
         let* fresh = fresh_var in
         let* t, e = infer_pattern env pat in
         let* ss = Subst.unify fresh t in
         let env = TypeEnv.apply e ss in
         let fresh = Subst.apply ss fresh in
         let env1 = TypeEnv.extend env x (Scheme (TVarSet.empty, fresh)) in
         let* s, t = helper env1 e1 in
         let* s1 = Subst.unify (Subst.apply s fresh) t in
         let* s2 = Subst.compose s s1 in
         let env = TypeEnv.apply env s2 in
         let t = Subst.apply s2 t in
         let s = generalize_rec env t x in
         let env = TypeEnv.extend env x s in
         let* sub, t = helper env e2 in
         let* sub = Subst.compose s2 sub in
         return (sub, t)
       | _ -> fail `InvalidRecLeftHand)
    | EConstraint (expr, expected_ty) ->
      let* sub1, ty = helper env expr in
      let* sub2 = Subst.unify ty (annotation_to_type expected_ty) in
      let* final_sub = Subst.compose sub1 sub2 in
      let final_ty = Subst.apply final_sub ty in
      return (final_sub, final_ty)
  in
  helper
;;

let update_name_list name names_list = name :: List.filter (( <> ) name) names_list

let infer_decl env name_list = function
  | NoRecDecl decls ->
    let rec ext_with_pat env name_list = function
      | PIdentifier name, ty ->
        let generalized_ty = generalize env ty in
        let new_names_list = update_name_list name name_list in
        return (TypeEnv.extend env name generalized_ty, new_names_list)
      | (PTuple ps as pat), (TVar _ as ty) ->
        let* tvs =
          List.fold_left
            (fun acc _ ->
              let* acc = acc in
              let* fv = fresh_var in
              return @@ (fv :: acc))
            (return [])
            ps
        in
        let new_ty = TTuple tvs in
        let* sub = Subst.unify ty new_ty in
        ext_with_pat (TypeEnv.apply env sub) name_list (pat, new_ty)
      | PTuple ps, TTuple ts when List.length ts = List.length ps ->
        List.fold_left2
          (fun acc pat ty ->
            let* env, name_list = acc in
            ext_with_pat env name_list (pat, ty))
          (return (env, name_list))
          ps
          ts
      | (PCons _ as pat), (TVar _ as ty) ->
        let* fv = fresh_var in
        let new_ty = TList fv in
        let* sub = Subst.unify ty new_ty in
        ext_with_pat (TypeEnv.apply env sub) name_list (pat, new_ty)
      | PCons (l, r), TList t ->
        let* env, name_list = ext_with_pat env name_list (l, t) in
        ext_with_pat env name_list (r, TList t)
      | PAny, _ | PNill, TList _ -> return (env, name_list)
      | (PConst _ as pat), pty ->
        let* pt, _ = infer_pattern env pat in
        let* _ = Subst.unify pt pty in
        return (env, name_list)
      | pat, ty ->
        let* typ, _ = infer_pattern env pat in
        fail @@ `Unification_failed (typ, ty)
    in
    let patterns = List.map (fun (DDeclaration (pat, _)) -> pat) decls in
    let* _ = check_unique_vars_list patterns in
    List.fold_left
      (fun acc (DDeclaration (pat, expr)) ->
        let* extended_env, name_list = acc in
        let* _, ty_expr = infer_expr env expr in
        let* extended_env, new_name_list =
          ext_with_pat extended_env name_list (pat, ty_expr)
        in
        return (extended_env, new_name_list))
      (return (env, name_list))
      decls
  | RecDecl decls ->
    let tmp_vars env decls =
      List.fold_left
        (fun acc (DDeclaration (pat, _)) ->
          let* env, vars = acc in
          match pat with
          | PIdentifier name ->
            let* fv = fresh_var in
            let env' = TypeEnv.extend env name (Scheme (TVarSet.empty, fv)) in
            return (env', (name, fv) :: vars)
          | _ -> fail `InvalidRecLeftHand)
        (return (env, []))
        decls
    in
    let infer_decls env decls temp_vars =
      List.fold_left
        (fun acc -> function
          | DDeclaration (PIdentifier name, expr) ->
            let* acc_env, acc_name_list = acc in
            let* sub', ty_expr = infer_expr env expr in
            let* tv =
              match List.assoc_opt name temp_vars with
              | Some temp_ty -> return temp_ty
              | None -> fail (`Unbound_variable name)
            in
            let* sub'' = Subst.unify tv ty_expr in
            let* final_sub = Subst.compose sub' sub'' in
            let final_typ = Subst.apply final_sub tv in
            let new_acc_env =
              TypeEnv.extend acc_env name (Scheme (TVarSet.empty, final_typ))
            in
            let new_acc_names_list = update_name_list name acc_name_list in
            return (new_acc_env, new_acc_names_list)
          | _ -> fail `InvalidRecLeftHand)
        (return (env, name_list))
        decls
    in
    let* extended_env, temp_vars = tmp_vars env decls in
    infer_decls extended_env decls temp_vars
;;

let start_env =
  let init_list =
    [ "( + )", TFunction (TInt, TFunction (TInt, TInt))
    ; "( - )", TFunction (TInt, TFunction (TInt, TInt))
    ; "( / )", TFunction (TInt, TFunction (TInt, TInt))
    ; "( * )", TFunction (TInt, TFunction (TInt, TInt))
    ; "( && )", TFunction (TBool, TFunction (TBool, TBool))
    ; "( || )", TFunction (TBool, TFunction (TBool, TBool))
    ; "( < )", TFunction (TVar 1, TFunction (TVar 1, TBool))
    ; "( > )", TFunction (TVar 1, TFunction (TVar 1, TBool))
    ; "( <= )", TFunction (TVar 1, TFunction (TVar 1, TBool))
    ; "( >= )", TFunction (TVar 1, TFunction (TVar 1, TBool))
    ; "( <> )", TFunction (TVar 1, TFunction (TVar 1, TBool))
    ; "( = )", TFunction (TVar 1, TFunction (TVar 1, TBool))
    ; "( == )", TFunction (TVar 1, TFunction (TVar 1, TBool))
    ; "( ~- )", TFunction (TInt, TInt)
    ; "( ~+ )", TFunction (TInt, TInt)
    ; "not", TFunction (TBool, TBool)
    ; "print_int", TFunction (TInt, TUnit)
    ; "is_empty", TFunction (TList (TVar 1), TBool)
    ; "list_head", TFunction (TList (TVar 1), TVar 1)
    ; "list_tail", TFunction (TList (TVar 1), TList (TVar 1))
    ; "tuple_element", TFunction (TVar 1, TFunction (TInt, TVar 2))
    ; "fail_match", TVar 1
    ]
  in
  let env = TypeEnv.empty in
  let bind env id typ = TypeEnv.extend env id (generalize env typ) in
  Base.List.fold_left init_list ~init:env ~f:(fun env (id, typ) -> bind env id typ)
;;

let infer_program program =
  let* env, id_list =
    Base.List.fold_left
      ~f:(fun acc item ->
        let* env, lst = acc in
        let* env, names_list = infer_decl env lst item in
        return (env, names_list))
      ~init:(return (start_env, []))
      program
  in
  return (env, List.rev id_list)
;;

let run_program_inferencer program = run (infer_program program)
