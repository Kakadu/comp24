(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Common.Typedtree
open Common.Errors
open Common.Base_lib

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
  val ident : int t
  val run : 'a t -> ('a, error) Result.t
end = struct
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Base.Result.fail e
  let return x last = last, Base.Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold_left xs ~init ~f =
      Base.Map.fold xs ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let ident : int t = fun last -> last, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  let rec occurs_in v = function
    | TVar b -> b = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TPrim _ -> false
    | TList t -> occurs_in v t
    | TTuple ts -> List.fold_left (fun ans t -> occurs_in v t || ans) false ts
  ;;

  let vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TPrim _ -> acc
      | TList t -> helper acc t
      | TTuple ts -> List.fold_left helper acc ts
    in
    helper VarSet.empty
  ;;
end

open R
open R.Syntax

let fresh_var = fresh >>| tvar
let poly_var s = ident >>| fun n -> tvar (n + Hashtbl.hash s)

module Subst : sig
  open Base

  type t = (fresh, ty, Int.comparator_witness) Map.t

  val empty : t
  val singleton : fresh -> ty -> t R.t
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open Base

  type t = (fresh, ty, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let mapping k v =
    if Type.occurs_in k v then fail @@ occurs_check (k, v) else return (k, v)
  ;;

  let singleton k v =
    let* k, v = mapping k v in
    return (Map.singleton (module Int) k v)
  ;;

  let find k xs = Map.find xs k
  let remove = Map.remove

  let apply s =
    let rec helper = function
      | TVar b as ty ->
        (match find b s with
         | None -> ty
         | Some x -> x)
      | TArrow (l, r) -> TArrow (helper l, helper r)
      | TList t -> tlist (helper t)
      | TTuple ts -> ttuple @@ List.map ts ~f:helper
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TPrim _, TPrim _ -> fail (unification_failed (l, r))
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TList t1, TList t2 -> unify t1 t2
    | TTuple t1, TTuple t2 ->
      (match
         List.fold2 t1 t2 ~init:(return empty) ~f:(fun acc e1 e2 ->
           let* subs = acc in
           let s1 = apply subs e1 in
           let s2 = apply subs e2 in
           let* sub = unify s1 s2 in
           let* final_subs = compose subs sub in
           return final_subs)
       with
       | Ok subs -> subs
       | _ -> fail (unification_failed (ttuple t1, ttuple t2)))
    | _ -> fail (unification_failed (l, r))

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  and extend k v s =
    match find k s with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2
  ;;

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose
end

module Scheme = struct
  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.vars t) bs
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;
end

module TypeEnv = struct
  open Base

  let empty = Map.empty (module String)
  let extend env (v, scheme) = Map.update env v ~f:(fun _ -> scheme)

  let rec extend_by_pattern (S (bs, ty) as scheme) acc pat =
    match pat, ty with
    | Pat_var v, _ -> extend acc (v, scheme)
    | Pat_cons (h, tl), TList t ->
      let env = extend_by_pattern (S (bs, t)) acc h in
      extend_by_pattern (S (bs, ty)) env tl
    | Pat_tuple es, TTuple ts ->
      let new_env =
        List.fold2 es ts ~init:acc ~f:(fun acc e t -> extend_by_pattern (S (bs, t)) acc e)
      in
      (match new_env with
       | Ok env -> env
       | _ -> acc)
    | _ -> acc
  ;;

  let free_vars =
    Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find = Map.find
end

let unify = Subst.unify

let instantiate (S (bs, t)) =
  VarSet.fold
    (fun name ty ->
      let* ty = ty in
      let* tv = fresh_var in
      let* s = Subst.singleton name tv in
      return (Subst.apply s ty))
    bs
    (return t)
;;

let generalize env ty is_rec ~pattern_name =
  let env =
    match is_rec, pattern_name with
    | Recursive, Some ident -> Base.Map.remove env ident
    | _ -> env
  in
  let free = VarSet.diff (Type.vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env e xs =
  match TypeEnv.find xs e with
  | None -> fail (unbound_variable e)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let pattern_vars =
  let rec helper acc = function
    | Pat_var v -> v :: acc
    | Pat_cons (e1, e2) -> helper (helper acc e1) e2
    | Pat_tuple es -> List.fold_left helper acc es
    | _ -> acc
  in
  helper []
;;

let check_several_bounds names env ty =
  RList.fold_left
    names
    ~init:(return ([], env, ty))
    ~f:(fun (acc, env, ty) name ->
      if List.exists (String.equal name) acc
      then fail (several_bounds name)
      else return (name :: acc, env, ty))
;;

let const_infer fst = function
  | Const_int _ -> return (fst, tint)
  | Const_bool _ -> return (fst, tbool)
  | Const_nil ->
    let* tv = fresh_var in
    return (fst, tlist tv)
  | Const_unit -> return (fst, tunit)
;;

let rec convert_ty_annot = function
  | Ptyp_int -> return tint
  | Ptyp_bool -> return tbool
  | Ptyp_unit -> return tunit
  | Ptyp_var s ->
    let* var = poly_var s in
    return var
  | Ptyp_list t ->
    let* el_ty = convert_ty_annot t in
    return @@ tlist el_ty
  | Ptyp_tuple ts ->
    let* ty =
      RList.fold_left ts ~init:(return []) ~f:(fun ts t ->
        let* p1 = convert_ty_annot t in
        return @@ (p1 :: ts))
    in
    return @@ ttuple ty
  | Ptyp_arrow (t, t') ->
    let* t1 = convert_ty_annot t in
    let* t2 = convert_ty_annot t' in
    return @@ tarrow t1 t2
;;

let pattern_infer =
  let rec helper env = function
    | Pat_const c -> const_infer env c
    | Pat_var v ->
      let* var = fresh_var in
      let env' = TypeEnv.extend env (v, S (VarSet.empty, var)) in
      return (env', var)
    | Pat_cons (p, p') as pat ->
      let* e, t = helper env p in
      let* e', t' = helper e p' in
      let* subst = unify (tlist t) t' in
      let env = TypeEnv.apply subst e' in
      let t' = Subst.apply subst t' in
      let* _, env, t' = check_several_bounds (pattern_vars pat) env t' in
      return (env, t')
    | Pat_any ->
      let* tv = fresh_var in
      return (env, tv)
    | Pat_tuple ps as pat ->
      let* env, ty =
        RList.fold_left
          ps
          ~init:(return (env, []))
          ~f:(fun (env, ts) pat ->
            let* env1, p1 = helper env pat in
            return (env1, p1 :: ts))
      in
      let* _, env, ty = check_several_bounds (pattern_vars pat) env (ttuple ty) in
      return (env, ty)
    | Pat_constraint (p, t) ->
      let* env', t' = helper env p in
      let* t_sp = convert_ty_annot t in
      let* sub = Subst.unify t' t_sp in
      let env = TypeEnv.apply sub env' in
      return (env, Subst.apply sub t')
  in
  helper
;;

let saturate_env env bindings =
  let* env =
    RList.fold_left bindings ~init:(return env) ~f:(fun acc { vb_pat; _ } ->
      let* env, _ = pattern_infer acc vb_pat in
      return env)
  in
  return env
;;

let rec infer =
  let rec helper env = function
    | Exp_constant c -> const_infer Subst.empty c
    | Exp_ident v -> lookup_env v env
    | Exp_apply (e, e') ->
      let* tv = fresh_var in
      let* s1, t1 = helper env e in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e' in
      let* sub = unify (tarrow t2 tv) (Subst.apply s2 t1) in
      let* final_subs = Subst.compose_all [ s1; s2; sub ] in
      let trez = Subst.apply final_subs tv in
      return (final_subs, trez)
    | Exp_function (v, e) ->
      let* env', t = pattern_infer env v in
      let* s, t' = helper env' e in
      let ty = tarrow (Subst.apply s t) t' in
      return (s, ty)
    | Exp_ifthenelse (i, th, e) ->
      let* si, ti = helper env i in
      let* st, tt = helper env th in
      let* se, te = helper env e in
      let* sub = unify ti tbool in
      let* tv = fresh_var in
      let* sub1 = unify tv tt in
      let* sub2 = unify tv te in
      let* final_subs = Subst.compose_all [ si; st; se; sub; sub1; sub2 ] in
      return (final_subs, Subst.apply final_subs tt)
    | Exp_let (decl, e) ->
      let* sub1, env3 = infer_decl env decl in
      let* s2, t2 = helper env3 e in
      let* final_subs = Subst.compose_all [ sub1; s2 ] in
      return (final_subs, t2)
    | Exp_match (match_exp, cases) ->
      let* s, tcond = helper env match_exp in
      let env = TypeEnv.apply s env in
      let* tv = fresh_var in
      RList.fold_left
        cases
        ~init:(return (s, tv))
        ~f:(fun (s, t) (p, e) ->
          let* env, tp = pattern_infer env p in
          let* s' = unify tcond tp in
          let* sub, te = helper env e in
          let* sub' = unify t te in
          let* final_subs = Subst.compose_all [ sub'; sub; s'; s ] in
          return (final_subs, Subst.apply final_subs t))
    | Exp_tuple es ->
      let* s, t =
        RList.fold_left
          ~f:(fun (s, ty) e ->
            let* s', ty' = helper env e in
            let* sub = Subst.compose s' s in
            return (sub, ty' :: ty))
          ~init:(return (Subst.empty, []))
          es
      in
      return (s, ttuple (List.rev_map (Subst.apply s) t))
    | Exp_list (e, e') ->
      let* s1, t1 = helper env e in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e' in
      let* subst = unify (tlist t1) t2 in
      let* final_subs = Subst.compose_all [ s1; s2; subst ] in
      let trez = Subst.apply final_subs t2 in
      return (final_subs, trez)
    | Exp_type (e, t) ->
      let* s1, t1 = helper env e in
      let* t2 = convert_ty_annot t in
      let* sub = Subst.unify t1 t2 in
      let trez = Subst.apply sub t1 in
      let* final_subs = Subst.compose_all [ s1; sub ] in
      return (final_subs, trez)
  in
  helper

and infer_decl env = function
  | Decl (Nonrecursive, [ { vb_pat; vb_expr } ]) ->
    let* s1, t1 = infer env vb_expr in
    let scheme = generalize (TypeEnv.apply s1 env) t1 Nonrecursive ~pattern_name:None in
    let* env1, t2 = pattern_infer env vb_pat in
    let env2 = TypeEnv.extend_by_pattern scheme env1 vb_pat in
    let* sub = unify t1 t2 in
    let* sub1 = Subst.compose s1 sub in
    let env3 = TypeEnv.apply sub1 env2 in
    return (sub1, env3)
  | Decl (Nonrecursive, _) -> fail not_specify_rec
  | Decl (Recursive, bindings) ->
    let* env = saturate_env env bindings in
    RList.fold_left
      bindings
      ~init:(return (Subst.empty, env))
      ~f:(fun (acc_subs, acc_env) { vb_pat; vb_expr } ->
        match vb_pat with
        | Pat_var v ->
          let* tv = fresh_var in
          let env = TypeEnv.extend acc_env (v, S (VarSet.empty, tv)) in
          let* s1, t1 = infer env vb_expr in
          let* s2 = unify (Subst.apply s1 tv) t1 in
          let* s = Subst.compose s2 s1 in
          let env = TypeEnv.apply s env in
          let t2 = generalize env (Subst.apply s tv) Recursive ~pattern_name:(Some v) in
          let* final_subs = Subst.compose_all [ acc_subs; s ] in
          return (final_subs, TypeEnv.extend env (v, t2))
        | _ -> fail no_variable_rec)
;;

let env_with_base_lib funcs env =
  let bind_ty env id typ =
    TypeEnv.extend env (id, generalize env typ Nonrecursive ~pattern_name:None)
  in
  RList.fold_left funcs ~init:(return env) ~f:(fun env' (id, tp) ->
    let* typ = convert_ty_annot tp in
    return @@ bind_ty env' id typ)
;;

let predef_funcs =
  let open LibF in
  let get_decl f =
    let _, tp, _ = get_type f in
    get_name f, tp
  in
  List.map get_decl (sys_funcs @ user_funcs)
;;

let check_program_internal funcs program =
  let helper env =
    RList.fold_left program ~init:(env_with_base_lib funcs env) ~f:(fun env ->
        function
        | Str_eval e ->
          let* _, _ = infer env e in
          return env
        | Str_value d ->
          let* _, env = infer_decl env d in
          return env)
  in
  run (helper TypeEnv.empty)
;;

let check_program = check_program_internal predef_funcs

module PP = struct
  (* Convert binders for further replacement with strings
     Example: type '10 * '22 -> '11 will be replaced by '0 * '1 -> '2 *)
  let reconstruct_binders ty =
    let open Base in
    let empty = Map.empty (module Int) in
    (* k - old binder, v - new binder*)
    let find_insert (k, v) binders =
      match Map.find binders k with
      | Some _ -> v, binders
      | None -> v + 1, Map.update binders k ~f:(fun _ -> v)
    in
    let repls =
      let rec helper (last, acc) = function
        | TVar n -> find_insert (n, last) acc
        | TArrow (l, r) ->
          let last1, acc1 = helper (last, acc) l in
          helper (last1, acc1) r
        | TList t -> helper (last, acc) t
        | TTuple ts -> List.fold_left ts ~init:(last, acc) ~f:helper
        | TPrim _ -> last, acc
      in
      snd @@ helper (0, empty) ty
    in
    let rec construct = function
      | TVar n -> tvar (Map.find_exn repls n)
      | TArrow (l, r) -> tarrow (construct l) (construct r)
      | TList t -> tlist (construct t)
      | TTuple ts -> ttuple (List.map ts ~f:construct)
      | other -> other
    in
    construct ty
  ;;

  let convert_to_string binder =
    let base = 97 in
    let letter = Char.chr (base + (binder mod 26)) in
    let number = binder / 26 in
    String.make 1 letter ^ if number = 0 then "" else string_of_int number
  ;;

  let pp_type ppf ty =
    let new_ty = reconstruct_binders ty in
    let open Format in
    let rec helper ppf = function
      | TVar n, _ -> fprintf ppf "'%s" (convert_to_string n)
      | TPrim s, _ -> pp_print_string ppf s
      | TArrow (l, r), pos ->
        let arrow ppf pos =
          match l with
          | TArrow (_, _) -> fprintf ppf "(%a) -> %a" helper (l, false) helper (r, pos)
          | _ -> fprintf ppf "%a -> %a" helper (l, pos) helper (r, pos)
        in
        if pos then fprintf ppf "(%a)" arrow pos else fprintf ppf "%a" arrow pos
      | TList t, pos -> fprintf ppf "%a list" helper (t, pos)
      | TTuple ts, pos ->
        let pp_tuple ppf (ts, pos) =
          fprintf
            ppf
            "%a"
            (pp_print_list
               ~pp_sep:(fun ppf () -> fprintf ppf " * ")
               (fun ppf t -> fprintf ppf "%a" helper (t, pos)))
            ts
        in
        if pos
        then fprintf ppf "(%a)" pp_tuple (ts, pos)
        else fprintf ppf "%a" pp_tuple (ts, true)
    in
    helper ppf (new_ty, false)
  ;;

  let stand_alone_init_env =
    run (env_with_base_lib predef_funcs TypeEnv.empty)
    |> function
    | Ok env -> env
    | Error _ -> raise @@ Failure "Impossible case"
  ;;

  let pp_program ppf env =
    Base.Map.iteri env ~f:(fun ~key:v ~data:(S (_, ty)) ->
      match TypeEnv.find stand_alone_init_env v with
      | Some (S (_, init_ty)) when ty = init_ty -> ()
      | _ -> Format.fprintf ppf "val %s: %a\n" v pp_type ty)
  ;;

  let map_binder t1 t2 binder =
    let rec helper acc = function
      | TVar n1, TVar n2 when n1 = binder -> convert_to_string n2
      | TVar _, TVar _ -> acc
      | TArrow (l1, r1), TArrow (l2, r2) ->
        let acc = helper acc (l1, l2) in
        helper acc (r1, r2)
      | TList t1, TList t2 -> helper acc (t1, t2)
      | TTuple ts1, TTuple ts2 -> List.fold_left helper acc (List.combine ts1 ts2)
      | _ -> acc
    in
    helper "" (t1, t2)
  ;;

  let pp_error ppf = function
    | Occurs_check (k, v) ->
      let v1 = reconstruct_binders v in
      let literal = map_binder v v1 k in
      Format.fprintf ppf "The type variable '%s occurs inside %a" literal pp_type v
    | Unbound_variable s -> Format.fprintf ppf "Unbound value '%s'" s
    | Unification_failed (l, r) ->
      Format.fprintf
        ppf
        "This expression has type %a but an expression was expected of type %a"
        pp_type
        l
        pp_type
        r
    | Several_bounds s -> Format.fprintf ppf "Variable %s is bound several times" s
    | Not_specify_rec ->
      Format.fprintf ppf "This definition is recursive but is missing the 'rec' keyword"
    | No_variable_rec ->
      Format.fprintf ppf "Only variables are allowed as left-side of 'let rec'"
  ;;
end
