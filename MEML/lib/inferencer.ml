(** Copyright 2023-2024, Perevalov Efim, Dyachkov Vitaliy *)

(** SPDX-License-Identifier: LGPL-3.0 *)

open Base
open Ast
open Ty

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  (int, 'a, Base.Int.comparator_witness) Base.Map.t
      -> init:'b t
      -> f:('b -> int * 'a -> 'b t)
      -> 'b t
  end

  val fresh : int t
  val get_fresh : int t
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

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f acc (key, data))
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let get_fresh : int t = fun last -> last, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TList t -> occurs_in v t
    | TTuple ts -> List.exists ts ~f:(occurs_in v)
    | TPrim _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TTuple ts -> List.fold ts ~init:acc ~f:helper
      | TList t -> helper acc t
      | TPrim _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t R.t
  val find : fresh -> t -> ty option
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  type t = (fresh, ty, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* k, v = mapping k v in
    return (Map.singleton (module Int) k v)
  ;;

  let find k xs = Base.Map.find xs k
  let remove = Map.remove

  let apply s =
    let rec helper = function
      | TVar b as ty ->
        (match find b s with
         | None -> ty
         | Some x -> x)
      | TArrow (l, r) -> TArrow (helper l, helper r)
      | TList t -> list_typ (helper t)
      | TTuple ts -> tuple_typ (List.map ~f:helper ts)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    let unify_lists l1 l2 =
      let subs =
        List.fold2 l1 l2 ~init:(return empty) ~f:(fun subs a b ->
          let* subs = subs in
          let sa = apply subs a in
          let sb = apply subs b in
          let* sub1 = unify sa sb in
          compose subs sub1)
      in
      match subs with
      | Ok res -> res
      | Unequal_lengths -> fail (`Unification_failed (l, r))
    in
    match l, r with
    | TVar a, TVar b when Int.equal a b -> return empty
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TList a, TList b -> unify a b
    | TTuple a, TTuple b -> unify_lists a b
    | _ -> fail (`Unification_failed (l, r))

  and extend s (k, v) =
    match find k s with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun acc (k, v) ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Map.add_exn acc ~key:k ~data:v))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc ss ->
      let* acc = acc in
      compose acc ss)
  ;;
end

module VarSet = struct
  include VarSet

  let fold_left_m f acc set =
    fold
      (fun x acc ->
         let open R.Syntax in
         let* acc = acc in
         f acc x)
      acc
      set
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;
end

module TypeEnv = struct
  open Base

  type t = (name, scheme, String.comparator_witness) Map.t

  let extend env (v, scheme) = Map.update env v ~f:(fun _ -> scheme)
  let remove = Map.remove
  let empty = Map.empty (module String)

  let free_vars : t -> VarSet.t =
    Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find x env = Map.find env x

  let rec ext (S (sub, type_var) as schema) env_ pat =
    match pat, type_var with
    | PVar (v, TUnknown), _ -> extend env_ (v, schema)
    | PCon (h, tl), TList t ->
      let env = ext (S (sub, t)) env_ h in
      ext (S (sub, type_var)) env tl
    | PTuple es, TTuple ts ->
      let new_env =
        List.fold2 es ts ~init:env_ ~f:(fun env_ e t -> ext (S (sub, t)) env_ e)
      in
      (match new_env with
       | Ok env_ -> env_
       | _ -> env_)
    | _ -> env_
  ;;
end

open R
open R.Syntax

let fresh_var = fresh >>| fun n -> TVar n

let instantiate : scheme -> ty R.t =
  fun (S (bs, t)) ->
  VarSet.fold_left_m
    (fun typ name ->
       let* f1 = fresh_var in
       let* s = Subst.singleton name f1 in
       return (Subst.apply s typ))
    bs
    (return t)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let generalize_rec env ty x =
  let env = TypeEnv.remove env x in
  generalize env ty
;;

let rec annot_to_ty = function
  | TInt -> int_typ
  | TBool -> bool_typ
  | TArrow (a1, a2) -> arrow (annot_to_ty a1) (annot_to_ty a2)
  | TUnknown -> failwith "TUnknown"
;;

let unify_annot an ty =
  match an with
  | Some an ->
    let* sub = Subst.unify (annot_to_ty an) ty in
    return (Subst.apply sub ty)
  | None -> return ty
;;

open R

let pattern_inf =
  let rec helper env = function
    | PWild ->
      let* int = fresh_var in
      return (env, int)
    | PConst c ->
      (match c with
       | CInt _ -> return (env, int_typ)
       | CBool _ -> return (env, bool_typ)
       | CNil ->
         let* int = fresh_var in
         return (env, list_typ int))
    | PVar ("()", _) -> return (env, unit_typ)
    | PVar (x, TUnknown) ->
      let* int = fresh_var in
      let env = TypeEnv.extend env (x, S (VarSet.empty, int)) in
      return (env, int)
    | PCon (p1, p2) ->
      let* env1, t1 = helper env p1 in
      let* env2, t2 = helper env1 p2 in
      let* int = fresh_var in
      let* sub_uni = Subst.unify t2 (list_typ int) in
      let t2 = Subst.apply sub_uni t2 in
      let* s3 = Subst.unify (list_typ t1) t2 in
      let* final_sub = Subst.compose_all [ s3; sub_uni ] in
      let env = TypeEnv.apply final_sub env2 in
      return (env, Subst.apply final_sub t2)
    | PTuple pl ->
      let* env, tl =
        List.fold_left
          ~f:(fun acc pat ->
            let* env1, tl = acc in
            let* env2, t = helper env1 pat in
            return (env2, t :: tl))
          ~init:(return (env, []))
          pl
      in
      return (env, tuple_typ (List.rev tl))
    | PVar (name, an) ->
      let* int = fresh_var in
      let env = TypeEnv.extend env (name, S (VarSet.empty, int)) in
      let* env1, t1 = return (env, int) in
      let* sub = Subst.unify t1 (annot_to_ty an) in
      let env = TypeEnv.apply sub env1 in
      return (env, Subst.apply sub t1)
  in
  helper
;;

let op_str = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | And -> "And"
  | Or -> "Or"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Less -> "Less"
  | Gre -> "Gre"
  | Leq -> "Leq"
  | Greq -> "Greq"
;;

let expression_infer =
  let rec helper env = function
    | EConst c ->
      (match c with
       | CInt _ -> return (Subst.empty, int_typ)
       | CBool _ -> return (Subst.empty, bool_typ)
       | CNil ->
         let* int = fresh_var in
         return (Subst.empty, list_typ int))
    | EVar ("()", _) -> return (Subst.empty, unit_typ)
    | EVar (x, TUnknown) ->
      (match TypeEnv.find x env with
       | Some s ->
         let* t = instantiate s in
         return (Subst.empty, t)
       | None -> fail (`No_variable x))
    | EIfElse (i, t, e) ->
      let* sub1, t1 = helper env i in
      let* sub2, t2 = helper (TypeEnv.apply sub1 env) t in
      let* sub3, t3 = helper (TypeEnv.apply sub2 env) e in
      let* sub4 = Subst.unify t1 bool_typ in
      let* sub5 = Subst.unify t2 t3 in
      let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4; sub5 ] in
      return (sub, Subst.apply sub t2)
    | EMatch (e, cl) ->
      let* sub1, t1 = helper env e in
      let env = TypeEnv.apply sub1 env in
      let* int = fresh_var in
      let* sub, t =
        List.fold_left
          ~f:(fun acc (pat, exp) ->
            let* sub1, t = acc in
            let* env1, pt = pattern_inf env pat in
            let* sub2 = Subst.unify t1 pt in
            let env2 = TypeEnv.apply sub2 env1 in
            let* sub3, t' = helper env2 exp in
            let* sub4 = Subst.unify t' t in
            let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
            return (sub, Subst.apply sub t))
          ~init:(return (sub1, int))
          cl
      in
      return (sub, t)
    | ELetIn (Rec, x, e1, e2) ->
      let* int = fresh_var in
      let env1 = TypeEnv.extend env (x, S (VarSet.empty, int)) in
      let* s1, t1 = helper env1 e1 in
      let* s2 = Subst.unify (Subst.apply s1 int) t1 in
      let* s3 = Subst.compose s1 s2 in
      let env2 = TypeEnv.apply s3 env in
      let t1 = Subst.apply s3 t1 in
      let s = generalize (TypeEnv.remove env2 x) t1 in
      let env3 = TypeEnv.extend env2 (x, s) in
      let* s4, t2 = helper env3 e2 in
      let* s5 = Subst.compose s3 s4 in
      return (s5, t2)
    | ELetIn (Notrec, pat, e1, e2) ->
      let pat = PVar (pat, TUnknown) in
      let* s1, t1 = helper env e1 in
      let env = TypeEnv.apply s1 env in
      let s = generalize env t1 in
      let* env1, t2 = pattern_inf env pat in
      let env2 = TypeEnv.ext s env1 pat in
      let* sub = Subst.unify t2 t1 in
      let* sub1 = Subst.compose sub s1 in
      let env3 = TypeEnv.apply sub1 env2 in
      let* s2, t2 = helper env3 e2 in
      let* s = Subst.compose sub1 s2 in
      return (s, t2)
    | ELetPatIn (pat, e1, e2) ->
      let* s1, t1 = helper env e1 in
      let env = TypeEnv.apply s1 env in
      let s = generalize env t1 in
      let* env1, t2 = pattern_inf env pat in
      let env2 = TypeEnv.ext s env1 pat in
      let* sub = Subst.unify t2 t1 in
      let* sub1 = Subst.compose sub s1 in
      let env3 = TypeEnv.apply sub1 env2 in
      let* s2, t2 = helper env3 e2 in
      let* s = Subst.compose sub1 s2 in
      return (s, t2)
    | EFun (p, e) ->
      let* env, t = pattern_inf env p in
      let* sub, t1 = helper env e in
      return (sub, Subst.apply sub (arrow t t1))
    | ETuple el ->
      let* sub, t =
        List.fold_left
          ~f:(fun acc e ->
            let* sub, t = acc in
            let* sub1, t1 = helper env e in
            let* sub2 = Subst.compose sub sub1 in
            return (sub2, t1 :: t))
          ~init:(return (Subst.empty, []))
          el
      in
      return (sub, tuple_typ (List.rev_map ~f:(Subst.apply sub) t))
    | EApp (e1, e2, TUnknown) ->
      let* int = fresh_var in
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
      let* s3 = Subst.unify (arrow t2 int) (Subst.apply s2 t1) in
      let* sub = Subst.compose_all [ s1; s2; s3 ] in
      let t = Subst.apply sub int in
      return (sub, t)
    | EVar (name, an) ->
      let* sub1, t1 = helper env (EVar (name, TUnknown)) in
      let* sub2 = Subst.unify t1 (annot_to_ty an) in
      let* sub = Subst.compose sub1 sub2 in
      return (sub, Subst.apply sub t1)
    | EApp (exp1, exp2, an) ->
      let* sub1, t1 = helper env (EApp (exp1, exp2, TUnknown)) in
      let* sub2 = Subst.unify t1 (annot_to_ty an) in
      let* sub = Subst.compose sub1 sub2 in
      return (sub, Subst.apply sub t1)
    | EBinaryOp (op, e1, e2) ->
      helper env (EApp (EApp (EVar (op_str op, TUnknown), e1, TUnknown), e2, TUnknown))
    | EList _ -> failwith "elist"
  in
  helper
;;

let bindings_infer env = function
  | Let (Rec, bindings) ->
    (* Create int type variables for all bindings *)
    let* fresh_vars =
      List.fold_left
        ~f:(fun acc (pat, _) ->
          let* acc_vars = acc in
          let* int = fresh_var in
          match pat with
          | PVar (x, _) -> return ((x, int) :: acc_vars)
          | _ -> fail `Bad_let)
        ~init:(return [])
        bindings
    in
    (* Extend environment with int type variables *)
    let initial_env =
      List.fold_left
        ~f:(fun acc (x, int) -> TypeEnv.extend acc (x, S (VarSet.empty, int)))
        ~init:env
        fresh_vars
    in
    (* Rest of the function remains the same *)
    let* subst_types =
      List.fold_left
        ~f:(fun acc (pat, exp) ->
          let* acc_subst_types = acc in
          let* s, t = expression_infer initial_env exp in
          match pat with
          | PVar (x, _) -> return ((x, s, t) :: acc_subst_types)
          | _ -> fail `Not_solo_var)
        ~init:(return [])
        bindings
    in
    let* final_subst =
      List.fold_left
        ~f:(fun acc (x, s, t) ->
          let* acc_subst = acc in
          let int = List.Assoc.find_exn ~equal:String.equal fresh_vars x in
          let* s1 = Subst.unify (Subst.apply s int) t in
          let* s2 = Subst.compose acc_subst s in
          let* s3 = Subst.compose s2 s1 in
          return s3)
        ~init:(return Subst.empty)
        subst_types
    in
    let env = TypeEnv.apply final_subst env in
    let final_env =
      List.fold_left
        ~f:(fun acc (x, _, t) ->
          let final_type = Subst.apply final_subst t in
          let scheme = generalize (TypeEnv.remove acc x) final_type in
          TypeEnv.extend acc (x, scheme))
        ~init:env
        subst_types
    in
    return final_env
  | Let (Notrec, [ (pattern_, e) ]) ->
    (* Original non-recursive case remains unchanged *)
    let* s, type1 = expression_infer env e in
    let env = TypeEnv.apply s env in
    let sc = generalize env type1 in
    let* env1, type2 = pattern_inf env pattern_ in
    let env2 = TypeEnv.ext sc env1 pattern_ in
    let* sub = Subst.unify type1 type2 in
    let* sub1 = Subst.compose s sub in
    let env3 = TypeEnv.apply sub1 env2 in
    return env3
  | Expression e ->
    let* _, _ = expression_infer env e in
    return env
  | _ -> fail `Bad_let
;;

let start_env =
  let bin_op_list =
    [ "Add", TArrow (TPrim "int", TArrow (TPrim "int", TPrim "int"))
    ; "Sub", TArrow (TPrim "int", TArrow (TPrim "int", TPrim "int"))
    ; "Mul", TArrow (TPrim "int", TArrow (TPrim "int", TPrim "int"))
    ; "Div", TArrow (TPrim "int", TArrow (TPrim "int", TPrim "int"))
    ; "And", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "Or", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "Eq", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "Neq", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "Less", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "Gre", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "Leq", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "Greq", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "print_int", TArrow (TPrim "int", TPrim "unit")
    ]
  in
  let env = TypeEnv.empty in
  let bind env id typ = TypeEnv.extend env (id, generalize env typ) in
  List.fold_left bin_op_list ~init:env ~f:(fun env (id, typ) -> bind env id typ)
;;

let statments_infer structure =
  List.fold_left
    ~f:(fun acc item ->
      let* env = acc in
      let* env = bindings_infer env item in
      return env)
    ~init:(return start_env)
    structure
;;

let run_infer s = run (statments_infer s)
