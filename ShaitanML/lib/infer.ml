(** Copyright 2024-2025, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typedtree
open Base

type fresh = int [@@deriving eq]

type error =
  [ `Occurs_check (** Type variable occurs inside type it must be unified with *)
  | `Unbound_variable of id (** Unbound variable *)
  | `Unification_failed of ty * ty (** Failed to unify two types *)
  | `NotVar
  | `NotStrItem
  | `Let_rec_lhs
  ]

type id = string

let pp_error ppf : error -> _ =
  let open Stdlib.Format in
  function
  | `Occurs_check -> fprintf ppf "Occurs check failed"
  | `Unbound_variable s -> fprintf ppf "Unbound variable '%s'" s
  | `Unification_failed (l, r) ->
    fprintf ppf "Unification failed on %a and %a" pp_typ l pp_typ r
  | `NotVar -> fprintf ppf "Unexpected pattern, expected variable'"
  | `NotStrItem -> fprintf ppf "Unexpected structure item, expected let or let rec"
  | `Let_rec_lhs -> fprintf ppf "Left-hand side of let rec should be a variable"
;;

module VarSet = struct
  include Stdlib.Set.Make (Int)
end

type scheme = S of VarSet.t * ty (** \forall a1 a2 ... an . ty *)

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t

  module RMap : sig
    val fold : ('a, 'b, 'c) Map.t -> init:'d t -> f:('a -> 'b -> 'd -> 'd t) -> 'd t
  end
end = struct
  (** A composition: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Result.fail x
    | Result.Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Base.Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun m f st ->
    match m st with
    | st, Ok x -> st, Result.return (f x)
    | st, Result.Error e -> st, Result.fail e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  let fresh : int t = fun last -> last + 1, Result.return last
  let run m = snd (m 0)

  module RMap = struct
    let fold m ~init ~f =
      Map.fold m ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end
end

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TTuple tl -> Base.List.exists tl ~f:(occurs_in v)
    | TList t -> occurs_in v t
    | TPrim _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TTuple tl -> List.fold_left tl ~init:acc ~f:helper
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
  val find : t -> fresh -> ty option
  val remove : t -> fresh -> t
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
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

  let find = Map.find
<<<<<<< HEAD
=======

>>>>>>> ae67fa7 (fix parser and infer for the sake of manytests)
  let remove = Map.remove

  let apply s =
    let rec helper = function
      | TVar b as ty ->
        (match find s b with
         | None -> ty
         | Some x -> x)
      | TArrow (l, r) -> arrow (helper l) (helper r)
      | TList t -> list_typ (helper t)
      | TTuple ts -> tuple_typ (List.map ~f:helper ts)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* s1 = unify l1 l2 in
      let* s2 = unify (apply s1 r1) (apply s1 r2) in
      compose s1 s2
    | TList t1, TList t2 -> unify t1 t2
    | TTuple ts1, TTuple ts2 ->
      (match
         List.fold2
           ts1
           ts2
           ~f:(fun acc t1 t2 ->
             let* acc = acc in
             let* s = unify (apply acc t1) (apply acc t2) in
             compose acc s)
           ~init:(return empty)
       with
       | Unequal_lengths -> fail (`Unification_failed (l, r))
       | Ok s -> s)
    | _ -> fail (`Unification_failed (l, r))

  and extend k v s =
    match find s k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold s2 ~init:(return s1) ~f:extend

  let compose_all ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v (S (xs, t)) = (not (VarSet.mem v xs)) && Type.occurs_in v t
  let free_vars (S (xs, t)) = VarSet.diff (Type.free_vars t) xs

  let apply s (S (xs, t)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) xs s in
    S (xs, Subst.apply s2 t)
  ;;
end

module TypeEnv = struct
  open Base

  type t = (id, scheme, String.comparator_witness) Map.t

  let extend env (v, scheme) = Map.update env v ~f:(fun _ -> scheme)
<<<<<<< HEAD
=======

>>>>>>> ae67fa7 (fix parser and infer for the sake of manytests)
  let remove = Map.remove
  let empty = Map.empty (module String)

  let free_vars : t -> VarSet.t =
    Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find x env = Map.find env x

  let rec ext_by_pat (S (sub, type_var) as schema) env_ pat =
    match pat, type_var with
    | PVar v, _ -> extend env_ (v, schema)
    | PCons (h, tl), TList t ->
      let env = ext_by_pat (S (sub, t)) env_ h in
      ext_by_pat (S (sub, type_var)) env tl
    | PTuple es, TTuple ts ->
      let new_env =
        List.fold2 es ts ~init:env_ ~f:(fun env_ e t -> ext_by_pat (S (sub, t)) env_ e)
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
  fun (S (xs, ty)) ->
  VarSet.fold
    (fun name typ ->
      let* typ = typ in
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    xs
    (return ty)
;;

let generalize env ty =
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let generalize_rec env ty x =
  let env = TypeEnv.remove env x in
  generalize env ty
;;

let rec annot_to_ty = function
  | AInt -> int_typ
  | ABool -> bool_typ
  | AString -> string_typ
  | AUnit -> unit_typ
  | AList a -> list_typ (annot_to_ty a)
  | AFun (a1, a2) -> arrow (annot_to_ty a1) (annot_to_ty a2)
  | ATuple al -> tuple_typ (List.map al ~f:annot_to_ty)
  | AVar id -> TVar (Hashtbl.hash id)
;;

let unify_annot an ty =
  match an with
  | Some an ->
    let* sub = Subst.unify (annot_to_ty an) ty in
    return (Subst.apply sub ty)
  | None -> return ty
;;

open R

let infer_pat =
  let rec helper env = function
    | PAny ->
      let* fresh = fresh_var in
      return (env, fresh)
    | PConst c ->
      (match c with
       | CInt _ -> return (env, int_typ)
       | CBool _ -> return (env, bool_typ)
       | CString _ -> return (env, string_typ)
       | CUnit -> return (env, unit_typ)
       | CNil ->
         let* fresh = fresh_var in
         return (env, list_typ fresh))
    | PVar x ->
      let* fresh = fresh_var in
      let env = TypeEnv.extend env (x, S (VarSet.empty, fresh)) in
      return (env, fresh)
    | PCons (p1, p2) ->
      let* env1, t1 = helper env p1 in
      let* env2, t2 = helper env1 p2 in
      let* fresh = fresh_var in
      let* sub_uni = Subst.unify t2 (list_typ fresh) in
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
    | PConstraint (pat, an) ->
      let* env1, t1 = helper env pat in
      let* sub = Subst.unify t1 (annot_to_ty an) in
      let env = TypeEnv.apply sub env1 in
      return (env, Subst.apply sub t1)
  in
  helper
;;

let infer_exp =
  let rec helper env = function
    | EConst c ->
      (match c with
       | CInt _ -> return (Subst.empty, int_typ)
       | CBool _ -> return (Subst.empty, bool_typ)
       | CString _ -> return (Subst.empty, string_typ)
       | CUnit -> return (Subst.empty, unit_typ)
       | CNil ->
         let* fresh = fresh_var in
         return (Subst.empty, list_typ fresh))
    | EVar x ->
      (match TypeEnv.find x env with
       | Some s ->
         let* t = instantiate s in
         return (Subst.empty, t)
       | None -> fail (`Unbound_variable x))
    | EIf (i, t, e) ->
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
      let* fresh = fresh_var in
      let* sub, t =
        List.fold_left
          ~f:(fun acc (pat, exp) ->
            let* sub1, t = acc in
            let* env1, pt = infer_pat env pat in
            let* sub2 = Subst.unify t1 pt in
            let env2 = TypeEnv.apply sub2 env1 in
            let* sub3, t' = helper env2 exp in
            let* sub4 = Subst.unify t' t in
            let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
            return (sub, Subst.apply sub t))
          ~init:(return (sub1, fresh))
          cl
      in
      return (sub, t)
    | ELet (Rec, (pat, e1), e2) ->
      let rec extract_nested = function
        | PConstraint (p, _) -> extract_nested p
        | _ as namaa -> namaa
      in
      (match extract_nested pat with
       | PVar x ->
         let* fresh = fresh_var in
         let env1 = TypeEnv.extend env (x, S (VarSet.empty, fresh)) in
         let* s1, t1 = helper env1 e1 in
         let* s2 = Subst.unify (Subst.apply s1 fresh) t1 in
         let* s3 = Subst.compose s1 s2 in
         let env2 = TypeEnv.apply s3 env in
         let t1 = Subst.apply s3 t1 in
         let s = generalize_rec env2 t1 x in
         let env3 = TypeEnv.extend env2 (x, s) in
         let* s4, t2 = helper env3 e2 in
         let* s5 = Subst.compose s3 s4 in
         return (s5, t2)
       | _ -> fail `Let_rec_lhs)
    | ELet (Nonrec, (pat, e1), e2) ->
      let* s1, t1 = helper env e1 in
      let env = TypeEnv.apply s1 env in
      let s = generalize env t1 in
      let* env1, t2 = infer_pat env pat in
      let env2 = TypeEnv.ext_by_pat s env1 pat in
      let* sub = Subst.unify t2 t1 in
      let* sub1 = Subst.compose sub s1 in
      let env3 = TypeEnv.apply sub1 env2 in
      let* s2, t2 = helper env3 e2 in
      let* s = Subst.compose sub1 s2 in
      return (s, t2)
    | EFun (p, e) ->
      let* env, t = infer_pat env p in
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
    | ECons (e1, e2) ->
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper env e2 in
      let* s3 = Subst.unify (list_typ t1) t2 in
      let t = Subst.apply s3 t2 in
      let* sub = Subst.compose_all [ s1; s2; s3 ] in
      return (sub, t)
    | EApply (e1, e2) ->
      let* fresh = fresh_var in
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
      let* s3 = Subst.unify (arrow t2 fresh) (Subst.apply s2 t1) in
      let* sub = Subst.compose_all [ s1; s2; s3 ] in
      let t = Subst.apply sub fresh in
      return (sub, t)
    | EConstraint (exp, an) ->
      let* sub1, t1 = helper env exp in
      let* sub2 = Subst.unify t1 (annot_to_ty an) in
      let* sub = Subst.compose sub1 sub2 in
      return (sub, Subst.apply sub t1)
  in
  helper
;;

let infer_str_item env = function
  | SValue (Rec, bindings) ->
    (* Create fresh type variables for all bindings *)
    let* fresh_vars =
      List.fold_left
        ~f:(fun acc (pat, _) ->
          let* acc_vars = acc in
          let* fresh = fresh_var in
          match pat with
          | PVar x -> return ((x, fresh) :: acc_vars)
          | _ -> fail `Let_rec_lhs)
        ~init:(return [])
        bindings
    in
    (* Extend environment with fresh type variables *)
    let initial_env =
      List.fold_left
        ~f:(fun acc (x, fresh) -> TypeEnv.extend acc (x, S (VarSet.empty, fresh)))
        ~init:env
        fresh_vars
    in
    (* Rest of the function remains the same *)
    let* subst_types =
      List.fold_left
        ~f:(fun acc (pat, exp) ->
          let* acc_subst_types = acc in
          let* s, t = infer_exp initial_env exp in
          match pat with
          | PVar x -> return ((x, s, t) :: acc_subst_types)
          | _ -> fail `NotVar)
        ~init:(return [])
        bindings
    in
    let* final_subst =
      List.fold_left
        ~f:(fun acc (x, s, t) ->
          let* acc_subst = acc in
          let fresh = List.Assoc.find_exn ~equal:String.equal fresh_vars x in
          let* s1 = Subst.unify (Subst.apply s fresh) t in
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
          let scheme = generalize_rec acc final_type x in
          TypeEnv.extend acc (x, scheme))
        ~init:env
        subst_types
    in
    return final_env
  | SValue (Nonrec, [ (pattern_, e) ]) ->
    (* Original non-recursive case remains unchanged *)
    let* s, type1 = infer_exp env e in
    let env = TypeEnv.apply s env in
    let sc = generalize env type1 in
    let* env1, type2 = infer_pat env pattern_ in
    let env2 = TypeEnv.ext_by_pat sc env1 pattern_ in
    let* sub = Subst.unify type1 type2 in
    let* sub1 = Subst.compose s sub in
    let env3 = TypeEnv.apply sub1 env2 in
    return env3
  | SEval e ->
    let* _, _ = infer_exp env e in
    return env
  | _ -> fail `NotStrItem
;;

let start_env =
  let bin_op_list =
    [ "( + )", TArrow (TPrim "int", TArrow (TPrim "int", TPrim "int"))
    ; "( - )", TArrow (TPrim "int", TArrow (TPrim "int", TPrim "int"))
    ; "( / )", TArrow (TPrim "int", TArrow (TPrim "int", TPrim "int"))
    ; "( * )", TArrow (TPrim "int", TArrow (TPrim "int", TPrim "int"))
    ; "( < )", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "( > )", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "( <= )", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "( >= )", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "( <> )", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "( = )", TArrow (TVar 1, TArrow (TVar 1, TPrim "bool"))
    ; "print_int", TArrow (TPrim "int", TPrim "unit")
    ]
  in
  let env = TypeEnv.empty in
  let bind env id typ = TypeEnv.extend env (id, generalize env typ) in
  List.fold_left bin_op_list ~init:env ~f:(fun env (id, typ) -> bind env id typ)
;;

let infer_structure (structure : structure) =
  List.fold_left
    ~f:(fun acc item ->
      let* env = acc in
      let* env = infer_str_item env item in
      return env)
    ~init:(return start_env)
    structure
;;

let run_infer s = run (infer_structure s)

let test_infer s =
  let open Stdlib.Format in
  match Parser.parse s with
  | Ok parsed ->
    (match run_infer parsed with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key:v ~data:(S (_, ty)) ->
         match TypeEnv.find v start_env with
         | Some (S (_, init_ty)) when Stdlib.(ty = init_ty) -> ()
         | _ -> Format.printf "val %s: %a\n" v pp_typ ty)
     | Error e -> printf "Infer error: %a\n" pp_error e)
  | Error e -> printf "Parsing error: %s\n" e
;;
