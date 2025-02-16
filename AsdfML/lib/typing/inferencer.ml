(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(* Based on https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml*)

open Types
open Pp_typing
open Base
open Utils
open Ast
open Tast

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
      :  ('k, 'v, 'a) Map.t
      -> init:'acc t
      -> f:('k -> 'v -> 'acc -> 'acc t)
      -> 'acc t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
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
    let fold_left xs ~init ~f =
      let f ~key ~data acc =
        let open Syntax in
        let* acc = acc in
        f key data acc
      in
      Map.fold xs ~init ~f
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module VarSet = struct
  include Set

  let empty = Set.empty (module Int)

  let fold_left_m f init set =
    fold set ~init ~f:(fun acc x ->
      let open R.Syntax in
      let* acc = acc in
      f acc x)
  ;;
end

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TGround _ -> false
    | TTuple (x1, x2, xs) ->
      let xs = x1 :: x2 :: xs in
      List.exists xs ~f:(occurs_in v)
    | TList t -> occurs_in v t
  ;;

  let free_vars =
    let rec helper acc = function
      | TVar b -> Set.add acc b
      | TArrow (l, r) -> helper (helper acc l) r
      | TGround _ -> acc
      | TTuple (x1, x2, xs) ->
        let xs = x1 :: x2 :: xs in
        List.fold xs ~init:VarSet.empty ~f:helper
      | TList t -> helper acc t
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t R.t
  val find : t -> fresh -> ty option
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
  val pp : Format.formatter -> t -> unit
end = struct
  open R
  open R.Syntax

  (* an association list. In real world replace it by a finite map *)
  type t = (fresh, ty, Int.comparator_witness) Map.t

  let pp fmt subst =
    let open Format in
    fprintf
      fmt
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt "\n")
         (fun fmt (k, v) -> fprintf fmt "%s => %a" (type_id_to_name k) pp_ty v))
      (Map.to_alist subst)
  ;;

  let empty = Map.empty (module Int)
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* k, v = mapping k v in
    Map.set empty ~key:k ~data:v |> return
  ;;

  let find = Map.find
  let remove = Map.remove

  let apply s =
    let rec helper = function
      | TVar b as ty ->
        (match find s b with
         | None -> ty
         | Some x -> x)
      | TArrow (l, r) -> helper l ^-> helper r
      | TList x -> TList (helper x)
      | TTuple (x1, x2, xs) -> TTuple (helper x1, helper x2, List.map xs ~f:helper)
      | TGround _ as g -> g
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TGround l, TGround r when equal_ground l r -> return empty
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TTuple (x1, x2, xs), TTuple (y1, y2, ys) ->
      let xs = x1 :: x2 :: xs in
      let ys = y1 :: y2 :: ys in
      (match
         List.fold2 xs ys ~init:(return empty) ~f:(fun acc l r ->
           let* acc = acc in
           let* sub = unify l r in
           compose acc sub)
       with
       | List.Or_unequal_lengths.Ok x -> x
       | List.Or_unequal_lengths.Unequal_lengths -> fail (`Unification_failed (l, r)))
    | TList t1, TList t2 -> unify t1 t2
    | _ -> fail (`Unification_failed (l, r))

  and extend k v s =
    match find s k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        Map.update acc k ~f:(fun _ -> v) |> return)
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc x ->
      let* acc = acc in
      compose acc x)
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | xs, t -> (not (VarSet.mem xs v)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | bs, t -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply sub (names, ty) =
    let sub' = VarSet.fold names ~init:sub ~f:Subst.remove in
    names, Subst.apply sub' ty
  ;;
end

module TypeEnv = struct
  include Map

  type t = (Ast.id, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let free_vars : t -> (var_id, Int.comparator_witness) VarSet.t =
    fold ~init:VarSet.empty ~f:(fun ~key:_ ~data acc ->
      VarSet.union acc (Scheme.free_vars data))
  ;;

  let extend env id scheme = set env ~key:id ~data:scheme

  let rec extend_pat (env : t) (pat : Ast.pattern) (scheme : Scheme.t) : t R.t =
    let open R in
    let open R.Syntax in
    match pat, scheme with
    | PConst CUnit, _ -> return env
    | PWild, _ -> return env
    | PIdent x, _ -> extend env x scheme |> return
    | PTuple (x1, x2, xs), (vars, (TTuple (y1, y2, ys) as ty)) ->
      let xs = x1 :: x2 :: xs in
      let ys = y1 :: y2 :: ys in
      List.fold2 xs ys ~init:(return env) ~f:(fun acc x y ->
        let* acc = acc in
        extend_pat acc x (vars, y))
      |> (function
       | List.Or_unequal_lengths.Ok env' -> env'
       | _ -> fail (`Arg_num_mismatch (pat, ty)))
    | PAnn (x, _), _ -> extend_pat env x scheme
    | PCons (hd, tl), (vars, (TList ty as list_ty)) ->
      let* env = extend_pat env hd (vars, ty) in
      extend_pat env tl (vars, list_ty)
    | PList xs, (vars, TList ty) ->
      List.fold xs ~init:(return env) ~f:(fun acc x ->
        let* acc = acc in
        extend_pat acc x (vars, ty))
    | PConst _, _ | PList _, _ | PCons (_, _), _ | PTuple _, _ ->
      fail
        (`Syntax_error
          (Format.asprintf
             "Unsupported pattern `%a`, scheme `%a` in let binding"
             Pp_ast.pp_pattern
             pat
             pp_scheme
             scheme))
  ;;

  let apply env sub = map env ~f:(Scheme.apply sub)

  let builtins =
    let open Std in
    stdlib |> List.map ~f:(fun x -> x.name, x.typ)
  ;;

  let default =
    List.fold builtins ~init:empty ~f:(fun env (op, ty) ->
      let fv = VarSet.diff (Type.free_vars ty) (free_vars env) in
      extend env op (fv, ty))
  ;;

  let _pp ?(no_default = true) fmt (xs : t) =
    Format.fprintf fmt "{|\n";
    (if no_default then default |> Map.keys |> List.fold ~init:xs ~f:Map.remove else xs)
    |> Map.iteri ~f:(fun ~key:n ~data:s -> Format.fprintf fmt "%s -> %a\n" n pp_scheme s);
    Format.fprintf fmt "|}%!"
  ;;

  let pp = _pp ~no_default:true
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> TVar n

let instantiate : scheme -> ty R.t =
  fun (set, t) ->
  VarSet.fold_left_m
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    (return t)
    set
;;

let generalize : TypeEnv.t -> Type.t -> scheme =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  free, ty
;;

let lookup_env e xs =
  match TypeEnv.find xs e with
  | None -> fail (`No_variable e)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans, TEVar (ans, e))
;;

open Types

let infer =
  let infer_const = function
    | Ast.CBool _ -> return bool_typ
    | Ast.CInt _ -> return int_typ
    | Ast.CUnit -> return unit_typ
    | Ast.CNil ->
      let* fv = fresh_var in
      return (TList fv)
  in
  let rec infer_pattern : TypeEnv.t -> Ast.pattern -> (TypeEnv.t * ty) R.t =
    fun env -> function
    | PConst c ->
      let* c = infer_const c in
      return (env, c)
    | PWild -> fresh_var >>| fun v -> env, v
    | PIdent x ->
      let* fv = fresh_var in
      let env' = TypeEnv.extend env x (VarSet.empty, fv) in
      return (env', fv)
    | PTuple (hd1, hd2, tl) ->
      let xs = hd1 :: hd2 :: tl in
      List.fold_right
        xs
        ~init:(return (env, []))
        ~f:(fun x acc ->
          let* env, fvs = acc in
          let* env', fv = infer_pattern env x in
          return (env', fv :: fvs))
      >>= fun (env, fvs) ->
      (match fvs with
       | fv1 :: fv2 :: fvs -> return (env, TTuple (fv1, fv2, fvs))
       | _ -> fail (`Syntax_error "Lost tuple element"))
    | PList xs ->
      let* fv = fresh_var in
      List.fold
        xs
        ~init:(return (env, fv))
        ~f:(fun acc x ->
          let* env, fv = acc in
          let* env', ty = infer_pattern env x in
          let* sub = unify fv ty in
          let env'' = TypeEnv.apply env' sub in
          let ty = Subst.apply sub fv in
          return (env'', ty))
    | PCons (hd, tl) ->
      let* env', ty' = infer_pattern env hd in
      let* env'', ty'' = infer_pattern env' tl in
      let* sub = Subst.unify (TList ty') ty'' in
      let final_env = TypeEnv.apply env'' sub in
      let final_ty = Subst.apply sub ty'' in
      return (final_env, final_ty)
    | PAnn (pat, ann_ty) ->
      let* pat_env, ty = infer_pattern env pat in
      let* sub = unify ty (an_ty_to_ty ann_ty) in
      let final_env = TypeEnv.apply pat_env sub in
      let final_ty = Subst.apply sub ty in
      return (final_env, final_ty)
  in
  let rec (infer_expr : TypeEnv.t -> Ast.expr -> (Subst.t * ty * texpr) R.t) =
    fun env -> function
    | EConst c ->
      let* t = infer_const c in
      return (Subst.empty, t, TEConst (t, c))
    | EVar x -> lookup_env x env
    | EApp (left, right) ->
      let* left_sub, left_ty, tleft = infer_expr env left in
      let* right_sub, right_ty, tright = infer_expr (TypeEnv.apply env left_sub) right in
      let* fv = fresh_var in
      let* sub = unify (Subst.apply right_sub left_ty) (right_ty ^-> fv) in
      let final_ty = Subst.apply sub fv in
      let* sub = Subst.compose_all [ sub; right_sub; left_sub ] in
      return (sub, final_ty, TEApp (final_ty, tleft, tright))
    | EIfElse (cond, th, el) ->
      let* s1, t1, tc = infer_expr env cond in
      let* s2, t2, tt = infer_expr env th in
      let* s3, t3, te = infer_expr env el in
      let* s4 = unify t1 bool_typ in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      let final_ty = Subst.apply s5 t2 in
      return (final_subst, final_ty, TEIfElse (final_ty, tc, tt, te))
    | EFun (pat, exp) ->
      let* env', pat_ty =
        List.fold
          pat
          ~init:(return (env, []))
          ~f:(fun acc pat ->
            let* acc_env, acc_ty = acc in
            let* env, ty = infer_pattern acc_env pat in
            return (env, ty :: acc_ty))
      in
      let* s, exp_ty, texp = infer_expr env' exp in
      let final_ty = exp_ty :: pat_ty |> List.reduce_exn ~f:(fun l r -> r ^-> l) in
      let ty = Subst.apply s final_ty in
      return (s, ty, TEFun (ty, pat, texp))
    | ELetIn (def, expr) ->
      let* let_env, let_sub, _, tdef = infer_def env def in
      let* exp_sub, exp_ty, texp = infer_expr let_env expr in
      let* sub = Subst.compose let_sub exp_sub in
      return (sub, exp_ty, TELetIn (exp_ty, tdef, texp))
    | ETuple (hd1, hd2, tl) ->
      let xs = hd1 :: hd2 :: tl in
      List.fold_right
        xs
        ~init:(return (Subst.empty, [], []))
        ~f:(fun x acc ->
          let* acc_sub, acc_t, acc_ty = acc in
          let* s, t, texp = infer_expr env x in
          let* s' = Subst.compose acc_sub s in
          return (s', t :: acc_t, texp :: acc_ty))
      >>= fun (s, t, tes) ->
      let* ty =
        match List.map t ~f:(Subst.apply s) with
        | t1 :: t2 :: ts -> return (TTuple (t1, t2, ts))
        | _ -> fail (`Syntax_error "Lost tuple element")
      in
      let* texp =
        match tes with
        | t1 :: t2 :: ts -> return (TETuple (ty, t1, t2, ts))
        | _ -> fail (`Syntax_error "Lost tuple element")
      in
      return (s, ty, texp)
    | EList xs ->
      (match xs with
       | [] ->
         let* fv = fresh_var in
         return (Subst.empty, TList fv, [])
       | hd :: tl ->
         let* subst, ty, texp = infer_expr env hd in
         let texp = [ texp ] in
         List.fold
           tl
           ~init:(return (subst, ty, texp))
           ~f:(fun acc x ->
             let* acc_sub, acc_t, acc_texp = acc in
             let* s, t, texp = infer_expr env x in
             let* s' = unify acc_t t in
             let* sub = Subst.compose_all [ s'; s; acc_sub ] in
             return (sub, acc_t, texp :: acc_texp)))
      >>| fun (s, t, (tes : texpr list)) -> s, TList t, TEList (t, List.rev tes)
    | EMatch (e, pe) ->
      let* match_sub, match_ty, te = infer_expr env e in
      let* fv = fresh_var in
      let* sub, ty, tes =
        List.fold
          pe
          ~init:(return (match_sub, fv, []))
          ~f:(fun acc (p, e) ->
            let* acc_sub, acc_ty, acc_te = acc in
            let* pat_env, pat_ty = infer_pattern env p in
            let* pat_sub = unify match_ty pat_ty in
            let pat_env' = TypeEnv.apply pat_env pat_sub in
            let* exp_sub, exp_ty, te = infer_expr pat_env' e in
            let* sub = unify exp_ty acc_ty in
            let* final_subst = Subst.compose_all [ acc_sub; pat_sub; exp_sub; sub ] in
            let final_ty = Subst.apply final_subst acc_ty in
            return (final_subst, final_ty, (p, te) :: acc_te))
      in
      return (sub, ty, TEMatch (ty, te, List.rev tes))
  and (infer_def
        : TypeEnv.t -> Ast.definition -> (TypeEnv.t * Subst.t * ty * tdefinition) R.t)
    =
    fun env -> function
    | DLet (NonRec, pat, expr) ->
      let* exp_sub, exp_ty, texp = infer_expr env expr in
      let env' = TypeEnv.apply env exp_sub in
      let scheme = generalize env' exp_ty in
      let* pat_env, pat_ty = infer_pattern env pat in
      let* pat_env' = TypeEnv.extend_pat pat_env pat scheme in
      let* sub = Subst.unify pat_ty exp_ty in
      let* final_sub = Subst.compose sub exp_sub in
      let final_env = TypeEnv.apply pat_env' final_sub in
      let final_ty = Subst.apply final_sub exp_ty in
      let tdef = TDLet (final_ty, NonRec, pat, texp) in
      return (final_env, final_sub, final_ty, tdef)
    | DLet (Rec, (PIdent x as pat), expr) | DLet (Rec, (PAnn (PIdent x, _) as pat), expr)
      ->
      let* pat_env, pat_ty = infer_pattern env pat in
      let* exp_sub, exp_ty, texp = infer_expr pat_env expr in
      let pat_ty = Subst.apply exp_sub pat_ty in
      let* sub = Subst.unify pat_ty exp_ty in
      let* sub' = Subst.compose exp_sub sub in
      let env = TypeEnv.apply pat_env sub' in
      let final_ty = Subst.apply sub' exp_ty in
      let scheme = generalize (TypeEnv.remove env x) final_ty in
      let env = TypeEnv.extend env x scheme in
      return (env, sub', final_ty, TDLet (final_ty, Rec, pat, texp))
    | DLet (_, pat, _) ->
      fail
        (`Syntax_error
          (Format.asprintf "Can't use %a in let rec expression" Pp_ast.pp_pattern pat))
  in
  infer_def
;;

let strip_annots (prog : Tast.tdefinition list) =
  let rec strip_pat = function
    | PAnn (pat, _) -> pat
    | PTuple (hd1, hd2, tl) ->
      PTuple (strip_pat hd1, strip_pat hd2, List.map tl ~f:strip_pat)
    | PList pats -> PList (List.map pats ~f:strip_pat)
    | PCons (l, r) -> PCons (strip_pat l, strip_pat r)
    | pat -> pat
  in
  let rec strip_expr = function
    | TEFun (ty, pats, exp) -> TEFun (ty, List.map pats ~f:strip_pat, strip_expr exp)
    | TEMatch (ty, expr, pes) ->
      TEMatch (ty, expr, List.map pes ~f:(fun (pat, exp) -> strip_pat pat, strip_expr exp))
    | TEApp (ty, l, r) -> TEApp (ty, strip_expr l, strip_expr r)
    | TEIfElse (ty, i, t, e) -> TEIfElse (ty, strip_expr i, strip_expr t, strip_expr e)
    | TELetIn (ty, def, body) -> TELetIn (ty, strip_def def, strip_expr body)
    | TETuple (ty, x1, x2, xs) ->
      TETuple (ty, strip_expr x1, strip_expr x2, List.map xs ~f:strip_expr)
    | TEList (ty, xs) -> TEList (ty, List.map xs ~f:strip_expr)
    | expr -> expr
  and strip_def = function
    | TDLet (ty, flag, pat, exp) -> TDLet (ty, flag, strip_pat pat, strip_expr exp)
  in
  List.map prog ~f:strip_def
;;

let infer_program (prog : Ast.definition list) =
  let rec helper env = function
    | head :: tail ->
      (match head with
       | Ast.DLet _ ->
         let* env', _, _, tdef = infer env head in
         let* tail = helper env' tail in
         return (tdef :: tail))
    | [] -> return []
  in
  let env = TypeEnv.default in
  helper env prog >>| strip_annots
;;

let inference_program prog = run (infer_program prog)
