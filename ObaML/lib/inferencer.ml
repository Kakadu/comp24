(** Copyright 2025, tepa46 *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open InferencerTypes
open Ast
open Format

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Base.Result.t
end = struct
  open Base

  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Base.Result.t

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
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
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
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | ITVar b -> b = v
    | ITPrim _ -> false
    | ITArr (l, r) -> occurs_in v l || occurs_in v r
    | ITTuple t -> List.fold_left (fun acc h -> acc || occurs_in v h) false t
    | ITList l -> occurs_in v l
  ;;

  let free_vars =
    let rec helper acc = function
      | ITVar b -> VarSet.add b acc
      | ITPrim _ -> acc
      | ITArr (l, r) -> helper (helper acc l) r
      | ITTuple t -> List.fold_left (fun acc h -> helper acc h) acc t
      | ITList l -> helper acc l
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

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : fresh -> t -> t
  val pp_subst : Format.formatter -> t -> unit
end = struct
  open R
  open R.Syntax
  open Base

  type t = ty VarMap.t

  let empty = VarMap.empty
  let mapping k vm = if Type.occurs_in k vm then fail `Occurs_check else return (k, vm)

  let singleton k vm =
    let* k, vm = mapping k vm in
    return (VarMap.singleton k vm)
  ;;

  let find k vm = VarMap.find_opt k vm
  let remove k vm = VarMap.remove k vm

  let apply s =
    let rec helper = function
      | ITVar b as ty ->
        (match find b s with
         | None -> ty
         | Some ty -> ty)
      | ITPrim _ as ty -> ty
      | ITArr (l, r) -> ITArr (helper l, helper r)
      | ITTuple t -> ITTuple (List.map t ~f:(fun elm -> helper elm))
      | ITList l -> ITList (helper l)
    in
    helper
  ;;

  let fold mp init f =
    VarMap.fold
      (fun k vm acc ->
        let* acc = acc in
        f k vm acc)
      mp
      init
  ;;

  let rec unify l r =
    match l, r with
    | ITVar a, ITVar b when Int.equal a b -> return empty
    | ITVar b, t | t, ITVar b -> singleton b t
    | ITPrim l, ITPrim r when String.equal l r -> return empty
    | ITArr (l1, r1), ITArr (l2, r2) ->
      let* subst1 = unify l1 l2 in
      let* subst2 = unify (apply subst1 r1) (apply subst1 r2) in
      compose subst1 subst2
    | ITTuple ty_lst1, ITTuple ty_lst2 ->
      let* subst =
        match
          List.fold2 ty_lst1 ty_lst2 ~init:(return empty) ~f:(fun subst h1 h2 ->
            let* subst = subst in
            let* subst2 = unify (apply subst h1) (apply subst h2) in
            compose subst subst2)
        with
        | Ok res -> res
        | Unequal_lengths -> fail (`Unification_failed (l, r))
      in
      return subst
    | ITList l, ITList r -> unify l r
    | _ -> fail (`Unification_failed (l, r))

  and extend k v s =
    match VarMap.find_opt k s with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      fold s (return s2) (fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (VarMap.add k v acc))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = fold s2 (return s1) extend

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose

  let pp_subst fmt subst =
    VarMap.iter (fun key value -> fprintf fmt "||| '%d : %a ||| " key pp_ty value) subst
  ;;
end

module VarSet = struct
  include VarSet

  let pp_varset fmt st = VarSet.iter (fun key -> fprintf fmt "'%d " key) st
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | Scheme (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | Scheme (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply subst (Scheme (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove k s) names subst in
    Scheme (names, Subst.apply s2 ty)
  ;;

  let pp_scheme fmt = function
    | Scheme (st, typ) ->
      if VarSet.is_empty st
      then fprintf fmt "%a" pp_ty typ
      else fprintf fmt "%a. %a" VarSet.pp_varset st pp_ty typ
  ;;
end

module TypeEnv = struct
  type t = scheme StringMap.t

  let fold f init mp = StringMap.fold (fun k v acc -> f k v acc) mp init
  let extend k v mp = StringMap.add k v mp
  let empty = StringMap.empty

  let init_env =
    let init_env =
      extend "( * )" (Scheme (VarSet.empty, tprim_int @-> tprim_int @-> tprim_int)) empty
    in
    let init_env =
      extend
        "( / )"
        (Scheme (VarSet.empty, tprim_int @-> tprim_int @-> tprim_int))
        init_env
    in
    let init_env =
      extend
        "( + )"
        (Scheme (VarSet.empty, tprim_int @-> tprim_int @-> tprim_int))
        init_env
    in
    let init_env =
      extend
        "( - )"
        (Scheme (VarSet.empty, tprim_int @-> tprim_int @-> tprim_int))
        init_env
    in
    let init_env =
      extend
        "( = )"
        (Scheme (VarSet.singleton (-1), type_var (-1) @-> type_var (-1) @-> tprim_bool))
        init_env
    in
    let init_env =
      extend
        "( == )"
        (Scheme (VarSet.singleton (-5), type_var (-5) @-> type_var (-5) @-> tprim_bool))
        init_env
    in
    let init_env =
      extend
        "( <> )"
        (Scheme (VarSet.singleton (-5), type_var (-5) @-> type_var (-5) @-> tprim_bool))
        init_env
    in
    let init_env =
      extend
        "( != )"
        (Scheme (VarSet.singleton (-5), type_var (-5) @-> type_var (-5) @-> tprim_bool))
        init_env
    in
    let init_env =
      extend
        "( < )"
        (Scheme (VarSet.singleton (-3), type_var (-3) @-> type_var (-3) @-> tprim_bool))
        init_env
    in
    let init_env =
      extend
        "( <= )"
        (Scheme (VarSet.singleton (-2), type_var (-2) @-> type_var (-2) @-> tprim_bool))
        init_env
    in
    let init_env =
      extend
        "( > )"
        (Scheme (VarSet.singleton (-3), type_var (-3) @-> type_var (-3) @-> tprim_bool))
        init_env
    in
    let init_env =
      extend
        "( >= )"
        (Scheme (VarSet.singleton (-2), type_var (-2) @-> type_var (-2) @-> tprim_bool))
        init_env
    in
    let init_env =
      extend
        "( && )"
        (Scheme (VarSet.empty, tprim_bool @-> tprim_bool @-> tprim_bool))
        init_env
    in
    let init_env =
      extend
        "( || )"
        (Scheme (VarSet.empty, tprim_bool @-> tprim_bool @-> tprim_bool))
        init_env
    in
    let init_env =
      extend "print_int" (Scheme (VarSet.empty, tprim_int @-> tprim_unit)) init_env
    in
    let init_env =
      extend "print_string" (Scheme (VarSet.empty, tprim_string @-> tprim_unit)) init_env
    in
    let init_env =
      extend "( ~+ )" (Scheme (VarSet.empty, tprim_int @-> tprim_int)) init_env
    in
    let init_env =
      extend "( ~- )" (Scheme (VarSet.empty, tprim_int @-> tprim_int)) init_env
    in
    init_env
  ;;

  let free_vars : t -> VarSet.t =
    fold (fun _ s acc -> VarSet.union acc (Scheme.free_vars s)) VarSet.empty
  ;;

  let apply s env = StringMap.map (Scheme.apply s) env
  let find name xs = StringMap.find_opt name xs

  let pp_env fmt environment =
    StringMap.iter
      (fun key data -> fprintf fmt "%S: %a\n" key Scheme.pp_scheme data)
      (StringMap.filter (fun tag _ -> StringMap.find_opt tag init_env = None) environment)
  ;;
end

open R
open R.Syntax

let fresh_var = fresh >>| fun n -> ITVar n

let instantiate : scheme -> ty R.t =
  fun (Scheme (bs, t)) ->
  VarSet.fold
    (fun name typ ->
      let* typ = typ in
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    bs
    (return t)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  Scheme (free, ty)
;;

let infer_const constant env =
  match constant with
  | Ast.CInt _ -> return (env, tprim_int)
  | Ast.CString _ -> return (env, tprim_string)
  | Ast.CBool _ -> return (env, tprim_bool)
  | Ast.CEmptyList ->
    let* fresh_var = fresh_var in
    return (env, tlist fresh_var)
  | Ast.CUnit -> return (env, tprim_unit)
;;

let convert_ast_type ast_typ =
  let rec helper env = function
    | Ast.TInt -> return (env, tprim_int)
    | Ast.TString -> return (env, tprim_string)
    | Ast.TBool -> return (env, tprim_bool)
    | Ast.TUnit -> return (env, tprim_unit)
    | Ast.TVar (Id var_name) ->
      (match StringMap.find_opt var_name env with
       | Some f -> return (env, f)
       | None ->
         let* fresh_var = fresh_var in
         let env = StringMap.add var_name fresh_var env in
         return (env, fresh_var))
    | Ast.TTuple ast_typ_lst ->
      let* new_env, typ_lst =
        List.fold_left
          (fun acc ast_typ ->
            let* env, typ_lst = acc in
            let* new_env, typ = helper env ast_typ in
            return (new_env, typ :: typ_lst))
          (return (env, []))
          ast_typ_lst
      in
      return (new_env, ttuple (List.rev typ_lst))
    | Ast.TArrow (ast_typ1, ast_typ2) ->
      let* env, typ1 = helper env ast_typ1 in
      let* env, typ2 = helper env ast_typ2 in
      return (env, typ1 @-> typ2)
    | Ast.TList ast_typ ->
      let* env, typ = helper env ast_typ in
      return (env, tlist typ)
  in
  helper StringMap.empty ast_typ >>= fun (_, typ) -> return typ
;;

let rec infer_pattern pattern env =
  match pattern with
  | Ast.PAny ->
    let* fresh_var = fresh_var in
    return (env, fresh_var)
  | Ast.PConst const -> infer_const const env
  | Ast.PVar (Id var_name) ->
    let* fresh_var = fresh_var in
    return (TypeEnv.extend var_name (Scheme (VarSet.empty, fresh_var)) env, fresh_var)
  | Ast.PTuple pattern_lst ->
    let* env, typ_lst =
      List.fold_left
        (fun acc pattern ->
          let* env, typ_lst = acc in
          let* env, typ = infer_pattern pattern env in
          return (env, typ :: typ_lst))
        (return (env, []))
        pattern_lst
    in
    return (env, ttuple (List.rev typ_lst))
  | Ast.PCons (pattern1, pattern2) ->
    let* env, typ1 = infer_pattern pattern1 env in
    let* env, typ2 = infer_pattern pattern2 env in
    let* subst = Subst.unify (tlist typ1) typ2 in
    let env = TypeEnv.apply subst env in
    return (env, Subst.apply subst typ2)
  | Ast.PType (pat, ast_typ) ->
    let* env, typ = infer_pattern pat env in
    let* conv_typ = convert_ast_type ast_typ in
    let* subst = Subst.unify typ conv_typ in
    let env = TypeEnv.apply subst env in
    return (env, Subst.apply subst typ)
;;

let rec extend_pat pat t env =
  match pat with
  | PAny -> return env
  | PConst _ -> return env
  | PVar (Id x) ->
    let get_scheme = generalize env t in
    let env = TypeEnv.extend x get_scheme env in
    return env
  | PTuple lst ->
    (match t with
     | ITTuple itlst ->
       List.fold_left2
         (fun acc p t1 ->
           let* env = acc in
           let* env = extend_pat p t1 env in
           return env)
         (return env)
         lst
         itlst
     | _ -> fail `Wrong_type)
  | PCons (p1, p2) ->
    (match t with
     | ITList el_t ->
       let* env = extend_pat p1 el_t env in
       let* env = extend_pat p2 t env in
       return env
     | _ -> fail `Wrong_type)
  | PType (p, _) ->
    let* env = extend_pat p t env in
    return env
;;

let rec infer_exp exp env =
  match exp with
  | Ast.EConst const ->
    let* _, typ = infer_const const env in
    return (Subst.empty, typ)
  | Ast.EVar (Ast.Id var_name) ->
    (match TypeEnv.find var_name env with
     | None -> fail (`Unbound_variable var_name)
     | Some x ->
       let* ty = instantiate x in
       return (Subst.empty, ty))
  | Ast.ETuple exp_lst ->
    let* subst, typ_lst =
      List.fold_left
        (fun acc exp ->
          let* subst, typ_lst = acc in
          let* subst1, typ1 = infer_exp exp env in
          let* subst = Subst.compose subst subst1 in
          return (subst, typ1 :: typ_lst))
        (return (Subst.empty, []))
        exp_lst
    in
    let typ_lst = List.rev_map (fun x -> Subst.apply subst x) typ_lst in
    return (subst, ttuple typ_lst)
  | Ast.EFun (pattern_lst, exp) ->
    let* env, typ_lst =
      List.fold_left
        (fun acc pattern ->
          let* env, typ_lst = acc in
          let* env, typ1 = infer_pattern pattern env in
          return (env, typ1 :: typ_lst))
        (return (env, []))
        pattern_lst
    in
    let* subst, typ2 = infer_exp exp env in
    let* fun_typ =
      List.fold_left
        (fun acc typ ->
          let* cur_type = acc in
          return (typ @-> cur_type))
        (return typ2)
        typ_lst
    in
    return (subst, Subst.apply subst fun_typ)
  | Ast.EApp (exp1, exp2) ->
    let* return_type = fresh_var in
    let* subst1, typ1 = infer_exp exp1 env in
    let* subst2, typ2 = infer_exp exp2 (TypeEnv.apply subst1 env) in
    let* subst3 = Subst.unify (Subst.apply subst2 typ1) (typ2 @-> return_type) in
    let* subst = Subst.compose_all [ subst1; subst2; subst3 ] in
    return (subst, Subst.apply subst return_type)
  | Ast.ELet (rec_flag, value_binding, expr) ->
    (match rec_flag with
     | Nonrecursive ->
       let* subst, env = infer_nonrec_let [ value_binding ] env in
       let* subst1, typ1 = infer_exp expr env in
       let* subst = Subst.compose subst subst1 in
       return (subst, Subst.apply subst typ1)
     | Recursive ->
       let* subst, env = infer_rec_let [ value_binding ] env in
       let* subst1, typ1 = infer_exp expr env in
       let* subst = Subst.compose subst subst1 in
       return (subst, Subst.apply subst typ1))
  | Ast.EMatch (exp1, lst) ->
    let* subst1, typ1 = infer_exp exp1 env in
    let env = TypeEnv.apply subst1 env in
    let* fresh_var = fresh_var in
    let* subst, typ =
      List.fold_left
        (fun acc case ->
          let pattern, exp = case in
          let* subst, typ = acc in
          let* pat_env, pat_type = infer_pattern pattern env in
          let* subst2 = Subst.unify typ1 pat_type in
          let pat_env = TypeEnv.apply subst2 pat_env in
          let* exp_subst, exp_typ = infer_exp exp pat_env in
          let* return_subst = Subst.unify exp_typ typ in
          let* subst = Subst.compose_all [ subst; subst2; exp_subst; return_subst ] in
          return (subst, Subst.apply subst typ))
        (return (subst1, fresh_var))
        lst
    in
    return (subst, typ)
  | Ast.EIf (exp1, exp2, exp3) ->
    let* subst1, typ1 = infer_exp exp1 env in
    let* subst2, typ2 = infer_exp exp2 (TypeEnv.apply subst1 env) in
    let* subst3, typ3 = infer_exp exp3 (TypeEnv.apply subst2 env) in
    let* subst4 = Subst.unify typ1 tprim_bool in
    let* subst5 = Subst.unify typ2 typ3 in
    let* subst = Subst.compose_all [ subst1; subst2; subst3; subst4; subst5 ] in
    return (subst, Subst.apply subst typ2)
  | Ast.ECons (exp1, exp2) ->
    let* subst1, typ1 = infer_exp exp1 env in
    let* subst2, typ2 = infer_exp exp2 (TypeEnv.apply subst1 env) in
    let* subst3 = Subst.unify (tlist typ1) typ2 in
    let* subst = Subst.compose_all [ subst1; subst2; subst3 ] in
    return (subst, Subst.apply subst typ2)
  | Ast.EType (expr, ast_typ) ->
    let* subst1, typ1 = infer_exp expr env in
    let* conv_typ = convert_ast_type ast_typ in
    let* subst = Subst.unify typ1 conv_typ in
    let* subst = Subst.compose subst subst1 in
    return (subst, Subst.apply subst typ1)

and infer_nonrec_let value_binding_lst env =
  List.fold_left
    (fun acc value_binding ->
      let* subst, env = acc in
      let pat, expr = value_binding in
      let* subst1, ty = infer_exp expr env in
      let env = TypeEnv.apply subst1 env in
      let* _, t = infer_pattern pat env in
      let* subst2 = Subst.unify ty t in
      let* subst3 = Subst.compose subst1 subst2 in
      let env = TypeEnv.apply subst3 env in
      let* env = extend_pat pat (Subst.apply subst3 t) env in
      let* new_subst = Subst.compose_all [ subst; subst1; subst2; subst3 ] in
      return (new_subst, env))
    (return (Subst.empty, env))
    value_binding_lst

and infer_rec_let value_binding_lst env =
  let helper1 =
    List.fold_left
      (fun acc value_binding ->
        let* env, ty_lst = acc in
        let pat, _ = value_binding in
        let* env, ty = infer_pattern pat env in
        return (env, ty :: ty_lst))
      (return (env, []))
      value_binding_lst
  and helper2 ty_lst env =
    List.fold_left2
      (fun acc value_binding t ->
        let* subst = acc in
        let _, expr = value_binding in
        let* subst1, ty = infer_exp expr env in
        let env = TypeEnv.apply subst env in
        let* subst2 = Subst.unify ty t in
        Subst.compose_all [ subst; subst1; subst2 ])
      (return Subst.empty)
      value_binding_lst
      ty_lst
  and helper3 subst ty_lst env =
    List.fold_left2
      (fun acc value_binding ty ->
        let* env = acc in
        let pat, _ = value_binding in
        let* env = extend_pat pat (Subst.apply subst ty) env in
        let env = TypeEnv.apply subst env in
        return env)
      (return env)
      value_binding_lst
      ty_lst
  in
  let* new_env, ty_lst = helper1 in
  let* subst = helper2 (List.rev ty_lst) new_env in
  let* new_env = helper3 subst (List.rev ty_lst) new_env in
  return (subst, new_env)
;;

let infer_structure (structure : Ast.structure) =
  List.fold_left
    (fun acc si ->
      let* env = acc in
      match si with
      | Ast.SILet (rec_flag, value_binding_lst) ->
        (match rec_flag with
         | Ast.Recursive ->
           let* _, new_env = infer_rec_let value_binding_lst env in
           return new_env
         | Ast.Nonrecursive ->
           let* _, new_env = infer_nonrec_let value_binding_lst env in
           return new_env)
      | Ast.SIExpr expr ->
        let* _ = infer_exp expr env in
        return env)
    (return TypeEnv.init_env)
    structure
;;

let run_expr_infer (expr : Ast.expr) =
  Result.map (fun (_, ty) -> ty) (run (infer_exp expr TypeEnv.init_env))
;;

let run_stucture_infer (structure : Ast.structure) = run (infer_structure structure)
