open Ast
open Base

type error =
  | Occurs_check of type_id * typ
  | Unification_failed of typ * typ
  | Tuple_unequal_lens of typ * typ
  | Variable_not_found of ident

(* type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of typ * typ
  ]

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

  let fail e st = st, Base.Result.fail e
  let return x last = last, Base.Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> let fresh_var() =
  let open R.Syntax in
  let open R in
  fresh >>= fun n -> Typ_var(n) |> returnst, Result.Error e
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
end *)

let unreachable() = failwith "Reached unreachable code"

module R = struct
  type 'a t = type_id -> type_id * ('a, error) Result.t

  let return v st = st, Result.return v
  let fail e st = st, Result.Error e
  let bind : 'a t -> ('a -> 'b t) -> 'b t  = fun m f st ->
    let last, r = m st in
    match r with
    | Error err -> last, Error(err)
    | Ok v -> f v last

    module Syntax = struct
      let (let*) = bind
      let (>>=) = bind
    end

    let foldl l ~init ~f =
      let open Syntax in
      List.fold_left l ~init ~f:(fun acc e ->
        let* acc = acc in
        f acc e)
    let foldr l ~f ~init =
      let open Syntax in
      List.fold_right l ~init ~f:(fun e acc ->
        let* acc = acc in
        f e acc)
    let fresh = fun id -> id + 1, Result.Ok(id)
end

module Varset = struct
  include Stdlib.Set.Make(Int)
  let pp fmt s =
    let open Stdlib.Format in
    fprintf fmt "[ ";
    iter (fprintf fmt "%d; ") s;
    fprintf fmt "]"
end

module Type = struct
  let rec occurs id = function
  | Typ_bool | Typ_int -> false
  | Typ_var v -> id = v
  | Typ_fun(x, y) -> occurs id x || occurs id y
  | Typ_list(x) -> occurs id x
  | Typ_tuple(x, y, rest) -> Base.List.exists (x::y::rest) ~f:(occurs id)
  | Typ_nil -> false


  let free_vars t =
    let rec helper acc = function
    | Typ_bool | Typ_int | Typ_nil -> acc
    | Typ_var v -> Varset.add v acc
    | Typ_fun(x, y) -> let acc = helper acc x in helper acc y
    | Typ_list(x) -> helper acc x
    | Typ_tuple(x, y, rest) -> List.fold_left (x::y::rest) ~init:acc ~f:helper
    in helper Varset.empty t
end

 
module Subst  = struct
  open R
  open R.Syntax

  type t = (type_id, typ, Int.comparator_witness) Base.Map.t  
  let empty : t =  Base.Map.empty(module Base.Int)
  let singleton k v = Base.Map.singleton(module Base.Int) k v
  let singleton_checked id t = if Type.occurs id t
     then Occurs_check(id, t) |> R.fail
     else singleton id t |> R.return
  let find key s = Map.find s key
  let remove k s = Map.remove s k
  let apply t s =
    let rec helper = function
    | Typ_var v -> 
      let r = match find v s with
      | Some v -> v
      | _ -> t
      in r  
    | Typ_fun(x, e) -> Typ_fun(helper x, helper e)
    | Typ_list(x) -> Typ_list(helper x)
    | Typ_tuple(x, y, rest) -> Typ_tuple(helper x, helper y, List.map ~f:helper rest)
    | _ -> t
    in helper t

  let rec unify_one t1 t2 =
    match t1, t2 with
    | Typ_bool, Typ_bool | Typ_int, Typ_int -> return empty
    | Typ_var x, Typ_var y when x = y -> return empty
    | Typ_var x, (_ as y)
    | (_ as y), Typ_var x -> singleton_checked x y
    | Typ_fun(x, xs), Typ_fun(y, ys) -> unify [x, y; xs, ys]
    | Typ_list(x), Typ_list(y) -> unify_one x y
    | Typ_tuple(x1, x2, xs), Typ_tuple(y1, y2, ys) ->
        let s = match List.zip xs ys with
        | Base.List.Or_unequal_lengths.Ok res -> (x1, y1)::(x2, y2)::res |> unify
        | Base.List.Or_unequal_lengths.Unequal_lengths -> Tuple_unequal_lens(t1, t2) |> fail
        in s
    | _-> Unification_failed(t1, t2) |> fail
  and unify types =
    match types with
    | [] -> return empty
    | (x, y)::rest ->
      let* rest_s = unify rest in
      let* curr_s = unify_one (apply x rest_s) (apply y rest_s) in
      compose rest_s curr_s
     
  and extend (id, t) s =
    match find id s with
    | Some t2 ->
      let t1 = apply t s in
      let* unified = unify_one t1 t2 in
      compose s unified
    | None ->
        let* new_s = singleton_checked id t in
        Map.fold s ~init:(return new_s) ~f:(fun ~key ~data acc ->
          let* acc = acc in
          let data = apply data acc in
          let* _ = singleton_checked key data in
          Map.add_exn acc ~key ~data |> return)

  and compose before after = Map.fold after ~init:(return before) ~f:(fun ~key ~data acc ->
    let* acc = acc in 
    extend (key, data) acc)

  let unify_many types : (t * typ) R.t =
    let open R.Syntax in
    let rec helper (repr, s) = function
    | [] -> R.return (s, repr)
    | t::ts ->
      let* unified = unify_one repr t in
      let* composed = compose s unified in
      helper (repr, composed) ts
    in
    match types with
    | x::xs -> helper (x, empty) xs
    | _ -> unreachable()

  let compose_all ss =
    List.fold ss ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)

  let pp fmt s =
    let open Stdlib.Format in
    fprintf fmt "[ ";
    Map.iteri s ~f:(fun ~key ~data -> fprintf fmt "%d -> %a;" key data fmt);
    fprintf  fmt "]";
end

type scheme = Scheme of Varset.t * typ (** Forall quantified vars * [typ] *)

let fresh_var() =
  let open R.Syntax in
  let open R in
  fresh >>= fun n -> Typ_var(n) |> return


module Scheme = struct
  type t = scheme
  
  let free_vars (Scheme(bs, t)) =
    Varset.diff (Type.free_vars t) bs
    
  let apply_subst subst (Scheme(bs, t)) =
    let refined_s = Varset.fold (fun bounded_v acc -> Subst.remove bounded_v acc) bs subst in
    Scheme(bs, Subst.apply t refined_s)

    let instantiate (Scheme(bs, t)) : typ R.t =
      let open R.Syntax in
      Varset.fold (fun b acc ->
        let* acc = acc in
        let* fr = fresh_var() in
        let subst = Subst.singleton b fr in
        Subst.apply acc subst |> R.return)
        bs
        (R.return t)
end

type var_name = string

module TypeEnv = struct
  type t = (var_name, Scheme.t, String.comparator_witness) Base.Map.t

  let empty : t = Base.Map.empty(module Base.String)

  let free_vars e =
     Map.fold e ~init:(Varset.empty) 
     ~f:(fun ~key:_ ~data:scheme acc -> Varset.union acc (Scheme.free_vars scheme))
  let extend name scheme e =
    Map.add_exn e ~key:name ~data: scheme

  let apply subst (env : t) =
    Map.fold env ~init:empty ~f:(fun ~key ~data acc ->
      let refined = Scheme.apply_subst subst data in
      extend key refined acc
    )
  
  let find id env = Map.find env id

  let generalize t env =
    let quantified = Varset.diff (Type.free_vars t) (free_vars env) in
    Scheme(quantified, t)

  let lookup var env : (Subst.t * typ) R.t =
    let open R.Syntax in
    match Map.find_exn env var with
    | exception Not_found_s  _ | exception Stdlib.Not_found -> Variable_not_found var |> R.fail
    | scheme -> 
      let* t = Scheme.instantiate scheme in
      R.return (Subst.empty, t)
end


open R.Syntax

let split_cons (Pat_cons(_, _) as cons) =
  let rec helper acc = function
  | Pat_cons(x, Pat_nil) -> acc @ [x], Pat_nil
  | Pat_cons(x, (Pat_var _ as tail)) -> acc @ [x], tail
  | Pat_cons(x, (Pat_cons(_, _) as tail)) -> helper (acc @ [x]) tail
  | _ -> unreachable()
in helper [] cons


let rec infer_pattern env p : (TypeEnv.t * typ) R.t  = match p with 
  | Pat_constrained(_, t) -> R.return (env, t)
  | Pat_const(Const_bool _) -> R.return (env, Typ_bool)
  | Pat_const(Const_int _) -> R.return(env, Typ_int)
  | Pat_wildcard ->
    let* fr = fresh_var() in
    R.return (env, fr)
  | Pat_var id ->
    let r = match TypeEnv.find id env with
    | Some(Scheme(_, t)) -> R.return (env, t)
    | None ->
      let* fr = fresh_var() in
      let env = TypeEnv.extend id (Scheme(Varset.empty, fr)) env in
      R.return (env, fr) in
    r
  | Pat_tuple(p1, p2, prest) ->
    let* env, t1 = infer_pattern env p1 in
    let* env, t2 = infer_pattern env p2 in
    let folder elem acc =
      let* (env, types) = acc in
      let* env, t = infer_pattern env elem in
      R.return (env, t::types)
    in
    let* env, trest = List.fold_right prest ~init:(R.return (env, [])) ~f:folder in
    let typ = Typ_tuple(t1, t2, trest) in
    R.return (env, typ)
  | Pat_cons(_, _) as cons ->
    let elems, tail = split_cons cons in
    let* env, elems_infered = R.foldr elems ~init:(R.return (env, [])) ~f:(fun elem (env, types) ->
      let* env, t = infer_pattern env elem in
      R.return (env, t::types)) in
    let* env, tail_typ = infer_pattern env tail in
    let* (elem_subst, representative) = Subst.unify_many elems_infered in
    let* tail_subst = Subst.unify_one tail_typ (Typ_list(representative)) in
    let* final_subst = Subst.compose elem_subst tail_subst in
    R.return (TypeEnv.apply final_subst env, tail_typ)
  | Pat_nil ->
    let* fr = fresh_var() in
    R.return (env, Typ_list(fr))

let flat_cons cons =
  let rec helper acc = function
  | Expr_cons(x, Expr_nil) -> acc @ [x], Expr_nil
  | Expr_cons(x, (Expr_var(_) as tail)) -> acc, tail 
  | Expr_cons(x, (Expr_cons(_, _) as rest)) -> helper (acc @ [x]) rest 
  | _ -> unreachable()
in helper [] cons
  
let infer_expr env expr : (Subst.t * typ) R.t =
  let open R.Syntax in
  let rec helper env = function
  | Expr_const(Const_bool _) -> R.return (Subst.empty, Typ_bool)
  | Expr_const(Const_int _) -> R.return (Subst.empty, Typ_int)
  | Expr_var "*" | Expr_var "/" | Expr_var "+" | Expr_var "-" -> 
    (Subst.empty, Typ_fun(Typ_int, Typ_fun(Typ_int, Typ_int))) |> R.return
  | Expr_var ">=" | Expr_var ">" | Expr_var "<=" | Expr_var "<" ->
    (Subst.empty, Typ_fun(Typ_int, Typ_fun(Typ_int, Typ_bool))) |> R.return
  | Expr_var "&&" | Expr_var "||" ->
    (Subst.empty, Typ_fun(Typ_bool, Typ_fun(Typ_bool, Typ_bool))) |> R.return
  | Expr_var id ->
    let lookup = TypeEnv.lookup id env in
    lookup
  | Expr_cons(_, _) as cons ->
    let elems, tail = flat_cons cons in
    let* (elem_s, elem_types) = R.foldr elems ~init:(R.return ([], [])) ~f: (fun elem (substs, types) ->
      let* s, t = helper env elem in
      R.return (s::substs, t::types)) in
    let* elem_unification_subst, representative = Subst.unify_many elem_types in
    let* tail_s, tail_typ = helper env tail in
    let final_typ = Typ_list(representative) in
    let* tail_unification_subst = Subst.unify_one tail_typ final_typ in
    let* final_subst = Subst.compose_all (elem_s @ [elem_unification_subst; tail_s; tail_unification_subst]) in
    R.return (final_subst, final_typ)
  | Expr_tuple(fst, snd, rest) ->
      let infered = List.map (fst::snd::rest) ~f:(helper env) in
      let* substs, types = List.fold_right infered ~init:(R.return ([], [])) ~f:(fun r acc ->
        let* (ss, ts) = acc in
        let* (s, t) = r in
        R.return (s::ss, t::ts)) in
      let typ = match types with
      | t1::t2::rest -> Typ_tuple(t1, t2, rest)
      | _ -> unreachable() in
      let* composed_subst = Subst.compose_all substs in
      (composed_subst, typ) |> R.return
  | Expr_let(NonRecursive, (p, v), scope) ->
    let* env, t1 = infer_pattern env p in
    let* subst, t2 = helper env v in
    let* unified = Subst.unify_one t1 t2 in
    let* scope_subst, scope_typ = helper env scope in
    let* final_subst = Subst.compose_all [subst; unified; scope_subst] in
    R.return (final_subst, scope_typ)
  | Expr_ite(cond, th, el) ->
      let* cond_subst, cond_typ = helper env cond in
      let* th_subst, th_typ = helper env th in
      let* el_subst, el_typ = helper env el in
      let* cond_uni = Subst.unify_one cond_typ Typ_bool in
      let* values_uni = Subst.unify_one th_typ el_typ in
      let* final_subst = Subst.compose_all [cond_subst; th_subst; el_subst; cond_uni; values_uni] in
      R.return (final_subst, th_typ)
  | Expr_fun(param, expr) ->
    let* env, pat_typ = infer_pattern env param in
    let* s, expr_typ = helper env expr in
    (* let env = TypeEnv.apply s env in *)
    R.return (s, Typ_fun(pat_typ, expr_typ))
  | Expr_match(expr, cases) ->
    let* expr_subst, expr_typ = helper env expr in
    let* pat_types, expr_types, substs =
      R.foldr cases ~init:(R.return ([], [], [])) ~f:(fun (p, e) (pats, exprs, substs) ->
        let* env, ptyp = infer_pattern env p in
        let* subst, etyp = helper env e in
        R.return (ptyp::pats, etyp::exprs, subst::substs)) in
    let* unified_patterns_subst, _ = Subst.unify_many (expr_typ::pat_types) in
    let* unified_values_subst, representative = Subst.unify_many expr_types in
    let* final_subst = Subst.compose_all (expr_subst::substs @ [unified_patterns_subst; unified_values_subst]) in
    R.return (final_subst, representative)
  | Expr_app(id, (arg, args)) ->
    let* body_typ = fresh_var() in
    let args = arg::args in
    let* id_s, id_t = helper env id in
    let* arg_s, arg_types = R.foldr args ~init:(R.return (Subst.empty, [])) ~f:(fun arg (acc_s, types) ->
      let* s, t = helper env arg in
      let* composed = Subst.compose acc_s s in
      R.return (composed, t::types)) in
    let last_arg, rest_args =
      match arg_types |> List.rev with
      | [] -> unreachable()
      | fst::rest -> fst, rest in
    let fun_typ = List.fold_left rest_args ~init:(tfun last_arg body_typ) ~f:tfun in
    let* f_s = Subst.unify_one id_t fun_typ in
    let* final_subst = Subst.compose_all [id_s; arg_s; f_s] in
    R.return (final_subst, fun_typ)
  | _  -> unreachable()
    
  in helper env expr


