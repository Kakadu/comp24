open Ast
open Base

type error =
  | Occurs_check of type_id * typ
  | Unification_failed of typ * typ
  | Tuple_unequal_lens of typ * typ
  | Variable_not_found of ident
[@@deriving show]

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

    let run m = snd (m 0)
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
  | Typ_unit -> false


  let free_vars t =
    let rec helper acc = function
    | Typ_bool | Typ_int | Typ_unit -> acc
    | Typ_var v -> Varset.add v acc
    | Typ_fun(x, y) -> let acc = helper acc x in helper acc y
    | Typ_list(x) -> helper acc x
    | Typ_tuple(x, y, rest) -> List.fold_left (x::y::rest) ~init:acc ~f:helper
    in helper Varset.empty t

  let rec pp fmt = let open Stdlib.Format in function
  | Typ_bool -> fprintf fmt "bool"
  | Typ_int -> fprintf fmt "int"
  | Typ_unit -> fprintf fmt "unit"
  | Typ_var id -> fprintf fmt "%d" id
  | Typ_fun(Typ_fun(_, _) as farg, y) ->
    fprintf fmt "(";
    pp fmt farg;
    fprintf fmt ")"; 
    fprintf fmt " -> ";
    pp fmt y;
  | Typ_fun(x, y) ->
    pp fmt x;
    fprintf fmt " -> ";
    pp fmt y
  | Typ_list x ->
    pp fmt x;
    fprintf fmt " list"
  | Typ_tuple(fst, snd, rest) ->
    let rec print_types list sep =
      match list with
      | [x] -> pp fmt x
      | x::xs ->
        let() = pp fmt x in
        fprintf fmt sep;
        print_types xs sep
      | _ -> unreachable() in
    print_types (fst::snd::rest) " *" |> ignore
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
    let rec helper t =
    match t with
    | Typ_var v -> 
      (match find v s with
      | Some v -> v
      | _ -> t)  
    | Typ_fun(x, e) -> Typ_fun(helper x, helper e)
    | Typ_list(x) -> Typ_list(helper x)
    | Typ_tuple(x, y, rest) -> Typ_tuple(helper x, helper y, List.map ~f:helper rest)
    | _ -> t
    in helper t

  let rec unify_pair t1 t2 =
    (* let open Stdlib.Format in
    let fmt = std_formatter in
    fprintf fmt "unifying ";
    Type.pp fmt t1;
    fprintf fmt " and ";
    Type.pp fmt t2;
    fprintf fmt "\n"; *)

    match t1, t2 with
    | Typ_bool, Typ_bool | Typ_int, Typ_int -> return empty
    | Typ_var x, Typ_var y when x = y -> return empty
    | Typ_var x, (_ as y)
    | (_ as y), Typ_var x -> singleton_checked x y
    | Typ_fun(x, xs), Typ_fun(y, ys) ->
      let* s1 = unify_pair x y in
      let* s2 = unify_pair (apply xs s1) (apply ys s1) in
      compose s1 s2
    | Typ_list(x), Typ_list(y) -> unify_pair x y
    | Typ_tuple(x1, x2, xs), Typ_tuple(y1, y2, ys) ->
       let* pairs = match List.zip xs ys with
        | Base.List.Or_unequal_lengths.Ok res -> (x1, y1)::(x2, y2)::res |> R.return
        | Base.List.Or_unequal_lengths.Unequal_lengths -> Tuple_unequal_lens(t1, t2) |> R.fail in
       R.foldl pairs ~init:(return empty) ~f:(fun s (x, y) ->
        let* uni = unify_pair x y in
        compose s uni)
    | _ -> Unification_failed(t1, t2) |> fail
     
  and extend (id, t) s =
    match find id s with
    | Some t2 ->
      let* unified = unify_pair t t2 in
      compose s unified
    | None ->
        let t = apply t s in
        let* new_s = singleton_checked id t in
        Map.fold s ~init:(return new_s) ~f:(fun ~key ~data acc ->
          let* acc = acc in
          let data = apply data acc in
          let* _ = singleton_checked key data in
          Map.set acc ~key ~data |> return)

  and compose before after =
    (* let () = Stdlib.print_endline "subst: compose" in *)
    (* let _ = pp_ident formater "4342334" in *)
    Map.fold after ~init:(return before) ~f:(fun ~key ~data acc ->
    let* acc = acc in 
    extend (key, data) acc)
  
  let compose_all ss =
  List.fold ss ~init:(return empty) ~f:(fun acc s ->
    let* acc = acc in
    compose acc s)

  let unify_many types : (t * typ) R.t =
    let open R.Syntax in
    let rec helper (repr, s) = function
    | [] -> R.return (s, repr)
    | t::ts ->
      let* unified = unify_pair repr t in
      let* composed = compose s unified in
      helper (repr, composed) ts
    in
    match types with
    | x::xs -> helper (x, empty) xs
    | _ -> unreachable()

  let pp ppf (subst : t) =
    let subst = Map.to_alist subst in
    let open Stdlib.Format in
    fprintf
      ppf
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%d -> %a\n" k Type.pp v))
      subst

end

type scheme = Scheme of Varset.t * typ (** Forall quantified vars * [typ] *)
[@@deriving show]

let fresh_var() =
  let open R.Syntax in
  let open R in
  fresh >>= fun n -> Typ_var(n) |> return


module Scheme = struct
  type t = scheme
  
  let free_vars (Scheme(bs, t)) =
    Varset.diff (Type.free_vars t) bs
    
  let apply_subst subst (Scheme(bs, t)) =
    let t = Subst.apply t subst in
    let new_bs = Varset.fold (fun b acc ->
       Varset.union acc  (Subst.apply (Typ_var(b)) subst |> Type.free_vars))
       bs
       Varset.empty in
    let partial_subst = Varset.fold (fun bounded_v acc -> Subst.remove bounded_v acc) bs subst in
    Scheme(new_bs, Subst.apply t partial_subst)

  let instantiate (Scheme(bs, t)) : typ R.t =
    let open R.Syntax in
    Varset.fold (fun b acc ->
      let* acc = acc in
      let* fr = fresh_var() in
      let subst = Subst.singleton b fr in
      Subst.apply acc subst |> R.return)
      bs
      (R.return t)
  let pp fmt (Scheme(bs, t)) =
    Varset.pp fmt bs;
    Type.pp fmt t
end

type var_name = string

module TypeEnv = struct
  type t = (var_name, Scheme.t, String.comparator_witness) Base.Map.t

  let empty : t = Base.Map.empty(module Base.String)

  let default : t =
    let data = ["print_int", Scheme(Varset.empty, Typ_fun(Typ_int, Typ_unit))] in
    List.fold data ~init:empty ~f:(fun acc (k, v) -> Map.set acc ~key:k ~data:v)
  (* let find_exn name env = Base.Map.find_exn env name *)

  let find name env = Map.find env name

  let free_vars e =
     Map.fold e ~init:(Varset.empty) 
     ~f:(fun ~key:_ ~data:scheme acc -> Varset.union acc (Scheme.free_vars scheme))

  let extend name scheme e =
    match Map.add e ~key:name ~data:scheme with
    | `Ok m -> m
    | `Duplicate -> Map.set e ~key:name ~data:scheme 

  let apply subst (env : t) =
    Map.fold env ~init:empty ~f:(fun ~key ~data acc ->
      let refined = Scheme.apply_subst subst data in
      extend key refined acc)

  let generalize t env =
    (* let fv = Type.free_vars t in *)
    (* let() = Varset.pp Stdlib.Format.std_formatter fv in
    let() = Varset.pp Stdlib.Format.std_formatter (free_vars env) in *)
    let quantified = Varset.diff (Type.free_vars t) (free_vars env) in
    (* let() = Varset.pp Stdlib.Format.std_formatter quantified in *)
    Scheme(quantified, t)

  let rec extend_pattern p rhs_typ env =
    match p, rhs_typ with
    | Pat_var id, t ->
      let s = generalize t env in
      extend id s env
    | Pat_cons(px, pxs), Typ_list(element_typ) ->
      let env = extend_pattern px element_typ env in
      extend_pattern pxs rhs_typ env
    | Pat_nil, _ | Pat_wildcard, _ -> env
    | Pat_tuple(pfst, psnd, prest), Typ_tuple(tfst, tsnd, trest) ->
      let pats = pfst::psnd::prest in
      let types = tfst::tsnd::trest in
      List.fold2_exn pats types ~init:env ~f:(fun acc p t -> extend_pattern p t acc)
    | Pat_constrained(p, typ), _ -> (* it is safe since infer_pattern checked compatibility*)
      extend_pattern p typ env
    | Pat_unit, _ -> env
    |_ -> failwith (Printf.sprintf "Unsupported pattern matching %s" (show_pattern p))

  let lookup var env : (Subst.t * typ) R.t =
    let open R.Syntax in
    match find var env with
    | None -> Variable_not_found var |> R.fail
    | Some scheme -> 
      let* t = Scheme.instantiate scheme in
      R.return (Subst.empty, t)

  let pp fmt (env : t) =
    let env = Map.to_alist env in
    let open Stdlib.Format in
    fprintf
     fmt
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%s -> %a\n" k Scheme.pp v))
      env
end


open R.Syntax

let rec infer_pattern env p : (TypeEnv.t * typ) R.t  = match p with 
  | Pat_constrained(_, t) -> R.return (env, t)
  | Pat_const(Const_bool _) -> R.return (env, Typ_bool)
  | Pat_const(Const_int _) -> R.return (env, Typ_int)
  | Pat_unit -> R.return (env, Typ_unit)
  | Pat_wildcard ->
    let* fr = fresh_var() in
    R.return (env, fr)
  | Pat_var id ->
    (* (match TypeEnv.find id env with
    | Some(Scheme(_, t)) -> R.return (env, t) (* todo seems incorrect *)
    | None -> *)
      let* fr = fresh_var() in
      let env = TypeEnv.extend id (Scheme(Varset.empty, fr)) env in
      R.return (env, fr)
  | Pat_tuple(p1, p2, prest) ->
    let* env, t1 = infer_pattern env p1 in
    let* env, t2 = infer_pattern env p2 in
    let* env, trest = R.foldr prest ~init:(R.return (env, [])) ~f:(fun elem (env, types) ->
      let* env, t = infer_pattern env elem in
      R.return (env, t::types)) in
    let typ = Typ_tuple(t1, t2, trest) in
    R.return (env, typ)
  | Pat_cons(x, xs) ->
    let* env, x_t = infer_pattern env x in
    let* env, xs_t = infer_pattern env xs in
    let* uni = Subst.unify_pair xs_t  (Typ_list(x_t)) in
    let env = TypeEnv.apply uni env in
    R.return (env, xs_t)
  | Pat_nil ->
    let* fr = fresh_var() in
    R.return (env, Typ_list(fr))
  
let infer_expr env expr : (Subst.t * typ) R.t =
  let rec helper env expr = match expr with 
  | Expr_const(Const_bool _) -> R.return (Subst.empty, Typ_bool)
  | Expr_const(Const_int _) -> R.return (Subst.empty, Typ_int)
  | Expr_var "*" | Expr_var "/" | Expr_var "+" | Expr_var "-" -> 
    (Subst.empty, Typ_fun(Typ_int, Typ_fun(Typ_int, Typ_int))) |> R.return
  | Expr_var ">=" | Expr_var ">" | Expr_var "<=" | Expr_var "<" | Expr_var "=" ->
    (Subst.empty, Typ_fun(Typ_int, Typ_fun(Typ_int, Typ_bool))) |> R.return
  | Expr_var "&&" | Expr_var "||" ->
    (Subst.empty, Typ_fun(Typ_bool, Typ_fun(Typ_bool, Typ_bool))) |> R.return
  | Expr_var id -> TypeEnv.lookup id env
  | Expr_cons(x, xs) ->
    let* x_s, x_t = helper env x in
    let* xs_s, xs_t = helper (TypeEnv.apply x_s env) xs in
    let* uni = Subst.unify_pair xs_t (Typ_list(x_t)) in
    let* final_subst = Subst.compose_all [uni; xs_s; x_s] in
    R.return (final_subst, Subst.apply xs_t final_subst)
  | Expr_nil ->
    let* fr = fresh_var() in
    R.return (Subst.empty, Typ_list(fr))
  | Expr_tuple(fst, snd, rest) ->
      let* fst_s, fst_t = helper env fst in
      let* snd_s, snd_t = helper env snd in 
      let rest = List.map rest ~f:(helper env) in
      let* substs, rest_t = R.foldr rest ~init:(R.return ([], [])) ~f:(fun r (ss, ts) ->
        let* s, t = r in
        R.return (s::ss, t::ts)) in
      let typ = Typ_tuple(fst_t, snd_t, rest_t) in
      let* final_subst = Subst.compose_all (fst_s::snd_s::substs |> List.rev) in
      R.return (final_subst, Subst.apply typ final_subst)
  | Expr_let(NonRecursive, (p, v), scope) ->
    (* let open Stdlib.Format in
    let fmt = std_formatter in
    fprintf fmt "infering expr %s\n" (show_expr expr); *)
    let* expr_subst, expr_typ = helper env v in
    let* env, _ = infer_pattern (TypeEnv.apply expr_subst env) p in
    let env = TypeEnv.extend_pattern p expr_typ env in
    let* scope_subst, scope_typ = helper env scope in
    let* final_subst = Subst.compose_all [scope_subst; expr_subst] in
    R.return (final_subst, Subst.apply scope_typ final_subst)
  | Expr_let(Recursive, (p, v), scope) ->
    let* env, _ = infer_pattern env p in
    let* expr_subst, expr_typ = helper env v in
    let env = TypeEnv.apply expr_subst env in
    let env = TypeEnv.extend_pattern p expr_typ env in
    let* scope_subst, scope_typ = helper env scope in
    let* final_subst = Subst.compose_all [scope_subst; expr_subst] in
    R.return (final_subst,  Subst.apply scope_typ final_subst)
  | Expr_ite(cond, th, el) ->
      let* cond_subst, cond_typ = helper env cond in
      let* th_subst, th_typ = helper (TypeEnv.apply cond_subst env) th in
      let* el_subst, el_typ = helper (TypeEnv.apply th_subst env) el in
      let* cond_uni = Subst.unify_pair cond_typ Typ_bool in
      let* values_uni = Subst.unify_pair th_typ el_typ in
      let* final_subst = Subst.compose_all [values_uni; cond_uni; el_subst; th_subst; cond_subst] in
      R.return (final_subst, Subst.apply th_typ final_subst)
  | Expr_fun(param, expr) ->
    let* env, pat_typ = infer_pattern env param in
    (* let open Stdlib.Format in
    fprintf std_formatter "fun: %s\n" (show_expr f);
    TypeEnv.pp std_formatter env; *)
    let* s, expr_typ = helper env expr in
    let refined = Subst.apply pat_typ s in
    R.return (s, Typ_fun(refined,  expr_typ))
  | Expr_match(expr, cases) ->
    let* expr_subst, expr_typ = helper env expr in
    let* pat_types, expr_types, substs =
      R.foldr cases ~init:(R.return ([], [], [])) ~f:(fun (p, e) (pats, exprs, substs) ->
        let* env, ptyp = infer_pattern env p in
        let* subst, etyp = helper env e in
        R.return (ptyp::pats, etyp::exprs, subst::substs)) in
    let* unified_patterns_subst, _ = Subst.unify_many (expr_typ::pat_types) in
    let* unified_values_subst, representative = Subst.unify_many expr_types in
    let substs = expr_subst::substs @ [unified_patterns_subst; unified_values_subst] |> List.rev in
    let* final_subst = Subst.compose_all substs in
    R.return (final_subst, Subst.apply representative final_subst)
  | Expr_app(f, arg) ->
    (* let open Stdlib.Format in
    let fmt = std_formatter in
    fprintf fmt "infering app %s\n" (show_expr expr); *)

    let* body_typ = fresh_var() in
    let* f_s, f_t = helper env f in
    let* arg_s, arg_t = helper (TypeEnv.apply f_s env) arg in
    let fun_typ = Typ_fun(arg_t, body_typ) in
    let* uni_subst = Subst.unify_pair fun_typ f_t in
    let* final_subst = Subst.compose_all [uni_subst; arg_s; f_s] in
    R.return (final_subst, Subst.apply body_typ final_subst)
  | p  ->
    let() = pp_expr Stdlib.Format.std_formatter p in
     unreachable()
  in helper env expr

let infer_program (p : structure) =
  let helper (s, env) = function
  | Str_value(NonRecursive, [(p, e)]) ->
    let* e_subst, e_typ = infer_expr env e in
    let* env, _ = infer_pattern (TypeEnv.apply e_subst env) p in
    let env = TypeEnv.extend_pattern p e_typ env in
    let* final_subst = Subst.compose_all [e_subst; s] in
    R.return (final_subst, TypeEnv.apply final_subst env)
  | Str_value(Recursive, bindings) -> 
    let* env = R.foldl bindings ~init:(R.return env) ~f:(fun acc (p, _) -> 
      let* env, _ = infer_pattern acc p in
      R.return env) in
    R.foldl bindings ~init:(R.return (Subst.empty, env)) ~f:(fun (acc_s, acc_env) (p, e) ->
      let* e_subst, e_typ = infer_expr acc_env e in
      let* env, _ = infer_pattern (TypeEnv.apply e_subst env) p in
      let env = TypeEnv.extend_pattern p e_typ env in
      let* final_subst = Subst.compose_all [e_subst; acc_s] in
      R.return (final_subst, TypeEnv.apply final_subst env))
  | _ -> unreachable()
    in R.foldl p ~init:(R.return (Subst.empty, TypeEnv.default)) ~f:helper
    