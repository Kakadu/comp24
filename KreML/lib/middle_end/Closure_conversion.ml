open Ast

module Closure_env = struct
  type t = (ident * expr) list

  let lookup env name =
    let _, v = List.find (fun (n, _) -> n = name) env in
    v 

  let extend env name value =
    (name, value)::env
end

type lambda =
  | Lconst of const
  | Lvar of ident
  | Lclosure of lambda * Closure_env.t
  | Lapp of lambda * lambda
  | Lite of lambda * lambda * lambda
  | Llet of rec_flag * ident * lambda * lambda
  | Lswitch (* TODO *)

let iconst i = Lconst(Const_int i)
let lapp f a = Lapp(f, a)
let lvar v = Lvar v
let llet id expr scope = Llet(NonRecursive, id, expr, scope)
let lletrec id expr scope = Llet(Recursive, id, expr, scope)

type lprogram = lambda list

module Vars = struct
  include Stdlib.Set.Make(String)

  type enumerate = (ident, int, Base.String.comparator_witness) Base.Map.t

  let lookup_exn e name = Base.Map.find_exn e name

  open Utils.Counter
  let rec enumerate_vars (init : enumerate t) pat : enumerate t =
    let enumerate_list = List.fold_left enumerate_vars in
    match pat with
    | Pat_wildcard | Pat_nil | Pat_const _ -> init
    | Pat_var x ->
      let* fresh = Utils.fresh_num in
      let* map = init in
      Base.Map.set map ~key:x ~data:fresh |> return
    | Pat_cons(x, y) ->  enumerate_list init [x; y]
    | Pat_constrained(x, _) -> enumerate_vars init x
    | Pat_tuple(fst, snd, rest) -> enumerate_list  init (fst::snd::rest)

end

let pattern_vars p =
    let rec helper acc = function
    | Pat_wildcard | Pat_nil | Pat_const _ -> acc
    | Pat_var x -> Vars.add x acc
    | Pat_cons(x, xs) -> helper (helper acc x) xs
    | Pat_constrained(p, _) -> helper acc p
    | Pat_tuple(fst, snd, rest) -> List.fold_left helper acc (fst :: snd :: rest) in
    helper Vars.empty p


  let rec expr_free_vars = function
    | Expr_const _ | Expr_nil -> Vars.empty
    | Expr_fun(p, e) -> Vars.diff (expr_free_vars e) (pattern_vars p)
    | Expr_app(x, y) | Expr_cons(x, y) -> Vars.union (expr_free_vars x) (expr_free_vars y)
    | Expr_constrained(e, _) -> expr_free_vars e
    | Expr_match(e, cases) ->
      list_free_vars (e::(List.map snd cases))
    | Expr_tuple(fst, snd, rest) ->
      list_free_vars (fst :: snd :: rest)
    | Expr_var v -> Vars.singleton v
    | Expr_ite(c, t, e) -> list_free_vars [c; t; e]
    | Expr_let(_, (p, e), scope) ->
      let scope_fv = expr_free_vars scope in
      let pfv = pattern_vars p in
      Vars.union (expr_free_vars e)  (Vars.diff scope_fv pfv)
  and list_free_vars l =
    List.fold_left (fun acc e -> Vars.union acc (expr_free_vars e)) Vars.empty l


open Utils.Counter
let rec lexpr global_env enumerated_vars e =
  let lexpr vars e = lexpr global_env vars e in
  let is_global_var v =
    match Base.Map.find global_env v with
    | Some _ -> true
    | None -> false in
  match e with
  | Expr_fun(p, e) ->
    let enumerated = Vars.enumerate_vars enumerated_vars p in
    extract_fun_body global_env enumerated e
  | Expr_var x when is_global_var x -> Lvar(x) |> return
  | Expr_var x ->
    let* enumerated = enumerated_vars in
    let number = Vars.lookup_exn enumerated x in
    lapp (lvar Runtime.access_closure) (iconst number) |> return
  | Expr_app(f, a) ->
    let* lf = lexpr enumerated_vars f in
    let* la = lexpr enumerated_vars a in
    lapp lf la|> return
  | Expr_constrained(e, _) -> lexpr enumerated_vars e
  | Expr_cons(x, xs) ->
    let* x' = lexpr enumerated_vars x in
    let* xs' = lexpr enumerated_vars xs in
    lapp (lapp (lvar Runtime.list_cons) x') xs' |> return
  | Expr_ite(c, t, e) ->
    let* c' = lexpr enumerated_vars c in
    let* t' = lexpr enumerated_vars t in
    let* e' = lexpr enumerated_vars e in
    Lite(c', t', e') |> return
  | Expr_let(rf, (p, e), scope) -> Utils.unreachable()
  | Expr_match _  | Expr_tuple _ -> Utils.unreachable() 
  | _ -> Utils.unreachable() (* TODO *)

and extract_fun_body global_env enumerated_vars = function
  | Expr_fun(p, e) ->
    let enumerated = Vars.enumerate_vars enumerated_vars p in
    extract_fun_body global_env enumerated e
  | e -> lexpr global_env enumerated_vars e

(* deconstructs [pattern] with [expr] in let and chains it in scope *)
and split_let global_env enumerated_vars pat expr k =
  match pat, expr with
  | Pat_const _, _ | Pat_nil _, _ | Pat_wildcard, _ -> Utils.internalfail "todo"
  | Pat_constrained(p, _), _ -> split_let global_env enumerated_vars p expr k
  | _, Expr_constrained(e, _) -> split_let global_env enumerated_vars pat e k
  | Pat_var id, expr ->
    let* expr = lexpr global_env enumerated_vars expr in
    let* scope = k (lvar id) in
    llet id expr scope |> return
  | Pat_cons(px, pxs), Expr_cons(ex, exs) ->
    split_let global_env enumerated_vars px ex  (fun _ ->
    split_let global_env enumerated_vars pxs exs  k)
  | Pat_cons(px, pxs), Expr_var e ->
    let head =  eapp (evar Runtime.list_head) [evar e] in
    split_let global_env enumerated_vars px head (fun _ ->
      let tail = eapp (evar Runtime.list_tail) [evar e] in
      split_let global_env enumerated_vars pxs tail k)
  | Pat_tuple(pfst, psnd, prest), Expr_tuple(efst, esnd, erest) ->
    let ps = pfst :: psnd :: prest in
    let es = efst :: esnd :: erest in
     split_list global_env enumerated_vars ps es k
  | Pat_tuple(pfst, psnd, prest), Expr_var v ->
    let ps = pfst :: psnd :: prest in
    let len = List.length ps in
    let es = Base.List.range ~start:`exclusive 0 len |> List.map (fun i ->
      eapp (evar Runtime.access_tuple) [Expr_const(Const_int i)]) in
    split_list global_env enumerated_vars ps es k
  | _, _ -> Utils.unreachable() (* program is type checked *)
  
  and split_list global_env enumerated_vars ps es k =
    let rec helper acc ps es =
    match ps, es with
    | [], [] -> acc |> List.hd |> k
    | p::ps, e::es ->
      split_let global_env enumerated_vars p e (fun last ->
        helper (last::acc) ps es)
    | _ -> Utils.unreachable() in
    helper [] ps es
    


let cc : structure -> lprogram = fun structure ->
  
