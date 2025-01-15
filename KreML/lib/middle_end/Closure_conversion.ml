open Ast

(* module Closure_env = struct
  type t = (ident * expr) list

  let lookup env name =
    let _, v = List.find (fun (n, _) -> n = name) env in
    v
  ;;

  let extend env name value = (name, value) :: env
end *)

type lambda =
  | Lconst of const
  | Lvar of ident
  | Lclosure of closure
  | Lapp of lambda * lambda
  | Lite of lambda * lambda * lambda
  | Llet of rec_flag * ident * lambda * lambda
  | Lswitch (* TODO *)

and closure =
  { body : lambda
  ; env : closure_env
  ; total_args : int
  ; applied_count : int
  }

and closure_env = (int * lambda) list

and global_value =
  | Fun of closure
  | Var of lambda

type lprogram = (ident * global_value) list

let iconst i = Lconst (Const_int i)
let lapp f a = Lapp (f, a)
let lvar v = Lvar v
let llet id expr scope = Llet (NonRecursive, id, expr, scope)
let lletrec id expr scope = Llet (Recursive, id, expr, scope)


module Indexed_vars = struct
  (* include Stdlib.Set.Make (String) *)

  type vars = (ident, int, Base.String.comparator_witness) Base.Map.t

  let lookup_exn e name = Base.Map.find_exn e name

  open Utils.Counter

  let rec index (init : vars t) pat : vars t =
    let index_list = List.fold_left index in
    match pat with
    | Pat_wildcard | Pat_nil | Pat_const _ -> init
    | Pat_var x ->
      let* fresh = Utils.fresh_num in
      let* map = init in
      Base.Map.set map ~key:x ~data:fresh |> return
    | Pat_cons (x, y) -> index_list init [ x; y ]
    | Pat_constrained (x, _) -> index init x
    | Pat_tuple (fst, snd, rest) -> index_list init (fst :: snd :: rest)
  ;;
end

(* let pattern_Indexed_vars p =
  let rec helper acc = function
    | Pat_wildcard | Pat_nil | Pat_const _ -> acc
    | Pat_var x -> Indexed_vars.add x acc
    | Pat_cons (x, xs) -> helper (helper acc x) xs
    | Pat_constrained (p, _) -> helper acc p
    | Pat_tuple (fst, snd, rest) -> List.fold_left helper acc (fst :: snd :: rest)
  in
  helper Indexed_vars.empty p
;; *)

(* let rec expr_free_Indexed_vars = function
  | Expr_const _ | Expr_nil -> Indexed_vars.empty
  | Expr_fun (p, e) -> Indexed_vars.diff (expr_free_Indexed_vars e) (pattern_Indexed_vars p)
  | Expr_app (x, y) | Expr_cons (x, y) -> Indexed_vars.union (expr_free_Indexed_vars x) (expr_free_Indexed_vars y)
  | Expr_constrained (e, _) -> expr_free_Indexed_vars e
  | Expr_match (e, cases) -> list_free_Indexed_vars (e :: List.map snd cases)
  | Expr_tuple (fst, snd, rest) -> list_free_Indexed_vars (fst :: snd :: rest)
  | Expr_var v -> Indexed_vars.singleton v
  | Expr_ite (c, t, e) -> list_free_Indexed_vars [ c; t; e ]
  | Expr_let (_, (p, e), scope) ->
    let scope_fv = expr_free_Indexed_vars scope in
    let pfv = pattern_Indexed_vars p in
    Indexed_vars.union (expr_free_Indexed_vars e) (Indexed_vars.diff scope_fv pfv)

and list_free_Indexed_vars l =
  List.fold_left (fun acc e -> Indexed_vars.union acc (expr_free_Indexed_vars e)) Indexed_vars.empty l
;; *)

open Utils.Counter

let rec lexpr global_env iv e =
  let lexpr iv e = lexpr global_env iv e in
  let is_global_var v =
    match Base.Map.find global_env v with
    | Some _ -> true
    | None -> false
  in
  match e with
  | Expr_fun (p, e) ->
    let iv = Indexed_vars.index iv p in
    let body, total_args = extract_fun_body global_env iv e in
    let* body = body in
    Lclosure { body; total_args; applied_count = 0; env = [] } |> return
  | Expr_var x when is_global_var x -> Lvar x |> return
  | Expr_var x ->
    let* iv = iv in
    let number = Indexed_vars.lookup_exn iv x in
    lapp (lvar Runtime.access_closure) (iconst number) |> return
  | Expr_app (f, a) ->
    let* lf = lexpr iv f in
    let* la = lexpr iv a in
    lapp lf la |> return
  | Expr_constrained (e, _) -> lexpr iv e
  | Expr_cons (x, xs) ->
    let* x' = lexpr iv x in
    let* xs' = lexpr iv xs in
    lapp (lapp (lvar Runtime.list_cons) x') xs' |> return
  | Expr_ite (c, t, e) ->
    let* c' = lexpr iv c in
    let* t' = lexpr iv t in
    let* e' = lexpr iv e in
    Lite (c', t', e') |> return
  | Expr_let (NonRecursive, (p, e), scope) ->
    let iv = Indexed_vars.index iv p in
    split_let global_env iv p e (fun _ -> lexpr iv scope)
  | Expr_match _ | Expr_tuple _ -> Utils.internalfail "todo"
  | _ -> Utils.internalfail "todo" (* TODO *)

and extract_fun_body global_env iv expr =
  let rec helper iv acc_count = function
    | Expr_fun (p, e) ->
      let iv = Indexed_vars.index iv p in
      helper iv (acc_count + 1) e
    | e -> lexpr global_env iv e, acc_count
  in
  helper iv 0 expr

(* deconstructs [pattern] with [expr] on atomic [Llet] without any patterns
 and chains it in scope *)
and split_let global_env iv pat expr k =
  match pat, expr with
  | Pat_const _, _ | Pat_nil, _ | Pat_wildcard, _ ->
    Utils.internalfail "todo: check it in runtime"
  | Pat_constrained (p, _), _ -> split_let global_env iv p expr k
  | _, Expr_constrained (e, _) -> split_let global_env iv pat e k
  | Pat_var id, expr ->
    let* expr = lexpr global_env iv expr in
    let* scope = k (lvar id) in
    llet id expr scope |> return
  | Pat_cons (px, pxs), Expr_cons (ex, exs) ->
    split_let global_env iv px ex (fun _ -> split_let global_env iv pxs exs k)
  | Pat_cons (px, pxs), (Expr_var _ as var) ->
    let head = eapp (evar Runtime.list_head) [ var ] in
    split_let global_env iv px head (fun _ ->
      let tail = eapp (evar Runtime.list_tail) [ var ] in
      split_let global_env iv pxs tail k)
  | Pat_tuple (pfst, psnd, prest), Expr_tuple (efst, esnd, erest) ->
    let ps = pfst :: psnd :: prest in
    let es = efst :: esnd :: erest in
    split_list global_env iv ps es k
  | Pat_tuple (pfst, psnd, prest), (Expr_var _ as var) ->
    let ps = pfst :: psnd :: prest in
    let len = List.length ps in
    let es =
      Base.List.range ~start:`exclusive 0 len
      |> List.map (fun i ->
        eapp (evar Runtime.access_tuple) [ var; Expr_const (Const_int i) ])
    in
    split_list global_env iv ps es k
  | _, _ -> Utils.unreachable () (* program is type checked *)

and split_list global_env iv ps es k =
  let rec helper acc ps es =
    match ps, es with
    | [], [] -> acc |> List.hd |> k
    | p :: ps, e :: es ->
      split_let global_env iv p e (fun last -> helper (last :: acc) ps es)
    | _ -> Utils.unreachable ()
  in
  helper [] ps es
;;

let cc _ = Utils.unreachable ()
