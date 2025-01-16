open Ast
open Utils

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
[@@deriving show]

and closure =
  { body : lambda
  ; env : closure_env
  ; total_args : int
  ; applied_count : int
  }
[@@deriving show]

and closure_env = (int * lambda) list [@@deriving show]

and global_value =
  | Fun of closure
  | Var of lambda
[@@deriving show]

type lprogram = (ident * global_value) list [@@deriving show]

let apply_arg { body; env; total_args; applied_count } arg =
  Lclosure
    { body
    ; total_args
    ; applied_count = applied_count + 1
    ; env = (applied_count, arg) :: env
    }
;;

let iconst i = Lconst (Const_int i)
let lclosure body total_args = Lclosure { body; total_args; applied_count = 0; env = [] }
let lapp f a = Lapp (f, a)
let lvar v = Lvar v
let llet id expr scope = Llet (NonRecursive, id, expr, scope)
let lletrec id expr scope = Llet (Recursive, id, expr, scope)

module CC_state = struct
  type vars = (ident, int, Base.String.comparator_witness) Base.Map.t
  type local_env = (ident, lambda, Base.String.comparator_witness) Base.Map.t

  type ctx =
    { global_env : lprogram
    ; local_env : local_env
    ; iv : vars
    ; indexer : int
    ; names_indexer : int
    }

  module Monad = State (struct
      type t = ctx
    end)

  let fresh_idx =
    let open Monad in
    get
    >>= fun { indexer; iv; global_env; local_env; names_indexer } ->
    put { indexer = indexer + 1; iv; global_env; local_env; names_indexer }
  ;;

  let fresh_name =
    let open Monad in
    get
    >>= fun { global_env; local_env; iv; indexer; names_indexer } ->
    let name = Format.sprintf "fresh_fun_%i" names_indexer in
    put { global_env; local_env; iv; indexer; names_indexer = names_indexer + 1 }
    >>= fun _ -> return name
  ;;

  let lookup_iv_exn e name = Base.Map.find_exn e name

  let rec index (init : ctx Monad.t) pat : ctx Monad.t =
    let open Monad in
    let index_list = List.fold_left index in
    match pat with
    | Pat_wildcard | Pat_nil | Pat_const _ -> init
    | Pat_var x ->
      let* ({ iv; indexer; _ } as state) = fresh_idx in
      let iv = Base.Map.set iv ~key:x ~data:indexer in
      put { state with iv }
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

open CC_state.Monad

let rec lexpr e =
  let is_global_var v =
    let* { global_env; _ } = get in
    match List.find_opt (fun (id, _) -> id = v) global_env with
    | Some _ -> return true
    | None -> return false
  in
  match e with
  | Expr_fun (p, e) ->
    let* _ = CC_state.index get p in
    let* body, total_args = extract_fun_body (return e) in
    lclosure body total_args |> return
  | Expr_var x ->
    let* is_global = is_global_var x in
    if is_global
    then Lvar x |> return
    else
      let* { iv; _ } = get in
      let number = CC_state.lookup_iv_exn iv x in
      lapp (lvar Runtime.access_closure) (iconst number) |> return
  | Expr_app ((Expr_fun _ as anonymous_fun), a) ->
    (* lift anonymous function to top level *)
    let* anonymous_fun' = lexpr anonymous_fun in
    let* fresh_name = CC_state.fresh_name in
    let* ({ global_env; _ } as state) = get in
    (match anonymous_fun' with
     | Lclosure closure ->
       let new_decl = Fun closure in
       let _ = put { state with global_env = (fresh_name, new_decl) :: global_env } in
       let pointer = evar fresh_name in
       lexpr @@ eapp pointer [ a ]
     | l ->
       internalfail
       @@ Format.asprintf
            "Unexpected term %a after resolving anonymous function"
            pp_lambda
            l)
  | Expr_app (f, (Expr_fun _ as anonymous_fun)) ->
    (* lift anonymous function to top level *)
    let* anonymous_fun' = lexpr anonymous_fun in
    let* fresh_name = CC_state.fresh_name in
    let* ({ global_env; _ } as state) = get in
    (match anonymous_fun' with
     | Lclosure closure ->
       let new_decl = Fun closure in
       let* _ = put { state with global_env = (fresh_name, new_decl) :: global_env } in
       let pointer = Expr_var fresh_name in
       lexpr @@ eapp f [ pointer ]
     | l ->
       internalfail
       @@ Format.asprintf
            "Unexpected term %a after resolving anonymous function"
            pp_lambda
            l)
  | Expr_app (f, a) ->
    let* lf = lexpr f in
    let* la = lexpr a in
    let* { global_env; _ } = get in
    (match lf with
     | Lvar id ->
       (match List.find_opt (fun (x, _) -> x = id) global_env with
        | Some (_, Fun f) -> apply_arg f la |> return
        | Some _ ->
          unreachable () (* program is type checked, can not apply arg to non function *)
        | None -> internalfail @@ Format.sprintf "Unknown identifier %s" id)
     | Lclosure f -> apply_arg f la |> return
     | f -> internalfail @@ Format.asprintf "Expected function, got %a" pp_lambda f)
  | Expr_constrained (e, _) -> lexpr e
  | Expr_cons (x, xs) ->
    let* x' = lexpr x in
    let* xs' = lexpr xs in
    lapp (lapp (lvar Runtime.list_cons) x') xs' |> return
  | Expr_ite (c, t, e) ->
    let* c' = lexpr c in
    let* t' = lexpr t in
    let* e' = lexpr e in
    Lite (c', t', e') |> return
  | Expr_let (_, (Pat_var id, (Expr_fun _ as f)), scope) ->
    (* lift anonmous fun to top level *)
    (* TODO: maybe need second pass on Llet for dumb constructions like let a, f = 5, (fun x -> x) *)
    let* f' = lexpr f in
    let* ({ global_env; _ } as state) = get in
    (match f' with
     | Lclosure closure ->
       let new_decl = Fun closure in
       let* _ = put { state with global_env = (id, new_decl) :: global_env } in
       lexpr scope
     | l ->
       internalfail
       @@ Format.asprintf
            "Unexpected term %a after resolving anonymous function"
            pp_lambda
            l)
  | Expr_let (NonRecursive, (p, e), scope) ->
    let* _ = CC_state.index get p in
    split_let p e (fun _ -> lexpr scope)
  | Expr_match _ | Expr_tuple _ -> internalfail "todo"
  | _ -> internalfail "todo" (* TODO *)

and extract_fun_body expr =
  let* expr = expr in
  let rec helper acc_count = function
    | Expr_fun (p, e) ->
      let* _ = CC_state.index get p in
      helper (acc_count + 1) e
    | e ->
      let* e' = lexpr e in
      (e', acc_count) |> return
  in
  helper 0 expr

(* deconstructs [pattern] with [expr] on atomic [Llet] without any patterns
 and chains it in scope *)
and split_let pat expr k =
  match pat, expr with
  | Pat_const _, _ | Pat_nil, _ | Pat_wildcard, _ ->
    internalfail "todo: check it in runtime"
  | Pat_constrained (p, _), _ -> split_let p expr k
  | _, Expr_constrained (e, _) -> split_let pat e k
  | Pat_var id, expr ->
    let* expr = lexpr expr in
    let* scope = k (lvar id) in
    llet id expr scope |> return
  | Pat_cons (px, pxs), Expr_cons (ex, exs) ->
    split_let px ex (fun _ -> split_let pxs exs k)
  | Pat_cons (px, pxs), (Expr_var _ as var) ->
    let head = eapp (evar Runtime.list_head) [ var ] in
    split_let px head (fun _ ->
      let tail = eapp (evar Runtime.list_tail) [ var ] in
      split_let pxs tail k)
  | Pat_tuple (pfst, psnd, prest), Expr_tuple (efst, esnd, erest) ->
    let ps = pfst :: psnd :: prest in
    let es = efst :: esnd :: erest in
    split_list ps es k
  | Pat_tuple (pfst, psnd, prest), (Expr_var _ as var) ->
    let ps = pfst :: psnd :: prest in
    let len = List.length ps in
    let es =
      Base.List.range ~start:`exclusive 0 len
      |> List.map (fun i ->
        eapp (evar Runtime.access_tuple) [ var; Expr_const (Const_int i) ])
    in
    split_list ps es k
  | _, _ -> unreachable () (* program is type checked *)

and split_list ps es k =
  let rec helper acc ps es =
    match ps, es with
    | [], [] -> acc |> List.hd |> k
    | p :: ps, e :: es -> split_let p e (fun last -> helper (last :: acc) ps es)
    | _ -> unreachable ()
  in
  helper [] ps es
;;

let cc _ = unreachable ()
