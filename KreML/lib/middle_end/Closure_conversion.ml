open Ast
open Utils
open Lambda



module CC_state = struct
  type vars = (ident, int, Base.String.comparator_witness) Base.Map.t

  let empty_vars = Base.Map.empty (module Base.String)

  type local_env = (ident, lambda, Base.String.comparator_witness) Base.Map.t

  let empty_local_env = empty_vars

  type ctx =
    { global_env : lprogram
    ; local_env : local_env
    ; iv : vars
    ; indexer : int
    ; names_indexer : int
    }

  let empty_ctx =
    { global_env = []
    ; local_env : local_env = empty_vars
    ; iv = empty_local_env
    ; indexer = 0
    ; names_indexer = 0
    }
  ;;

  module Monad = State (struct
      type t = ctx
    end)

  let fresh_idx =
    let open Monad in
    get >>= fun ({ indexer; _ } as state) -> put { state with indexer = indexer + 1 }
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

module Freevars = struct
  include Stdlib.Set.Make(String)

  let rec collect_pat = function
    | Pat_wildcard | Pat_nil | Pat_const _ -> empty
    | Pat_var x -> singleton x
    | Pat_cons (x, xs) -> union (collect_pat x) (collect_pat xs)
    | Pat_constrained (p, _) -> collect_pat p
    | Pat_tuple (fst, snd, rest) ->
      List.fold_left (fun acc p -> union acc (collect_pat p)) empty (fst :: snd :: rest)
  ;;

  let rec collect_expr = function
    | Expr_const _ | Expr_nil -> empty
    | Expr_fun (p, e) -> diff (collect_expr e) (collect_pat p)
    | Expr_app (x, y) | Expr_cons (x, y) -> union (collect_expr x) (collect_expr y)
    | Expr_constrained (e, _) -> collect_expr e
    | Expr_match (e, cases) -> collect_list (e :: List.map snd cases)
    | Expr_tuple (fst, snd, rest) -> collect_list (fst :: snd :: rest)
    | Expr_var v -> singleton v
    | Expr_ite (c, t, e) -> collect_list [ c; t; e ]
    | Expr_let (_, (p, e), scope) ->
      let scope_fv = collect_expr scope in
      let pfv = collect_pat p in
      union (collect_expr e) (diff scope_fv pfv)

  and collect_list l = List.fold_left (fun acc e -> union acc (collect_expr e)) empty l
end

open CC_state.Monad

(* passes env variables from outer function to inner function *)
let extend_inner_fun_env call =
  let* lambda = call in
  match lambda with
  | Lclosure { body; env; total_args; applied_count; freevars; _ }
    when applied_count = total_args ->
    let rec extend = function
      | Lclosure
          ({ body = inner_body; env = innner_env; freevars = inner_free_vars; _ } as
           inner_fun) ->
        (* extend inner *)
        let* extended_body = extend inner_body in
        let* { iv; local_env; _ } : CC_state.ctx = get in
        (* find var either in closure or local environment *)
        let find_var id =
          match Base.Map.find local_env id with
          | Some v -> v
          | None ->
            let id = CC_state.lookup_iv_exn iv id in
            List.find (fun (closure_arg_id, _) -> closure_arg_id = id) env |> snd
        in
        let intersection = Freevars.inter freevars inner_free_vars in
        let extended_env =
          Freevars.fold
            (fun id acc ->
              let idx = Base.Map.find_exn iv id in
              let value = find_var id in
              (idx, value) :: acc)
            intersection
            innner_env
        in
        Lclosure { inner_fun with env = extended_env; body = extended_body } |> return
      | (Lvar _ | Lconst _) as atom -> return atom
      | Lite (c, t, e) ->
        let* c = extend c in
        let* t = extend t in
        let* e = extend e in
        Lite (c, t, e) |> return
      | LApp _ as a -> return a (* for now it is used only for runtime calls *)
      | Llet (rf, id, e, scope) ->
        let* e = extend e in
        let* scope = extend scope in
        Llet (rf, id, e, scope) |> return
      | Lswitch -> internalfail "TODO"
    in
    extend body
  | _ -> return lambda
;;

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
    let open Freevars in
    let free_vars = diff (collect_expr e) (collect_pat p) in
    lclosure body total_args free_vars |> return
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
        | Some (_, Fun f) -> apply_arg f la |> return |> extend_inner_fun_env
        | Some _ ->
          unreachable () (* program is type checked, can not apply arg to non function *)
        | None -> internalfail @@ Format.sprintf "Unknown identifier %s" id)
     | Lclosure f -> apply_arg f la |> return |> extend_inner_fun_env
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
    (* TODO: maybe need second pass on Llet for dumb constructions like let a, f = 5, (fun x -> x) in... *)
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
    let* ({ local_env; _ } as state) = get in
    let local_env = Base.Map.set local_env ~key:id ~data:expr in
    let* _ = put { state with local_env } in
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

let cc structure =
  let init = CC_state.empty_ctx in
  let item_to_lambda acc_state (Str_value (_, bindings)) =
    let split_binding acc_state (p, e) =
      let l = split_let p e return in
      let state, _ = run l acc_state in
      { state with indexer = 0; names_indexer = 0 }
    in
    List.fold_left split_binding acc_state bindings
  in
  let { global_env; _ } : CC_state.ctx = List.fold_left item_to_lambda init structure in
  global_env
;;
