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

  (* let ppvars fmt v =
    let v = Base.Map.to_alist v in
    let open Stdlib.Format in
    fprintf
      fmt
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%s -> %i\n" k v))
      v
  ;; *)

  (* let fresh_idx =
    let open Monad in
    get >>= fun ({ indexer; _ } as state) -> put { state with indexer = indexer + 1 }
  ;; *)

  let fresh_name =
    let open Monad in
    get
    >>= fun { global_env; local_env; iv; indexer; names_indexer } ->
    let name = Format.sprintf "fresh_fun_%i" names_indexer in
    put { global_env; local_env; iv; indexer; names_indexer = names_indexer + 1 }
    >>= fun _ -> return name
  ;;

  let lookup_iv_exn e name =
    match Base.Map.find e name with
    | Some v -> v
    | None -> internalfail @@ Format.sprintf "Unbound variable %s" name
  ;;

    let lookup_iv e name = Base.Map.find e name
  ;;

  let rec index pat =
    let open Monad in
    let* pat = pat in
    let index p = index (return p) in
    (* let* {iv; _} = get in
    let open Stdlib.Format in
    fprintf std_formatter "indexing with init %a\n" ppvars iv; *)
    match pat with
    | Pat_wildcard | Pat_nil | Pat_const _ -> return ()
    | Pat_var x ->
      let* ({ iv; indexer; _ } as state) = get in
      (* let open Stdlib.Format in
      fprintf std_formatter "indexed var %s with %i\n" x indexer; *)
      let iv = Base.Map.set iv ~key:x ~data:indexer in
      (* ppvars Format.std_formatter iv; *)
      let* _ = put { state with iv; indexer = indexer + 1 } in
      return ()
    | Pat_cons (x, y) ->
      let* _ = index x in
      index y
    | Pat_constrained (x, _) -> index x
    | Pat_tuple (fst, snd, rest) ->
      List.fold_left
        (fun acc p ->
          let* _ = acc in
          index p)
        (return ())
        (fst :: snd :: rest)
  ;;

  (* let%expect_test "asd" =
    let p = Pat_cons(Pat_var "x", Pat_cons(Pat_var "y", Pat_var "ys")) in
    let p = Monad.return p in
    let monad = index p in
    let _ = Monad.run  monad empty_ctx in
    () *)
end

module Freevars = struct
  include Stdlib.Set.Make (String)

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
    | Expr_var v -> if Ast.is_binary v then empty else singleton v
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
      | Lapp _ as a -> return a (* for now it is used only for runtime calls *)
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
  let* e = e in
  (* let* { iv; _ } = get in
  Stdlib.Format.fprintf
    Stdlib.Format.std_formatter
    "iv: %a; expr  %a:\n"
    CC_state.ppvars
    iv
    pp_expr
    e; *)
  let lexpr e = lexpr (return e) in
  let is_global_var v =
    let* { global_env; _ } = get in
    match List.find_opt (fun (id, _) -> id = v) global_env with
    | Some _ -> return true
    | None -> return false
  in
  match e with
  | Expr_const c -> Lconst c |> return
  | Expr_fun (p, e) as f ->
    let* body, total_args = extract_fun_body (return f) in
    let open Freevars in
    let free_vars = diff (collect_expr e) (collect_pat p) in
    lclosure body total_args free_vars |> return
  | Expr_var x ->
    let* is_global = is_global_var x in
    if is_global
    then Lvar x |> return
    else
      let* { iv; _ } = get in
      (* CC_state.ppvars Format.std_formatter iv; *)
      (match CC_state.lookup_iv iv x with
      | Some idx -> lapp (lvar Runtime.access_closure) (iconst idx) |> return
      | None when Runtime.is_stdlib_fun x -> lvar x |> return
      | None  -> internalfail (Format.sprintf "Unexpected identifier %s" x)) 
  | Expr_app (Expr_app (Expr_var op, x), y) when Ast.is_binary op ->
    let* x = lexpr x in
    let* y = lexpr y in
    lapp (lapp (lvar "+") x) y |> return
  | Expr_app ((Expr_fun _ as anonymous_fun), a) ->
    (* lift anonymous function to top level *)
    let* anonymous_fun' = lexpr anonymous_fun in
    let* fresh_name = CC_state.fresh_name in
    let* ({ global_env; _ } as state) = get in
    (match anonymous_fun' with
     | Lclosure closure ->
       let new_decl = Fun closure in
       let* _ = put { state with global_env = (fresh_name, new_decl) :: global_env } in
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
        | Some (_, Var lam) ->
          print_endline ( Format.asprintf "unexpected lambda %a" pp_lam lam);
          unreachable () (* program is type checked, can not apply arg to non function *)
        | None -> internalfail @@ Format.sprintf "Unknown identifier %s" id)
     | Lclosure f -> apply_arg f la |> return |> extend_inner_fun_env
     | Lapp _ as app -> lapp app la |> return
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
    let* () = CC_state.index (return p) in
    split_let p e ~is_top_level:false (fun _ -> lexpr scope)
  | Expr_match _ | Expr_tuple _ -> internalfail "todo"
  | _ -> internalfail (Format.asprintf "unexpected expr %a" pp_expr e)
(* TODO *)

and extract_fun_body expr =
  let rec helper acc_count e =
    let* e = e in
    match e with
    | Expr_fun (p, e) ->
      let* () = CC_state.index (return p) in
      helper (acc_count + 1) (return e)
    | e ->
      let* e' = lexpr (return e) in
      (e', acc_count) |> return
  in
  helper 0 expr

(* deconstructs [pattern] with [expr] on atomic [Llet] without any patterns
 and chains it in scope *)
and split_let pat expr ~is_top_level k =
  let split_let p e = split_let p e ~is_top_level in
  match pat, expr with
  | Pat_const _, _ | Pat_nil, _ | Pat_wildcard, _ ->
    internalfail "todo: check it in runtime"
  | Pat_constrained (p, _), _ -> split_let p expr k
  | _, Expr_constrained (e, _) -> split_let pat e k
  | Pat_var id, expr ->
    let* expr = lexpr (return expr) in
    (* let open Stdlib.Format in
    pp_lambda std_formatter expr; *)
    let* scope = k (lvar id) in
    let* _ =
      if is_top_level
      then
        let* ({ global_env; _ } as state) = get in
        let global_value =
          match expr with
          | Lclosure closure -> Fun closure
          | lam -> Var lam
        in
        let global_env = (id, global_value) :: global_env in
        put { state with global_env }
      else
        let* ({ local_env; _ } as state) = get in
        let local_env = Base.Map.set local_env ~key:id ~data:expr in
        put { state with local_env }
    in
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
    split_list ps es ~is_top_level k
  | Pat_tuple (pfst, psnd, prest), (Expr_var _ as var) ->
    let ps = pfst :: psnd :: prest in
    let len = List.length ps in
    let es =
      Base.List.range ~start:`exclusive 0 len
      |> List.map (fun i ->
        eapp (evar Runtime.access_tuple) [ var; Expr_const (Const_int i) ])
    in
    split_list ps es ~is_top_level k
  | _, _ -> unreachable () (* program is type checked *)

and split_list ps es ~is_top_level k =
  let rec helper acc ps es =
    match ps, es with
    | [], [] -> acc |> List.hd |> k
    | p :: ps, e :: es ->
      split_let p e ~is_top_level (fun last -> helper (last :: acc) ps es)
    | _ -> unreachable ()
  in
  helper [] ps es
;;

let cc structure =
  let init = CC_state.empty_ctx in
  let item_to_lambda acc_state (Str_value (_, bindings)) =
    let split_binding acc_state (p, e) =
      let l = split_let p e ~is_top_level:true return in
      let state, _ = run l acc_state in
      { state with indexer = 0; names_indexer = 0 }
    in
    List.fold_left split_binding acc_state bindings
  in
  let { global_env; _ } : CC_state.ctx = List.fold_left item_to_lambda init structure in
  let global_env = List.rev global_env in
  (* let open Stdlib.Format in
  let s = asprintf "count: %i xdddd %a" (List.length global_env) pp global_env in
  print_endline s; *)
  global_env
;;
