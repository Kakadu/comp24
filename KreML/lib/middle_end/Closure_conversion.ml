open Ast
open Utils
open Lambda

module CC_state = struct
  let empty_local_env = StringSet.empty

  type ctx =
    { global_env : lprogram
    ; local_env : StringSet.t
    ; names_indexer : int
    }

  let empty_ctx =
    { global_env = []
    ; local_env = empty_local_env
    ; names_indexer = 0
    }
  ;;

  module Monad = State (struct
      type t = ctx
    end)

  (* let pp fmt v =
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

  let fresh_name =
    let open Monad in
    get
    >>= fun ({ names_indexer; _ } as state) ->
    let name = Format.sprintf "fresh_fun_%i" names_indexer in
    put { state with names_indexer = names_indexer + 1 } >>= fun _ -> return name
  ;;

  let lookup_global_opt name =
    let open Monad in
    let* name = name in
    let* { global_env; _ } = get in
    return
      (match List.find_opt (fun (name', _) -> name = name') global_env with
       | Some (_, v) -> Some v
       | None -> None)
  ;;

  let lookup_var_opt name =
    let open Monad in
    let* name = name in
    let* { global_env; local_env; _ } = get in
    match StringSet.find_opt name local_env with
    | Some v -> return (Some (lvar v))
    | None ->
      (match List.find_opt (fun (id, _) -> id = name) global_env with
       | Some (_, l) -> Some l |> return
       | None -> None |> return)
  ;;

  let lookup_var_exn name =
    let open Monad in
    let* opt = lookup_var_opt name in
    match opt with
    | Some v -> return v
    | None ->
      let* name = name in
      internalfail @@ Format.sprintf "Unbound variable %s" name
  ;;
end

module Freevars = struct
  include StringSet

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
  | Lclosure ({ name; env; total_args; applied_count; freevars; _ } as outer_fun)
    when applied_count = total_args ->
    let rec extend = function
      | Lclosure ({ env = innner_env; freevars = inner_free_vars; _ } as inner_fun) ->
        (* extend inner *)
        let intersection = Freevars.inter freevars inner_free_vars in
        let* extended_env =
          Freevars.fold
            (fun id acc ->
              let* acc = acc in
              (* free var is either in closure env or in ctx.local_env*)
              let* value =
                match List.find_opt (fun (id', _) -> id = id') env with
                | Some (_, v) -> return v
                | None -> CC_state.lookup_var_exn (return id)
              in
              (id, value) :: acc |> return)
            intersection
            (return innner_env)
        in
        Lclosure { inner_fun with env = extended_env } |> return
      | Lcall (name, args, inner_env) ->
        let* f = CC_state.lookup_var_exn (return name) in
        (match f with
         | Lclosure { freevars = inner_free_vars; _ } ->
           let intersection = Freevars.inter freevars inner_free_vars in
           let inner_env' =
             Freevars.fold
               (fun id acc ->
                 let value = lookup_closure_env_exn env id in
                 match acc with
                 | None -> Some [ id, value ]
                 | Some inner_env -> Some ((id, value) :: inner_env))
               intersection
               inner_env
           in
           Lcall (name, args, inner_env') |> return
         | _ -> internalfail @@ Format.sprintf "unbound function name %s" name)
      | (Lvar _ | Lconst _) as atom -> return atom
      | Lite (c, t, e) ->
        let* c = extend c in
        let* t = extend t in
        let* e = extend e in
        Lite (c, t, e) |> return
      | Llet (rf, id, e, scope) ->
        let* e = extend e in
        let* scope = extend scope in
        Llet (rf, id, e, scope) |> return
      | Lbinop _ as l -> return l
      | Lswitch -> internalfail "TODO"
    in
    let* body = extend body in
    Lclosure { outer_fun with body } |> return
  | _ -> return lambda
;;

(* fun (a, b) -> ...  ~>  fun a -> fun b  -> ...*)

(* Updates environment on function based on pattern of incoming argument 
Example: (fun (a, b) -> ... ) (5, x) ~> env = {a -> 5; b -> x}*)
let rec apply_arg closure e =
  let* ({ arg_patterns; applied_count; _ } as c) = closure in
  let p, ps =
    match arg_patterns with
    | p :: ps -> p, ps
    | [] ->
      internalfail
      @@ Format.asprintf
           "Applying %a arg to non-function %a"
           pp_expr
           e
           pp_lam
           (Lclosure c)
  in
  (* pop arg *)
  let c = { c with arg_patterns = ps; applied_count = applied_count + 1 } in
  let zipped = Utils.zip_idents_with_exprs p e in
  List.fold_left
    (fun acc_cl (id, e) ->
      let* ({ env; _ } as cl) = acc_cl in
      let* e = lexpr (return e) in
      return { cl with env = (id, e) :: env })
    (return c)
    zipped

and lexpr e =
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
  match e with
  | Expr_const c -> Lconst c |> return
  | Expr_fun _ as f ->
    (* it is guaranteed by split_let that function is anonymous if it is 
      resolved in lexpr, so we generate fresh name and lift it to top level *)
    let* body, patterns, freevars = resolve_fun (return f) in
    let* fresh_name = CC_state.fresh_name in
    let closure = lclosure fresh_name body patterns freevars in
    let* ({ global_env; _ } as state) = get in
    let* _ = put { state with global_env = (fresh_name, body) :: global_env } in
    lvar fresh_name |> return
  | Expr_var x ->
    let* lookup = CC_state.lookup_var_opt (return x) in
    (match lookup with
     | Some _ -> lvar x |> return (* either local or global *)
     | None -> lcall Runtime.access_closure [ lvar x ] |> return)
    (* closure var *)
  | Expr_app (Expr_app (Expr_var op, x), y) when Ast.is_binary op ->
    let* x = lexpr x in
    let* y = lexpr y in
    let binop = resolve_binop op in
    Lbinop (binop, x, y) |> return
  | Expr_app (f, a) ->
    let* lf = lexpr f in
    (match lf with
     | Lvar id ->
       let* lookup = CC_state.lookup_var_opt (return id) in
       (match lookup with
        | Some (Lclosure f) ->
          let* f' = apply_arg (return f) a in
          Lclosure f' |> return |> extend_inner_fun_env
        | Some lam ->
          print_endline (Format.asprintf "unexpected lambda %a" pp_lam lam);
          unreachable () (* program is type checked, can not apply arg to non function *)
        | None -> internalfail @@ Format.sprintf "Unknown identifier %s" id)
     | Lclosure f ->
       let* f' = apply_arg (return f) a in
       Lclosure f' |> return |> extend_inner_fun_env
     | f -> internalfail @@ Format.asprintf "Expected function, got %a" pp_lambda f)
  | Expr_constrained (e, _) -> lexpr e
  | Expr_cons (x, xs) ->
    let* x' = lexpr x in
    let* xs' = lexpr xs in
    lcall Runtime.list_cons [ x'; xs' ] |> return
  | Expr_ite (c, t, e) ->
    let* c' = lexpr c in
    let* t' = lexpr t in
    let* e' = lexpr e in
    Lite (c', t', e') |> return
  | Expr_let (rec_flag, binding, (Expr_fun _ as f)) ->
    (* lift anonymous fun with closure *)
    let* f' = lexpr f in
    let* ({ global_env; _ } as state) = get in
    (match f' with
     | Lclosure closure ->
       let new_decl = closure in
       let* fresh_name = CC_state.fresh_name in
       let* _ = put { state with global_env = (fresh_name, new_decl) :: global_env } in
       elet ~rec_flag binding (evar fresh_name) |> lexpr
     | _ -> unreachable ())
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
    split_let p e ~is_top_level:false (fun _ -> lexpr scope)
  | Expr_match _ | Expr_tuple _ -> internalfail "todo"
  | _ -> internalfail (Format.asprintf "unexpected expr %a" pp_expr e)

and resolve_fun expr =
  let rec collect_params l =
    let* l = l in
    match l with
    | Lbinop _ | Lcall _ | Lconst _ -> return []
    | Lclosure {arg_patterns; _} -> return arg_patterns
    | Llet(_, _, _, scope) -> collect_params (return scope) in
    | Lite(c, t, e)
  let* expr = expr in
  match expr with
  | Expr_fun (p, e) as f ->
    let* body = extract_fun_body (return f) in
    let patterns = List.rev patterns in
    let open Freevars in
    let free_vars = diff (collect_expr e) (collect_pat p) in
    let* { global_env; _ } = get in
    let free_vars = diff free_vars (List.map fst global_env |> StringSet.of_list) in
    return (body, patterns, free_vars)
  | _ ->
    internalfail @@ Format.asprintf "resolve fun: %a is not a function" Ast.pp_expr expr

and extract_fun_body expr =
  let* expr = expr in
    match expr with
    | Expr_fun (p, e) ->
      (* we need to clear local environment when resolving body and restore it after *)
      let* ({ local_env; _ } as state) = get in
      let locals = Freevars.collect_pat p in
      let* _ = put { state with local_env = locals } in
      let* res = extract_fun_body (return e) in
      (* restore *)
      let* _ = put { state with local_env } in
      return res
    | e -> lexpr (return e) in

(* deconstructs [pattern] with [expr] on atomic [Llet] without any patterns
 and chains it in scope *)
and split_let pat expr ~is_top_level k =
  let zipped = zip_idents_with_exprs pat expr in
  let* zipped =
    List.fold_right
      (fun (id, e) acc ->
        let* acc = acc in
        let* e = lexpr (return e) in
        (id, e) :: acc |> return)
      zipped
      (return [])
  in
  let* _ =
    List.fold_left
      (fun acc (id, e) ->
        let* () = acc in
        if is_top_level
        then
          let* ({ global_env; _ } as state) = get in
          let global_value =
            match e with
            | Lclosure closure -> Fun closure
            | _ -> unreachable ()
          in
          let global_env = (id, global_value) :: global_env in
          let* _ = put { state with global_env } in
          return ()
        else
          let* ({ local_env; _ } as state) = get in
          let local_env = StringSet.add id local_env in
          let* _ = put { state with local_env } in
          return ())
      (return ())
      zipped
  in
  let* scope = k () in
  if not is_top_level
  then (* create locals declarations *)
    List.fold_right
      (fun (id, l) acc ->
        let* acc = acc in
        let* lookup = CC_state.lookup_global_opt (return id) in
        return
          (match lookup with
           | Some _ -> llet id l acc
           | None -> acc))
      zipped
      (return scope)
  else return scope
;;

let cc structure =
  let init = CC_state.empty_ctx in
  let item_to_lambda acc_state (Str_value (_, bindings)) =
    let split_binding (({ arg_counts; _ } : CC_state.ctx ) as acc_state) (p, e) =
      let arg_counts', _ = count_args_with_state e in
      let state =
        { acc_state with
          arg_counts =
            Base.Map.merge_skewed arg_counts arg_counts' ~combine:(fun ~key:_ _ _ ->
              internalfail "maps are expected to be disjoint")
        }
      in
      let l = split_let p e ~is_top_level:true (fun _ -> lvar "" |> return) in
      let state, _ = run l state in
      { state with names_indexer = 0 }
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

let f x y =
  let a = x + y in
  fun t -> t + a
;;

(* rewrites to *)

let extra env t = t + env

let f x y =
  let a = x + y in
  extra a
;;

(* *)

let partial1 = f 5

(* Lclosure {name = f, env = {x -> 5}}*)

let partial2 = partial1 10
(* Lcall(f, 10, {x -> 5})*)
(**)

let f =
  fun x ->
  let a = x + 1 in
  fun y ->
    let b = a + 1 in
    fun z ->
      let c = b + 2 in
      x + y + z + a + b + c
;;

(*let f3 env z =
  let c = (env.b) + 2
  (env.x + env.y + z + env.a + env.b + c) *)

(*let f2 env y =
  let b = env.a + 1 in
  maybe update env from my env and locals?
  Closure(name: f3, y::b::env)
*)

(*let f1 x =
  let a = x + 1 in
  Closure(name: f2, a::x::env )*)

(* let p1 = f 1 = Closure(name: f1, x -> 1)*)
(* let p2 = p1 5 = Closure(name: f1, x -> 1, y -> 5)*)
(* let p3 = p2 10 = Closure (name: f1, x -> 1, y -> 5, z -> 10)*)
