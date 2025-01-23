(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Utils
open Flambda

module CC_state = struct
  type freevars = (string, string list, Base.String.comparator_witness) Base.Map.t

  type ctx =
    { global_env : flstructure
    ; freevars : freevars
    ; arities : arities
    }

  let empty_ctx arities =
    { global_env = []; freevars = Base.Map.empty (module Base.String); arities }
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

  let lookup_global_opt name =
    let open Monad in
    let* name = name in
    let* { global_env; _ } = get in
    return
      (match List.find_opt (fun (name', _) -> name = name') global_env with
       | Some (_, v) -> Some v
       | None -> None)
  ;;
end

module Freevars = struct
  include StringSet

  let collect_imm = function
    | Avar id -> singleton id
    | _ -> empty
  ;;

  let rec collect_cexpr = function
    | CImm imm -> collect_imm imm
    | CBinop (_, x, y) -> union (collect_imm x) (collect_imm y)
    | CGetfield (_, i) -> collect_imm i
    | CApp (f, args) ->
      List.fold_left (fun acc e -> union acc (collect_imm e)) empty (f :: args)
    | CIte (c, t, e) ->
      let acc = union (collect_aexpr t) (collect_imm c) in
      union acc (collect_aexpr e)
    | CFun (id, body) -> remove id (collect_aexpr body)
    | CCons (x, xs) -> union (collect_imm x) (collect_imm xs)
    | CTuple elems -> List.fold_left (fun acc e -> union acc (collect_imm e)) empty elems

  and collect_aexpr = function
    | AExpr e -> collect_cexpr e
    | ALet (_, id, c, scope) ->
      let acc = remove id (collect_aexpr scope) in
      union acc (collect_cexpr c)
  ;;
end

open CC_state
open CC_state.Monad

let fun_call_args_reversed f =
  let rec helper acc = function
    | AExpr (CFun (id, body)) -> helper (id :: acc) body
    | body -> acc, body
  in
  helper [] f
;;

let imm i =
  let* i = i in
  match i with
  | Avar id ->
    let* lookup = lookup_global_opt (return id) in
    (match lookup with
     | None ->
       let* { arities; freevars; _ } = get in
       (match Base.Map.find freevars id with
        | None -> Fl_var id |> return (* just a local var *)
        | Some inherited_env ->
          (* self recursive function, lets build its closure *)
          let arity = Base.Map.find_exn arities id in
          let env_size = arity - 1 + List.length inherited_env in
          let start_index = arity - 1 in
          let arrange = List.mapi (fun i id -> i + start_index, flvar id) inherited_env in
          Fl_closure { name = id; env_size; arrange } |> return)
     | Some (Fun_with_env { arity; captured_args; _ }) ->
       let env_size = List.length captured_args in
       (* inherited args come after call args *)
       let _, inherited_env = Base.List.split_n captured_args (arity - 1) in
       let start_index = arity - 1 in
       let arrange = List.mapi (fun i id -> i + start_index, flvar id) inherited_env in
       Fl_closure { name = id; env_size; arrange } |> return
     | Some (Fun_without_env _) ->
       Fl_closure { name = id; env_size = 0; arrange = [] } |> return)
  | Aconst c -> Fl_const c |> return
;;

let rec resolve_fun name f =
  let* f = f in
  let call_args_rev, body = fun_call_args_reversed (AExpr f) in
  let arg, call_args_env =
    match call_args_rev with
    | x :: xs -> x, List.rev xs
    | [] -> Utils.unreachable ()
  in
  let* { freevars; _ } = get in
  let inherited_vars =
    match Base.Map.find freevars name with
    | Some fv -> fv
    | None -> []
  in
  let* ({ freevars; _ } as state) = get in
  let* _ =
    put { state with freevars = Base.Map.set freevars ~key:name ~data:inherited_vars }
  in
  let free_vars = call_args_env @ inherited_vars in
  let* body = aexpr (return body) in
  let decl =
    match free_vars with
    | [] -> Fun_without_env (Some arg, body)
    | _ ->
      let arity = List.length call_args_rev in
      Fun_with_env { arg; arity; body; captured_args = free_vars }
  in
  let* ({ global_env; _ } as state) = get in
  let* _ = put { state with global_env = (name, decl) :: global_env } in
  return ()

and cexpr e =
  let* e = e in
  match e with
  | CImm i -> imm (return i)
  | CCons (x, xs) ->
    let* x' = imm (return x) in
    let* xs' = imm (return xs) in
    Fl_cons (x', xs') |> return
  | CGetfield (idx, im) ->
    let* im' = imm (return im) in
    Fl_getfield (idx, im') |> return
  | CBinop (op, x, y) ->
    let* x' = imm (return x) in
    let* y' = imm (return y) in
    Fl_binop (op, x', y') |> return
  | CIte (c, t, e) ->
    let* c' = imm (return c) in
    let* t' = aexpr (return t) in
    let* e' = aexpr (return e) in
    Fl_ite (c', t', e') |> return
  | CApp (f, args) ->
    let* f' = imm (return f) in
    let* args' = transform_list (return args) in
    Fl_app (f', args') |> return
  | CTuple elems ->
    let* elems =
      List.fold_right
        (fun e acc ->
           let* acc = acc in
           let* e' = imm (return e) in
           e' :: acc |> return)
        elems
        (return [])
    in
    Fl_tuple elems |> return
  | CFun _ ->
    (* added to global scope in aexpr *)
    Utils.unreachable ()

and transform_list imms =
  let* imms = imms in
  List.fold_right
    (fun i acc ->
      let* acc = acc in
      let* fl' = imm (return i) in
      fl' :: acc |> return)
    imms
    (return [])

and aexpr ae =
  let* ae = ae in
  match ae with
  | AExpr c -> cexpr (return c)
  | ALet (_, id, (CFun _ as f), scope) ->
    let fv = Freevars.(collect_cexpr f |> remove id |> to_seq |> List.of_seq) in
    let* ({ freevars; _ } as state) = get in
    let* _ = put { state with freevars = Base.Map.set freevars ~key:id ~data:fv } in
    let* () = resolve_fun id (return f) in
    aexpr (return scope)
  | ALet (_, id, e, scope) ->
    let* e = cexpr (return e) in
    let* scope = aexpr (return scope) in
    Fl_let (id, e, scope) |> return
;;

let cc arities astracture =
  let add_item acc (AStr_value (_, bindings) as item) =
    let* ({ freevars; _ } as state) = get in
    let update_fv str_value =
      let binding_names = List.map fst bindings |> Freevars.of_list in
      match str_value with
      | AStr_value (NonRecursive, _) -> freevars
      | AStr_value (Recursive, bindings) ->
        List.fold_left
          (fun acc (id, ae) ->
             let function_fv = Freevars.collect_aexpr ae in
             let without_top_lvl_names =
               Freevars.diff function_fv binding_names |> Freevars.to_seq |> List.of_seq
             in
             Base.Map.set acc ~key:id ~data:without_top_lvl_names)
          freevars
          bindings
    in
    let* _ = put { state with freevars = update_fv item } in
    let add_binding acc (id, e) =
      let* () = acc in
      match e with
      | AExpr (CFun _ as f) ->
        let* () = resolve_fun id (return f) in
        return ()
      | e ->
        let* body = aexpr (return e) in
        let f = Fun_without_env (None, body) in
        let* ({ global_env; _ } as state) = get in
        let* _ = put { state with global_env = (id, f) :: global_env } in
        return ()
    in
    let* () = acc in
    let* () = List.fold_left add_binding (return ()) bindings in
    return ()
  in
  let state = List.fold_left add_item (return ()) astracture in
  let { global_env; _ }, _ = run state (empty_ctx arities) in
  List.rev global_env
;;
