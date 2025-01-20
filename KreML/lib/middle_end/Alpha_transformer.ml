open Ast
open Utils

type context = (string, string, Base.String.comparator_witness) Base.Map.t

let empty : context = Base.Map.empty (module Base.String)

let default =
  List.fold_left
    (fun map f -> Base.Map.set map ~key:f ~data:f)
    empty
    (Ast.binary_ops @ Runtime.stdlib_funs)
;;

open Utils.Counter

let rec transform_pattern id_gen ctx = function
  | (Pat_const _ | Pat_wildcard) as p -> return (p, ctx)
  | Pat_var prev_id ->
    let* n = id_gen prev_id in
    (Pat_var n, Base.Map.set ctx ~key:prev_id ~data:n) |> return
  | Pat_cons (x, xs) ->
    let* x', ctx = transform_pattern id_gen ctx x in
    let* xs', ctx = transform_pattern id_gen ctx xs in
    (Pat_cons (x', xs'), ctx) |> return
  | Pat_tuple (fst, snd, rest) ->
    let* fst', ctx = transform_pattern id_gen ctx fst in
    let* snd', ctx = transform_pattern id_gen ctx snd in
    let* rest', ctx =
      List.fold_right
        (fun elem acc ->
          let* elems, ctx = acc in
          let* elem', ctx = transform_pattern id_gen ctx elem in
          return (elem' :: elems, ctx))
        rest
        (return ([], ctx))
    in
    return (Pat_tuple (fst', snd', rest'), ctx)
  | Pat_constrained (p, _) -> transform_pattern id_gen ctx p
;;

let rec transform_expr ctx e =
  let id_gen = fresh_name in
  match e with
  | (Expr_const _) as e -> return e
  | Expr_var id ->
    let unique = Base.Map.find_exn ctx id in
    (* program is type checked *)
    Expr_var unique |> return
  | Expr_cons (x, xs) ->
    let* x' = transform_expr ctx x in
    let* xs' = transform_expr ctx xs in
    Expr_cons (x', xs') |> return
  | Expr_app (f, a) ->
    let* f' = transform_expr ctx f in
    let* a' = transform_expr ctx a in
    Expr_app (f', a') |> return
  | Expr_fun (p, e) ->
    let* p', ctx = transform_pattern id_gen ctx p in
    let* e' = transform_expr ctx e in
    Expr_fun (p', e') |> return
  | Expr_tuple (fst, snd, rest) ->
    let* fst' = transform_expr ctx fst in
    let* snd' = transform_expr ctx snd in
    let* rest' =
      List.fold_right
        (fun elem acc ->
          let* acc = acc in
          let* elem' = transform_expr ctx elem in
          elem' :: acc |> return)
        rest
        (return [])
    in
    Expr_tuple (fst', snd', rest') |> return
  | Expr_constrained (e, t) ->
    let* e' = transform_expr ctx e in
    Expr_constrained (e', t) |> return
  | Expr_ite (c, t, e) ->
    let* c' = transform_expr ctx c in
    let* t' = transform_expr ctx t in
    let* e' = transform_expr ctx e in
    Expr_ite (c', t', e') |> return
  | Expr_match (e, cases) ->
    let* e' = transform_expr ctx e in
    let* cases' =
      List.fold_right
        (fun (p, e) acc ->
          let* cases = acc in
          let* p, ctx = transform_pattern id_gen ctx p in
          let* e = transform_expr ctx e in
          (p, e) :: cases |> return)
        cases
        (return [])
    in
    Expr_match (e', cases') |> return
  | Expr_let (NonRecursive, (p, e), scope) ->
    let* e' = transform_expr ctx e in
    let* p', ctx = transform_pattern id_gen ctx p in
    let* scope' = transform_expr ctx scope in
    Expr_let (NonRecursive, (p', e'), scope') |> return
  | Expr_let (Recursive, (p, e), scope) ->
    let* p', ctx = transform_pattern id_gen ctx p in
    let* e' = transform_expr ctx e in
    let* scope' = transform_expr ctx scope in
    Expr_let (Recursive, (p', e'), scope') |> return
;;

let transform s =
  let id_gen i = return i in
  let default_ctx = default in
  let transform_struct_item ctx (Str_value (rf, bindings)) =
    let* ctx =
      List.fold_left
        (fun ctx (p, _) ->
          let* ctx = ctx in
          let* _, ctx = transform_pattern id_gen ctx p in
          return ctx)
        ctx
        bindings
    in
    let* bindings', ctx =
      List.fold_right
        (fun (p, e) acc ->
          let* acc, ctx = acc in
          let* _, ctx = transform_pattern id_gen ctx p in
          let* e' = transform_expr ctx e in
          ((p, e') :: acc, ctx) |> return)
        bindings
        (return ([], ctx))
    in
    (Str_value (rf, bindings'), ctx) |> return
  in
  let res = List.fold_left
    (fun acc item ->
      let* acc_str, ctx = acc in
      let* item, ctx = transform_struct_item (return ctx) item in
      return (acc_str @ [ item ], ctx))
    (return ([], default_ctx))
    s in
    let open Utils.Counter
    in run res 0 |> snd |> fst
;;
