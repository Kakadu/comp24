open Ast
open Utils

module Alpha_transformer = struct
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
    | (Pat_const _ | Pat_nil | Pat_wildcard) as p -> return (p, ctx)
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
    | (Expr_const _ | Expr_nil) as e -> return e
    | Expr_var id ->
      let unique = Base.Map.find_exn ctx id in (* program is type checked *)
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
      Expr_let(NonRecursive, (p', e'), scope') |> return
    | Expr_let (Recursive, (p, e), scope) ->
      let* p', ctx = transform_pattern id_gen ctx p in
      let* e' = transform_expr ctx e in
      let* scope' = transform_expr ctx scope in
      Expr_let (Recursive, (p', e'), scope') |> return
  ;;

  let transform_structure s =
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
    List.fold_left
      (fun acc item ->
        let* acc_str, ctx = acc in
        let* item, ctx = transform_struct_item (return ctx) item in
        return (acc_str @ [ item ], ctx))
      (return ([], default_ctx))
      s
  ;;
end

module Anf_transformer = struct
  open Utils.Counter

  (* let  rec expr_in_anf = function
  | Expr_const _ | Expr_var _ | Expr_nil -> true
  | Expr_constrained(e, _) -> expr_in_anf e
  | Expr_app(x, y) | Expr_cons(x, y) -> expr_in_anf x && expr_in_anf y
  | Expr_tuple(fst, snd, rest) -> List.for_all expr_in_anf (fst :: snd :: rest)
  | Expr_fun(_, e) -> expr_in_anf e
  | Expr_match(e, cases) ->
    expr_in_anf e && List.for_all (fun (_, e) -> expr_in_anf e) cases
  | Expr_ite((Expr_var _ | Expr_const _), t, e ) -> expr_in_anf t && expr_in_anf e
  | Expr_ite _ -> false
  | Expr_let(_, (_, e), scope) -> expr_in_anf e && expr_in_anf scope *)
  let rec transform_expr expr k : expr t =
    let fresh_name = fresh_name "t" in
    let temp_binding name value scope =
      Expr_let (NonRecursive, (Pat_var name, value), scope)
    in
    match expr with
    | (Expr_const _ | Expr_var _ | Expr_nil) as imm -> imm |> k
    | Expr_app (Expr_app (Expr_var op, x), y)
      when Base.List.exists Ast.binary_ops ~f:(( = ) op) ->
      transform_expr x (fun x' ->
        transform_expr y (fun y' ->
          let* name = fresh_name in
          let e' = Expr_app (Expr_app (Expr_var op, x'), y') in
          let* scope = Expr_var name |> k in
          match scope with
          | Expr_var n when n = name -> e' |> return
          | _ -> temp_binding name e' scope |> return))
    | Expr_app (f, a) ->
      transform_expr f (fun f' -> transform_expr a (fun a' -> Expr_app (f', a') |> k))
    | Expr_ite (c, t, e) ->
      transform_expr c (fun c' ->
        let* t' = transform_expr t k in
        let* e' = transform_expr e k in
        Expr_ite (c', t', e') |> return)
    | Expr_cons (x, xs) ->
      transform_expr x (fun x' ->
        transform_expr xs (fun xs' ->
          let* name = fresh_name in
          let value = Expr_cons (x', xs') in
          let* scope = Expr_var name |> k in
          temp_binding name value scope |> return))
    | Expr_constrained (e, t) ->
      transform_expr e (fun e' -> Expr_constrained (e', t) |> k)
      (* let* e' = transform_expr e k in
      Expr_constrained (e', t) |> return *)
    | Expr_tuple (fst, snd, rest) ->
      transform_expr fst (fun fst' ->
        transform_expr snd (fun snd' ->
          transform_list rest
           (fun rest' -> Expr_tuple(fst', snd', rest'))
           k))
    | Expr_fun (p, e) ->
      let* e' = transform_expr e return in
      Expr_fun (p, e') |> k
    | Expr_let (rec_flag, (p, e), scope) ->
      transform_expr e (fun e' ->
        let* scope' = transform_expr scope k in
        Expr_let (rec_flag, (p, e'), scope') |> return)
    | Expr_match (e, cases) ->
      transform_expr e (fun e' ->
        let* cases =
          List.fold_right
            (fun (p, e) acc ->
              let* acc = acc in
              let* e' = transform_expr e return in
              (* maybe k here*)
              return ((p, e') :: acc))
            cases
            (return [])
        in
        Expr_match (e', cases) |> k)
  and transform_list l unifier k =
    let rec helper acc l =
    match l with
    | [] -> List.rev acc |> unifier |> k
    | x::xs ->
      transform_expr x (fun x' -> helper (x'::acc) xs)
      in helper [] l
  ;;

  let transform_structure s =
    let transform_item (Str_value (rf, bindings)) acc_items =
      let* acc_items = acc_items in
      let transform_binding (p, e) acc_bindings =
        let* acc_bindings = acc_bindings in
        let* e' = transform_expr e (fun e' -> return e') in
        (p, e') :: acc_bindings |> return
      in
      let* bindings = List.fold_right transform_binding bindings (return []) in
      Str_value (rf, bindings) :: acc_items |> return
    in
    List.fold_right transform_item s (return [])
  ;;
end


let transform_structure s =
  let alpha_s = Alpha_transformer.transform_structure s in
  let alpha_s = Counter.run alpha_s 0 |> snd |> fst in
  let anf_s = Anf_transformer.transform_structure alpha_s in
  let anf_s = Counter.run anf_s 0 |> snd in
  anf_s (* type check it *)
;;
