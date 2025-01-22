(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type binop =
  | Mul
  | Div
  | Plus
  | Minus
  | Eq
  | Neq
  | Gt
  | Geq
  | Lt
  | Leq
  | And
  | Or

let resolve_binop = function
  | "*" -> Mul
  | "/" -> Div
  | "+" -> Plus
  | "-" -> Minus
  | "=" | "==" -> Eq
  | "<>" -> Neq
  | ">" -> Gt
  | ">=" -> Geq
  | "<" -> Lt
  | "<=" -> Leq
  | "&&" -> And
  | "||" -> Or
  | s -> Utils.internalfail @@ Format.sprintf "unexpected binop %s" s
;;

type immediate =
  | Avar of ident
  | Aconst of const

type cexpr =
  | CImm of immediate
  | CBinop of binop * immediate * immediate
  | CTuple of immediate list
  | CGetfield of int * immediate (* tuple or list access *)
  | CCons of immediate * immediate
  | CFun of ident * aexpr
  | CApp of immediate * immediate
  | CIte of immediate * aexpr * aexpr

and aexpr =
  | ALet of rec_flag * ident * cexpr * aexpr
  | AExpr of cexpr

type astructure_item = AStr_value of rec_flag * (ident * aexpr) list
type astructure = astructure_item list
type arities = (string, int, Base.String.comparator_witness) Base.Map.t

let ivar id = Avar id
let cite c t e = CIte (c, t, e)
let capp f a = CApp (f, a)
let temp_binding ?(rf = NonRecursive) name value scope = ALet (rf, name, value, scope)

let collect_fun_args_reversed f =
  let rec helper acc = function
    | Expr_fun (p, e) -> helper (p :: acc) e
    | e -> acc, e
  in
  helper [] f
;;

type anf_ctx =
  { temps : int
  ; params : int
  ; funs : int
  ; arities : arities
  }

module AnfCtx = Utils.State (struct
    type t = anf_ctx
  end)

open AnfCtx

let fresh_temp =
  get
  >>= fun ({ temps; _ } as state) ->
  let res = Format.sprintf "t_%i" temps in
  put { state with temps = temps + 1 } >>= fun _ -> return res
;;

let fresh_fun =
  get
  >>= fun ({ funs; _ } as state) ->
  let res = Format.sprintf "fresh_fun_%i" funs in
  put { state with funs = funs + 1 } >>= fun _ -> return res
;;

let fresh_param =
  get
  >>= fun ({ params; _ } as state) ->
  let res = Format.sprintf "fresh_param_%i" params in
  put { state with params = params + 1 } >>= fun _ -> return res
;;

let put_arity id arity =
  get
  >>= fun ({ arities; _ } as state) ->
  put { state with arities = Base.Map.set arities ~key:id ~data:arity }
;;

(* let  rec expr_in_anf = function
  | Expr_const _ | Expr_var _ |     let fresh_name = Utils.fresh_name "t" in
Expr_nil -> true
  | Expr_constrained(e, _) -> expr_in_anf e
  | Expr_app(x, y) | Expr_cons(x, y) -> expr_in_anf x && expr_in_anf y
  | Expr_tuple(fst, snd, rest) -> List.for_all expr_in_anf (fst :: snd :: rest)
  | Expr_fun(_, e) -> expr_in_anf e
  | Expr_match(e, cases) ->
    expr_in_anf e && List.for_all (fun (_, e) -> expr_in_anf e) cases
  | Expr_ite((Expr_var _ | Expr_const _), t, e ) -> expr_in_anf t && expr_in_anf e
  | Expr_ite _ -> false
  | Expr_let(_, (_, e), scope) -> expr_in_anf e && expr_in_anf scope *)
let rec transform_expr expr k : aexpr t =
  match expr with
  | Expr_const c -> Aconst c |> k
  | Expr_app (Expr_app (Expr_var op, x), y) when Ast.is_binary op ->
    let binop = resolve_binop op in
    transform_expr x (fun x' ->
      transform_expr y (fun y' ->
        let* fresh = fresh_temp in
        let value = CBinop (binop, x', y') in
        let* scope = ivar fresh |> k in
        temp_binding fresh value scope |> return))
  | Expr_app (Expr_app (Expr_var "getfield", Expr_const (Const_int i)), e) ->
    transform_expr e (fun e' ->
      let* fresh = fresh_temp in
      let* scope = ivar fresh |> k in
      let value = CGetfield (i, e') in
      temp_binding fresh value scope |> return)
  | Expr_app (f, a) ->
    transform_expr f (fun f' ->
      transform_expr a (fun a' ->
        let* name = fresh_temp in
        let call = capp f' a' in
        let* scope = ivar name |> k in
        match scope with
        | AExpr (CImm (Avar n)) when n = name -> AExpr call |> return
        | _ -> temp_binding name call scope |> return))
  | Expr_ite (c, t, e) ->
    transform_expr c (fun c' ->
      let* t' = transform_expr t k in
      let* e' = transform_expr e k in
      AExpr (cite c' t' e') |> return)
  | Expr_cons (x, xs) ->
    transform_expr x (fun x' ->
      transform_expr xs (fun xs' ->
        let* name = fresh_temp in
        let value = CCons (x', xs') in
        let* scope = ivar name |> k in
        temp_binding name value scope |> return))
  | Expr_constrained (e, _) -> transform_expr e k
  | Expr_tuple (fst, snd, rest) ->
    transform_list (fst :: snd :: rest) (fun list ->
      let tuple = CTuple list in
      let* name = fresh_temp in
      let* scope = ivar name |> k in
      match scope with
      | AExpr (CImm (Avar n)) when n = name -> AExpr tuple |> return
      | _ -> temp_binding name tuple scope |> return)
  | Expr_fun _ ->
    (* it is guaranteed by let processing that function resolved here is anonymous *)
    let* fun_name = fresh_fun in
    let* scope = ivar fun_name |> k in
    let* f, arity = resolve_fun expr in
    let* _ = put_arity fun_name arity in
    temp_binding fun_name f scope |> return
  | Expr_let (rf, (Pat_var id, (Expr_fun _ as f)), scope) ->
    let* scope = transform_expr scope (fun imm -> AExpr (CImm imm) |> return) in
    let* f, arity = resolve_fun f in
    let* _ = put_arity id arity in
    temp_binding ~rf id f scope |> return
  | Expr_let (rf, (Pat_var id, e), scope) ->
    let* scope = transform_expr scope k in
    transform_expr e (fun e' -> ALet (rf, id, CImm e', scope) |> return)
  | Expr_let (rec_flag, (p, e), scope) ->
    let zipped = Utils.zip_idents_with_exprs p e in
    let split_let =
      List.fold_right
        (fun (id, e) acc_scope -> Expr_let (rec_flag, (Pat_var id, e), acc_scope))
        zipped
        scope
    in
    transform_expr split_let k
  | Expr_var id -> ivar id |> k
  | Expr_match _ -> Utils.internalfail "match must be eliminated here"

and transform_list l k =
  let rec helper acc = function
    | [] -> List.rev acc |> k
    | x :: xs -> transform_expr x (fun x' -> helper (x' :: acc) xs)
  in
  helper [] l

and resolve_fun f =
  let args, body = collect_fun_args_reversed f in
  let* body = transform_expr body (fun imm -> AExpr (CImm imm) |> return) in
  let abstraction acc p =
    let* acc_body, acc_args = acc in
    match p with
    | Pat_var id ->
      (* let* body = transform_expr e (fun imm -> AExpr(CImm imm) |> return) in *)
      (* AExpr (CFun(id, acc)) |> return *)
      return (acc_body, id :: acc_args)
    | p ->
      (* Performs codegen fun (a, b) --> body ~~~->
        fun ab -> let a = ab.first in
         let b = ab.second in body *)
      let* var_name = fresh_param in
      let zipped = Utils.zip_idents_with_exprs p (evar var_name) in
      (* let* body = transform_expr e (fun imm -> AExpr(CImm imm) |> return ) in *)
      let* body =
        transform_list (List.map snd zipped) (fun imms ->
          List.fold_right2
            (fun imm (name, _) acc_body ->
              let* acc_body = acc_body in
              temp_binding name (CImm imm) acc_body |> return)
            imms
            zipped
            (return acc_body))
      in
      return (body, var_name :: acc_args)
    (* in AExpr(CFun(var_name, body)) |> return *)
  in
  let* body, args = List.fold_left abstraction (return (body, [])) args in
  let f = List.fold_right (fun id body -> AExpr (CFun (id, body))) args body in
  match f with
  | AExpr (CFun _ as f) -> return (f, List.length args)
  | _ -> Utils.unreachable ()
;;

let transform_structure s =
  let transform_item acc_items (Str_value (rf, bindings)) =
    let* acc_items = acc_items in
    let transform_binding acc_bindings (p, e) =
      let* acc_bindings = acc_bindings in
      let zipped = Utils.zip_idents_with_exprs p e in
      let* decls =
        List.fold_left
          (fun acc (id, e) ->
            let* acc = acc in
            match e with
            | Expr_fun _ as f ->
              let* f, arity = resolve_fun f in
              let* _ = put_arity id arity in
              let binding = id, AExpr f in
              binding :: acc |> return
            | _ ->
              let* e = transform_expr e (fun imm -> AExpr (CImm imm) |> return) in
              (id, e) :: acc |> return)
          (return [])
          zipped
      in
      acc_bindings @ decls |> return
    in
    let* bindings = List.fold_left transform_binding (return []) bindings in
    let str_values =
      match rf with
      | NonRecursive ->
        List.map (fun (id, e) -> AStr_value (NonRecursive, [ id, e ])) bindings
      | Recursive -> [ AStr_value (rf, bindings) ]
    in
    acc_items @ str_values |> return
  in
  List.fold_left transform_item (return []) s
;;

let transform s =
  let anf_s = transform_structure s in
  let { arities; _ }, astructure =
    run
      anf_s
      { funs = 0; temps = 0; params = 0; arities = Base.Map.empty (module Base.String) }
  in
  arities, astructure
;;
