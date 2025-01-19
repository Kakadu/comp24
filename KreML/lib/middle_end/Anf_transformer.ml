open Ast

type immediate = Avar of ident | Aconst of const

type cexpr =
  | CImm of immediate
  | CTuple of immediate list
  | CNil
  | CCons of immediate * immediate
  | CFun of ident * aexpr
  | CApp of immediate * immediate list
  | CIte of immediate * aexpr * aexpr
  | CConstrained of cexpr * typ


and aexpr =
    | ALet of rec_flag * ident * cexpr * aexpr
    | AExpr of cexpr

type astructure_item = AStr_value of rec_flag * (ident * aexpr) list
type astructure = astructure_item list

let ivar id = Avar id
let capp f args = CApp(f, args)
let cite c t e = CIte(c, t, e)

let temp_binding ?(rf = NonRecursive) name value scope = ALet (rf, name, value, scope)

let collect_app_args app = 
  let rec helper acc = function
  | Expr_app(f, a) -> a::(helper acc f)
  | _ -> acc
in helper [] app |> List.rev

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
  let rec transform_expr expr k : aexpr t =
    let fresh_name = Utils.fresh_name "t" in
    (match expr with
    | Expr_const c -> Aconst c |> k
    | Expr_app (Expr_app (Expr_var op, x), y) when is_binary op ->
      transform_expr x (fun x' ->
        transform_expr y (fun y' ->
          let* name = fresh_name in
          let e' = capp (ivar op) [x'; y'] in
          let* scope = ivar name |> k in
          (match scope with
          | AExpr(CImm(Avar n)) when n = name -> AExpr e' |> return
          | _ -> temp_binding name e' scope |> return)))
    | Expr_app (f, _) ->
      let args = collect_app_args expr in
      transform_expr f (fun f' ->
         transform_list args (fun args' ->
          let* name = fresh_name in
          let call = capp f' args' in
          let* scope = ivar name |> k in
          (match scope with
          | AExpr(CImm(Avar n)) when n = name -> AExpr call |> return
          | _ -> temp_binding name call scope |> return)))
    | Expr_ite (c, t, e) ->
      transform_expr c (fun c' ->
        let* t' = transform_expr t k in
        let* e' = transform_expr e k in
        AExpr(cite c' t' e') |> return)
    | Expr_cons (x, xs) ->
      transform_expr x (fun x' ->
        transform_expr xs (fun xs' ->
          let* name = fresh_name in
          let value = capp (ivar Runtime.list_cons) [x' ;xs'] in
          let* scope = ivar name |> k in
          temp_binding name value scope |> return))
    | Expr_constrained (e, _) -> transform_expr e k
    | Expr_tuple (fst, snd, rest) ->
      transform_list (fst :: snd :: rest) (fun list ->
        let tuple = CTuple list in
        let* name = fresh_name in
        let* scope = ivar name |> k in
        (match scope with
        | AExpr(CImm(Avar n)) when n = name -> AExpr tuple |> return
        | _ -> temp_binding name tuple scope |> return)
        )
    | Expr_fun _ ->
      (* it is guaranteed by let processing that function resolved here is anonymous *)
      let* fun_name = fresh_name in
      let* scope = ivar fun_name |> k in
      let* f = resolve_fun expr in
      temp_binding fun_name f scope |> return 
    | Expr_let(rf, (Pat_var id, (Expr_fun _  as f)), scope) ->
      let* scope = transform_expr scope (fun imm -> AExpr(CImm(imm)) |> return) in
      let* f = resolve_fun f in
      temp_binding ~rf id f scope |> return
    | Expr_let(rf, (Pat_var id, e), scope) ->
      let* scope = transform_expr scope k in
      transform_expr e (fun e' ->
        ALet(rf, id, CImm e', scope) |> return)
    | Expr_let (rec_flag, (p, e), scope) ->
      let zipped = Utils.zip_idents_with_exprs p e in
      let split_let = List.fold_right (fun (id, e) acc_scope  ->
         Expr_let(rec_flag, (Pat_var id, e), acc_scope)) zipped scope in
      transform_expr split_let k
    | Expr_var id -> ivar id |> k
    | Expr_match (_) -> Utils.internalfail "match must be eliminated here")
  and transform_list l k =
    let rec helper acc l =
    match l with
    | [] -> List.rev acc |> k
    | x::xs ->
      transform_expr x (fun x' -> helper (x'::acc) xs)
      in helper [] l
  and resolve_fun f =
     match f with
    | Expr_fun(p, e) ->
      (match p with
      | Pat_var id -> 
        let* body = transform_expr e (fun imm -> AExpr(CImm imm) |> return) in
        CFun(id, body) |> return
      | p ->
        (* Performs transformations fun (a, b) --> body ~~~-> fun ab -> let a = ab.first in
         let b = ab.second in body*)
        let* var_name = Utils.fresh_name "t" in
        let zipped =  Utils.zip_idents_with_exprs p (evar var_name) in
        let* body = transform_expr e (fun imm -> AExpr(CImm imm) |> return ) in
        let* body = transform_list (List.map snd zipped) (fun imms ->
          List.fold_right2 (fun imm (name, _) acc_body ->
          let* acc_body in
          temp_binding name (CImm imm) acc_body |> return) 
          imms
          zipped
          (return body))
          in CFun(var_name, body) |> return)
      | _ -> Utils.internalfail "not a function"


  let transform_structure s =
    let transform_item  acc_items (Str_value (rf, bindings)) =
      let* acc_items = acc_items in
      let transform_binding acc_bindings (p, e) =
        let* acc_bindings = acc_bindings in
        let zipped = Utils.zip_idents_with_exprs p e in
        let* decls = List.fold_left (fun acc (id, e) ->
          let* acc = acc in
          match e with
            | Expr_fun _ as f ->
              let* f = resolve_fun f in
              let binding = id, AExpr f in
              binding::acc |> return
            | _ -> 
              let* e = transform_expr e (fun imm -> AExpr(CImm imm) |> return) in
              (id, e)::acc |> return) (return []) zipped in
        acc_bindings @ decls |> return
      in
      let* bindings = List.fold_left transform_binding (return []) bindings in
      let str_value = AStr_value(rf, bindings) in
      acc_items @ [str_value] |> return in
    List.fold_left transform_item (return []) s
  ;;


let transform_structure s =
  let anf_s = transform_structure s in
  run anf_s 0 |> snd
;;
