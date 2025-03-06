open LL
open Ast
open PrinterAst

let rec ll_to_prog (prog : ll_prog) =
  List.rev @@ Base.List.fold prog ~init:[] ~f:(fun acc expr -> ll_to_expr expr :: acc)

and ll_to_expr (expr : ll_expr) : expr =
  match expr with
  | LLConst v -> EConst v
  | LLVar id -> EVar id
  | LLOperation op -> EOperation op
  | LLConstraint (l, dt) -> EConstraint (ll_to_expr l, dt)
  | LLListConcat (l, r) -> EListConcat (ll_to_expr l, ll_to_expr r)
  | LLApplication (l, r) -> Application (ll_to_expr l, ll_to_expr r)
  | LLList lst -> EList (List.map ll_to_expr lst)
  | LLTuple (a, b, tl) ->
    let a = ll_to_expr a in
    let b = ll_to_expr b in
    let tl = List.map ll_to_expr tl in
    ETuple (a, b, tl)
  | LLIf (_if, _then, Some _else) ->
    If (ll_to_expr _if, ll_to_expr _then, Some (ll_to_expr _else))
  | LLIf (_if, _then, None) -> If (ll_to_expr _if, ll_to_expr _then, None)
  | LLMatch (expr, cases) ->
    let expr = ll_to_expr expr in
    let cases =
      Base.List.fold cases ~init:[] ~f:(fun acc (p, e) -> (p, ll_to_expr e) :: acc)
    in
    Match (expr, List.rev cases)
  | LLLet (rec_flag, binds, Some scope) ->
    let binds =
      Base.List.fold binds ~init:[] ~f:(fun acc (name, args, body) ->
        (name, args, ll_to_expr body) :: acc)
    in
    Let (rec_flag, List.rev binds, Some (ll_to_expr scope))
  | LLLet (rec_flag, binds, None) ->
    let binds =
      Base.List.fold binds ~init:[] ~f:(fun acc (name, args, body) ->
        (name, args, ll_to_expr body) :: acc)
    in
    Let (rec_flag, List.rev binds, None)
;;

let pretty_print_ll_prog (prog : ll_prog) = prog |> ll_to_prog |> pretty_print_prog
