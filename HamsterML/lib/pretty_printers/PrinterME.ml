open ME
open Ast
open PrinterAst

let rec me_to_prog (prog : me_prog) =
  List.rev @@ Base.List.fold prog ~init:[] ~f:(fun acc expr -> me_to_expr expr :: acc)

and me_to_expr (expr : me_expr) : expr =
  match expr with
  | MEConst v -> EConst v
  | MEVar id -> EVar id
  | MEOperation op -> EOperation op
  | MEConstraint (l, dt) -> EConstraint (me_to_expr l, dt)
  | MEListConcat (l, r) -> EListConcat (me_to_expr l, me_to_expr r)
  | MEApplication (l, r) -> Application (me_to_expr l, me_to_expr r)
  | MEList lst -> EList (List.map me_to_expr lst)
  | METuple (a, b, tl) ->
    let a = me_to_expr a in
    let b = me_to_expr b in
    let tl = List.map me_to_expr tl in
    ETuple (a, b, tl)
  | MEIf (_if, _then, Some _else) ->
    If (me_to_expr _if, me_to_expr _then, Some (me_to_expr _else))
  | MEIf (_if, _then, None) -> If (me_to_expr _if, me_to_expr _then, None)
  | MELet (rec_flag, binds, Some scope) ->
    let binds =
      Base.List.fold binds ~init:[] ~f:(fun acc (name, args, body) ->
        (name, args, me_to_expr body) :: acc)
    in
    Let (rec_flag, List.rev binds, Some (me_to_expr scope))
  | MELet (rec_flag, binds, None) ->
    let binds =
      Base.List.fold binds ~init:[] ~f:(fun acc (name, args, body) ->
        (name, args, me_to_expr body) :: acc)
    in
    Let (rec_flag, List.rev binds, None)
;;

let pretty_print_me_prog (prog : me_prog) = prog |> me_to_prog |> pretty_print_prog
