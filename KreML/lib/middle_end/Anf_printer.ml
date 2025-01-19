open Anf_transformer
open Stdlib.Format

let pp_imm ppf = function
  | Avar id -> fprintf ppf "%s" id
  | Aconst (Const_int i) -> fprintf ppf "%i" i
  | Aconst (Const_bool b) -> fprintf ppf "%b" b
  | Aconst Const_nil -> fprintf ppf "[]"
  | Aconst Const_unit -> fprintf ppf "()"
;;

let rec pp_cexpr ppf = function
  | CImm imm -> pp_imm ppf imm
  | CCons (x, xs) -> fprintf ppf "@[ %a :: %a @]" pp_imm x pp_imm xs
  | CApp (f, args) -> fprintf ppf "@[%a(%a)@]" pp_imm f pp_list args
  | CFun (f, a) -> fprintf ppf "@[fun %s -> %a @,@]" f pp_aexpr a
  | CIte (c, t, e) ->
    fprintf ppf "@[if %a @, then %a @, else %a @, @]" pp_imm c pp_aexpr t pp_aexpr e
  | CTuple elems -> pp_list ppf elems

and pp_aexpr ppf = function
  | ALet (Recursive, id, e, scope) ->
    fprintf ppf "@[let rec %s = %a in @, %a@]" id pp_cexpr e pp_aexpr scope
  | ALet (NonRecursive, id, e, scope) ->
    fprintf ppf "@[let %s = %a in @, %a@]" id pp_cexpr e pp_aexpr scope
  | AExpr cexpr -> pp_cexpr ppf cexpr

and pp_list ppf list =
  let rec helper = function
    | [] -> Utils.unreachable ()
    | [ e ] -> pp_imm ppf e
    | e :: es ->
      fprintf ppf "[%a, ]" pp_imm e;
      helper es
  in
  helper list
;;

let pp ppf astructure =
  let pp_item (AStr_value (rf, bindings)) =
    let pp_decl ppf start (id, e) = fprintf ppf "@[%s %s = %a @]@." start id pp_aexpr e in
    match rf, bindings with
    | Recursive, [ (id, e) ] -> pp_decl ppf "let rec" (id, e)
    | NonRecursive, [ (id, e) ] -> pp_decl ppf "let" (id, e)
    | Recursive, (id, e) :: bs ->
      pp_decl ppf "let rec" (id, e);
      List.iter (fun (id, e) -> pp_decl ppf "and" (id, e)) bs
    | _ -> Utils.internalfail "unexpected declaration"
  in
  List.iter pp_item astructure
;;
