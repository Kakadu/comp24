open Ast

module StringSet = struct
  include Stdlib.Set.Make (String)

  let pp fmt s =
    let open Stdlib.Format in
    fprintf fmt "[ ";
    iter (fprintf fmt "%s; ") s;
    fprintf fmt "]"
  ;;
end

type lambda =
  | Lconst of const
  | Lvar of ident
  | Lclosure of closure
  | Lite of lambda * lambda * lambda
  | Lapp of lambda * lambda
  | Llet of rec_flag * ident * lambda * lambda
  | Lswitch (* TODO *)
[@@deriving show]

and closure =
  { body : lambda
  ; env : closure_env
  ; arg_patterns : pattern list
  ; total_args : int
  ; applied_count : int
  ; freevars : StringSet.t
  }
[@@deriving show]

and closure_env = (ident * lambda) list [@@deriving show]

and global_value =
  | Fun of closure
  | Var of lambda
[@@deriving show]

type lprogram = (ident * global_value) list [@@deriving show]

let apply_arg ({ applied_count; env; _ } as c) id arg =
  { c with applied_count = applied_count + 1; env = (id, arg) :: env }
;;

let iconst i = Lconst (Const_int i)

let lclosure body arg_patterns freevars =
  Lclosure
    { body
    ; arg_patterns
    ; total_args = List.length arg_patterns
    ; applied_count = 0
    ; env = []
    ; freevars
    }
;;

let lvar v = Lvar v
let lapp f a = Lapp (f, a)
let llet id expr scope = Llet (NonRecursive, id, expr, scope)
(* let lletrec id expr scope = Llet (Recursive, id, expr, scope) *)

open Stdlib.Format

let rec pp_lam ppf = function
  | Lvar id -> fprintf ppf "%s" id
  | Lconst (Const_int i) -> fprintf ppf "%i" i
  | Lconst (Const_bool b) -> fprintf ppf "%b" b
  | Lconst Const_unit -> fprintf ppf "unit"
  | Lite (c, t, e) ->
    fprintf ppf "@[if %a then %a @, else %a @]@." pp_lam c pp_lam t pp_lam e
  | Llet (_, id, e, scope) ->
    fprintf ppf "@[let %s = %a in @, %a @]@." id pp_lam e pp_lam scope
  | Lapp (f, a) -> fprintf ppf "@[(%a %a)@]" pp_lam f pp_lam a
  | Lclosure { body; env; arg_patterns; total_args; applied_count; freevars } ->
    fprintf
      ppf
      "@[<2>{body: %a @,\
      \ env: %a @,\
      \ arg_patterns: %a @.\n\
      \       total args: %i @,\
      \ applied_count: %i @,\
      \ free_vars: %a } @]@."
      pp_lam
      body
      pp_clos_env
      env
      pp_pat_list
      arg_patterns
      total_args
      applied_count
      StringSet.pp
      freevars
  | Lswitch -> Utils.internalfail "TODO"

and pp_pat_list ppf ps =
  fprintf ppf "[ ";
  List.iter (fun p -> fprintf ppf " %a ;" Ast_printer.pp_pat p) ps;
  fprintf ppf "]"

and pp_clos_env ppf ce =
  fprintf ppf "[ ";
  List.iter (fun (id, v) -> fprintf ppf "@[ (%s,%a) @]" id pp_lam v) ce;
  fprintf ppf "]"
;;

let pp ppf lprogram =
  let item_printer (id, gv) =
    match gv with
    | Fun closure ->
      let lam = Lclosure closure in
      fprintf ppf "@[let %s = %a @]@." id pp_lam lam
    | Var lam -> fprintf ppf "@[let %s = %a@]@." id pp_lam lam
  in
  List.iter item_printer lprogram
;;
