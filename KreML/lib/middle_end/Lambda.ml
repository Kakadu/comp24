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

type binop =
  | Mul
  | Div
  | Plus
  | Minus
  | Eq
  | Gt
  | Geq
  | Lt
  | Leq
  | And
  | Or
[@@deriving show]

let resolve_binop = function
  | "*" -> Mul
  | "/" -> Div
  | "+" -> Plus
  | "-" -> Minus
  | "=" | "==" -> Eq
  | ">" -> Gt
  | ">=" -> Geq
  | "<" -> Lt
  | "<=" -> Leq
  | "&&" -> And
  | "||" -> Or
  | s -> Utils.internalfail @@ Format.sprintf "unexpected binop %s" s
;;

type lambda =
  | Lconst of const
  | Lvar of ident
  | Lbinop of binop * lambda * lambda
  | Lclosure of closure
  | Lite of lambda * lambda * lambda
  | Lcall of ident * lambda list * closure_env option
  | Llet of rec_flag * ident * lambda * lambda
  | Lswitch (* TODO *)
[@@deriving show]

and closure =
  { name : ident
  ; body : lambda
  ; env : closure_env
  ; arg_patterns : pattern list
  ; total_args : int
  ; applied_count : int
  ; freevars : StringSet.t
  }
[@@deriving show]

and closure_env = (ident * lambda) list [@@deriving show]
and global_value = Fun of closure [@@deriving show]

type lprogram = (ident * global_value) list [@@deriving show]

let lookup_closure_env_exn c id = List.find (fun (id', _) -> id = id') c |> snd

let apply_arg ({ applied_count; env; _ } as c) id arg =
  { c with applied_count = applied_count + 1; env = (id, arg) :: env }
;;

let iconst i = Lconst (Const_int i)

let lclosure name body arg_patterns freevars =
  { name
  ; body
  ; arg_patterns
  ; total_args = List.length arg_patterns
  ; applied_count = 0
  ; env = []
  ; freevars
  }
;;

let lvar v = Lvar v
let lcall f ?env args = Lcall (f, args, env)
let llet id expr scope = Llet (NonRecursive, id, expr, scope)
(* let lletrec id expr scope = Llet (Recursive, id, expr, scope) *)

open Stdlib.Format

let rec pp_lam ppf = function
  | Lvar id -> fprintf ppf "%s" id
  | Lconst (Const_int i) -> fprintf ppf "%i" i
  | Lconst (Const_bool b) -> fprintf ppf "%b" b
  | Lconst Const_unit -> fprintf ppf "unit"
  | Lbinop (op, x, y) ->
    let pp_op ppf = function
      | Mul -> fprintf ppf "*"
      | Div -> fprintf ppf "/"
      | Plus -> fprintf ppf "+"
      | Minus -> fprintf ppf "-"
      | Gt -> fprintf ppf ">"
      | Geq -> fprintf ppf ">="
      | Lt -> fprintf ppf "<"
      | Leq -> fprintf ppf "<="
      | Eq -> fprintf ppf "="
      | And -> fprintf ppf "&&"
      | Or -> fprintf ppf "||"
    in
    fprintf ppf "@[ %a %a %a @]" pp_lam x pp_op op pp_lam y
  | Lite (c, t, e) ->
    fprintf ppf "@[ if %a then %a @, else %a @]@." pp_lam c pp_lam t pp_lam e
  | Llet (_, id, e, scope) ->
    fprintf ppf "@[ let %s = %a in @, %a @]@." id pp_lam e pp_lam scope
  | Lcall (f, args, Some env) ->
    let pp_args fmt list = pp_list fmt list ~print_bounds:false in
    fprintf ppf "@[ %s(%a) env = { %a } @]@." f pp_args args pp_clos_env env
  | Lcall (f, args, None) ->
    let pp_args fmt list = pp_list fmt list ~print_bounds:false in
    fprintf ppf "@[ %s(%a) @]" f pp_args args
  | Lclosure { name; body; env; arg_patterns; total_args; applied_count; freevars } ->
    fprintf
      ppf
      "@[ { name: %s @, body: %a @,\
      \ env: %a  @,\
      \ arg_patterns: %a @,\
      \ total args: %i @,\
      \ applied_count: %i @,\
      \ free_vars: %a } @]@."
      name
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

and pp_list ppf list ~print_bounds =
  if print_bounds then fprintf ppf "[ ";
  List.iter (fun e -> fprintf ppf " %a ;" pp_lam e) list;
  if print_bounds then fprintf ppf "]"

and pp_clos_env ppf ce =
  fprintf ppf "[ ";
  List.iter (fun (id, v) -> fprintf ppf "@[ (%s,%a) @]" id pp_lam v) ce;
  fprintf ppf "]"

and pp_pat_list ppf list =
  fprintf ppf "[ ";
  List.iter (fun e -> fprintf ppf " %a ;" Ast_printer.pp_pat e) list;
  fprintf ppf "]"
;;

let pp ppf lprogram =
  let item_printer (id, gv) =
    match gv with
    | Fun closure ->
      let lam = Lclosure closure in
      fprintf ppf "@[let %s = %a @]@." id pp_lam lam
  in
  List.iter item_printer lprogram
;;
