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

let f a = 
  let a = a + 1 in
  let b = a in
  if a >  b then
    fun x -> x + a + b
  else fun y -> y + b 

let f b =
  if b then
    fun (a, b) -> (a + 1, b + 1)
  else fun ab -> ab 

(*
f1 x = 
  x + 1
f 2 x =
  x + 2
let f b = if b then
  f_1 
else f_2*)

let x = 1, 2 in

(fun (a, b) -> a + b) x

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
  ; env : closure_env
  ; arg_patterns : pattern list
  ; total_args : int
  ; applied_count : int
  ; freevars : StringSet.t
  }
[@@deriving show]

and closure_env = (ident * lambda) list [@@deriving show]

type lprogram = (ident * lambda) list [@@deriving show]

let lookup_closure_env_exn c id = List.find (fun (id', _) -> id = id') c |> snd

let apply_arg ({ applied_count; env; _ } as c) id arg =
  { c with applied_count = applied_count + 1; env = (id, arg) :: env }
;;

let iconst i = Lconst (Const_int i)

let lclosure name arg_patterns freevars =
  { name
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
  | Lclosure { name; env; arg_patterns; total_args; applied_count; freevars } ->
    fprintf
      ppf
      "@[ { name: %s @,
      \ env: %a  @,\
      \ arg_patterns: %a @,\
      \ total args: %i @,\
      \ applied_count: %i @,\
      \ free_vars: %a } @]@."
      name
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
  let item_printer (id, f) =
      fprintf ppf "@[let %s = %a @]@." id pp_lam f
  in
  List.iter item_printer lprogram
;;
