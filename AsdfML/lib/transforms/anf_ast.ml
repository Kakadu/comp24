open Ast

type id = string

type imm_expr =
  | ImmInt of int
  | ImmBool of bool
  | ImmUnit
  | ImmNil
  | ImmId of id
  | ImmTuple of imm_expr list
  | ImmList of imm_expr list
[@@deriving show { with_path = false }]

type cexpr =
  | CApp of imm_expr * imm_expr
  | CIfElse of imm_expr * aexpr * aexpr
  | CImmExpr of imm_expr
[@@deriving show { with_path = false }]

and aexpr =
  | ALet of id * cexpr * aexpr
  | ACExpr of cexpr
[@@deriving show { with_path = false }]

type fn = Fn of id * id list * aexpr [@@deriving show { with_path = false }]
type program = fn list [@@deriving show { with_path = false }]

open Base
open Format
open Utils

let rec pp_imm_expr fmt = function
  | ImmInt n -> fprintf fmt "%d" n
  | ImmBool b -> fprintf fmt "%b" b
  | ImmId id -> fprintf fmt "%s" id
  | ImmUnit -> fprintf fmt "()"
  | ImmNil -> fprintf fmt "[]"
  | ImmTuple xs -> pp_list ~op:"(" ~cl:")" ~sep:", " fmt pp_imm_expr xs
  | ImmList xs -> pp_list ~op:"[" ~cl:"]" ~sep:"; " fmt pp_imm_expr xs
;;

let rec pp_cexpr fmt = function
  | CApp (e1, e2) -> fprintf fmt "%a %a" pp_imm_expr e1 pp_imm_expr e2
  | CIfElse (c, t, e) ->
    fprintf fmt "if %a @\n@[<hov 2>then@ %a@] @\n@[<hov 2>else@ %a@]"pp_imm_expr c pp_aexpr t pp_aexpr e
  | CImmExpr e -> pp_imm_expr fmt e

and pp_aexpr fmt = function
  | ALet (id, cexpr, aexpr) ->
    fprintf fmt "@[<hov2>let %s =@ %a @]in@\n%a" id pp_cexpr cexpr pp_aexpr aexpr
  | ACExpr cexpr -> pp_cexpr fmt cexpr
;;

let pp_fn fmt = function
  | Fn (id, args, aexpr) ->
    (match args with
     | [] -> fprintf fmt "@[<hov 2>let %s =@ %a@]@." id pp_aexpr aexpr
     | _ ->
       fprintf
         fmt
          "@[<hov 2>let %s %s =@ %a@]@." 
         id
         (String.concat args ~sep:" ")
         pp_aexpr
         aexpr)
;;

let pp_program fmt = pp_list ~op:"" ~cl:"" ~sep:"" fmt pp_fn
