open Ast
open Types
open Pp_ast
open Pp_typing

type texpr =
  | TEConst of ty * Ast.constant
  | TEVar of ty * Ast.id
  | TEApp of ty * texpr * texpr
  | TEIfElse of ty * texpr * texpr * texpr
  | TEFun of ty * pattern list * texpr
  | TELetIn of ty * tdefinition * texpr
  | TETuple of ty list * texpr list
  | TEList of ty * texpr list
  | TEMatch of ty * texpr * (pattern * texpr) list
[@@deriving show { with_path = false }]

and tdefinition = TDLet of ty * Ast.rec_flag * pattern * texpr
[@@deriving show { with_path = false }]

type tprogram = tdefinition list [@@deriving show { with_path = false }]

let te_const t c = TEConst (t, c)
let te_var t x = TEVar (t, x)
let te_app t f x = TEApp (t, f, x)
let te_if_else t cond e_true e_false = TEIfElse (t, cond, e_true, e_false)
let te_fun t p e = TEFun (t, p, e)
let te_let_in t def e = TELetIn (t, def, e)
let te_tuple t exprs = TETuple (t, exprs)
let te_list t exprs = TEList (t, exprs)
let te_match t e branches = TEMatch (t, e, branches)
let td_let t p e = TDLet (t, NonRec, p, e)
let td_let_rec t p e = TDLet (t, Rec, p, e)

let td_let_flag = function
  | Rec -> td_let_rec
  | NonRec -> td_let
;;

open Format
open Utils

let rec pp_texpr fmt = function
  | TEConst (_, c) -> fprintf fmt "%a" pp_constant c
  | TEVar (_, v) -> fprintf fmt "%s" v
  | TEApp (_, e1, e2) ->
    (match e1 with
     | TEApp (_, _, _) ->
       let rec pp_rest fmt = function
         | TEApp (_, e1, e2) -> fprintf fmt "%a %a" pp_rest e1 pp_texpr e2
         | e -> fprintf fmt "(%a" pp_texpr e
       in
       fprintf fmt "%a %a)" pp_rest e1 pp_texpr e2
     | _ -> fprintf fmt "(%a %a)" pp_texpr e1 pp_texpr e2)
  | TEIfElse (_, e1, e2, e3) ->
    fprintf fmt "if %a then %a else %a" pp_texpr e1 pp_texpr e2 pp_texpr e3
  | TEFun (_, pats, e) ->
    fprintf fmt "(fun ";
    pp_pattern_list fmt pats;
    fprintf fmt " -> %a)" pp_texpr e
  | TELetIn (_, d, e) -> fprintf fmt "%a in %a" pp_tdefinition d pp_texpr e
  | TETuple (_, es) -> pp_list ~sep:", " fmt pp_texpr es
  | TEList (_, es) -> pp_list ~op:"[" ~cl:"]" ~sep:"; " fmt pp_texpr es
  | TEMatch (_, e, pes) ->
    fprintf fmt "match %a with\n" pp_texpr e;
    pp_print_list
      ~pp_sep:Format.pp_print_newline
      (fun fmt (p, e) -> fprintf fmt "| %a -> %a" pp_pattern p pp_texpr e)
      fmt
      pes

and pp_tdefinition fmt = function
  | TDLet (_, NonRec, pat, e) -> fprintf fmt "let %a = %a\n" pp_pattern pat pp_texpr e
  | TDLet (_, Rec, pat, e) -> fprintf fmt "let rec %a = %a\n" pp_pattern pat pp_texpr e
;;
