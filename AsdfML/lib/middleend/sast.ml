open Ast

(* Simplified AST after pattern and match elimination and closure conversion *)

type is_fun = bool [@@deriving show { with_path = false }]

type sexpr =
  | SConst of constant
  | SVar of id
  | SApp of sexpr * sexpr
  | SIfElse of sexpr * sexpr * sexpr
  | SFun of id list * sexpr
  | SLetIn of sdefinition * sexpr
  | STuple of sexpr list
  | SList of sexpr list
[@@deriving show { with_path = false }]

and sdefinition = SLet of is_fun * Ast.rec_flag * id * sexpr
[@@deriving show { with_path = false }]

type sprogram = sdefinition list [@@deriving show { with_path = false }]

let s_const c = SConst c
let s_var x = SVar x
let s_app f x = SApp (f, x)
let s_if_else cond e_true e_false = SIfElse (cond, e_true, e_false)
let s_fun p e = SFun (p, e)
let s_let_in def e = SLetIn (def, e)
let s_tuple exprs = STuple exprs
let s_list exprs = SList exprs
let s_let f r x e = SLet (f, r, x, e)

open Format
open Utils

let rec pp_sexpr fmt = function
  | SConst c -> fprintf fmt "%a" Pp_ast.pp_constant c
  | SVar v -> fprintf fmt "%s" v
  | SApp (e1, e2) ->
    (match e1 with
     | SApp (_, _) ->
       let rec pp_rest fmt = function
         | SApp (e1, e2) -> fprintf fmt "%a %a" pp_rest e1 pp_sexpr e2
         | e -> fprintf fmt "(%a" pp_sexpr e
       in
       fprintf fmt "%a %a)" pp_rest e1 pp_sexpr e2
     | _ -> fprintf fmt "(%a %a)" pp_sexpr e1 pp_sexpr e2)
  | SIfElse (c, t, e) ->
    fprintf fmt "if %a then %a else %a" pp_sexpr c pp_sexpr t pp_sexpr e
  | SFun (p, e) ->
    fprintf fmt "(fun ";
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt " ")
      (fun fmt x -> fprintf fmt "%s" x)
      fmt
      p;
    fprintf fmt " -> %a)" pp_sexpr e
  | SLetIn (d, e) -> fprintf fmt "%a in %a" pp_sdef d pp_sexpr e
  | STuple xs -> pp_list ~sep:", " fmt pp_sexpr xs
  | SList xs -> pp_list ~op:"[" ~cl:"]" ~sep:"; " fmt pp_sexpr xs

and pp_sdef fmt = function
  | SLet (_, NonRec, name, e) -> fprintf fmt "let %s = %a\n" name pp_sexpr e
  | SLet (_, Rec, name, e) -> fprintf fmt "let rec %s = %a\n" name pp_sexpr e
;;
