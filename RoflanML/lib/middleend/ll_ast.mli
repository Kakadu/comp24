open Ast

type llexpr =
  | LLConst of const
  | LLVar of string
  | LLTuple of llexpr * llexpr * llexpr list
  | LLList of llexpr list
  | LLBranch of llexpr * llexpr * llexpr
  | LLMatch of llexpr * (pattern * llexpr) list
  | LLLetIn of string * llexpr * llexpr
  | LLApp of llexpr * llexpr

type lldecl =
  | LLDLet of is_rec * string * typed_arg list * llexpr
  | LLDMutualLet of (string * typed_arg list * llexpr) list

type llprogram = lldecl list

val ll_to_ast : lldecl -> decl
