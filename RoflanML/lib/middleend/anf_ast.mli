open Ast

type immexpr =
  | ImmConst of const
  | ImmVar of string
  | ImmTuple of immexpr * immexpr * immexpr list
  | ImmList of immexpr list

type cexpr =
  | CImm of immexpr
  | CBranch of immexpr * aexpr * aexpr
  | CApp of immexpr * immexpr list

and aexpr =
  | ALetIn of string * cexpr * aexpr
  | ACExpr of cexpr

type adecl =
  | ADLet of is_rec * string * typed_arg list * aexpr
  | ADMutualLet of (id * typed_arg list * aexpr) list

type aprogram = adecl list

val anf_to_ast : adecl -> decl
