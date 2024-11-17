open Ast

type immexpr =
  | ImmNum of int 
  | ImmId of id
  | ImmBool of bool 
  | ImmUnit 

type pexpr =
  | PImmExpr of immexpr 
  | PImmWild 

type cexpr =
  | CBinOp of bin_op * immexpr * immexpr 
  | CApp of immexpr * immexpr
  | CImmExpr of immexpr 
  | CIf of immexpr * aexpr * aexpr 

and aexpr =
  | ALetIn of id * cexpr * aexpr 
  | ACExpr of cexpr 

type bexpr = ALet of bool * id * pexpr list * aexpr

type prexpr = bexpr list