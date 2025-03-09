type imm_expr =
  | ImmInt of int
  | ImmString of string
  | ImmBool of bool
  | ImmId of string
  | ImmList of imm_expr list
  | ImmTuple of imm_expr list
  | ImmOperation of Ast.op
  | ImmUnit

type cexpr =
  | CApplication of cexpr * cexpr
  | CIf of imm_expr * aexpr * aexpr option
  | CConstructList of imm_expr * imm_expr
  | CImm of imm_expr

and aexpr =
  | ALetIn of Ast.pattern * cexpr * aexpr
  | ACExpr of cexpr

type single_anf_binding = ALet of string * string list * aexpr

type anf_decl =
  | ADSingleLet of Ast.funType * single_anf_binding
  | ADMutualRecDecl of single_anf_binding list

type anf_prog = anf_decl list


val anf_prog : ME.me_prog -> anf_decl list Utils.R.t
