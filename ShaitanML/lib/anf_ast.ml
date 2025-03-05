type immexpr =
  | ImmInt of int
  | ImmBool of bool
  | ImmUnit
  | ImmVal of string
  | ImmEmptyList
  | ImmTuple of immexpr list

let imm_int int_ = ImmInt int_
let imm_bool bool_ = ImmBool bool_
let imm_val val_ = ImmVal val_
let imm_tuple lst_ = ImmTuple lst_

type cexp =
  | CEXPRAtom of immexpr
  | CEXPRApp of string * immexpr list
  | CEXPRIf of immexpr * aexpr * aexpr
  | CEXPRCons of immexpr * immexpr

and aexpr =
  | AELetIn of string * cexp * aexpr
  | AEcomplex of cexp

  let cexp_atom aexp = CEXPRAtom aexp
  let cexp_app e e_list = CEXPRApp (e, e_list)
  let cexp_ite i t e = CEXPRIf (i, t, e)
  let cexp_cons_list aexp1 aexp2 = CEXPRCons (aexp1, aexp2)

let aexpr_let_in name cexp exp = AELetIn (name, cexp, exp)
let aexpr_complex cexp = AEcomplex cexp

type func = string * string list * aexpr

type top =
  | Value of string * aexpr
  | Non_rec of func
  | Rec of func list

type anf_structure = top list
type error = IncorrectAst of string
