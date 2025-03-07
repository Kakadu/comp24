open Base
open Pat_elim_ast

type immexpr =
  | ImmInt of int
  | ImmBool of bool
  | ImmUnit
  | ImmVar of string
  | ImmNil
  | ImmTuple of immexpr list

let imm_int int_ = ImmInt int_
let imm_bool bool_ = ImmBool bool_
let imm_var var_ = ImmVar var_
let imm_tuple lst_ = ImmTuple lst_

let const_to_aexp = function
  | PECint i -> ImmInt i
  | PECBool b -> ImmBool b
  | PECUnit -> ImmUnit
  | PECNil -> ImmNil
;;

type cexp =
  | CImm of immexpr
  | CEApply of string * immexpr list
  | CEIf of immexpr * aexpr * aexpr
  | CECons of immexpr * immexpr

and aexpr =
  | AELetIn of string * cexp * aexpr
  | ACExpr of cexp

let cexp_atom aexp = CImm aexp
let cexp_app e e_list = CEApply (e, e_list)
let cexp_ite i t e = CEIf (i, t, e)
let cexp_cons aexp1 aexp2 = CECons (aexp1, aexp2)
let aexpr_let_in name cexp exp = AELetIn (name, cexp, exp)
let aexpr_complex cexp = ACExpr cexp

type func = string * string list * aexpr

type anf_str_item =
  | Value of string * aexpr
  | Non_rec of func
  | Rec of func list

type anf_structure = anf_str_item list

let rec atom_to_str = function
  | ImmInt i -> Int.to_string i
  | ImmBool b -> Bool.to_string b
  | ImmUnit -> "()"
  | ImmVar v -> v
  | ImmNil -> "[]"
  | ImmTuple l ->
    Format.sprintf
      "(%s)"
      (atom_to_str (List.hd_exn l)
       ^ List.fold_left
           ~f:(fun acc e -> acc ^ Format.sprintf ", %s" (atom_to_str e))
           ~init:""
           (List.tl_exn l))
;;

let rec cexp_to_str = function
  | CImm a -> atom_to_str a
  | CEApply (a1, a_list) ->
    List.fold_left ~init:a1 ~f:(fun acc a -> "(" ^ acc ^ " " ^ atom_to_str a ^ ")") a_list
  | CEIf (e1, e2, e3) ->
    Format.sprintf
      "if %s\nthen %s\nelse %s"
      (atom_to_str e1)
      (exp_to_str e2)
      (exp_to_str e3)
  | CECons (e1, e2) -> Format.sprintf "(%s::%s)" (atom_to_str e1) (atom_to_str e2)

and exp_to_str = function
  | AELetIn (name, c, e) ->
    Format.sprintf "let %s = %s in\n%s" name (cexp_to_str c) (exp_to_str e)
  | ACExpr e -> cexp_to_str e
;;

let fun_to_str (name, args, body) =
  Format.sprintf
    "%s = %s"
    (Base.List.fold args ~init:name ~f:(fun acc arg -> acc ^ " " ^ arg))
    (exp_to_str body)
;;

let str_item_to_str = function
  | Value (name, e) -> Format.sprintf "let %s = %s" name (exp_to_str e)
  | Non_rec fun1 -> "let " ^ fun_to_str fun1
  | Rec func_list ->
    let fun1 = List.hd_exn func_list in
    let tl = List.tl_exn func_list in
    Base.List.fold_left
      tl
      ~init:(Format.sprintf "let rec " ^ fun_to_str fun1)
      ~f:(fun acc fun1 -> acc ^ "\nand " ^ fun_to_str fun1)
;;

let pp_anf_structure ppf p =
  let len = List.length p in
  List.iteri
    ~f:(fun i a ->
      if i = len - 1
      then Format.fprintf ppf "%s" (str_item_to_str a)
      else Format.fprintf ppf "%s\n\n" (str_item_to_str a))
    p
;;
