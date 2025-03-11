open Llvm

let sym_to_value : (string, llvalue) Hashtbl.t = Hashtbl.create 1
let sym_to_type : (string, lltype) Hashtbl.t = Hashtbl.create 1
let lookup_name name = Hashtbl.find_opt sym_to_value name
let lookup_type name = Hashtbl.find_opt sym_to_type name
let add_sym name value = Hashtbl.add sym_to_value name value
let add_type name ty = Hashtbl.add sym_to_type name ty

let id_to_runtime_name = function
  | "( + )" -> "add"
  | "( - )" -> "sub"
  | "( * )" -> "mul"
  | "( / )" -> "divd"
  | "( = )" -> "eq"
  | "( != )" -> "neq"
  | "( < )" -> "less"
  | "( <= )" -> "leq"
  | "( > )" -> "gre"
  | "( >= )" -> "geq"
  | "( && )" -> "and"
  | "( || )" -> "or"
  | other -> other
;;
