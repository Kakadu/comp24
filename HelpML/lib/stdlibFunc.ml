open Typ

let print_int_type = TArrow (TInt, TUnit)
let print_bool_type = TArrow (TBool, TUnit)
let stdlib = [ "print_int", print_int_type; "print_bool", print_bool_type ]