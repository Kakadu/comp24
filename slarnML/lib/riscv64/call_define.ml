open Riscv_ast

let exit = 93
let default_func = [ "print_int", 1; "print_char", 1 ]
let init_part_apps = Call (Id "init_part_apps")
let part_app = Call (Id "part_app")
