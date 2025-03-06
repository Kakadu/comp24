(* Apply closure conversion to string *)
val closure_conv_prog : string -> HamsterML.Ast.expr list
(* Print closure conversion result *)
val pp_closure_conv_prog : string -> unit
