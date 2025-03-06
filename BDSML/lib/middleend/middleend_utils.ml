open Reduced_ast

let fun_exception name =
  RExp_apply (RExp_ident "__exception", RExp_constant (Const_string name))
;;

let exp_to_string = function
  | Invalid_pattern s -> "invalid pattern: " ^ s
  | Invalid_ast s -> "invalid ast: " ^ s
;;
