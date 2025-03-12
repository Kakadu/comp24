open Reduced_ast

let exp_to_string = function
  | Invalid_pattern s -> "invalid pattern: " ^ s
  | Invalid_ast s -> "invalid ast: " ^ s
  | Invalid_previous_result (cur, err) ->
    "invalid previous middleend result in " ^ cur ^ ": " ^ err
;;

module Monads =
  Utils.Counter_monad.Make
    (Int)
    (struct
      type t = error
    end)
