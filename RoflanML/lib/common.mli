module Counter_Monad : sig
  type ('a, 'err) t = int -> int * ('a, 'err) result

  val return : 'a -> ('a, 'err) t
  val fail : 'err -> ('a, 'err) t
  val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  val fresh : int -> int * (int, 'a) result
  val run : (int -> 'a * 'b) -> 'b
end

val gen_name : string -> (string, 'a) Counter_Monad.t

open Ast

module Middleend_Common : sig
  val uncurry : expr -> typed_arg list * expr
  val curry : typed_arg list -> expr -> expr
end
