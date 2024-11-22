open Base

module StateM = struct
  type ('state, 'a) t = 'state -> 'a * 'state

  let return (x : 'a) : ('state, 'a) t = fun s -> x, s

  let bind (m : ('state, 'a) t) (f : 'a -> ('state, 'b) t) : ('state, 'b) t =
    fun s ->
    let a, new_state = m s in
    f a new_state
  ;;

  let get : ('state, 'state) t = fun s -> s, s
  let put (new_state : 'state) : ('state, unit) t = fun _ -> (), new_state
  let run (m : ('state, 'a) t) (init : 'state) : 'a * 'state = m init
  let modify (f : 'state -> 'state) : ('state, unit) t = bind get (fun s -> put (f s))

  module Syntax = struct
    let ( >>= ) = bind
    let ( let* ) = bind

    let ( >>| ) : ('state, 'a) t -> ('a -> 'b) -> ('state, 'b) t =
      fun m f state ->
      let value, st = m state in
      f value, st
    ;;
  end
end

module IntStateM = struct
  include StateM
  type 'a t = (int, 'a) StateM.t

  let fresh last = last, last + 1
  let run m = fst (m 0)
end
