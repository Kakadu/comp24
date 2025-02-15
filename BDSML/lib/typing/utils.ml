open Types

type 'a t = VarId.t -> VarId.t * ('a, error) Result.t

let fail e st = st, Error e
let return x last = last, Ok x

let ( >>= ) m f st =
  let last, r = m st in
  match r with
  | Error x -> last, Error x
  | Ok a -> f a last
;;

let ( >>| ) x f = x >>= fun a -> return @@ f a
let ( let* ) x f = x >>= f
let ( let+ ) x f = x >>| f
let fresh (last : VarId.t) = VarId.( + ) last 1, Ok last

let run p start_value : ('a, error) Result.t =
  snd (p start_value)
;;
