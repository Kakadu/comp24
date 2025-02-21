open Types

type 'a t = TVarId.t -> TVarId.t * ('a, error) Result.t

let fail e st = st, Error e
let return x last = last, Ok x

let ( >>= ) (m : 'a t) (f : 'a -> 'b t) st =
  let last, r = m st in
  match r with
  | Error x -> last, Error x
  | Ok a -> f a last
;;

let ( >>| ) x f = x >>= fun a -> return @@ f a
let ( let* ) x f = x >>= f
let ( let+ ) x f = x >>| f

let ( and+ ) (o1 : 'a t) (o2 : 'b t) st =
  let res1 = o1 st in
  let res2 = o2 (fst res1) in
  ( fst res2
  , match snd res1, snd res2 with
    | Ok a, Ok b -> Ok (a, b)
    | Ok _, (Error _ as a) -> a
    | (Error _ as a), _ -> a )
;;

let rec map (f : 'a -> 'b t) = function
  | h :: tl ->
    let+ h = f h
    and+ tl = map f tl in
    h :: tl
  | _ -> return []
;;

let rec fold_left (f : 'a -> 'b -> 'a t) (acc : 'a t) : 'b list -> 'a t = function
  | h :: tl ->
    let* acc = acc in
    fold_left f (f acc h) tl
  | _ -> acc
;;

let fresh (last : TVarId.t) = TVarId.( + ) last 1, Ok last
let run p start_value : ('a, error) Result.t = snd (p start_value)
