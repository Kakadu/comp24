type 'a res =
  | Result of 'a
  | Error of string
[@@deriving show { with_path = false }]

let map f = function
  | Result o -> f o
  | Error e -> Error e
;;

let bind r f = f r
let ( >>= ) r f = map f r
let ( |> ) = bind
