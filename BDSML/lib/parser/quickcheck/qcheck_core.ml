type tree =
  | Leaf of int
  | Node of tree * tree

let leaf x = Leaf x
let node x y = Node (x, y)

let g =
  QCheck.Gen.(
    sized
    @@ fix (fun self n ->
      match n with
      | 0 -> map leaf nat
      | n -> frequency [ 1, map leaf nat; 2, map2 node (self (n / 2)) (self (n / 2)) ]))
;;

let bruh = QCheck.Gen.generate ~n:20 g
