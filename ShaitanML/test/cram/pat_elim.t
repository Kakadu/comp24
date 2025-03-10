  $ pat_elim << EOF
  > let test_cons =
  >   let (h::t) = [1; 2; 3] in
  >   h + 1
  let test_cons = let a0 = (1::(2::(3::[]))) in
  if ((( > ) (list_len a0)) 0)
  then let h = (list_head a0) in
  let t = (list_tail a0) in
  ((( + ) h) 1)
  else fail_match


  $ pat_elim << EOF
  > let test_tuple =
  >   let (x, (y, z)) = (5, (10, 15)) in
  >   x + y + z
  let test_tuple = let a0 = (5, (10, 15)) in
  let x = ((tuple_element a0) 0) in
  let y = ((tuple_element ((tuple_element a0) 1)) 0) in
  let z = ((tuple_element ((tuple_element a0) 1)) 1) in
  ((( + ) ((( + ) x) y)) z)


  $ pat_elim << EOF
  > let test_list =
  >   let lst = [1; 2; 3; 4; 5] in
  >   let rec sum lst = match lst with
  >   | [] -> 0
  >   | h::t -> h + (sum t)
  > in
  > sum lst
  let test_list = let lst = (1::(2::(3::(4::(5::[]))))) in
  let rec sum = (fun lst -> if ((( = ) lst) [])
  then 0
  else if ((( > ) (list_len lst)) 0)
  then let h = (list_head lst) in
  let t = (list_tail lst) in
  ((( + ) h) (sum t))
  else fail_match) in
  (sum lst)


  $ pat_elim << EOF
  > let test_multiple_cases x =
  >   match x with
  >   | 0 -> "zero"
  >   | 1 -> "one"
  >   | _ -> "other"
  let test_multiple_cases = (fun x -> if ((( = ) x) 0)
  then zero
  else if ((( = ) x) 1)
  then one
  else other)

  $ pat_elim < manytests/typed/004manyargs.ml
  let wrap = (fun f -> if ((( = ) 1) 1)
  then f
  else f)
  
  let test3 = (fun a b c -> let a = (print_int a) in
  let b = (print_int b) in
  let c = (print_int c) in
  0)
  
  let test10 = (fun a b c d e f g h i j -> ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) ((( + ) a) b)) c)) d)) e)) f)) g)) h)) i)) j))
  
  let main = let rez = (((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000) in
  let a0 = (print_int rez) in
  let temp2 = ((((wrap test3) 1) 10) 100) in
  0

  $ pat_elim < manytests/typed/006partial.ml
  let foo = (fun b -> if b
  then (fun foo -> ((( + ) foo) 2))
  else (fun foo -> ((( * ) foo) 10)))
  
  let foo = (fun x -> ((foo true) ((foo false) ((foo true) ((foo false) x)))))
  
  let main = let a0 = (print_int (foo 11)) in
  0

  $ pat_elim < manytests/typed/006partial2.ml
  let foo = (fun a b c -> let a2 = (print_int a) in
  let a1 = (print_int b) in
  let a0 = (print_int c) in
  ((( + ) a) ((( * ) b) c)))
  
  let main = let foo = (foo 1) in
  let foo = (foo 2) in
  let foo = (foo 3) in
  let a3 = (print_int foo) in
  0
