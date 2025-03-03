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
