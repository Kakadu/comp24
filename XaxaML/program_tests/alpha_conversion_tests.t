  $ ./run_alpha_conversion.exe << EOF
  > let (a, b, c) = (1, 2, 3)
  > let f a b = a + b
  let #0 = (1, 2, 3)
  
  let a = ((#unpack_tuple #0) 0)
  
  let b = ((#unpack_tuple #0) 1)
  
  let c = ((#unpack_tuple #0) 2)
  
  let f = (fun #1 #2 -> ((+ #1) #2))
  $ ./run_alpha_conversion.exe << EOF
  > let f a b =
  >   let a x = b + x in
  >   let b x = a 1 + x in
  >   a 1 + b 2;;
  let f = (fun a b -> let #0 = (fun x -> ((+ b) x)) in
  let #1 = (fun x -> ((+ (#0 1)) x)) in
  ((+ (#0 1)) (#1 2)))
  $ ./run_alpha_conversion.exe << EOF
  > let f (a, b) xs =
  >   let rec f xs = 
  >     match xs with 
  >     | a::b -> a + f b
  >     | [] -> 0
  >    in
  >    f xs 
  let f = (fun #0 xs -> let a = ((#unpack_tuple #0) 0) in
  let b = ((#unpack_tuple #0) 1) in
  let rec #1 = (fun #2 -> if ((> (#list_length #2)) 0)
  then let #3 = (#list_hd #2) in
  let #4 = (#list_tl #2) in
  ((+ #3) (#1 #4))
  else if ((= #2) [])
  then 0
  else #match_failure) in
  (#1 xs))

  $ ./run_alpha_conversion.exe << EOF
  > let f = let f = 1 in f
  let f = let #0 = 1 in
  #0

  $ ./run_alpha_conversion.exe << EOF
  > let xs = 
  >   let xs = [1; 2; 3] in 
  >   let xs = 3 in 
  >   let t a b = 
  >     let a = b in
  >     let b = a in 
  >     a + b 
  >   in
  >   t xs xs 
  > 
  let xs = let #0 = (1::(2::(3::[]))) in
  let #1 = 3 in
  let t = (fun a b -> let #2 = b in
  let #3 = #2 in
  ((+ #2) #3)) in
  ((t #1) #1)

  $ ./run_alpha_conversion.exe << EOF
  > let a = 1
  > let b = 2
  > let x = match (a, b) with 
  >   | (1, c) -> let a = c in let c = 2 in b
  >   | _ -> 0 
  let a = 1
  
  let b = 2
  
  let x = let #0 = (a, b) in
  if ((= ((#unpack_tuple #0) 0)) 1)
  then let c = ((#unpack_tuple #0) 1) in
  let #1 = c in
  let #2 = 2 in
  b
  else 0
