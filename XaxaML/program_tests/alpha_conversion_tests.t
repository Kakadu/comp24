  $ ./run_alpha_conversion.exe << EOF
  > let (a, b, c) = (1, 2, 3)
  > let f a b = a + b
  let a0 = (1, 2, 3)
  
  let a = ((#unpack_tuple a0) 0)
  
  let b = ((#unpack_tuple a0) 1)
  
  let c = ((#unpack_tuple a0) 2)
  
  let f = (fun a1 a2 -> ((+ a1) a2))
  $ ./run_alpha_conversion.exe << EOF
  > let f a b =
  >   let a x = b + x in
  >   let b x = a 1 + x in
  >   a 1 + b 2;;
  let f = (fun a b -> let a0 = (fun x -> ((+ b) x)) in
  let a1 = (fun x -> ((+ (a0 1)) x)) in
  ((+ (a0 1)) (a1 2)))
  $ ./run_alpha_conversion.exe << EOF
  > let f (a, b) xs =
  >   let rec f xs = 
  >     match xs with 
  >     | a::b -> a + f b
  >     | [] -> 0
  >    in
  >    f xs 
  let f = (fun a0 xs -> let a = ((#unpack_tuple a0) 0) in
  let b = ((#unpack_tuple a0) 1) in
  let rec a1 = (fun a2 -> if ((> (#list_length a2)) 0)
  then let a3 = (#list_hd a2) in
  let a4 = (#list_tl a2) in
  ((+ a3) (a1 a4))
  else if ((= a2) [])
  then 0
  else #match_failure) in
  (a1 xs))

  $ ./run_alpha_conversion.exe << EOF
  > let f = let f = 1 in f
  let f = let a0 = 1 in
  a0

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
  let xs = let a0 = (1::(2::(3::[]))) in
  let a1 = 3 in
  let t = (fun a b -> let a2 = b in
  let a3 = a2 in
  ((+ a2) a3)) in
  ((t a1) a1)

  $ ./run_alpha_conversion.exe << EOF
  > let a = 1
  > let b = 2
  > let x = match (a, b) with 
  >   | (1, c) -> let a = c in let c = 2 in b
  >   | _ -> 0 
  let a = 1
  
  let b = 2
  
  let x = let a0 = (a, b) in
  if ((= ((#unpack_tuple a0) 0)) 1)
  then let c = ((#unpack_tuple a0) 1) in
  let a1 = c in
  let a2 = 2 in
  b
  else 0
