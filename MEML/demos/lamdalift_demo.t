  $ dune exec ./lamdalift_demo.exe << EOF
  > let prog = 5 + 5
  > EOF
  let  prog  = (5 + 5)

  $ dune exec ./lamdalift_demo.exe << EOF
  > let test1 x = let test2 y = x + y in test2
  > EOF
  let  lambada0 x y  = (x + y)
  let  test1 x  = (lambada0 x)

  $ dune exec ./lamdalift_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  > EOF
  let  lambada0 x y i  = (x, y, i)
  let  test1 (x, y)  = ((lambada0 x) y)

  $ dune exec ./lamdalift_demo.exe << EOF
  > let nested1 = let nested2 = 5 in 
  > let nested3 = 6 in
  > let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55
  > EOF
  let  lambada1 nested2 nested3 i  = (nested2 + nested3)
  let  lambada0 nested2 nested3 x  = (x + (((lambada1 nested2) nested3) 8))
  let  nested1  = 
    let  nested2 = 5
    in 
    let  nested3 = 6
    in (((lambada0 nested2) nested3) 55)

  $ dune exec ./lamdalift_demo.exe << EOF
  > let rec fix f x = f (fix f) x;;
  > let fixpoly l =
  > fix (fun self l -> map (fun li x -> li (self l) x) l) l;;
  > let feven p n =
  > let e = p in
  > if n = 0 then 1 else o (n - 1);;
  > let fodd p n =
  > let e = p in
  > if n = 0 then 0 else e (n - 1);;
  > let tie = fixpoly (feven, fodd);;
  let rec fix f x  = ((f (fix f)) x)
  let  lambada1 l self li x  = ((li (self l)) x)
  let  lambada0 map self l  = ((map ((lambada1 l) self)) l)
  let  fixpoly l  = ((fix (lambada0 map)) l)
  let  feven p n  = 
    let  e = p
    in 
    if (n = 0)
    then 1
    else (o (n - 1))
  let  fodd p n  = 
    let  e = p
    in 
    if (n = 0)
    then 0
    else (e (n - 1))
  let  tie  = (fixpoly (feven, fodd))

  $ dune exec ./lamdalift_demo.exe << EOF
  > let test a1 a2 a3 = a1 + a2 + a3
  > let () = test (5 + 5) (6 * 6) (7 * 7)
  > EOF
  let  test a1 a2 a3  = ((a1 + a2) + a3)
  let  ()  = (((test (5 + 5)) (6 * 6)) (7 * 7))


  $ dune exec ./lamdalift_demo.exe << EOF
  > let prog = 5 + 5
  > EOF
  let  prog  = (5 + 5)

  $ dune exec ./lamdalift_demo.exe << EOF
  > let test1 x = let test2 y = x + y in test2
  > EOF
  let  lambada0 x y  = (x + y)
  let  test1 x  = (lambada0 x)

  $ dune exec ./lamdalift_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  > EOF
  let  lambada0 x y i  = (x, y, i)
  let  test1 (x, y)  = ((lambada0 x) y)

  $ dune exec ./lamdalift_demo.exe << EOF
  > let test1 (x, y) = let test2 i = (x, y, i) in test2
  > EOF
  let  lambada0 x y i  = (x, y, i)
  let  test1 (x, y)  = ((lambada0 x) y)

  $ dune exec ./lamdalift_demo.exe << EOF
  > match n with
  >   | 0 -> 1
  >   | n -> 2 ;;
  (match n with
  | 0 -> 1
  | n -> 2

  $ dune exec ./lamdalift_demo.exe << EOF
  > let nested1 = let nested2 = 5 in 
  > let nested3 = 6 in
  > let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55
  > EOF
  let  lambada1 nested2 nested3 i  = (nested2 + nested3)
  let  lambada0 nested2 nested3 x  = (x + (((lambada1 nested2) nested3) 8))
  let  nested1  = 
    let  nested2 = 5
    in 
    let  nested3 = 6
    in (((lambada0 nested2) nested3) 55)

  $ dune exec ./lamdalift_demo.exe << EOF
  > let rec fac_cps n k = match n with
  >   | 0 -> k 1
  >   | n -> fac_cps (n - 1) (fun t -> k (n * t))
  > EOF
  let  lambada0 k n t  = (k (n * t))
  let rec fac_cps n k  = (match n with
  | 0 -> (k 1)
  | n -> ((fac_cps (n - 1)) ((lambada0 k) n))

  $ dune exec ./lamdalift_demo.exe << EOF
  > let fibo = fun n -> let rec fibo_cps = fun n acc -> match n with
  >   | 0 -> acc 0
  >   | 1 -> acc 1
  >   | _ -> fibo_cps (n - 1) (fun x -> fibo_cps (n - 2) (fun y -> acc (x + y)))
  > in
  > fibo_cps n (fun x -> x)
  > EOF
  let  lambada2 acc x y  = (acc (x + y))
  let  lambada1 acc fibo_cps n x  = ((fibo_cps (n - 2)) ((lambada2 acc) x))
  let rec lambada0 n acc  = (match n with
  | 0 -> (acc 0)
  | 1 -> (acc 1)
  | _ -> ((lambada0 (n - 1)) (((lambada1 acc) lambada0) n))
  let  lambada3 x  = x
  let  fibo n  = ((lambada0 n) lambada3)

  $ dune exec ./lamdalift_demo.exe << EOF
  > let rec fix f x = f (fix f) x;;
  > let map f p = let a = p in (f a, f b);;
  > let fixpoly l =
  > fix (fun self l -> map (fun li x -> li (self l) x) l) l;;
  > let feven p n =
  > let e = p in
  > if n = 0 then 1 else o (n - 1);;
  > let fodd p n =
  > let o = p in
  > if n = 0 then 0 else e (n - 1);;
  > let tie = fixpoly (feven, fodd);;
  let rec fix f x  = ((f (fix f)) x)
  let  map f p  = 
    let  a = p
    in ((f a), (f b))
  let  lambada1 l self li x  = ((li (self l)) x)
  let  lambada0 map self l  = ((map ((lambada1 l) self)) l)
  let  fixpoly l  = ((fix (lambada0 map)) l)
  let  feven p n  = 
    let  e = p
    in 
    if (n = 0)
    then 1
    else (o (n - 1))
  let  fodd p n  = 
    let  o = p
    in 
    if (n = 0)
    then 0
    else (e (n - 1))
  let  tie  = (fixpoly (feven, fodd))

  $ dune exec ./lamdalift_demo.exe << EOF
  > let test a1 a2 a3 = a1 + a2 + a3
  > let () = test (5 + 5) (6 * 6) (7 * 7)
  > EOF
  let  test a1 a2 a3  = ((a1 + a2) + a3)
  let  ()  = (((test (5 + 5)) (6 * 6)) (7 * 7))
