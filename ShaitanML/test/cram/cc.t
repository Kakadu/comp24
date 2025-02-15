  $ cc < manytests/typed/001fac.ml
  let rec fac = (fun n -> (if ((<= n) 1) then 1 else ((* n) (fac ((- n) 1)))))
  let main = (let () = (print_int (fac 4)) in 0)

  $ cc << EOF
  > let f x = let g y = x + y in g
  > EOF
  let f = (fun x -> (let g = (fun x -> (fun y -> ((+ x) y))) in (g x)))

  $ cc << EOF
  > let main = fun x -> x
  > EOF
  let main = (fun x -> x)

  $ cc << EOF
  > let f k =
  >   let g x y = (x + y) * k
  >   in g 1 2
  > EOF
  let f = (fun k -> (let g = (fun k -> (fun x -> (fun y -> ((* ((+ x) y)) k)))) in (((g k) 1) 2)))

  $ cc << EOF
  > let f x =
  >   let g acc = x :: acc in
  >   let h (hd :: tl) = tl in
  >   g (h [1; 2; 3; 42])
  > EOF
  let f = (fun x -> (let g = (fun x -> (fun acc -> (x :: acc))) in (let h = (fun (hd :: tl) -> tl) in ((g x) (h (1 :: (2 :: (3 :: (42 :: [])))))))))

  $ cc << EOF
  > let a c d =
  >   let m = c + d in
  >   let k l = l + m in
  >   k (5 + m)
  > EOF
  let a = (fun c -> (fun d -> (let m = ((+ c) d) in (let k = (fun m -> (fun l -> ((+ l) m))) in ((k m) ((+ 5) m))))))

  $ cc << EOF
  > let fac n =
  >   let rec fack n k =
  >     if n <= 1 then k 1
  >     else fack (n - 1) (fun m -> k (m * n))
  >   in
  >   fack n (fun x -> x)
  > EOF
  let fac = (fun n -> (let rec fack = (fun <= -> (fun n -> (fun k -> (if ((<= n) 1) then (k 1) else (((fack <=) ((- n) 1)) (((fun k -> (fun n -> (fun m -> (k ((* m) n))))) k) n)))))) in (((fack <=) n) (fun x -> x))))

  $ cc << EOF
  > let gen seed1 seed2 =
  >   let gen n = n * seed2 + seed1 * 42 in
  >   [gen 1; gen 2; gen 3]
  > EOF
  let gen = (fun seed1 -> (fun seed2 -> (let gen = (fun seed1 -> (fun seed2 -> (fun n -> ((+ ((* n) seed2)) ((* seed1) 42))))) in ((((gen seed1) seed2) 1) :: ((((gen seed1) seed2) 2) :: ((((gen seed1) seed2) 3) :: []))))))

  $ cc << EOF
  > let gen seed1 seed2 =
  >   let gen n = n * seed2 + seed1 * 42 in
  >   gen 0 :: [gen 1; gen 2; gen 3]
  > EOF
  let gen = (fun seed1 -> (fun seed2 -> (let gen = (fun seed1 -> (fun seed2 -> (fun n -> ((+ ((* n) seed2)) ((* seed1) 42))))) in ((((gen seed1) seed2) 0) :: ((((gen seed1) seed2) 1) :: ((((gen seed1) seed2) 2) :: ((((gen seed1) seed2) 3) :: [])))))))

  $ cc << EOF
  > let main x =
  >   let const f = fun s -> f in
  >   let rev_const f s = const s in
  >   rev_const (fun _ -> x)
  > EOF
  let main = (fun x -> (let const = (fun x -> (fun f -> (fun s -> f))) in (let rev_const = (fun x -> (fun f -> (fun s -> (((const x) x) s)))) in ((rev_const x) ((fun x -> (fun _ -> x)) x)))))

  $ cc << EOF
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = factorial 6
  > EOF
  let rec factorial = (fun n -> (if ((<= n) 1) then 1 else ((* n) (factorial ((- n) 1)))))
  let main = (factorial 6)

  $ cc << EOF
  > let rec map f list = match list with
  >   | head :: tail -> f head :: map f tail
  >   | _ -> []
  > 
  > let tuple_map f tuple = match tuple with
  >   | (x, y) -> (f x, f y)
  > 
  > let main = map (tuple_map (fun x -> x * 2)) [(1, 2); (5, 6)]
  > EOF
  let rec map = (fun f -> (fun list -> (match list with
  | (head :: tail) -> ((f head) :: ((map f) tail))
  | _ -> [])))
  let tuple_map = (fun f -> (fun tuple -> (match tuple with
  | (x, y) -> ((f x), (f y)))))
  let main = ((map (tuple_map (fun x -> ((* x) 2)))) ((1, 2) :: ((5, 6) :: [])))

  $ cc << EOF
  > let thrice_cps f_cps x = fun k ->
  >   f_cps x (fun fx ->
  >     f_cps fx (fun ffx ->
  >       f_cps ffx k))
  > EOF
  let thrice_cps = (fun f_cps -> (fun x -> (fun k -> ((f_cps x) (((fun f_cps -> (fun k -> (fun fx -> ((f_cps fx) (((fun f_cps -> (fun k -> (fun ffx -> ((f_cps ffx) k)))) f_cps) k))))) f_cps) k)))))

  $ cc << EOF
  > let main k i =
  >   match (fun x y -> x + k) with
  >   | f ->
  >     let id = fun x -> x in
  >     f i
  > EOF
  let main = (fun k -> (fun i -> (match (fun x -> (fun y -> ((+ x) k))) with
  | f -> (let id = (fun f -> (fun i -> (fun x -> x))) in (f i)))))
