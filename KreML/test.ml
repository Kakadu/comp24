let rec fix f x = f (fix f) x
let map f p = let (a,b) = p in (f a, f b)
let fixpoly l =
  fix (fun self l -> map (fun li x -> li (self l) x) l) l
let feven p n =
  let (e, o) = p in
  if n == 0 then 1 else o (n - 1)
let fodd p n =
  let (e, o) = p in
  if n == 0 then 0 else e (n - 1)
let tie = fixpoly (feven, fodd)