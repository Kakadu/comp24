(* Uncomment to run with OCaml *)
(* open Format
let print_int x = printf "%d " x
let println_int x = printf "%d\n" x
let print_newline () = printf "\n"
let print_char x = printf "%c" (Char.chr x) *)

let pow x n =
  let rec helper acc n =
    match n with
    | 0 -> acc
    | n -> helper (acc * x) (n - 1)
  in
  helper 1 n

let shl x n = x * pow 2 n
let shr x n = x / pow 2 n


(* Fixed point arithmetic *)
let frac_bits = 8
let frac = shl 1 frac_bits
let to_fp x = shl x frac_bits
let zero = to_fp 0
let one = to_fp 1
let two = to_fp 2
let mul x y = (x*y) / (frac) 
let sq x = mul x x
let div x y = (x * frac) / y
let eps = div one (to_fp (pow 2 (frac_bits - 2)))

(* TODO: remove h when top level patterns are fixed *)
let dot a b = 
  let h (x1, y1, z1) (x2, y2, z2) = 
    mul x1 x2 + mul y1 y2 + mul z1 z2
  in h a b
let add_vec a b = 
  let h (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
  in h a b
let sub_vec a b = 
  let h (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2) 
  in h a b
let map_vec f v = 
  let h f (x, y, z) = (f x, f y, f z) 
  in h f v
let mul_vec v c = 
  let h (x, y, z) scalar = map_vec (mul scalar) (x, y, z) 
  in h v c

let sqrt_ x =
  let rec binary_search low high =
    if high - low <= eps
    then low
    else (
      let mid = div (low + high) two in
      let mid_sq = sq mid in
      if mid_sq = x
      then mid
      else if mid_sq < x
      then binary_search mid high
      else binary_search low mid)
  in
  binary_search zero x
;;

let normalize vec = 
  let helper (x, y, z) = 
    let len_sq = dot vec vec in
    let len = sqrt_ (len_sq) in
    if len = zero then (zero, zero, zero) else (map_vec (fun x -> div x len) vec) 
  in 
  helper vec
;;

let abs x = if x < zero then -x else x

let hit_sphere sphere ray =
  let (center, radius) = sphere in
  let (origin, dir) = ray in
  let oc = sub_vec center origin in
  let a = dot dir dir in
  let b = dot dir oc in
  let c = dot oc oc - sq radius in
  let discr = sq b - mul a c in
  if discr < zero
  then (false, (zero, zero, zero))
  else (
    let sqrt_discr = sqrt_ discr in
    let t1 = div (b - sqrt_discr) a in
    let t2 = div (b + sqrt_discr) a in
    let t = if t1 >= zero then t1 else if t2 >= zero then t2 else zero in
    if t <= zero
    then (false, (zero, zero, zero))
    else (
      let normal = sub_vec (add_vec origin (mul_vec dir t)) center in
      let normal = normalize normal in
      (true, normal)))
;;

let loop fn start stop =
  let rec helper i =
    if i = stop
    then ()
    else (
      let _ = fn i in
      helper (i + 1))
  in
  helper start
;;

let loop2 fn start1 stop1 start2 stop2 = 
  loop (fun y -> loop (fun x -> fn x y) start2 stop2) start1 stop1

(* 
  Min and max x,y values for the screen 
  For 64, it takes around 30sec and 1.5gb of ram
*)
let max = 32
let min = -max 

let cam_to_screen_distance = 2 * max 
let camera = (zero, zero, to_fp cam_to_screen_distance)
let sphere_radius = max
let cam_to_sphere_distance = min
let sphere = ((zero, zero, to_fp cam_to_sphere_distance), to_fp sphere_radius)

(*
  x right, y up, -z forward. (probably)
  screen is in x-y plane, z = 0, x and y in [main.min, main.max]
  rays are cast from the camera to the point (x, y) on screen
*)
let raytrace x y =
  let (x, y) = (to_fp x, to_fp y) in
  let screen_point = (x, y, zero) in
  let dir = sub_vec screen_point camera  in
  let dir = normalize dir in
  let ray = (camera, dir) in
  let (hit, normal) = hit_sphere sphere ray in
  if hit
  then (
    let normal = map_vec abs normal in
    (* 
      normal is normalized and abs, so each coord should be fixed-point in range [0;1].
      offset fp to 8 bits to convert underlying int into [0;255] range
    *)
    let normal =
      if frac_bits = 8
      then normal
      else if frac_bits > 8
      then map_vec (fun x -> shr x (frac_bits - 8)) normal
      else map_vec (fun x -> shl x (8 - frac_bits)) normal
    in
    let (nx, ny, nz) = normal in
    let _ = print_int nx in 
    let _ = print_int ny in 
    let _ = print_int nz in 
    print_newline ()) 
  else (
    let _ = print_int 0 in 
    let _ = print_int 0 in 
    let _ = print_int 0 in 
    print_newline () 
  )
;;

let main =
  (* let _ = panic () in *)
  (* .PPM header *)
  let _ = print_char 80 in 
  let _ = println_int 3 in 
  let _ = print_int (max - min) in 
  let _ = print_int (max - min) in 
  let _ = print_newline () in 
  let _ = print_int 255 in
  let _ = loop2
    (fun x y ->
      let _ = if x = min 
        then (print_newline ())
        else ()
      in
      raytrace x y
   ) min max min max
  in
  ()
;;
