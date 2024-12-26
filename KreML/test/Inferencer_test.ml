open Kreml_lib.Parser
open Kreml_lib.Inferencer

  
let parse_program input =
  let w p = R.run (infer_program p) in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All program input with
  | Ok rest ->
    let r = w rest in 
    (match r with
    | Result.Ok (s, env) -> 
      let fmt = Stdlib.Format.std_formatter in 
      let() = TypeEnv.pp fmt env in
      let () = Format.fprintf fmt "\n" in
      Subst.pp fmt s
    | Error e -> pp_error (Stdlib.Format.std_formatter) e)
  | Error _ -> print_endline "3228"


let%expect_test "fac" =
  let fac = 
  "let rec fac n = if n<=1 then 1 else n * fac (n-1)

   let main =
     let () = print_int (fac 4) in
     0" in
 parse_program fac;
  [%expect {|
    [ fac -> [ ]int -> int
    , main -> [ ]int
    , print_int -> [ ]int -> unit
     ]
    [ 0 -> int -> int
    , 1 -> int
    , 2 -> bool
    , 3 -> int -> bool
    , 4 -> int
    , 5 -> int -> int
    , 6 -> int
    , 7 -> int
    , 8 -> int -> int
    , 10 -> unit
    , 11 -> int
     ] |}]


let%expect_test "fac_cps" =
  let fac_cps = 
  "let rec fac_cps n k =
    if n=1 then k 1 else
    fac_cps (n-1) (fun p -> k (p*n))

  let main =
    let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
    0" in
 parse_program fac_cps;
  [%expect {|
    [ fac_cps -> [ 6; ]int -> (int -> 6) -> 6
    , main -> [ ]int
    , print_int -> [ ]int -> unit
     ]
    [ 0 -> int -> (int -> 6) -> 6
    , 1 -> int
    , 2 -> int -> 6
    , 3 -> bool
    , 4 -> int -> bool
    , 5 -> 6
    , 7 -> (int -> 6) -> 6
    , 8 -> int
    , 9 -> int -> int
    , 10 -> int
    , 11 -> 6
    , 12 -> int
    , 13 -> int -> int
    , 15 -> unit
    , 16 -> int
    , 17 -> (int -> int) -> int
    , 18 -> int
    , 19 -> int
     ] |}]

  let%expect_test "asd" =
    let m = Base.Map.empty(module Base.Int) in
    let m1 = Base.Map.set m ~key:1 ~data:2 in
    match Base.Map.find m1 1 with
    | Some v -> print_int v
    | _ -> print_endline "there is nothing";
    [%expect {| 2 |}]


