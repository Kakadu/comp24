open Kreml_lib.Parser
open Kreml_lib.Inferencer

let parse_expr input =
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

let ()=
  let examples = [
    (* "let rec f n = if (n < 1) then 1 else n * f (n-1)"; *)
  (* "let rec is_even n =
         if n = 0 then true
         else if n = 1 then false
         else is_odd (n - 1)
      and is_odd n =
         if n = 1 then true
         else if n = 0 then false
         else is_odd (n - 1)"; *)
    (* "let rec fold l folder init =
         match l with
         | x::xs ->
            let acc = folder init x in
            fold xs folder acc
         | [] -> init
    let mul x y = x * y 

    let f = fold [1;2;3;4] mul 1
   "; *)
   (* "let id x = x
   
    let i = id 5" *)
    (* "let rec fix f x = f (fix f) x"; *)
    (* "let rec fib_acc a b n =
      if n=1 then b
      else
        let n1 = n-1 in
        let ab = a+b in
        fib_acc b ab n1

    let rec fib n =
      if n<2
      then n
      else fib (n - 1) + fib (n - 2) "; *)
      "let wrap f = if 1 = 1 then f else f

let test3 a b c =
  let a = print_int a in
  let b = print_int b in
  let c = print_int c in
  0

let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j

let main =
  let rez =
      (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
         1000000000)
  in
  let () = print_int rez in
  let temp2 = wrap test3 1 10 100 in
  0"
    ] in
  List.iter (fun e -> parse_expr e) examples
