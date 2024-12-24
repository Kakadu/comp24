open Kreml_lib.Parser
open Kreml_lib.Inferencer

let parse_expr input =
  let w p = R.run (infer_program p) in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All program input with
  | Ok rest ->
    let r = w rest in
    let asd = 
    match r with
    | Result.Ok s -> 
      let fmt = Stdlib.Format.std_formatter in 
      TypeEnv.pp fmt s;
    | Error e -> pp_error (Stdlib.Format.std_formatter) e
    in asd
  | Error _ -> print_endline "3228"

let ()=
  let examples = ["let rec f n = if (n < 1) then 1 else n * f (n-1)";
  (* "let rec is_even n =
         if n = 0 then true
         else if n = 1 then false
         else is_odd (n - 1)
      and is_odd n =
         if n = 1 then true
         else if n = 0 then false
         else is_odd (n - 1)"; *)
    "let rec fold l folder init =
         match l with
         | x::xs ->
            let acc = folder init x in
            fold xs folder acc
         | [] -> init"] in
  List.iter (fun e -> parse_expr e) examples
