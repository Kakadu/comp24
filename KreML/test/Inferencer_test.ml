open Kreml_lib.Parser
open Kreml_lib.Inferencer
open Kreml_lib.Ast

  
let parse_expr input =
  let w e = Result.map snd (R.run (infer_expr TypeEnv.empty  e)) in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All expr input with
  | Ok rest ->
    let r = w rest in
    let asd = 
    match r with
    | Result.Ok s -> 
      (* let fmt = Stdlib.Format.std_formatter in *)
      print_endline (show_typ s )
      (* Subst.pp fmt s; *)
    | Error e -> pp_error (Stdlib.Format.std_formatter) e
    in asd
  | Error _ -> print_endline "3228"

let%expect_test "simple fun" =
  let examples = ["let rec fact n = if n < 1 then 1 else n * fact (n - 1) in fact 8"] in
  List.iter (fun e -> parse_expr e) examples;
  [%expect {| Ast.Typ_int |}]  


let%expect_test "simple fun" =
  let examples = ["let f a = 5 + a in f 6"] in
  List.iter (fun e -> parse_expr e) examples;
  [%expect {| Ast.Typ_int |}]
