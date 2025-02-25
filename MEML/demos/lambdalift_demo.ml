open MEML_lib.Closure
open MEML_lib.Lambdalift
open MEML_lib.PprinterLLAST

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match MEML_lib.Parser.parser s with
  | Ok ast ->
    let lift = lambda_lift @@ closure ast in
    Format.print_string @@ pp_lambda_lifting lift
  | Error message -> Format.printf "Error: %s\n" message
;;