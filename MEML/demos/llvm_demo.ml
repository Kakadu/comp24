open MEML_lib.Closure
open MEML_lib.Lambdalift

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match MEML_lib.Parser.parser s with
  | Ok ast ->
    let lift = lambda_lift @@ closure ast in
    let codegen = MEML_lib.Codegen.codegen lift in
    codegen
  | Error message -> Format.printf "Error: %s\n" message
;;