open MEML_lib.Closure
open MEML_lib.Lamdalift
open MEML_lib.Llprinter

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match MEML_lib.Parser.parser s with
  | Ok ast ->
    let lift = lambada_lift @@ closure ast in
    Format.print_string @@ llprinter lift
  | Error message -> Format.printf "Error: %s\n" message
;;
