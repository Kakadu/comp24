open MEML_lib.Closure
open MEML_lib.Parser
open MEML_lib.Printer

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match parser s with
  | Ok ast ->
    let converted = closure ast in
    Format.print_string @@ printer converted
  | Error message -> Format.printf "Error: %s\n" message
;;