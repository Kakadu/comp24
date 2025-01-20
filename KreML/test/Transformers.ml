open Kreml_lib.Inferencer
open Kreml_lib

let () =
  let open Stdlib.Format in
  let input = In_channel.input_all stdin in
  match Parser.run input with
  | Ok structure ->
    (match Inferencer.run structure with
     | Ok _ ->
       let alpha = Alpha_transformer.transform structure in
       let anf = Anf_transformer.transform alpha in
       Anf_printer.pp std_formatter anf
     | Error error ->
       fprintf std_formatter "An error occured while type checking: %a" pp_error error)
  | Error _ -> fprintf std_formatter "Could not parse the program %s" input
;;
