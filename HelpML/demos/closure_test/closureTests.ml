open Helpml_lib.Parser
open Helpml_lib.Closure
open Helpml_lib.AstPrinter
open Helpml_lib.Inferencer
open Helpml_lib.Typ
open Base

let print_prog_result prog =
  match parse prog with
  | Ok prog ->
    (match run_prog_inference prog with
     | Ok _ ->
       let prog_closure = prog_conversion prog in
       List.iter prog_closure ~f:(fun binding ->
         Stdlib.Format.printf "%a\n" pp_binding binding)
     | Error e -> print_typ_err e)
  | Error e -> Stdlib.Format.printf "Error%s" e
;;

let () = print_prog_result (Stdio.In_channel.input_all Stdlib.stdin)
