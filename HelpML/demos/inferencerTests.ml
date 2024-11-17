open Helpml_lib.Parser
open Helpml_lib.Inferencer
open Helpml_lib.Typ
open Base

let print_prog_result prog =
  match parse prog with
  | Ok prog ->
    (match run_prog_inference prog with
     | Ok l ->
       List.iter l ~f:(fun (id, t) -> Stdlib.Format.printf "%s : %a\n" id pp_typ t)
     | Error e -> print_typ_err e)
  | Error e -> Stdlib.Format.printf "Error%s" e
;;

let () = print_prog_result (Stdio.In_channel.input_all Stdlib.stdin)
