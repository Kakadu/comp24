(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse_program s with
  | Ok ast ->
    let lifted =
      ast |> Middleend.Closure_conversion.convert_ast |> Middleend.Lambda_lifting.lift_ast
    in
    (match Middleend.Match_elimination.transform lifted with
     | _, Ok new_ast ->
       List.iter
         (fun binding ->
           Stdlib.Format.printf
             "%a\n"
             Middleend.Restore_llast.pp_lllet_declaration
             binding)
         new_ast
     | _, Error message -> Format.printf "Error: %s\n" message)
  | Error message -> Format.printf "Error: %s\n" message
;;
