(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  let rec print_anf = function
    | h :: tl ->
      (match h with
       | Ok decl ->
         Stdlib.Format.printf "%a\n" Middleend.Restore_anf_ast.restore_anf_decl decl;
         print_anf tl
       | Error message -> Format.printf "Error: %s\n" message)
    | [] -> ()
  in
  match Parser.parse_program s with
  | Ok ast ->
    (match
       Common.StateMonad.run
         (Middleend.Alpha_conversion.alpha_convert_decl_list
            Middleend.Alpha_conversion.init_context
            []
            ast)
         0
     with
     | _, Ok lst ->
       lst
       |> Middleend.Closure_conversion.convert_ast
       |> Middleend.Lambda_lifting.lift_ast
       |> Middleend.Anf.transform
       |> print_anf
     | _, Error err -> Format.printf "%s" err)
  | Error message -> Format.printf "Error: %s\n" message
;;
