(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse_program s with
  | Ok ast ->
    let final =
      ast
      |> Middleend.Closure_conversion.convert_ast
      |> Middleend.Lambda_lifting.lift_ast
      |> Middleend.Anf.transform
    in
    List.iter
      (fun binding ->
        Stdlib.Format.printf "%a\n" Middleend.Restore_anf_ast.restore_anf_decl binding)
      final
  | Error message -> Format.printf "Error: %s\n" message
;;
