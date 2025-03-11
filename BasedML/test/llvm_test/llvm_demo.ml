(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

let test_llvm out_name =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse_program s with
  | Ok ast ->
    (match Typeinference.infer_prog ast with
     | Ok _ ->
       (match Middleend.Full.middleend_transform_prog ast with
        | Ok prog ->
          (* Format.printf "Anf:\n %s\n" (Middleend.Anf_to_str.program_to_string prog); *)
          Llvm_cg.Codegen.compile out_name prog
        | Error e -> Format.printf "Anf error: %s" e)
     | Error e -> Format.printf "Typeinference error: %s\n" e)
  | Error message -> Format.printf "Parser error: %s\n" message
;;

let () = test_llvm "out.ll"
