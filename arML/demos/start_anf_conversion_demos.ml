(* * Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  let () = Format.printf "ANF \n" in
  let () = anf_conversion_program s in
  let () = Format.printf "\nTypes (before anf conversion): \n" in
  let () = inference_program s in
  let () = Format.printf "\nTypes (after anf conversion): \n" in
  convert_to_anf_and_back_types s
;;
