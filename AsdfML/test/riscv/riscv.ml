(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Lib

let out_file = ref "/tmp/out.s"
let print_anf = ref false

let compile code =
  match Parser.parse_program code with
  | Error e -> failwith e
  | Ok ast ->
    (match Inferencer.inference_program ast with
     | Error e -> failwith (Format.asprintf "%a" Pp_typing.pp_error e)
     | Ok ast ->
       ast
       |> Tast.strip_types_program
       |> Remove_patterns.remove_patterns
       |> Remove_match.remove_match
       |> Closure_conversion.closure_conversion
       |> Lambda_lifting.lambda_lifting
       |> Anf.anf
       |> Riscv64.compile ~out_file:!out_file ~print_anf:!print_anf)
;;

let () =
  Arg.parse
    [ "-o", Arg.String (fun f -> out_file := f), "output file"
    ; "-anf", Arg.Unit (fun () -> print_anf := true), "print ANF"
    ]
    (fun _ -> ())
    "";
  match In_channel.input_all Stdlib.stdin |> compile with
  | Error e -> print_endline e
  | Ok _ -> ()
;;
