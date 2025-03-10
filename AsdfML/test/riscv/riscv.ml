(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Lib
open Test.Utils

let out_file = ref "/tmp/out.s"
let print_anf = ref false

let compile code =
  anf code (fun x ->
    match Riscv64.compile ~out_file:!out_file ~print_anf:!print_anf x with
    | Ok _ -> ()
    | Error e -> print_endline e)
;;

let () =
  Arg.parse
    [ "-o", Arg.String (fun f -> out_file := f), "output file"
    ; "-anf", Arg.Unit (fun () -> print_anf := true), "print ANF"
    ]
    (fun _ -> ())
    "";
  In_channel.input_all Stdlib.stdin |> compile
;;
