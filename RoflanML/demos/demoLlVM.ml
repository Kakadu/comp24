(* Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Result
open Roflanml_lib

let ( let* ) = ( >>= )

let () =
  let input = Stdio.In_channel.input_all Stdlib.stdin in
  let llvm_prog =
    let* prog = Parser.parse input in
    let* _ =
      match Typechecker.typecheck prog with
      | Ok _ -> return prog
      | Error err ->
        Stdlib.Format.printf "%a" Typing.pp_error err;
        fail "Typecheck error"
    in
    let prog = Closure_conversion.close_program prog in
    let* prog = Lambda_lifting.lift_program prog in
    let* prog = Anf.anf_program prog in
    return (Llvm_gen.compile_program prog)
  in
  match llvm_prog with
  | Ok llvm_prog -> Stdlib.Format.printf "%s" (Llvm.string_of_llmodule llvm_prog)
  | Error err -> Stdlib.Format.printf "%s%!" err
;;
