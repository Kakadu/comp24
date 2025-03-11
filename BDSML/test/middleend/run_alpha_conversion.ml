(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Middleend

let test str =
  match Parser.Main_parser.parse str with
  | Result.Error e -> Format.eprintf "Parser error: %s" e
  | Result.Ok ast ->
    (match Typing.Inference.infer_program ast with
     | Result.Error s -> Format.eprintf "Inference error: %s" s
     | Result.Ok _ ->
       let ( >>= ) = Result.bind in
       let res =
         Pattern_remover.remove_patterns ast
         >>= Alpha_conversion.alpha_conversion
         |> Result.map_error Middleend_utils.exp_to_string
       in
       let res =
         match res with
         | Result.Ok s ->
           s |> Converter.rast_to_ast |> Quickcheck.Main_unparser.unparse_structure
         | Result.Error s -> s
       in
       print_string res)
;;

(* let __disassemble : 'a -> 'b = ()
   let __get_from_tuple : 'a -> int -> 'c = ()
   let __same_cons : 'a -> 'b -> bool = ()
   let __get_cons_param : 'a -> 'b = ()
   let __exception : string -> 'a = ()

   let rec __var_helper __var_a0 __reserved_1 __reserved_2 = (let __anf_0 = ((__same_cons __reserved_2) "[]") in
   (if __anf_0 then (let __nothing = ((__same_cons __reserved_2) (([]))) in
   __reserved_1) else (let __anf_1 = ((__same_cons __reserved_2) "::") in
   (let __anf_2 = ((( && ) true) true) in
   (let __anf_3 = ((( && ) __anf_2) true) in
   (let __anf_4 = ((( && ) __anf_1) __anf_3) in
   (if __anf_4 then (let __reserved_4 = ((__disassemble "::") __reserved_2) in
   (let __var_h = ((__get_from_tuple __reserved_4) 0) in
   (let __var_tl = ((__get_from_tuple __reserved_4) 1) in
   (let __anf_5 = ((( + ) __reserved_1) __var_a0) in
   ((__var_helper __anf_5) __var_tl))))) else (__exception "Match_failure"))))))));;
   let __var_f __reserved_0 = ((__var_helper __reserved_0) 0);; *)
(**)
let () = test @@ Stdio.In_channel.input_all Stdlib.stdin
