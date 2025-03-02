(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Lib

let ( $ ) = Fn.compose

let parse code cont =
  match Parser.parse_program code with
  | Ok ast -> cont ast
  | Error e -> Format.eprintf "%s" e
;;

let infer code cont =
  parse code (fun ast ->
    match Inferencer.inference_program ast with
    | Ok tast -> cont tast
    | Error e -> Format.eprintf "%a" Pp_typing.pp_error e)
;;

let infer_strip code cont =
  infer code (cont $ Alpha.alpha_program $ Tast.strip_types_program)
;;

let remove_patterns code cont = infer_strip code (cont $ Remove_patterns.remove_patterns)
let remove_match code cont = remove_patterns code (cont $ Remove_match.remove_match)

let closure_conversion code cont =
  remove_match code (cont $ Closure_conversion.closure_conversion)
;;

let lambda_lift code cont = closure_conversion code (cont $ Lambda_lifting.lambda_lifting)
let anf code cont = lambda_lift code (cont $ Anf.anf)
