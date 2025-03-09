(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)


let pp_env_before_modification env = 
  Format.printf
    "Bindings before transformations:\n%a\n"
    Inferencer.PP.pp_program env
;;

let pp_env_after_modification env = 
  Format.printf
    "Bindings after transformations:\n%a\n"
    Inferencer.PP.pp_program env;
  Printf.printf "------------------------------\n\n" 
;;