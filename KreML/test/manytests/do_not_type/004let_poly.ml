(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let temp = (fun f -> f 1, f true) (fun x -> x)