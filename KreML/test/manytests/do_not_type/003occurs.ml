(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let fix f = (fun x -> f (fun f -> x x f)) (fun x -> f (fun f -> x x f))
