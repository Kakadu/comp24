(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let internalfail = failwith
let unreachable () = internalfail "Reached unreachable by assumption code"
