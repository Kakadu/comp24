(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Containers

val convert : Simple_ast.sstructure -> VarSSet.t -> (Anf.program, string) Result.t
