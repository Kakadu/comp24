(** Copyright 2023-2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type constant =
    | Const_int of int (*Integer literal, e.g. [69]*)
    | Const_char of char (*Character literal, e.g. ['m']*)
    | Const_string of string (*String literal, e.g. ["something"]*)
    | Const_int32 of int32 (*Interger literal with size 32 bits, e.g. [69l]*)
    | Const_int64 of int64 (*Interger literal with size 64 bits, e.g. [69L]*)
  | Const_float of string (*Float literal, e.g. [0.69]*)
[@@deriving eq, show {with_path= false}]

type rec_flag = Nonrecursive | Recursive
[@@deriving eq, show {with_path= false}]
