[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open LMiddle

module LLId = struct
  module T = struct
    type t = LId of string [@@deriving ord, sexp_of]
  end
  include T
  include Comparator.Make (T)

  let from_tagged ((I id, tag) : IdTagged.t) : t =
    let tag = match tag with Gen -> "g" | User -> "u" in
    let id =
      String.concat_map id ~f:(fun ch ->
          if Char.is_alphanum ch then Char.to_string ch
          else Int.to_string (Char.to_int ch) )
    in
    LId (tag ^ id)
end
