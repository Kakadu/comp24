[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open LMisc
open PPrint

let parens p =
  group @@ align @@ char '(' ^^ align (break 0 ^^ p) ^^ break 0 ^^ char ')'

let infixl op l r = group @@ l ^^ group (break 1 ^^ op ^^ r)
let infixr op l r = group @@ l ^^ group (break 1) ^^ op ^^ r

let pp_id (Id.I id) =
  match id with
  | id when is_op_char (String.get id 0) ->
      PPrint.parens (string id)
  | id ->
      string id

let pp_const =
  let open LAst.Const in
  function
  | Int x -> OCaml.int x | Char x -> OCaml.char x | String x -> OCaml.string x

let plet ppat pexpr rec_flag bindings expr =
  let open LAst.Expr in
  let rec_flag = match rec_flag with Rec -> string " rec" | Nonrec -> empty in

  let bindings =
    let docs =
      List.map (List1.to_list bindings) ~f:(fun {pat; expr} ->
          group @@ nest 2 @@ ppat pat ^^ group (string " =" ^/^ pexpr expr) )
    in
    separate (break 1 ^^ string "and" ^^ nest 2 (break 1)) docs
  in

  let in_, expr =
    match expr with
    | None ->
        (empty, empty)
    | Some expr ->
        (break 1 ^^ string "in", break 1 ^^ pexpr expr)
  in

  group @@ string "let" ^^ rec_flag
  ^^ group (break 1)
  ^^ group (bindings ^^ in_)
  ^^ expr

module PrecedencePrinter (Prec : sig
  type t
  val min : int
  val to_enum : t -> int

  val parens : document -> document
end) =
struct
  type 'a t = P of (int -> 'a)

  let run' (P f) = f
  let run (P f) = f Prec.min
  let runf p x = run (p x)

  let ( let* ) p f = P (fun lvl -> run' (f (run' p lvl)) lvl)

  let cur_lvl = P Fn.id

  let pure x = P (fun _ -> x)

  let return' lvl doc =
    let* cur = cur_lvl in
    pure @@ if cur > lvl then Prec.parens doc else doc

  let return lvl = return' (Prec.to_enum lvl)

  let with_lvl lvl p = pure @@ run' p lvl

  let rprefix lvl op p =
    let lvl = Prec.to_enum lvl in
    let* doc = with_lvl (lvl + 1) p in
    return' lvl (op doc)

  let rinfixl lvl op l r =
    let lvl = Prec.to_enum lvl in
    let* l = with_lvl lvl l in
    let* r = with_lvl (lvl + 1) r in
    return' lvl (op l r)

  let rinfixr lvl op l r =
    let lvl = Prec.to_enum lvl in
    let* l = with_lvl (lvl + 1) l in
    let* r = with_lvl lvl r in
    return' lvl (op l r)

  let rinfixn lvl op ps =
    let lvl = Prec.to_enum lvl in
    let* docs =
      List.fold_right ps ~init:(pure []) ~f:(fun p acc ->
          let* acc = acc in
          let* doc = with_lvl (lvl + 1) p in
          pure (doc :: acc) )
    in
    return' lvl (op docs)
end
