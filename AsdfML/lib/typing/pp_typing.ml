(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Types
open Format
open Utils

let rec type_id_to_name n =
  let rec helper n =
    match n with
    | _ when n < 0 -> helper (26 - n)
    | n when n < 26 -> "'" ^ String.make 1 (Char.chr (n + Char.code 'a'))
    | _ ->
      type_id_to_name ((n / 26) - 1)
      ^ String.make 1 (Char.chr ((n mod 26) + Char.code 'a'))
  in
  helper n
;;

let rec int_to_alphabet_str n =
  if n < 26
  then "'" ^ String.make 1 (Char.chr (n + Char.code 'a'))
  else
    int_to_alphabet_str ((n / 26) - 1)
    ^ String.make 1 (Char.chr ((n mod 26) + Char.code 'a'))
;;

let rec pp_typ fmt = function
  | TVar var -> fprintf fmt "%s" (type_id_to_name var)
  | TGround x ->
    fprintf
      fmt
      (match x with
       | TInt -> "int"
       | TBool -> "bool"
       | TUnit -> "()")
  | TArrow (left, right) ->
    (match left with
     | TArrow (_, _) -> fprintf fmt "(%a) -> %a" pp_typ left pp_typ right
     | _ -> fprintf fmt "%a -> %a" pp_typ left pp_typ right)
  | TTuple xs -> pp_list ~sep: " * " fmt pp_typ xs
  | TList t -> fprintf fmt "%a list" pp_typ t
;;

let pp_error fmt : error -> _ = function
  | `Occurs_check -> fprintf fmt "Occurs check failed"
  | `No_variable s -> fprintf fmt "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    fprintf fmt "Unification failed on %a and %a" pp_typ l pp_typ r
  | `Arg_num_mismatch (pat, ty) ->
    fprintf
      fmt
      "Mismatched number of arguments in pattern %a and expression %a"
      Pp_ast.pp_pattern
      pat
      pp_typ
      ty
  | `Syntax_error s -> fprintf fmt "Syntax error: %s" s
  | `TODO s -> fprintf fmt "TODO: %s" s
;;

module VarSet = struct
  include Set

  let pp ppf s =
    fprintf ppf "VarSet = [ ";
    Base.Set.iter s (fun x -> fprintf ppf "%s; " (type_id_to_name x));
    fprintf ppf "]"
  ;;
end

let pp_scheme fmt = function
  | xs, ty -> fprintf fmt "forall %a . %a" VarSet.pp xs pp_typ ty
;;
