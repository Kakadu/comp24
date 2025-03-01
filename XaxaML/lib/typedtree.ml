(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type typ =
  | T_var of int (** type var *)
  | T_prim of string (** ground type *)
  | T_arr of typ * typ (** function type *)
  | T_tuple of typ * typ list (** tuple type *)
  | T_list of typ (** list type *)

module TypeVarSet = Stdlib.Set.Make (Int)

type scheme = Scheme of TypeVarSet.t * typ

let type_var x = T_var x
let int_typ = T_prim "int"
let bool_typ = T_prim "bool"
let unit_typ = T_prim "unit"
let arrow l r = T_arr (l, r)
let ( @-> ) = arrow

(* map typed variables to letters *)
let assign_names typ =
  let calc_name letter num =
    "\'"
    ^
    if num = 0
    then Base.Char.to_string letter
    else Base.Char.to_string letter ^ Int.to_string num
  in
  let next_letter c = Base.Char.of_int_exn (Base.Char.to_int c + 1) in
  let next letter num =
    if Char.equal letter 'z' then 'a', num + 1 else next_letter letter, num
  in
  let rec helper names letter num = function
    | T_var n ->
      (match Base.Map.add names ~key:n ~data:(calc_name letter num) with
       | `Ok new_names -> new_names, next letter num
       | `Duplicate -> names, (letter, num))
    | T_arr (l, r) ->
      let names, (letter, num) = helper names letter num l in
      helper names letter num r
    | T_list t -> helper names letter num t
    | T_tuple (h, list) ->
      List.fold_left
        (fun (names, (letter, num)) -> helper names letter num)
        (names, (letter, num))
        (h :: list)
    | _ -> names, (letter, num)
  in
  let names, (_, _) = helper (Base.Map.empty (module Base.Int)) 'a' 0 typ in
  names
;;

let pp_internal ppf typ names_mapping =
  let rec helper ppf = function
    | T_var n ->
      (match names_mapping with
       | Some names ->
         (try Format.fprintf ppf "%s" (Base.Map.find_exn names n) with
          | Base.Not_found_s _ | Stdlib.Not_found ->
            Format.fprintf ppf "Names for types are counted incorrectly")
       | None -> Format.fprintf ppf "%i" n)
    | T_prim s -> Format.fprintf ppf "%s" s
    | T_arr (l, r) ->
      (match l with
       | T_arr _ -> Format.fprintf ppf "(%a) -> %a" helper l helper r
       | _ -> Format.fprintf ppf "%a -> %a" helper l helper r)
    | T_tuple (h, list) ->
      let print_item item fmt =
        match item with
        | T_arr _ | T_tuple _ -> Format.fprintf ppf (fmt ^^ "(%a)") helper item
        | _ -> Format.fprintf ppf (fmt ^^ "%a") helper item
      in
      List.fold_left (fun _ item -> print_item item " * ") (print_item h "") list
    | T_list t ->
      (match t with
       | T_arr _ | T_tuple _ -> Format.fprintf ppf "(%a) list" helper t
       | _ -> Format.fprintf ppf "%a list" helper t)
  in
  helper ppf typ
;;

let pp_typ ppf typ =
  let names = assign_names typ in
  pp_internal ppf typ (Some names)
;;

let pp_typ_debug ppf typ = pp_internal ppf typ None
