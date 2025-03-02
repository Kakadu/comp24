(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open QCheck.Gen

let gen_rest_of_ident =
  string_size
    ~gen:
      (oneof [ char_range 'a' 'z'; char_range 'A' 'Z'; char_range '0' '9'; return '_' ])
    (int_range 0 10)
;;

let gen_ident =
  let+ first_char = char_range 'a' 'z'
  and+ rest = gen_rest_of_ident in
  String.make 1 first_char ^ rest
;;

let gen_capitalized_ident =
  let+ first_char = char_range 'A' 'Z'
  and+ rest = gen_rest_of_ident in
  String.make 1 first_char ^ rest
;;

let gen_list_helper main_gen_fun depth =
  let* len = int_range 2 5 in
  list_repeat len (main_gen_fun (depth / len))
;;

let rec gen_construct gen construct depth tuple_list ?(list_cons = false) =
  let l = [ return "::"; return "[]" ] in
  let* constr =
    match list_cons with
    | false -> oneof (gen_capitalized_ident :: l)
    | _ -> oneof l
  in
  match constr with
  | "::" ->
    let+ g = gen (depth / 2)
    and+ rest = gen_construct gen construct (depth / 2) tuple_list ~list_cons:true in
    construct (constr, Some (tuple_list [ g; rest ]))
  | "[]" -> return (construct (constr, None))
  | _ ->
    let+ gt = gen_list_helper gen depth in
    construct (constr, Some (tuple_list gt))
;;
