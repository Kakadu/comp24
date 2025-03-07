(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Format
open Parser.Ast

let list_unparser ?(add_before = false) ppf l ~f ~s =
  List.iteri
    (fun i x ->
      if add_before || i <> 0 then fprintf ppf s else ();
      fprintf ppf "%a" f x)
    l
;;

let unparse_rec_flag ppf = function
  | Nonrecursive -> ()
  | Recursive -> fprintf ppf " rec"
;;
