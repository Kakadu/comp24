(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Format
open Parser.Ast

let list_unparser ppf l ~f ~s =
  List.iteri
    (fun i x ->
      if i <> 0 then fprintf ppf s else ();
      fprintf ppf "%a" f x)
    l
;;

let unparse_rec_flag ppf = function
  | Nonrecursive -> ()
  | Recursive -> fprintf ppf " rec"
;;
