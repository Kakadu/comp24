(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypeTree

let pp_const_type ppf = function
  | GTInt -> Format.fprintf ppf "int"
  | GTBool -> Format.fprintf ppf "bool"
  | GTUnit -> Format.fprintf ppf "unit"
  | GTChar -> Format.fprintf ppf "char"
  | GTString -> Format.fprintf ppf "string"
;;

let pp_type ppf typ =
  let rec helper ppf = function
    | TGround t -> pp_const_type ppf t
    | TVar v ->
      let var_name = Char.chr (Char.code 'a' + v) in
      Format.fprintf ppf "'%c" var_name
    | TArr (l, r) ->
      (match l with
       | TArr (_, _) -> Format.fprintf ppf "(%a) -> %a" helper l helper r
       | _ -> Format.fprintf ppf "%a -> %a" helper l helper r)
    | TList t ->
      (match t with
       | TArr (_, _) -> Format.fprintf ppf "(%a) list" helper t
       | _ -> Format.fprintf ppf "%a list" helper t)
    | TTuple tl ->
      Format.fprintf
        ppf
        "%a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf " * ")
           (fun ppf ty ->
              match ty with
              | TTuple _ | TArr _ -> Format.fprintf ppf "(%a)" helper ty
              | _ -> helper ppf ty))
        tl
  in
  helper ppf (TypeVarsRecalculate.recalculate_vars typ)
;;

let type_to_string typ = Format.asprintf "%a" pp_type typ

let expr_without_name typ = "- : " ^ type_to_string typ
let expr_with_name name typ = String.concat " " [ "val"; name; ":"; type_to_string typ ]

let print_expr_type typ = Format.printf "%s\n" (expr_without_name typ)
let print_program_type env names_list =
  Base.List.iter names_list ~f:(fun name ->
      let (Schema.Schema (_, ty)) = Base.Map.find_exn env name in
      Format.printf "%s\n" (expr_with_name name ty))
;;
