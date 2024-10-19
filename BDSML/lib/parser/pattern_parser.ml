open Utils
open Angstrom
open Const_parser
open Ast

let parse_constr_name =
  let parse_bool_constr = string "true" <|> string "false" in
  let parse_unit_constr = string "()" in
  choice [ parse_capitalized_ident; parse_bool_constr; parse_unit_constr ]
;;

let parse_any = check_char '_' *> return Pat_any
let parse_pconst p = rec_remove_parents p <|> (parse_const >>| fun c -> Pat_constant c)

let parse_ptuple p =
  sep_by (check_char ',') p
  >>= function
  | [] -> p
  | [ h ] -> return h
  | h :: tl -> return (Pat_tuple (h :: tl))
;;

let parse_por p =
  let helper = check_char '|' *> return (fun ptr1 ptr2 -> Pat_or (ptr1, ptr2)) in
  chainl1 p helper
;;

let parse_plist p =
  let parse_list =
    sep_by (check_char ';') p
    >>| fun list ->
    let rec helper = function
      | h :: tl -> Pat_construct ("::", Some (Pat_tuple [ h; helper tl ]))
      | [] -> Pat_construct ("[]", None)
    in
    helper list
  in
  check_char '[' *> parse_list <* check_char ']'
;;

let parse_pat_constr p =
  let* name = parse_constr_name in
  let* arg = option None (p <* ws >>| Option.some) in
  return (Pat_construct (name, arg))
;;

let parse_pattern =
  fix
  @@ fun p ->
  let pattern = parse_pconst p in
  let pattern = parse_any <|> pattern in
  let pattern = parse_por pattern <|> pattern in
  let pattern = parse_plist pattern <|> pattern in
  let pattern = parse_ptuple pattern <|> pattern in
  pattern
;;
