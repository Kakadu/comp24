open Utils
open Angstrom
open Const_parser
open Ast

let parse_any = check_char '_' *> return Pat_any
let parse_pconst p = rec_remove_parents p <|> (parse_const >>| fun c -> Pat_constant c)

let parse_ptuple p =
  sep_by (check_char ',') p
  >>= function
  | [] -> p
  | [ h ] -> return h
  | h :: tl -> return (Pat_tuple (h :: tl))
;;

let parse_pcons p =
  let helper = check_string "::" *> return (fun ptr1 ptr2 -> Pat_cons (ptr1, ptr2)) in
  chainr1 p helper
;;

let parse_por p =
  let helper = check_char '|' *> return (fun ptr1 ptr2 -> Pat_or (ptr1, ptr2)) in
  chainl1 p helper
;;

let parse_plist p =
  remove_square_brackets (sep_by (check_char ';') p >>| fun l -> Pat_list l)
;;

let parse_pattern =
  fix
  @@ fun p ->
  let pattern = parse_pconst p in
  let pattern = parse_any <|> pattern in
  let pattern = parse_por pattern <|> pattern in
  let pattern = parse_plist pattern <|> pattern in
  let pattern = parse_pcons pattern <|> pattern in
  let pattern = parse_ptuple pattern <|> pattern in
  pattern
;;
