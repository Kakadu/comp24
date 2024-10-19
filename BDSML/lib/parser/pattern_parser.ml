open Utils
open Angstrom
open Const_parser
open Ast

let parse_any = check_char '_' *> return Pat_any
let parse_pconst = parse_const >>| fun c -> Pat_constant c

let parse_ptuple p =
  sep_by (check_char ',') (parse_pconst <|> parse_any <|> p)
  >>= function
  | [] -> fail "It cannot be this way"
  | [ h ] -> return h
  | h :: tl -> return (Pat_tuple (h :: tl))
;;

let parse_plist p =
  let parse_list =
    sep_by (check_char ';') (parse_ptuple p)
    >>| fun l ->
    let rec helper = function
      | h :: tl -> Pat_construct ("::", Some (Pat_tuple [ h; helper tl ]))
      | [] -> Pat_construct ("[]", None)
    in
    helper l
  in
  remove_square_brackets (return (Pat_construct ("[]", None)))
  <|> remove_square_brackets parse_list
  <|> parse_ptuple p
;;

let parse_pcons p =
  let helper =
    check_string "::"
    *> return (fun p1 p2 -> Pat_construct ("::", Some (Pat_tuple [ p1; p2 ])))
  in
  chainr1 (parse_plist p) helper
;;

let parse_por p =
  let helper = check_char '|' *> return (fun ptr1 ptr2 -> Pat_or (ptr1, ptr2)) in
  chainl1 (parse_pcons p) helper
;;

let parse_pattern = fix @@ fun p -> remove_parents (parse_por p) <|> parse_por p
