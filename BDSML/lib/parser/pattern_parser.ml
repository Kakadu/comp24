open Utils
open Angstrom
open Const_parser
open Ast

let parse_any = check_char '_' *> return Pat_any

let parse_pconst =
  let+ c = parse_const in
  Pat_constant c
;;

let parse_var =
  let+ ident = ws *> parse_lowercase_ident in
  Pat_var ident
;;

let parse_ptuple p =
  sep_by (check_char ',') p
  >>= function
  | [] -> fail "It cannot be this way"
  | [ h ] -> return h
  | h :: tl -> return (Pat_tuple (h :: tl))
;;

let parse_pcons p =
  let constructor =
    let+ ident = ws *> parse_capitalized_ident
    and+ arg = p >>| Option.some <|> return None in
    Pat_construct (ident, arg)
  in
  let list_cons =
    let helper =
      check_string "::"
      *> return (fun p1 p2 -> Pat_construct ("::", Some (Pat_tuple [ p1; p2 ])))
    in
    chainr1 p helper
  in
  constructor <|> list_cons <|> p
;;

let parse_por p =
  let helper = check_char '|' *> return (fun ptr1 ptr2 -> Pat_or (ptr1, ptr2)) in
  chainl1 p helper
;;

let parse_plist p =
  let parse_list =
    sep_by (check_char ';') p
    >>| fun l ->
    let rec helper = function
      | h :: tl -> Pat_construct ("::", Some (Pat_tuple [ h; helper tl ]))
      | [] -> Pat_construct ("[]", None)
    in
    helper l
  in
  remove_square_brackets (return (Pat_construct ("[]", None)))
  <|> remove_square_brackets parse_list
;;

let pat_typexpr p =
  remove_parents
  @@
  let+ pat = p
  and+ typexpr = Typexpr_parser.parse_typexpr in
  Pat_type (pat, typexpr)
;;

(** https://ocaml.org/manual/5.2/patterns.html#start-section *)
let priority =
  [ parse_pcons; choice_pass_prev [ parse_plist; Fun.id ]; parse_ptuple; parse_por ]
;;

let parse_pattern =
  fix (fun self ->
    parse_by_priority priority
    @@ choice
         [ parse_any
         ; parse_pconst
         ; parse_var
         ; pat_typexpr self
         ; remove_parents self
         ; parse_plist self
         ])
;;
