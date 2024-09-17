(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Angstrom
open Ast

(* Utilities *)
let is_keyword = function
  | "if"
  | "else"
  | "let"
  | "bool"
  | "string"
  | "int"
  | "rec"
  | "match"
  | "fun"
  | "then"
  | "false"
  | "true"
  | "in" -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\n' -> true
  | _ -> false
;;

let is_quotes = function
  | '\"' -> true
  | _ -> false
;;

let is_valid_fst_char = function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false
;;

let skip_whitespace = skip_while is_whitespace
let between left right exp = left *> exp <* right

let between_parens exp =
  skip_whitespace *> between (Angstrom.char '(') (Angstrom.char ')') exp
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

(* Constant parsers*)
let p_cint =
  let* sign =
    skip_whitespace *> option "" (Angstrom.string "-" <|> Angstrom.string "+")
  in
  let* number = take_while1 is_digit in
  return @@ CInt (sign ^ number |> int_of_string)
;;

let p_cbool =
  let* bool_val =
    skip_whitespace *> (Angstrom.string "true" <|> Angstrom.string "false")
  in
  return (CBool (bool_val |> bool_of_string))
;;

let p_const_expr =
  let* const = p_cint <|> p_cbool in
  return (EConstant const)
;;

let p_const_pattern =
  let* const = p_cint <|> p_cbool in
  return (PConstant const)
;;

(* If the else parser *)

let p_if_then_else p_expr =
  let* guard = skip_whitespace *> Angstrom.string "if" *> p_expr in
  let* if_expr = skip_whitespace *> Angstrom.string "then" *> p_expr in
  let* else_expr = skip_whitespace *> Angstrom.string "else" *> p_expr in
  return (EIfThenElse (guard, if_expr, else_expr))
;;

(* Identifiers (Vars) parsers *)
let p_ident_string =
  skip_whitespace
  *>
  let* fst_char = peek_char in
  match fst_char with
  | Some c when is_valid_fst_char c ->
    let* identifier = take_while is_identifier_char in
    if is_keyword identifier
    then fail "Identifier parsing failed: identifier can't be a keyword"
    else return identifier
  | _ -> fail "Identifier parsing failed: first character must start with [a-z]"
;;

let p_ident =
  let* ident_name = p_ident_string in
  return @@ EIdentifier ident_name
;;

let p_ident_pattern =
  let* ident_name = p_ident_string in
  return @@ PIdentifier ident_name
;;

(* List parsers *)
let p_list p_exp constructor =
  let* fst_component = skip_whitespace *> Angstrom.string "[" *> p_exp in
  let* exp_list =
    many (skip_whitespace *> Angstrom.char ';' *> p_exp) <* Angstrom.string "]"
  in
  fst_component :: exp_list |> constructor |> return
;;

let p_list_expr p_exp = p_list p_exp (fun x -> EList x)
let p_wild_card_pattern = skip_whitespace *> Angstrom.string "_" *> return PWildCard

let p_list_pattern p_exp =
  p_list p_exp (fun x -> PList x)
  <|> skip_whitespace
      *> Angstrom.string "["
      *> skip_whitespace
      *> Angstrom.string "]"
      *> return PNil
;;

(* Tuple parsers *)

let p_tuple p_exp constructor =
  let* fst_component = skip_whitespace *> Angstrom.string "(" *> p_exp in
  let* exp_list =
    many1 (skip_whitespace *> Angstrom.char ',' *> p_exp) <* Angstrom.string ")"
  in
  fst_component :: exp_list |> constructor |> return
;;

let p_tuple_expr p_exp = p_tuple p_exp (fun x -> ETuple x)
let p_tuple_pattern p_exp = p_tuple p_exp (fun x -> PTuple x)

(* Binary operations parsers & delimiter for chains*)

let app_delim = return (fun operand1 operand2 -> EApplication (operand1, operand2))

let cons_delim_pattern =
  skip_whitespace
  *> Angstrom.string "::"
  *> return (fun operand1 operand2 -> PCons (operand1, operand2))
;;

let cons_delim_expr =
  skip_whitespace
  *> Angstrom.string "::"
  *> return (fun operand1 operand2 -> EBinop (operand1, Cons, operand2))
;;

let mul_delim =
  skip_whitespace
  *> Angstrom.string "*"
  *> return (fun operand1 operand2 -> EBinop (operand1, Mul, operand2))
;;

let div_delim =
  skip_whitespace
  *> Angstrom.string "/"
  *> return (fun operand1 operand2 -> EBinop (operand1, Div, operand2))
;;

let add_delim =
  skip_whitespace
  *> Angstrom.string "+"
  *> return (fun operand1 operand2 -> EBinop (operand1, Add, operand2))
;;

let sub_delim =
  skip_whitespace
  *> Angstrom.string "-"
  *> return (fun operand1 operand2 -> EBinop (operand1, Sub, operand2))
;;

let eq_delim =
  skip_whitespace
  *> Angstrom.string "="
  *> return (fun operand1 operand2 -> EBinop (operand1, Eq, operand2))
;;

let gr_delim =
  skip_whitespace
  *> Angstrom.string ">"
  *> return (fun operand1 operand2 -> EBinop (operand1, Gr, operand2))
;;

let gr_or_eq_delim =
  skip_whitespace
  *> Angstrom.string ">="
  *> return (fun operand1 operand2 -> EBinop (operand1, GrOrEq, operand2))
;;

let ls_delim =
  skip_whitespace
  *> Angstrom.string "<"
  *> return (fun operand1 operand2 -> EBinop (operand1, Ls, operand2))
;;

let ls_or_eq_delim =
  skip_whitespace
  *> Angstrom.string "<="
  *> return (fun operand1 operand2 -> EBinop (operand1, LsOrEq, operand2))
;;

let not_eq_delim =
  skip_whitespace
  *> Angstrom.string "<>"
  *> return (fun operand1 operand2 -> EBinop (operand1, NotEq, operand2))
;;

(* final pattern parsers*)
let p_cons_pattern p_pattern = chainl1 p_pattern cons_delim_pattern

let p_pattern =
  fix (fun p_pattern ->
    let atomic_pat = p_ident_pattern <|> p_const_pattern <|> between_parens p_pattern in
    let list_pat = p_list_pattern atomic_pat <|> atomic_pat in
    let w_card_pat = p_wild_card_pattern <|> list_pat in
    let cons_pat = p_cons_pattern w_card_pat <|> w_card_pat in
    let tuple_pat = p_tuple_pattern cons_pat <|> cons_pat in
    tuple_pat)
;;

(* functions parser *)
let p_function p_expr =
  let rec p_function_helper p_expr_h =
    let* pattern = p_pattern in
    let* fun_or_expr =
      p_function_helper p_expr_h <|> skip_whitespace *> Angstrom.string "->" *> p_expr_h
    in
    return @@ EFunction (pattern, fun_or_expr)
  in
  skip_whitespace *> Angstrom.string "fun" *> p_function_helper p_expr
;;

(* let in parser *)
let p_let_in p_exp =
  skip_whitespace
  *> Angstrom.string "let"
  *> let* flag =
       skip_whitespace *> Angstrom.string "rec" *> return Rec <|> return NotRec
     in
     let* pattern = skip_whitespace *> p_pattern in
     let* let_expr = skip_whitespace *> Angstrom.string "=" *> p_exp in
     let* body_expr = skip_whitespace *> Angstrom.string "in" *> p_exp in
     return (ELetIn (flag, pattern, let_expr, body_expr))
;;

(* application parser *)
let p_app p_exp =
  chainl1 p_exp (return (fun operand1 operand2 -> EApplication (operand1, operand2)))
;;

(* match parser *)
let p_match p_pattern p_expr =
  let p_match_case p_pattern p_expr =
    let* pattern =
      skip_whitespace *> Angstrom.string "|" *> skip_whitespace *> p_pattern
    in
    let* expr = skip_whitespace *> Angstrom.string "->" *> skip_whitespace *> p_expr in
    return (pattern, expr)
  in
  let* sub_pat =
    skip_whitespace *> Angstrom.string "match" *> skip_whitespace *> p_pattern
  in
  let* cases =
    skip_whitespace *> Angstrom.string "with" *> many1 (p_match_case p_pattern p_expr)
  in
  return @@ EMatch (sub_pat, cases)
;;

(* expression parser *)
let p_exp =
  fix (fun p_exp ->
    let atomic_exp =
      p_const_expr
      <|> p_ident
      <|> p_function p_exp
      <|> between_parens p_exp
      <|> p_list_expr p_exp
    in
    let cons_term = chainr1 atomic_exp cons_delim_expr <|> atomic_exp in
    let app_term = chainl1 cons_term app_delim <|> cons_term in
    let high_pr_op_term = chainl1 app_term (mul_delim <|> div_delim) in
    let low_pr_op_term = chainl1 high_pr_op_term (add_delim <|> sub_delim) in
    let eq_term = chainl1 low_pr_op_term (eq_delim <|> not_eq_delim) in
    let gr_ls_term = chainl1 eq_term (gr_or_eq_delim <|> ls_or_eq_delim) in
    let gr_ls_eq_term = chainl1 gr_ls_term (gr_delim <|> ls_delim) in
    let other_exp =
      p_tuple_expr gr_ls_eq_term
      <|> p_let_in gr_ls_eq_term
      <|> p_if_then_else gr_ls_eq_term
      <|> p_match p_pattern gr_ls_eq_term
      <|> gr_ls_eq_term
    in
    other_exp)
;;

(* let declarations parser*)
let p_let_decl p_exp =
  skip_whitespace
  *> Angstrom.string "let"
  *> skip_whitespace
  *> let* flag =
       skip_whitespace *> Angstrom.string "rec" *> return Rec <|> return NotRec
     in
     let* pattern = skip_whitespace *> p_pattern in
     let* expr = skip_whitespace *> Angstrom.string "=" *> p_exp in
     return (DLet (flag, pattern, expr))
;;

let parse p s = parse_string ~consume:All p s

(* takes in code in OCamal and returns its AST*)
let parse_program =
  parse
    (sep_by (Angstrom.string ";;" <|> Angstrom.string "\n") (p_let_decl p_exp)
     <* option "" (Angstrom.string ";;" <|> Angstrom.string "\n"))
;;

(* parser testing function *)
let test_parse input =
  match parse_program input with
  | Ok ast -> Format.printf "%a\n" Ast.pp_declarations ast
  | Error message -> Format.printf "Error: %s\n" message
;;

(* tests *)

let%expect_test _ =
  test_parse {|
let x = 5
|};
  [%expect {| [(DLet (NotRec, (PIdentifier "x"), (EConstant (CInt 5))))] |}]
;;

let%expect_test _ =
  test_parse {|
let x = true;;
let y = false
|};
  [%expect
    {|
    [(DLet (NotRec, (PIdentifier "x"), (EConstant (CBool true))));
      (DLet (NotRec, (PIdentifier "y"), (EConstant (CBool false))))] |}]
;;

let%expect_test _ =
  test_parse {|
let x = (5 + 5) * 6 + (5 + 5) / 2
|};
  [%expect
    {|
    [(DLet (NotRec, (PIdentifier "x"),
        (EBinop (
           (EBinop ((EBinop ((EConstant (CInt 5)), Add, (EConstant (CInt 5)))),
              Mul, (EConstant (CInt 6)))),
           Add,
           (EBinop ((EBinop ((EConstant (CInt 5)), Add, (EConstant (CInt 5)))),
              Div, (EConstant (CInt 2))))
           ))
        ))
      ] |}]
;;

let%expect_test _ =
  test_parse {|
let x = if 5 > 4 then true else false
|};
  [%expect
    {|
    [(DLet (NotRec, (PIdentifier "x"),
        (EIfThenElse ((EBinop ((EConstant (CInt 5)), Gr, (EConstant (CInt 4)))),
           (EConstant (CBool true)), (EConstant (CBool false))))
        ))
      ] |}]
;;

let%expect_test _ =
  test_parse {|
let succ = fun n -> n + 1
|};
  [%expect
    {|
    [(DLet (NotRec, (PIdentifier "succ"),
        (EFunction ((PIdentifier "n"),
           (EBinop ((EIdentifier "n"), Add, (EConstant (CInt 1))))))
        ))
      ] |}]
;;

let%expect_test _ =
  test_parse {|
let x = let y = 5 in y
|};
  [%expect
    {|
    [(DLet (NotRec, (PIdentifier "x"),
        (ELetIn (NotRec, (PIdentifier "y"), (EConstant (CInt 5)),
           (EIdentifier "y")))
        ))
      ] |}]
;;

let%expect_test _ =
  test_parse {|
let (x, y) = (1, 2)
|};
  [%expect
    {|
    [(DLet (NotRec, (PTuple [(PIdentifier "x"); (PIdentifier "y")]),
        (ETuple [(EConstant (CInt 1)); (EConstant (CInt 2))])))
      ] |}]
;;

let%expect_test _ =
  test_parse {|
let x :: y = [1; 2]
|};
  [%expect
    {|
    [(DLet (NotRec, (PCons ((PIdentifier "x"), (PIdentifier "y"))),
        (EList [(EConstant (CInt 1)); (EConstant (CInt 2))])))
      ] |}]
;;

let%expect_test _ =
  test_parse {|
let rec fac = fun n -> if n = 0 then 1 else n * (fac (n - 1))
|};
  [%expect
    {|
    [(DLet (Rec, (PIdentifier "fac"),
        (EFunction ((PIdentifier "n"),
           (EIfThenElse ((EBinop ((EIdentifier "n"), Eq, (EConstant (CInt 0)))),
              (EConstant (CInt 1)),
              (EBinop ((EIdentifier "n"), Mul,
                 (EApplication ((EIdentifier "fac"),
                    (EBinop ((EIdentifier "n"), Sub, (EConstant (CInt 1))))))
                 ))
              ))
           ))
        ))
      ] |}]
;;

let%expect_test _ =
  test_parse
    {|
let rec facCPS = fun n k -> if n = 0 then k 1 else facCPS (n - 1) (fun t -> k (n * t))|};
  [%expect
    {|
    [(DLet (Rec, (PIdentifier "facCPS"),
        (EFunction ((PIdentifier "n"),
           (EFunction ((PIdentifier "k"),
              (EIfThenElse (
                 (EBinop ((EIdentifier "n"), Eq, (EConstant (CInt 0)))),
                 (EApplication ((EIdentifier "k"), (EConstant (CInt 1)))),
                 (EApplication (
                    (EApplication ((EIdentifier "facCPS"),
                       (EBinop ((EIdentifier "n"), Sub, (EConstant (CInt 1)))))),
                    (EFunction ((PIdentifier "t"),
                       (EApplication ((EIdentifier "k"),
                          (EBinop ((EIdentifier "n"), Mul, (EIdentifier "t")))))
                       ))
                    ))
                 ))
              ))
           ))
        ))
      ] |}]
;;

let%expect_test _ =
  test_parse
    {|
let rec map = fun f list -> match list with
  | [] -> list
  | h :: tl -> map f ((f h) :: tl)
|};
  [%expect
    {|
    [(DLet (Rec, (PIdentifier "map"),
        (EFunction ((PIdentifier "f"),
           (EFunction ((PIdentifier "list"),
              (EMatch ((PIdentifier "list"),
                 [(PNil, (EIdentifier "list"));
                   ((PCons ((PIdentifier "h"), (PIdentifier "tl"))),
                    (EApplication (
                       (EApplication ((EIdentifier "map"), (EIdentifier "f"))),
                       (EBinop (
                          (EApplication ((EIdentifier "f"), (EIdentifier "h"))),
                          Cons, (EIdentifier "tl")))
                       )))
                   ]
                 ))
              ))
           ))
        ))
      ] |}]
;;

let%expect_test _ =
  test_parse
    {|
let rec facCPS = fun n k -> match n with
  | 0 -> k 1
  | n -> facCPS (n - 1) (fun t -> k (n * t))
|};
  [%expect
    {|
    [(DLet (Rec, (PIdentifier "facCPS"),
        (EFunction ((PIdentifier "n"),
           (EFunction ((PIdentifier "k"),
              (EMatch ((PIdentifier "n"),
                 [((PConstant (CInt 0)),
                   (EApplication ((EIdentifier "k"), (EConstant (CInt 1)))));
                   ((PIdentifier "n"),
                    (EApplication (
                       (EApplication ((EIdentifier "facCPS"),
                          (EBinop ((EIdentifier "n"), Sub, (EConstant (CInt 1))))
                          )),
                       (EFunction ((PIdentifier "t"),
                          (EApplication ((EIdentifier "k"),
                             (EBinop ((EIdentifier "n"), Mul, (EIdentifier "t")))
                             ))
                          ))
                       )))
                   ]
                 ))
              ))
           ))
        ))
      ] |}]
;;

let%expect_test _ =
  test_parse
    {|
let fibo = fun n -> let rec fiboCPS = fun n acc -> match n with
  | 0 -> acc 0
  | 1 -> acc 1
  | _ -> fiboCPS (n - 1) (fun x -> fiboCPS (n - 2) (fun y -> acc (x + y)))
    in fiboCPS n (fun x -> x)
|};
  [%expect
    {|
    [(DLet (NotRec, (PIdentifier "fibo"),
        (EFunction ((PIdentifier "n"),
           (ELetIn (Rec, (PIdentifier "fiboCPS"),
              (EFunction ((PIdentifier "n"),
                 (EFunction ((PIdentifier "acc"),
                    (EMatch ((PIdentifier "n"),
                       [((PConstant (CInt 0)),
                         (EApplication ((EIdentifier "acc"), (EConstant (CInt 0))
                            )));
                         ((PConstant (CInt 1)),
                          (EApplication ((EIdentifier "acc"),
                             (EConstant (CInt 1)))));
                         (PWildCard,
                          (EApplication (
                             (EApplication ((EIdentifier "fiboCPS"),
                                (EBinop ((EIdentifier "n"), Sub,
                                   (EConstant (CInt 1))))
                                )),
                             (EFunction ((PIdentifier "x"),
                                (EApplication (
                                   (EApplication ((EIdentifier "fiboCPS"),
                                      (EBinop ((EIdentifier "n"), Sub,
                                         (EConstant (CInt 2))))
                                      )),
                                   (EFunction ((PIdentifier "y"),
                                      (EApplication ((EIdentifier "acc"),
                                         (EBinop ((EIdentifier "x"), Add,
                                            (EIdentifier "y")))
                                         ))
                                      ))
                                   ))
                                ))
                             )))
                         ]
                       ))
                    ))
                 )),
              (EApplication (
                 (EApplication ((EIdentifier "fiboCPS"), (EIdentifier "n"))),
                 (EFunction ((PIdentifier "x"), (EIdentifier "x")))))
              ))
           ))
        ))
      ] |}]
;;
