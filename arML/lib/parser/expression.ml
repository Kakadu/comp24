(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Common
open Pattern
open Type

(* Constant expression parsers *)

let parse_constant_expr =
  let* constant = parse_constant in
  return @@ EConstant constant

(* ---------------- *)

(* Identifiers expression parsers *)

let parse_identifier_expr =
  let* identifier = parse_identifier <|> parse_operator in
  return @@ EIdentifier identifier

(* ---------------- *)

(* Empty list parsers *)

let parse_empty_list_expr =
  let* _ = skip_wspace *> brackets skip_wspace in
  return EEmptyList

(* ---------------- *)

(* Function parsers *)

let parse_fun p =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr = choice 
      [ p.parse_type_defition p
      ; p.parse_list_constructor p
      ; p.parse_binary_operation p
      ; p.parse_application p
      ; p.parse_tuple p
      ; p.parse_list p
      ; p.parse_constant_expr
      ; p.parse_identifier_expr
      ; p.parse_let_in p
      ; p.parse_if_then_else p
      ; p.parse_match_with p
      ; p.parse_function p
      ; self
      ; p.parse_empty_list_expr
      ]
  in

  parens self
  <|>
  let* args = string "fun" *> skip_wspace1 *> sep_by1 skip_wspace (parse_pattern true) in

  let* args_tuple = match args with
    | hd :: tl -> return (hd, tl)
    | [] -> fail "Syntax error: function must have at least one argument."
  in

  let* typ_opt = 

    let typ_parser = 
      skip_wspace *> char ':' *> skip_wspace *>
      let* typ = parse_type in
      return @@ Some typ
    in

    option None typ_parser
  in

  let* expr = skip_wspace *> string "->" *> skip_wspace *> parse_expr in

  let* expr =
    match typ_opt with
    | Some typ -> return @@ ETyped (expr, typ)
    | _ -> return expr
  in

  return @@ EFun (args_tuple, expr)
;;

let parse_function p =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr = choice
      [ p.parse_type_defition p
      ; self
      ; p.parse_list_constructor p
      ; p.parse_binary_operation p
      ; p.parse_application p
      ; p.parse_tuple p
      ; p.parse_list p
      ; p.parse_fun p
      ; p.parse_match_with p
      ; p.parse_if_then_else p
      ; p.parse_let_in p
      ; p.parse_identifier_expr
      ; p.parse_constant_expr
      ; p.parse_empty_list_expr
      ]
  in
  let* _ = string "function" *> skip_wspace1 in

  let parse_case =
    let* pattern = (parse_pattern false) <* skip_wspace in
    let* case_expr = string "->" *> skip_wspace *> parse_expr in
    return (pattern, case_expr)
  in

  let* first_case = (char '|' *> skip_wspace *> parse_case) <|> parse_case in

  let* other_cases = many (skip_wspace *> char '|' *> skip_wspace *> parse_case) in

  return @@ EFunction (first_case, other_cases)
;;

(* ---------------- *)

(* Tuple parsers *)

let parse_tuple p =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ p.parse_type_defition p
      ; p.parse_list_constructor p
      ; p.parse_binary_operation p
      ; p.parse_application p
      ; p.parse_list p
      ; p.parse_fun p
      ; p.parse_function p
      ; p.parse_let_in p
      ; p.parse_match_with p
      ; p.parse_if_then_else p
      ; parse_constant_expr
      ; parse_identifier_expr
      ; parens @@ self
      ; p.parse_empty_list_expr
      ]
  in

  let main_parser = (sep_by (skip_wspace *> char ',' <* skip_wspace) parse_expr) in

  let* elements = main_parser <|> parens main_parser in

  match elements with
  | pat_1 :: pat_2 :: pats -> return (ETuple (pat_1, pat_2, pats))
  | _ -> fail "Syntax error: tuple must have at least two elements"
;;

(* ---------------- *)

(* List parsers *)

let parse_list p =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr = 
    choice
      [ p.parse_type_defition p
      ; p.parse_list_constructor p
      ; self
      ; p.parse_binary_operation p
      ; p.parse_application p
      ; p.parse_tuple p
      ; p.parse_fun p
      ; p.parse_function p
      ; p.parse_match_with p
      ; p.parse_if_then_else p
      ; p.parse_let_in p
      ; p.parse_identifier_expr
      ; p.parse_constant_expr
      ; p.parse_empty_list_expr
      ]
  in
  let* elements = brackets (sep_by (skip_wspace *> char ';' <* skip_wspace) parse_expr) in
  let rec construct_list = function
    | [] -> return EEmptyList
    | hd :: tl ->
      let* tail = construct_list tl in
      return @@ EListConstructor (hd, tail)
  in
  construct_list elements
;;

let parse_list_constructor p =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ p.parse_type_defition p
      ; p.parse_binary_operation p
      ; p.parse_application p
      ; parens @@ p.parse_tuple p
      ; p.parse_fun p
      ; p.parse_function p
      ; p.parse_match_with p
      ; p.parse_if_then_else p
      ; p.parse_let_in p
      ; p.parse_identifier_expr
      ; p.parse_constant_expr
      ; p.parse_empty_list_expr
      ; parens self
      ]
  in

  let parse_cons_chain =
    let* elements = sep_by1 (skip_wspace *> string "::" *> skip_wspace) parse_expr in
    let rec build_list = function
      | [] -> fail "Empty list constructor chain"
      | [last] -> return last
      | head :: tail ->
        let* rest = build_list tail in
        return @@ EListConstructor (head, rest)
    in
    build_list elements
  in

  parse_cons_chain
;;

(* ---------------- *)

(* If then else parser *)

let parse_if_then_else p =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ p.parse_type_defition p
      ; p.parse_list_constructor p
      ; p.parse_binary_operation p
      ; p.parse_application p
      ; p.parse_tuple p
      ; p.parse_list p
      ; p.parse_fun p
      ; p.parse_let_in p
      ; self
      ; p.parse_constant_expr
      ; p.parse_identifier_expr
      ; p.parse_function p
      ; p.parse_match_with p
      ; p.parse_empty_list_expr
      ]
  in

  parens self
  <|> 
  let* cond = string "if" *> parse_expr in

  let* then_branch = skip_wspace *> string "then" *> parse_expr in

  let opt p = option None (p >>| Option.some) in

  let* else_branch = opt (skip_wspace *> string "else") 
    >>= function
    | Some _ -> skip_wspace *> parse_expr >>| Option.some
    | None -> return None
  in

  return @@ EIfThenElse (cond, then_branch, else_branch)
;;

(* ---------------- *)

(* Let ... in ... parser *)

let parse_let_in p =
  fix
  @@ fun self ->
  skip_wspace
  *>
  parens self
  <|>
  let parse_expr =
    choice 
      [ p.parse_type_defition p
      ; p.parse_list_constructor p
      ; p.parse_tuple p
      ; p.parse_binary_operation p
      ; p.parse_application p
      ; p.parse_list p
      ; p.parse_fun p
      ; p.parse_function p
      ; p.parse_if_then_else p
      ; p.parse_constant_expr
      ; p.parse_identifier_expr
      ; self
      ; p.parse_match_with p
      ; p.parse_empty_list_expr
      ]
  in

  let* decl =
    skip_wspace *> string "let" *> skip_wspace1 *>
    option "" (string "rec" <* skip_wspace1)
  in

  let parse_binding () =
    let* args = many1 (parse_pattern false) in
    let* typ_opt = 
      let typ_parser = 
        skip_wspace *> char ':' *> skip_wspace *>
        let* typ = parse_type in
        return @@ Some typ
      in
      option None typ_parser
    in
    let tying = skip_wspace *> string "=" in
    let main_pattern = List.hd args in
    let* binding_expr = tying *> skip_wspace *> parse_expr in
    let* binding_expr =
      match args with
      | _ :: md :: tl -> EFun ((md, tl), binding_expr) |> return
      | _ -> return binding_expr
    in
    let* binding_expr =
      match typ_opt with
      | Some typ -> return @@ ETyped (binding_expr, typ)
      | _ -> return binding_expr
    in
    return (main_pattern, binding_expr)
  in

  let* main_binding = parse_binding () in
  let* and_bindings = 
    many (skip_wspace *> string "and" *> skip_wspace1 *> parse_binding ())
  in

  let* in_expr = skip_wspace *> string "in" *> skip_wspace *> parse_expr in

  let let_binding main_binding and_bindings expr =
    match decl with
    | "rec" -> ERecLetIn (main_binding, and_bindings, expr)
    | _ -> ELetIn (main_binding, and_bindings, expr)
  in

  return (let_binding main_binding and_bindings in_expr)
;;

(* ---------------- *)

(* Match with parser *)

let parse_match_with p =
  fix
  @@ fun self ->
  skip_wspace
  *>
  parens self
  <|>
  let parse_expr =
    choice
      [ p.parse_type_defition p
      ; p.parse_list_constructor p
      ; p.parse_binary_operation p
      ; self
      ; p.parse_application p
      ; p.parse_tuple p
      ; p.parse_list p
      ; p.parse_fun p
      ; p.parse_function p
      ; p.parse_let_in p
      ; p.parse_if_then_else p
      ; p.parse_constant_expr
      ; p.parse_identifier_expr
      ; p.parse_empty_list_expr
      ]
  in

  let* expr = string "match" *> skip_wspace *> parse_expr <* skip_wspace <* string "with" <* skip_wspace in

  let parse_case =
    let* pattern = skip_wspace *> (parse_pattern false) <* skip_wspace in
    let* case_expr = string "->" *> skip_wspace *> parse_expr <* skip_wspace in
    return (pattern, case_expr)
  in

  let* first_case = (char '|' *> skip_wspace *> parse_case) <|> parse_case in

  let* other_cases =
    many (char '|' *> skip_wspace *> parse_case)
  in

  return (EMatchWith (expr, first_case, other_cases))
;;

(* ---------------- *)

(* Application parser *)

let parse_application p =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ p.parse_type_defition p
      ; parens @@ p.parse_fun p
      ; parens @@ p.parse_list_constructor p
      ; parens @@ p.parse_binary_operation p
      ; p.parse_list p
      ; p.parse_function p
      ; p.parse_let_in p
      ; p.parse_if_then_else p
      ; p.parse_constant_expr
      ; p.parse_identifier_expr
      ; parens @@ p.parse_tuple p
      ; p.parse_empty_list_expr
      ; p.parse_match_with p
      ; parens @@ self
      ]
  in
  parens self <|>
  let* func = parse_expr in
  let* args = many1 (skip_wspace *> parse_expr) in
  return @@ EApplication (func, List.hd args, List.tl args)
;;

(* ---------------- *)

(* Binary operation parser *)

let parse_binary_operation p =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ p.parse_type_defition p
      ; p.parse_application p
      ; parens self
      ; p.parse_list p
      ; p.parse_fun p
      ; p.parse_function p
      ; p.parse_let_in p
      ; p.parse_match_with p
      ; p.parse_if_then_else p
      ; p.parse_constant_expr
      ; p.parse_identifier_expr
      ; parens @@ p.parse_tuple p
      ]
  in

  let addition = skip_wspace *> char '+' *> (return @@ EIdentifier (Id "( + )")) in
  let subtraction = skip_wspace *> char '-' *> (return @@ EIdentifier (Id "( - )")) in
  let multiplication = skip_wspace *> char '*' *> (return  @@ EIdentifier (Id "( * )")) in
  let division = skip_wspace *> char '/' *> (return @@ EIdentifier (Id "( / )")) in
  let mod_div = skip_wspace *> char '%' *> (return @@ EIdentifier (Id "( % )")) in
  let eqality = skip_wspace *> char '=' *> (return @@ EIdentifier (Id "( = )")) in
  let neqality1 = skip_wspace *> string "<>" *> (return @@ EIdentifier (Id "( <> )")) in
  let neqality2 = skip_wspace *> string "!=" *> (return @@ EIdentifier (Id "( != )")) in
  let logand = skip_wspace *> string "&&" *> (return @@ EIdentifier (Id "( && )")) in
  let logor = skip_wspace *> string "||" *> (return @@ EIdentifier (Id "( || )")) in
  let larger = skip_wspace *> char '>'*> (return @@ EIdentifier (Id "( > )")) in
  let largerEq = skip_wspace *> string ">=" *> (return @@ EIdentifier (Id "( >= )")) in
  let less = skip_wspace *> char '<' *> (return @@ EIdentifier (Id "( < )")) in
  let lessEq = skip_wspace *> string "<=" *> (return @@ EIdentifier (Id "( <= )")) in

  let binary_operations =
    [ multiplication
    ; division
    ; mod_div
    ; addition
    ; subtraction
    ; larger
    ; largerEq
    ; less
    ; lessEq
    ; eqality
    ; neqality1
    ; neqality2
    ; logand
    ; logor
    ]
  in

  let application_constructor op left right = EApplication (op, left, [right]) in

  let chainl1 e op =
    let rec go acc = lift2 (fun f x -> application_constructor f acc x) op e >>= go <|> return acc in
    let* init = e in
    go init
  in

  List.fold_left
    (fun acc x -> chainl1 acc x)
    (chainl1 parse_expr multiplication)
    binary_operations
;;

(* ---------------- *)

(* Type defition parsers *)

let parse_type_defition p =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice 
      [ p.parse_tuple p
      ; parens @@ p.parse_list_constructor p
      ; p.parse_binary_operation p
      ; p.parse_list p
      ; p.parse_if_then_else p
      ; p.parse_application p
      ; p.parse_let_in p
      ; p.parse_match_with p
      ; p.parse_fun p
      ; p.parse_function p
      ; p.parse_identifier_expr
      ; p.parse_constant_expr
      ; p.parse_empty_list_expr
      ; parens @@ self
      ]
  in
  parens @@
  let* expr = parse_expr in
  let* typ = skip_wspace *> char ':' *> parse_type in
  return @@ ETyped (expr, typ)

(* ---------------- *)

(* Main expression parser *)

let parsers = 
  { parse_type_defition
  ; parse_constant_expr
  ; parse_identifier_expr
  ; parse_fun
  ; parse_application
  ; parse_binary_operation
  ; parse_if_then_else
  ; parse_let_in
  ; parse_match_with
  ; parse_function
  ; parse_tuple
  ; parse_list
  ; parse_list_constructor
  ; parse_empty_list_expr
  }
;;

let parse_expression = 
  skip_wspace *> 
  choice 
    [ parsers.parse_tuple parsers
    ; parsers.parse_type_defition parsers
    ; parsers.parse_list_constructor parsers
    ; parsers.parse_binary_operation parsers
    ; parsers.parse_list parsers
    ; parsers.parse_application parsers
    ; parsers.parse_constant_expr
    ; parsers.parse_identifier_expr
    ; parsers.parse_fun parsers
    ; parsers.parse_function parsers
    ; parsers.parse_if_then_else parsers
    ; parsers.parse_let_in parsers
    ; parsers.parse_match_with parsers
    ; parsers.parse_empty_list_expr
    ] <* skip_wspace
;;

(* ---------------- *)