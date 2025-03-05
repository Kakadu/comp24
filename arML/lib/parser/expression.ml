(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast.AbstractSyntaxTree
open Common

(* Constant expression parsers *)

let parse_constant_expr =
  let* constant = parse_constant in
  return @@ EConstant constant
;;

(* ---------------- *)

(* Identifiers expression parsers *)

let parse_identifier_expr =
  let* identifier = parse_identifier <|> parse_operator in
  return @@ EIdentifier identifier
;;

(* ---------------- *)

(* Empty list parsers *)

let parse_empty_list_expr =
  let* _ = skip_wspace *> brackets skip_wspace in
  return EEmptyList
;;

(* ---------------- *)

(* Function parsers *)

let parse_fun_expr pe =
  let* args = string "fun" *> skip_wspace1 *> sep_by1 skip_wspace Pattern.parse_pattern in
  let* args_tuple =
    match args with
    | hd :: tl -> return (hd, tl)
    | [] -> fail "Syntax error: function must have at least one argument."
  in
  let* typ_opt =
    let typ_parser =
      skip_wspace
      *> char ':'
      *> skip_wspace
      *>
      let* typ = Type.parse_type in
      return @@ Some typ
    in
    option None typ_parser
  in
  let* expr = skip_wspace *> string "->" *> skip_wspace *> pe in
  let* expr =
    match typ_opt with
    | Some typ -> return @@ ETyped (expr, typ)
    | _ -> return expr
  in
  return @@ EFun (args_tuple, expr)
;;

let parse_function_expr pe =
  let* _ = string "function" *> skip_wspace1 in
  let parse_case =
    let* pattern = Pattern.parse_pattern <* skip_wspace in
    (* !!! *)
    let* case_expr = string "->" *> skip_wspace *> pe in
    return (pattern, case_expr)
  in
  let* first_case = char '|' *> skip_wspace *> parse_case <|> parse_case in
  let* other_cases = many (skip_wspace *> char '|' *> skip_wspace *> parse_case) in
  return @@ EFunction (first_case, other_cases)
;;

(* ---------------- *)

(* If then else parser *)

let parse_if_then_else_expr pe =
  let* cond = string "if" *> pe in
  let* then_branch = skip_wspace *> string "then" *> pe in
  let* else_branch =
    let opt p = option None (p >>| Option.some) in
    let p_else = skip_wspace *> string "else" in
    let* option_else_branch = opt p_else in
    match option_else_branch with
    | Some _ -> skip_wspace *> pe >>| Option.some
    | None -> return None
  in
  return @@ EIfThenElse (cond, then_branch, else_branch)
;;

(* ---------------- *)

(* Application parser *)

let parse_application_expr pe =
  let* func = pe in
  let* args = many1 (skip_wspace *> pe) in
  return @@ EApplication (func, List.hd args, List.tl args)
;;

(* ---------------- *)

(* List parsers *)

let parse_list_expr pe =
  let main_parser = brackets @@ sep_by (skip_wspace *> char ';' <* skip_wspace) pe in
  let rec constructor_helper = function
    | [] -> EEmptyList
    | hd :: tl -> EListConstructor (hd, constructor_helper tl)
  in
  let* patterns = main_parser in
  return @@ constructor_helper patterns
;;

let parse_list_constructor_expr pe =
  let constructor hd tl = EListConstructor (hd, tl) in
  let parse_constructor =
    skip_wspace *> string "::" *> skip_wspace *> return constructor
  in
  chainr1 pe parse_constructor
;;

(* ---------------- *)

(* Tuple pattern parser *)

let parse_tuple_expr pe =
  parens
  @@
  let* patterns = sep_by (skip_wspace *> char ',' <* skip_wspace) pe in
  match patterns with
  | p1 :: p2 :: ps -> return (ETuple (p1, p2, ps))
  | _ -> fail "Syntax error: tuple must have at least two patterns"
;;

(* ---------------- *)

(* Let ... in ... parser *)

let parse_let_in_expr pe =
  let* decl =
    skip_wspace *> string "let" *> skip_wspace1 *> option "" (string "rec" <* skip_wspace1)
  in
  let parse_binding () =
    let* args = many1 Pattern.parse_pattern in
    let* typ_opt =
      let typ_parser =
        skip_wspace
        *> char ':'
        *> skip_wspace
        *>
        let* typ = Type.parse_type in
        return @@ Some typ
      in
      option None typ_parser
    in
    let tying = skip_wspace *> string "=" in
    let main_pattern = List.hd args in
    let* binding_expr = tying *> skip_wspace *> pe in
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
  let* in_expr = skip_wspace *> string "in" *> skip_wspace *> pe in
  let let_binding main_binding and_bindings expr =
    match decl with
    | "rec" -> ERecLetIn (main_binding, and_bindings, expr)
    | _ -> ELetIn (main_binding, and_bindings, expr)
  in
  return (let_binding main_binding and_bindings in_expr)
;;

(* ---------------- *)

(* Match with parser *)

let parse_match_with_expr pe =
  let* expr =
    string "match" *> skip_wspace *> pe <* skip_wspace <* string "with" <* skip_wspace
  in
  let parse_case =
    let* pattern = skip_wspace *> Pattern.parse_pattern <* skip_wspace in
    let* case_expr = string "->" *> skip_wspace *> pe <* skip_wspace in
    return (pattern, case_expr)
  in
  let* first_case = char '|' *> skip_wspace *> parse_case <|> parse_case in
  let* other_cases = many (char '|' *> skip_wspace *> parse_case) in
  return (EMatchWith (expr, first_case, other_cases))
;;

(* ---------------- *)

(* Binary operation parser *)

let parse_binary_operation_expr pe =
  let addition = skip_wspace *> char '+' *> (return @@ EIdentifier (Id "( + )")) in
  let subtraction = skip_wspace *> char '-' *> (return @@ EIdentifier (Id "( - )")) in
  let multiplication = skip_wspace *> char '*' *> (return @@ EIdentifier (Id "( * )")) in
  let division = skip_wspace *> char '/' *> (return @@ EIdentifier (Id "( / )")) in
  let mod_div = skip_wspace *> char '%' *> (return @@ EIdentifier (Id "( % )")) in
  let eqality1 = skip_wspace *> char '=' *> (return @@ EIdentifier (Id "( = )")) in
  let eqality2 = skip_wspace *> string "==" *> (return @@ EIdentifier (Id "( == )")) in
  let neqality1 = skip_wspace *> string "<>" *> (return @@ EIdentifier (Id "( <> )")) in
  let neqality2 = skip_wspace *> string "!=" *> (return @@ EIdentifier (Id "( != )")) in
  let logand = skip_wspace *> string "&&" *> (return @@ EIdentifier (Id "( && )")) in
  let logor = skip_wspace *> string "||" *> (return @@ EIdentifier (Id "( || )")) in
  let larger = skip_wspace *> char '>' *> (return @@ EIdentifier (Id "( > )")) in
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
    ; eqality2
    ; eqality1
    ; neqality1
    ; neqality2
    ; logand
    ; logor
    ]
  in
  let application_constructor op left right = EApplication (op, left, [ right ]) in
  let chainl1 e op =
    let rec go acc =
      lift2 (fun f x -> application_constructor f acc x) op e >>= go <|> return acc
    in
    let* init = e in
    go init
  in
  List.fold_left
    (fun acc x -> chainl1 acc x)
    (chainl1 pe multiplication)
    binary_operations
;;

(* ---------------- *)

(* Unary operation parser *)

let parse_unary_operation pe =
  let unary_plus = skip_wspace *> char '+' *> (return @@ EIdentifier (Id "U+")) in
  let unary_minus = skip_wspace *> char '-' *> (return @@ EIdentifier (Id "U-")) in
  let unary_not = skip_wspace *> string "not" *> (return @@ EIdentifier (Id "UNot")) in
  let unary_operations = choice [ unary_plus; unary_minus; unary_not ] in
  let application_constructor op expr = EApplication (op, expr, []) in
  let unary_chain =
    let* op = unary_operations in
    let* expr = pe in
    return @@ application_constructor op expr
  in
  unary_chain <|> parens unary_chain
;;

(* ---------------- *)

(* Type defition parsers *)

let parse_type_defition pe =
  parens
  @@
  let* expr = pe in
  let* typ = skip_wspace *> char ':' *> Type.parse_type in
  return @@ ETyped (expr, typ)
;;

(* ---------------- *)

(* Main expression parser *)

let parse_expression =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let basic_parsers =
    choice
      [ parens self
      ; parse_constant_expr
      ; parse_identifier_expr
      ; parse_empty_list_expr
      ; parse_fun_expr self
      ; parse_function_expr self
      ; parse_if_then_else_expr self
      ; parse_let_in_expr self
      ; parse_match_with_expr self
      ; parse_tuple_expr self
      ; parse_list_expr self
      ; parse_type_defition self
      ]
  in
  let chain_parsers = parse_application_expr basic_parsers <|> basic_parsers in
  let chain_parsers = parse_list_constructor_expr chain_parsers <|> chain_parsers in
  let chain_parsers = parse_binary_operation_expr chain_parsers <|> chain_parsers in
  let chain_parsers = parse_unary_operation chain_parsers <|> chain_parsers in
  chain_parsers
;;

(* ---------------- *)
