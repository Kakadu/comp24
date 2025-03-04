(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open AbstractSyntaxTree

let pp_identifier ppf (Id id) = Format.fprintf ppf "%s" id

let pp_constant ppf = function
  | CInt i -> Format.fprintf ppf "%d" i
  | CBool b -> Format.fprintf ppf "%b" b
  | CChar c -> Format.fprintf ppf "'%s'" (Char.escaped c)
  | CString s -> Format.fprintf ppf "\"%s\"" (String.escaped s)
  | CUnit -> Format.fprintf ppf "()"
;;

let pp_ground_type_definition ppf = function
  | GTDInt -> Format.fprintf ppf "int"
  | GTDBool -> Format.fprintf ppf "bool"
  | GTDUnit -> Format.fprintf ppf "unit"
  | GTDChar -> Format.fprintf ppf "char"
  | GTDString -> Format.fprintf ppf "string"
;;

let rec pp_type_definition ppf = function
  | TDPolymorphic id -> Format.fprintf ppf "'%a" pp_identifier id
  | TDGround gtd -> pp_ground_type_definition ppf gtd
  | TDArrow (t1, t2) ->
    Format.fprintf ppf "(%a -> %a)" pp_type_definition t1 pp_type_definition t2
  | TDTuple (t1, t2, ts) ->
    Format.fprintf
      ppf
      "(%a)"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " * ")
         pp_type_definition)
      (t1 :: t2 :: ts)
  | TDList t -> Format.fprintf ppf "(%a list)" pp_type_definition t
;;

let rec pp_pattern ppf = function
  | PAny -> Format.fprintf ppf "_"
  | PNill -> Format.fprintf ppf "[]"
  | PConst c -> pp_constant ppf c
  | PVar id -> pp_identifier ppf id
  | PTuple (p1, p2, ps) ->
    Format.fprintf
      ppf
      "(%a)"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_pattern)
      (p1 :: p2 :: ps)
  | PListConstructor (p1, p2) ->
    Format.fprintf ppf "(%a :: %a)" pp_pattern p1 pp_pattern p2
  | PTyped (p, t) -> Format.fprintf ppf "(%a : %a)" pp_pattern p pp_type_definition t
;;

let rec pp_expression ppf = function
  | EConstant c -> pp_constant ppf c
  | EIdentifier id -> pp_identifier ppf id
  | EFun ((p, ps), e) ->
    Format.fprintf
      ppf
      "(fun %a -> %a)"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_pattern)
      (p :: ps)
      pp_expression
      e
  | EFunction (c, cs) ->
    Format.fprintf
      ppf
      "(function %a)"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " | ")
         pp_match_case)
      (c :: cs)
  | EApplication (e1, e2, es) ->
    Format.fprintf
      ppf
      "(%a)"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_expression)
      (e1 :: e2 :: es)
  | EIfThenElse (cond, t, f) ->
    Format.fprintf
      ppf
      "(if %a then %a%a)"
      pp_expression
      cond
      pp_expression
      t
      (fun ppf -> function
        | None -> ()
        | Some e -> Format.fprintf ppf " else %a" pp_expression e)
      f
  | ETuple (e1, e2, es) ->
    Format.fprintf
      ppf
      "(%a)"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_expression)
      (e1 :: e2 :: es)
  | EListConstructor (e1, e2) ->
    Format.fprintf ppf "(%a :: %a)" pp_expression e1 pp_expression e2
  | EEmptyList -> Format.fprintf ppf "[]"
  | EMatchWith (e, c, cs) ->
    Format.fprintf
      ppf
      "(match %a with %a)"
      pp_expression
      e
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " | ")
         pp_match_case)
      (c :: cs)
  | ELetIn (c, cs, e) ->
    Format.fprintf
      ppf
      "(let %a in %a)"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " and ")
         pp_decl_case)
      (c :: cs)
      pp_expression
      e
  | ERecLetIn (c, cs, e) ->
    Format.fprintf
      ppf
      "(let rec %a in %a)"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " and ")
         pp_decl_case)
      (c :: cs)
      pp_expression
      e
  | ETyped (e, t) -> Format.fprintf ppf "(%a : %a)" pp_expression e pp_type_definition t

and pp_match_case ppf (p, e) = Format.fprintf ppf "%a -> %a" pp_pattern p pp_expression e

and pp_decl_case ppf (p, e) =
  let rec collect_args acc = function
    | EFun ((p, ps), body) -> collect_args (acc @ (p :: ps)) body
    | body -> acc, body
  in
  match e with
  | EFun _ ->
    let args, body = collect_args [] e in
    Format.fprintf
      ppf
      "%a %a = %a"
      pp_pattern
      p
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_pattern)
      args
      pp_expression
      body
  | _ -> Format.fprintf ppf "%a = %a" pp_pattern p pp_expression e
;;

let pp_declaration ppf = function
  | DOrdinary (c, cs) ->
    Format.fprintf
      ppf
      "let %a"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " and ")
         pp_decl_case)
      (c :: cs)
  | DRecursive (c, cs) ->
    Format.fprintf
      ppf
      "let rec %a"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " and ")
         pp_decl_case)
      (c :: cs)
;;

let pp_program ppf p =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n")
    pp_declaration
    ppf
    p
;;

let print_expression expr = Format.printf "%s\n" (Format.asprintf "%a" pp_expression expr)
let print_program program = Format.printf "%s\n" (Format.asprintf "%a" pp_program program)
