(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Ast

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_lowercase = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_letter x = is_lowercase x || is_uppercase x

(* Not all keywords are forbidden *)
let is_keyword = function
  | "let"
  | "rec"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "match"
  | "with"
  | "in"
  | "fun"
  | "type"
  | "int"
  | "string"
  | "bool" -> true
  | _ -> false
;;

let is_operator_char = function
  | '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | '%' | '&' | '|' | '^' -> true
  | _ -> false
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let pspaces = skip_while is_space
let ptoken p = skip_while is_space *> p
let pstoken s = ptoken (Angstrom.string s)
let pparens p = pstoken "(" *> take_while is_space *> p <* pstoken ")"
let poperator = pparens (take_while1 is_operator_char) >>= return

let pid =
  let pfirst = satisfy (fun ch -> is_letter ch || Char.equal ch '_') >>| Char.escaped in
  let plast = take_while (fun ch -> is_letter ch || is_digit ch || Char.equal ch '_') in
  ptoken (lift2 ( ^ ) pfirst plast)
  >>= fun s ->
  if is_keyword s
  then fail ("Keyword identifiers are forbidden: " ^ s)
  else return s <?> "identifier"
;;

let pint = ptoken (take_while1 is_digit >>| fun x -> CInt (Int.of_string x)) <?> "integer"

let pbool =
  ptoken
  @@ choice
       [ pstoken "true" *> return (CBool true)
       ; pstoken "false" *> return (CBool false)
       ; fail "Expected 'true' or 'false'"
       ]
  <?> "boolean"
;;

let punit = pstoken "()" *> return CUnit <?> "unit"
let pconst = choice [ pint; pbool; punit ] >>| fun x -> EConst x
let pvar = pid >>| fun e -> EVar e
let poperatorvar = poperator >>| fun op -> EVar op

let ptype =
  fix (fun ptype ->
    let pint_type = pstoken "int" *> return TInt in
    let pbool_type = pstoken "bool" *> return TBool in
    let punit_type = pstoken "unit" *> return TUnit in
    let simple_type = choice [ pint_type; pbool_type; punit_type; pparens ptype ] in
    (* Парсер для списков, обязательно должен идти после обработки кортежей *)
    let list_type = simple_type >>= fun t -> pstoken "list" *> return (TList t) in
    (* Парсер для кортежей с минимум двумя элементами *)
    let tuple_type =
      lift3
        (fun t1 t2 ts -> TTuple (t1, t2, ts))
        (choice [ list_type; simple_type ])
        (pstoken "*" *> choice [ list_type; simple_type ])
        (many (pstoken "*" *> choice [ list_type; simple_type ]))
    in
    (* Обработка кортежей и функциональных типов *)
    let composed_type = choice [ tuple_type; list_type; simple_type ] in
    chainr1 composed_type (pstoken "->" *> return (fun t1 t2 -> TFun (t1, t2))))
;;

let ptyped_var =
  pparens (pid >>= fun id -> pstoken ":" *> ptype >>| fun ty -> id, Some ty)
  <|> (pid >>= fun id -> return (id, None))
;;

let plet pexpr =
  let rec pbody pexpr =
    ptyped_var >>= fun id -> pbody pexpr <|> pstoken "=" *> pexpr >>| fun e -> EFun (id, e)
  in
  pstoken "let"
  *> lift4
       (fun r id e1 e2 -> ELetIn (r, id, e1, e2))
       (pstoken "rec" *> return Rec <|> return NonRec)
       (poperator <|> pstoken "()" <|> pid)
       (pstoken "=" *> pexpr <|> pbody pexpr)
       (pstoken "in" *> pexpr)
;;

let plet_decl pexpr =
  let rec pbody pexpr =
    ptyped_var >>= fun id -> pbody pexpr <|> pstoken "=" *> pexpr >>| fun e -> EFun (id, e)
  in
  pstoken "let"
  *> lift3
       (fun r id e -> DLet (r, id, e))
       (pstoken "rec" *> return Rec <|> return NonRec)
       (poperator <|> pstoken "()" <|> pid)
       (pstoken "=" *> pexpr <|> pbody pexpr)
;;

let pbranch pexpr =
  ptoken
  @@ lift3
       (fun cond t f -> EBranch (cond, t, f))
       (pstoken "if" *> pexpr)
       (pstoken "then" *> pexpr)
       (pstoken "else" *> pexpr <|> return (EConst CUnit))
;;

let plist pexpr =
  let psqparens p = pstoken "[" *> p <* pstoken "]" in
  psqparens @@ sep_by (pstoken ";") pexpr >>| fun x -> EList x
;;

let ppconst = choice [ pint; pbool ] >>| fun x -> PConst x
let ppvar = pid >>| fun x -> PVar x

let ppattern =
  fix
  @@ fun ppattern ->
  let ppt =
    choice
      [ pparens
          (sep_by1 (pstoken ",") ppattern
           >>= function
           | [ p ] -> return p (* одиночный паттерн в скобках *)
           | p1 :: p2 :: ps -> return (PTuple (p1, p2, ps)) (* кортеж из 2+ элементов *)
           | [] -> fail "Unexpected empty list in tuple pattern")
      ; ppconst
      ; (pstoken "_" >>| fun _ -> PWild)
      ; (pstoken "[]" >>| fun _ -> PEmpty)
      ; ppvar
      ]
  in
  (* Остальная часть парсера остается без изменений *)
  let ppt =
    lift2
      (fun p1 -> function
        | h :: tl -> PCons (p1, h, tl)
        | _ -> p1)
      ppt
      (many (pstoken "::" *> ppt))
  in
  let ppt =
    lift2
      (fun p1 -> function
        | h :: tl -> POr (p1, h, tl)
        | _ -> p1)
      ppt
      (many (pstoken "|" *> ppt))
  in
  ppt
;;

let pmatch pexpr =
  let pcase ppattern pexpr =
    lift2 (fun p e -> p, e) (pstoken "|" *> ppattern) (pstoken "->" *> pexpr)
  in
  lift2
    (fun expr cases -> EMatch (expr, cases))
    (pstoken "match" *> pexpr <* pstoken "with")
    (many1 (pcase ppattern pexpr))
;;

let pfun pexpr =
  let rec pbody pexpr =
    ptyped_var
    >>= fun id -> pbody pexpr <|> pstoken "->" *> pexpr >>| fun e -> EFun (id, e)
  in
  pstoken "fun" *> pbody pexpr
;;

let pebinop chain1 e pbinop =
  chain1 e (pbinop >>| fun op e1 e2 -> EApp (EApp (EVar op, e1), e2))
;;

let plbinop = pebinop chainl1
let padd = pstoken "+" *> return "+"
let psub = pstoken "-" *> return "-"
let pmul = pstoken "*" *> return "*"
let pdiv = pstoken "/" *> return "/"
let peq = pstoken "=" *> return "="
let pneq = pstoken "<>" *> return "<>"
let ples = pstoken "<" *> return "<"
let pleq = pstoken "<=" *> return "<="
let pgre = pstoken ">" *> return ">"
let pgeq = pstoken ">=" *> return ">="

let pexpr =
  fix
  @@ fun pexpr ->
  let pe =
    choice [ pparens pexpr; pconst; poperatorvar; pvar; plist pexpr; pfun pexpr ]
  in
  let pe =
    lift2
      (fun f args -> List.fold_left ~f:(fun f arg -> EApp (f, arg)) ~init:f args)
      pe
      (many (char ' ' *> ptoken pe))
  in
  let pe = plbinop pe (pmul <|> pdiv) in
  let pe = plbinop pe (padd <|> psub) in
  let pe = plbinop pe (choice [ peq; pneq; pgeq; pleq; ples; pgre ]) in
  let pe =
    lift2
      (fun e1 -> function
        | h :: tl -> ETuple (e1, h, tl)
        | _ -> e1)
      pe
      (many (pstoken "," *> pe))
  in
  choice [ plet pexpr; pbranch pexpr; pmatch pexpr; pfun pexpr; pe ]
;;

let parse_expr = parse_string ~consume:Consume.All (pexpr <* pspaces)
let parse_decl = parse_string ~consume:Consume.All (plet_decl pexpr <* pspaces)
let parse = parse_string ~consume:Consume.All (many1 (plet_decl pexpr) <* pspaces)
