(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Ast

(* --- Вспомогательные функции --- *)

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
  | "and"
  | "fun"
  | "type"
  | "int"
  | "string"
  | "bool" -> true
  | _ -> false
;;

let is_operator_char = function
  | '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | '%' | '&' | '|' | '^' | ':' -> true
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

(* Парсер идентификатора: разрешаем также операторы и "()" как имена *)
let pid =
  let pfirst = satisfy (fun ch -> is_letter ch || Char.equal ch '_') >>| Char.escaped in
  let plast = take_while (fun ch -> is_letter ch || is_digit ch || Char.equal ch '_') in
  ptoken (lift2 ( ^ ) pfirst plast)
  >>= fun s ->
  if is_keyword s
  then fail ("Keyword identifiers are forbidden: " ^ s)
  else return s <?> "identifier"
;;

(* Для имён связываний разрешаем альтернативу: оператор, "()", либо идентификатор *)
let pbind_id = poperator <|> pstoken "()" <|> pid

(* --- Парсер констант --- *)

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
let pvar = pbind_id >>| fun e -> EVar e
let poperatorvar = poperator >>| fun op -> EVar op

(* --- Парсер типов (упрощённо) --- *)

let ptype =
  fix (fun ptype ->
    let pint_type = pstoken "int" *> return TInt in
    let pbool_type = pstoken "bool" *> return TBool in
    let punit_type = pstoken "unit" *> return TUnit in
    let simple_type = choice [ pint_type; pbool_type; punit_type; pparens ptype ] in
    let list_type = simple_type >>= fun t -> pstoken "list" *> return (TList t) in
    let tuple_type =
      lift3
        (fun t1 t2 ts -> TTuple (t1, t2, ts))
        (choice [ list_type; simple_type ])
        (pstoken "*" *> choice [ list_type; simple_type ])
        (many (pstoken "*" *> choice [ list_type; simple_type ]))
    in
    let composed_type = choice [ tuple_type; list_type; simple_type ] in
    chainr1 composed_type (pstoken "->" *> return (fun t1 t2 -> TFun (t1, t2))))
;;

let ptyped_var =
  pparens (pid >>= fun id -> pstoken ":" *> ptype >>| fun ty -> id, Some ty)
  <|> (pid >>= fun id -> return (id, None))
;;

(* --- Парсер связываний let --- *)

let parse_binding pexpr =
  let rec pbody pexpr =
    ptyped_var
    >>= fun (id, ty_opt) ->
    pbody pexpr <|> pstoken "=" *> pexpr >>| fun e -> EFun ((id, ty_opt), e)
  in
  pbind_id >>= fun id -> pstoken "=" *> pexpr <|> pbody pexpr >>| fun expr -> id, expr
;;

let parse_bindings pexpr =
  lift2
    (fun first rest -> first :: rest)
    (parse_binding pexpr)
    (many (pstoken "and" *> parse_binding pexpr))
;;

(* --- Верхнеуровневые let‑объявления --- *)

let plet_decl pexpr =
  pstoken "let"
  *> lift2
       (fun r bindings ->
         match bindings with
         | [ (id, e) ] -> DLet (r, id, e)
         | binds -> DMutualLet (r, binds))
       (choice [ pstoken "rec" *> return Rec; return NonRec ])
       (parse_bindings pexpr)
;;

(* --- Локальный let с in (поддерживает только одно связывание) --- *)

let plet_in pexpr =
  pstoken "let"
  *> lift3
       (fun r binding body ->
         let id, e = binding in
         ELetIn (r, id, e, body))
       (choice [ pstoken "rec" *> return Rec; return NonRec ])
       (parse_binding pexpr)
       (pstoken "in" *> pexpr)
;;

(* --- Остальные парсеры выражений --- *)

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
  psqparens (sep_by (pstoken ";") pexpr) >>| fun x -> EList x
;;

let ppconst = choice [ pint; pbool ] >>| fun x -> PConst x
let ppvar = pid >>| fun x -> PVar x

let ppattern =
  fix (fun ppattern ->
    let ppt =
      choice
        [ pparens
            (sep_by1 (pstoken ",") ppattern
             >>= function
             | [ p ] -> return p
             | p1 :: p2 :: ps -> return (PTuple (p1, p2, ps))
             | [] -> fail "Unexpected empty list in tuple pattern")
        ; ppconst
        ; (pstoken "_" >>| fun _ -> PWild)
        ; (pstoken "[]" >>| fun _ -> PEmpty)
        ; ppvar
        ]
    in
    let ppt =
      lift2
        (fun p1 rest ->
          match rest with
          | h :: tl -> PCons (p1, h, tl)
          | [] -> p1)
        ppt
        (many (pstoken "::" *> ppt))
    in
    lift2
      (fun p1 rest ->
        match rest with
        | h :: tl -> POr (p1, h, tl)
        | [] -> p1)
      ppt
      (many (pstoken "|" *> ppt)))
;;

let pcase ppattern pexpr =
  lift2 (fun p e -> p, e) (pstoken "|" *> ppattern) (pstoken "->" *> pexpr)
;;

let pmatch pexpr =
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
let prbinop = pebinop chainr1
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
let pand = pstoken "&&" *> return "&&"
let por = pstoken "||" *> return "||"
let pcons = pstoken "::" *> return "::"

let pexpr_fun () =
  fix (fun pexpr ->
    let pe_base =
      choice [ pparens pexpr; pconst; poperatorvar; pvar; plist pexpr; pfun pexpr ]
    in
    let pe_app =
      lift2
        (fun f args -> List.fold_left ~f:(fun f arg -> EApp (f, arg)) ~init:f args)
        pe_base
        (many (char ' ' *> ptoken pe_base))
    in
    let pe_bin =
      let op_bin pe ops = plbinop pe ops in
      let pe1 = pe_app in
      let pe2 = op_bin pe1 (pmul <|> pdiv) in
      let pe3 = op_bin pe2 (padd <|> psub) in
      op_bin pe3 (choice [ peq; pneq; pgeq; pleq; ples; pgre; pand; por ])
    in
    let pe_cons = prbinop pe_bin pcons in
    let pe_tuple =
      lift2
        (fun e1 rest ->
          match rest with
          | [] -> e1
          | hd :: tl -> ETuple (e1, hd, tl))
        pe_cons
        (many (pstoken "," *> pe_cons))
    in
    choice [ plet_in pexpr; pbranch pexpr; pmatch pexpr; pfun pexpr; pe_tuple ])
;;

let pexpr = pexpr_fun ()
let parse_expr = parse_string ~consume:Consume.All (pexpr <* pspaces)
let parse_decl = parse_string ~consume:Consume.All (plet_decl pexpr <* pspaces)
let parse = parse_string ~consume:Consume.All (many1 (plet_decl pexpr) <* pspaces)
