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
  e >>= fun init -> go init
;;

let pspaces = skip_while is_space
let ptoken p = skip_while is_space *> p
let pstoken s = ptoken (Angstrom.string s)
let pparens p = pstoken "(" *> p <* pstoken ")"
let poperator = pparens (take_while1 is_operator_char) >>= fun op -> return op

let pid =
  let pfirst =
    satisfy (fun ch -> is_letter ch || Char.equal ch '_') >>| fun ch -> Char.escaped ch
  in
  let plast = take_while (fun ch -> is_letter ch || is_digit ch || Char.equal ch '_') in
  ptoken (lift2 (fun x y -> x ^ y) pfirst plast)
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

let ptype =
  fix (fun ptype ->
    let pint_type = pstoken "int" *> return TInt in
    let pbool_type = pstoken "bool" *> return TBool in
    let punit_type = pstoken "unit" *> return TUnit in
    let pfun_type = lift2 (fun t1 t2 -> TFun (t1, t2)) (ptype <* pstoken "->") ptype in
    let plist_type = pparens (ptype <* pstoken "list") >>| fun t -> TList t in
    let pwrapped_type = pparens ptype in
    choice [ pint_type; pbool_type; punit_type; pfun_type; plist_type; pwrapped_type ])
;;

let ptyped_var =
  pparens (pid >>= fun id -> pstoken ":" *> ptype >>| fun ty -> id, ty)
  <|> (pid >>| fun id -> id, TUnknown)
;;

let plet_no_args pexpr =
  pstoken "let"
  *> lift4
       (fun r id e1 e2 -> ELet (r, id, e1, e2))
       (pstoken "rec" *> return Rec <|> return NonRec)
       (poperator <|> pstoken "()" <|> pid)
       (pstoken "=" *> pexpr)
       (pstoken "in" *> pexpr >>| (fun x -> Some x) <|> return None)
;;

let plet_with_args pexpr =
  let lift5 f p1 p2 p3 p4 p5 = f <$> p1 <*> p2 <*> p3 <*> p4 <*> p5 in
  pstoken "let"
  *> lift5
       (fun r id args e1 e2 -> ELet (r, id, EFun (args, e1), e2))
       (pstoken "rec" *> return Rec <|> return NonRec)
       (poperator <|> pstoken "()" <|> pid)
       (many1 ptyped_var)
       (pstoken "=" *> pexpr)
       (pstoken "in" *> pexpr >>| (fun x -> Some x) <|> return None)
;;

let plet pexpr = choice [ plet_no_args pexpr; plet_with_args pexpr ]

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
      [ pparens ppattern
      ; ppconst
      ; (pstoken "_" >>| fun _ -> PWild)
      ; (pstoken "[]" >>| fun _ -> PEmpty)
      ; ppvar
      ]
  in
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
  let pbody pexpr =
    many1 ptyped_var >>= fun vars -> pstoken "->" *> pexpr >>| fun e -> EFun (vars, e)
  in
  pstoken "fun" *> pbody pexpr
;;

let pebinop chain1 e pbinop = chain1 e (pbinop >>| fun op e1 e2 -> EBinop (op, e1, e2))
let plbinop = pebinop chainl1
let padd = pstoken "+" *> return Add
let psub = pstoken "-" *> return Sub
let pmul = pstoken "*" *> return Mul
let pdiv = pstoken "/" *> return Div
let peq = pstoken "=" *> return Eq
let pneq = pstoken "<>" *> return Neq
let ples = pstoken "<" *> return Les
let pleq = pstoken "<=" *> return Leq
let pgre = pstoken ">" *> return Gre
let pgeq = pstoken ">=" *> return Geq

let pexpr =
  fix
  @@ fun pexpr ->
  let pe = choice [ pparens pexpr; pconst; pvar; plist pexpr; pfun pexpr ] in
  let pe =
    lift2
      (fun f args ->
        match args with
        | [] -> f
        | _ -> EApp (f, args))
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
let parse = parse_string ~consume:Consume.All (many1 (plet pexpr) <* pspaces)
