(* Copyright 2023-2024, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Angstrom

(* open List *)
let is_idc c = Char.is_alphanum c || Char.equal c '_'

let is_op c =
  match List.find [ '*'; '/'; '+'; '-'; '='; '<'; '>'; '&'; '|' ] ~f:(Char.equal c) with
  | None -> false
  | _ -> true
;;

let is_keyword = function
  | "let"
  | "and"
  | "rec"
  | "in"
  | "fun"
  | "match"
  | "with"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false" -> true
  | _ -> false
;;

let ws = take_while Char.is_whitespace

let obligatory_ws =
  ws
  >>= fun res ->
  match res with
  | "" -> fail "There is no whitespace"
  | _ -> return res
;;

let token s = ws *> string s
let lp = token "("
let rp = token ")"
let lsb = token "["
let rsb = token "]"
let semi = token ";"
let dsemi = token ";;"
let dcol = token "::"
let comma = token ","
let grd = token "|"
let arr = token "->"
let parens p = lp *> p <* rp
let brackets p = lsb *> p <* rsb

(*--------------------------------- Constants --------------------------------*)

let cint i = CInt i
let cbool b = CBool b
let cstring s = CString s

let pcint =
  let* sign = choice [ token "-"; token "+"; token "" ] in
  let* num = take_while1 Char.is_digit in
  return @@ Int.of_string (sign ^ num) >>| cint
;;

let pcbool =
  let t = token "true" *> return (cbool true) in
  let f = token "false" *> return (cbool false) in
  choice [ t; f ]
;;

let pcstring =
  token "\""
  *> take_while (function
    | '"' -> false
    | _ -> true)
  <* char '"'
  >>| cstring
;;

let pcunit = token "()" *> return CUnit
let pcnil = token "[]" *> return CNil
let const = choice [ pcint; pcbool; pcstring; pcunit; pcnil ]

(*--------------------------------- Types ------------------------------------*)

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let chainr1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op (e >>= go) <|> return acc in
  e >>= go
;;

let annot_primitive =
  let tvar =
    char '\''
    *> (satisfy is_idc >>= fun varname -> return (AVar ("'" ^ String.make 1 varname)))
  in
  choice
    [ string "int" *> return AInt
    ; string "string" *> return AString
    ; string "bool" *> return ABool
    ; string "unit" *> return AUnit
    ; tvar
    ]
;;

let annot_tuple p =
  let* t = ws *> p in
  let* ts = many1 (ws *> char '*' *> ws *> p) in
  return (ATuple (t :: ts))
;;

let annot_fun p =
  chainr1 p (ws *> string "->" *> ws *> return (fun arg ret -> AFun (arg, ret)))
;;

let rec annot_list t =
  let* base = t in
  let* _ = ws *> string "list" in
  annot_list (return (AList base)) <|> return (AList base)
;;

let annot =
  fix (fun annot ->
    let single = annot_primitive <|> parens annot in
    let list_type = annot_list single <|> single <|> annot_list annot in
    let tuple_type = annot_tuple list_type <|> list_type in
    annot_fun tuple_type <|> tuple_type)
;;

(*------------------------------ Patterns ------------------------------------*)

let pconst c = PConst c
let pvar x = PVar x
let pcons p1 p2 = PCons (p1, p2)
let ptuple ps = PTuple ps
let ppconst = const >>| pconst
let ppany = token "_" *> return PAny

let varname =
  let* fst =
    ws
    *> satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  let* rest = take_while is_idc in
  match String.of_char fst ^ rest with
  | "_" -> fail "Wildcard can't be used as indetifier"
  | s when is_keyword s -> fail "Keyword can't be used as identifier"
  | _ as name -> return name
;;

let opname_ =
  let* op = ws *> lp *> ws *> take_while is_op <* ws <* rp in
  return op
;;

(* let ppvar = varname >>| pvar *)
let ppvar =
  let pvar =
    let* name = varname in
    return (PVar name)
  in
  let opname =
    let* op = opname_ in
    return (PVar op)
  in
  choice [ pvar; opname ]
;;

let pptuple p = lift2 List.cons p (many1 (comma *> p)) >>| ptuple
let pplist p = brackets @@ sep_by1 semi p >>| List.fold_right ~f:pcons ~init:(pconst CNil)

let pattern =
  let pattern_with_type ppat =
    let* pat = ws *> lp *> ppat in
    let* constr = ws *> token ":" *> ws *> annot <* ws <* rp in
    return (PConstraint (pat, constr))
  in
  fix (fun pat ->
    let term =
      choice [ ppconst; ppvar; ppany; pplist pat; pattern_with_type pat; parens pat ]
    in
    let cons = chainr1 term (dcol *> return pcons) in
    let tuple = pptuple cons <|> cons in
    tuple)
;;

(*----------------------------- Expressions ----------------------------------*)
let econst c = EConst c
let evar x = EVar x
let eif e1 e2 e3 = EIf (e1, e2, e3)
let ematch e cl = EMatch (e, cl)
let elet f bl e = ELet (f, bl, e)
let efun p e = EFun (p, e)
let etuple el = ETuple el
let econs e1 e2 = ECons (e1, e2)
let eapply e1 e2 = EApply (e1, e2)
let peconst = const >>| econst
let pevar = varname >>| evar

let pevar =
  let pvar =
    let* name = varname in
    return (EVar name)
  in
  let opname =
    let* op = opname_ in
    return (EVar op)
  in
  choice [ pvar; opname ]
;;

let peif pe =
  fix (fun peif ->
    lift3
      eif
      (token "if" *> (peif <|> pe))
      (token "then" *> (peif <|> pe))
      (token "else" *> (peif <|> pe)))
;;

let pematch pe =
  let pexpr = token "match" *> pe <* token "with" <* option "" grd in
  let pcase = lift2 (fun p e -> p, e) (pattern <* arr) pe in
  lift2 ematch pexpr (sep_by1 grd pcase)
;;

let petuple p = lift2 List.cons p (many1 (comma *> p)) >>| etuple
let pelist p = brackets @@ sep_by1 semi p >>| List.fold_right ~f:econs ~init:(econst CNil)
let efunf ps e = List.fold_right ps ~f:efun ~init:e
let pefun pe = lift2 efun (token "fun" *> pattern) (lift2 efunf (many pattern <* arr) pe)

let pelet pe =
  lift3
    elet
    (token "let" *> option Nonrec (token "rec" *> obligatory_ws *> return Rec))
    (sep_by (token "and") (both pattern (lift2 efunf (many pattern <* token "=") pe)))
    (token "in" *> pe)
;;

let op_starts_with s =
  let* start = token s in
  let* rest =
    take_while (function
      | '*' | '/' | '+' | '-' | '=' | '<' | '>' | '&' | '|' -> true
      | _ -> false)
  in
  return (start ^ rest)
;;

let parse_op start =
  let* op = op_starts_with start in
  return (fun e1 e2 -> EApply (EApply (EVar op, e1), e2))
;;

let pmul = parse_op "*"
let pdiv = parse_op "/"
let padd = parse_op "+"
let psub = parse_op "-"
let peq = parse_op "="
let plt = parse_op "<"
let plte = parse_op "<="
let pneq = parse_op "<>"
let pgt = parse_op ">"
let pgte = parse_op ">="
let pand = parse_op "&&"
let por = parse_op "||"

let expr =
  fix (fun expr ->
    let term = choice [ pevar; peconst; pelist expr; parens expr ] in
    let apply = chainl1 term (return eapply) in
    let fact = chainl1 apply (pmul <|> pdiv) in
    let sum = chainl1 fact (padd <|> psub) in
    let cons = chainr1 sum (dcol *> return econs) in
    let cmp1 = chainl1 cons (choice [ plte; pneq; plt; peq ]) in
    let cmp2 = chainl1 cmp1 (pgte <|> pgt) in
    let band = chainr1 cmp2 pand in
    let bor = chainr1 band por in
    let tuple = petuple bor <|> bor in
    let ife = peif tuple <|> tuple in
    choice [ pelet expr; pematch expr; pefun expr; ife ])
;;

(*----------------------------- Structure ------------------------------------*)

let str_item =
  let pseval = expr >>| fun e -> SEval e in
  let svalue f bl = SValue (f, bl) in
  let psvalue =
    lift2
      svalue
      (token "let" *> option Nonrec (token "rec" *> obligatory_ws *> return Rec))
      (sep_by (token "and") (both pattern (lift2 efunf (many pattern <* token "=") expr)))
  in
  choice [ pseval; psvalue ]
;;

let structure : structure t = sep_by dsemi str_item
let parse s = parse_string ~consume:Prefix structure s

let test_parse input =
  let open Stdlib.Format in
  match parse input with
  | Ok ast -> printf "%a\n" pp_structure ast
  | Error s -> printf "Parsing error: %s\n" s
;;
