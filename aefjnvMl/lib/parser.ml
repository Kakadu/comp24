(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Common.Ast
open Common.Base_lib

(*===================== const =====================*)

let cint n = Const_int n
let cbool b = Const_bool b
let cnil = Const_nil
let cunit = Const_unit

(*===================== core_type =====================*)

let ptint = Ptyp_int
let ptbool = Ptyp_bool
let ptunit = Ptyp_unit
let ptlist tp = Ptyp_list tp
let ptvar idnt = Ptyp_var idnt
let pttuple pt_list = Ptyp_tuple pt_list
let ptarrow before_pt after_pt = Ptyp_arrow (before_pt, after_pt)

(*===================== pattern =====================*)

let pconst c = Pat_const c
let pvar c = Pat_var c
let pcons a b = Pat_cons (a, b)
let pany = Pat_any
let ptuple ps = Pat_tuple ps
let pconstraint p ct = Pat_constraint (p, ct)

(*===================== expression =====================*)

let etype e tp = Exp_type (e, tp)
let econst c = Exp_constant c
let eval c = Exp_ident c
let eapp f a = Exp_apply (f, a)
let ematch v ptrns = Exp_match (v, ptrns)
let efun i e = Exp_function (i, e)
let ebinop o l r = Exp_apply (Exp_apply (o, l), r)
let eunop o e = Exp_apply (o, e)
let eite b t e = Exp_ifthenelse (b, t, e)
let elet d e = Exp_let (d, e)
let etuple es = Exp_tuple es
let econs a b = Exp_list (a, b)
let evalue_binding vb_pat vb_expr = { vb_pat; vb_expr }
let edecl rec_flag bind_list = Decl (rec_flag, bind_list)
let streval e = Str_eval e
let strval d = Str_value d

(*===================== Check conditions =====================*)

let is_whitespace = Char.is_whitespace
let is_digit = Char.is_digit

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "if"
  | "else"
  | "fun"
  | "function"
  | "then"
  | "rec"
  | "true"
  | "false"
  | "match"
  | "with"
  | "object"
  | "end"
  | "val"
  | "not"
  | "method"
  | "unit"
  | "and"
  | "in" -> true
  | _ -> false
;;

let is_alpha c = is_upper c || is_lower c

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false
;;

(*===================== Control characters =====================*)

let skip_whitespace = take_while is_whitespace
let ptoken p = skip_whitespace *> p
let token p = skip_whitespace *> string p
let lp = token "("
let rp = token ")"
let parens p = lp *> p <* rp
let sbrcts p = token "[" *> p <* token "]"
let dsmcln = token ";;"

(*===================== Fold infix operators =====================*)

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let rec chainr1 e op =
  e >>= fun sub_e -> op >>= (fun f -> chainr1 e op >>| f sub_e) <|> return sub_e
;;

(*===================== Constants =====================*)

let c_int =
  ptoken @@ take_while1 is_digit
  >>= fun whole ->
  let num = Stdlib.int_of_string_opt whole in
  match num with
  | Some n -> return @@ cint n
  | None -> fail "Integer literal exceeds the range of representable integers of type int"
;;

let c_bool =
  ptoken @@ take_while1 is_alpha
  >>= function
  | "true" -> return @@ cbool true
  | "false" -> return @@ cbool false
  | _ -> fail "not a bool"
;;

let c_nil = token "[]" *> return cnil
let c_unit = token "()" *> return cunit
let const = choice [ c_int; c_bool; c_nil; c_unit ]

(*===================== Identifiers =====================*)

let check_ident i =
  if is_keyword i
  then fail "keyword"
  else if String.equal "_" i
  then fail "wildcard not expected"
  else return i
;;

let unchecked_ident =
  ptoken peek_char
  >>= function
  | Some x when Char.equal x '_' || is_lower x -> take_while is_ident_char
  | _ -> fail "not a valid identifier"
;;

let ident = unchecked_ident >>= check_ident

let infix_op =
  choice
    [ token op_mul
    ; token op_div
    ; token op_plus
    ; token op_minus
    ; token op_less_eq
    ; token op_less
    ; token op_more
    ; token op_more_eq
    ; token op_eq
    ; token op_2eq
    ; token op_not_eq
    ; token op_and
    ; token op_or
    ]
;;

let un_op =
  choice [ token (un_op_prefix ^ un_op_minus); token (un_op_prefix ^ un_op_not) ]
;;

(*===================== Core types =====================*)

let p_core_type =
  fix (fun core_type ->
    let p_tint = token "int" *> return ptint in
    let p_tbool = token "bool" *> return ptbool in
    let p_tunit = token "unit" *> return ptunit in
    let p_tvar = ident >>| ptvar in
    let p_primitive_types =
      choice [ p_tint; p_tbool; p_tvar; parens core_type; p_tunit ]
    in
    let p_tlist = p_primitive_types >>= fun tp -> token "list" *> return (ptlist tp) in
    let p_ttuple =
      let p_base_types = choice [ p_primitive_types; p_tlist ] in
      lift2
        (fun h tl -> pttuple (h :: tl))
        p_base_types
        (many1 (token "*" *> p_base_types))
    in
    let p_composite_types = choice [ p_ttuple; p_tlist; p_primitive_types ] in
    chainr1 p_composite_types (token "->" *> return ptarrow))
;;

(*===================== Patterns =====================*)

let p_const = const >>| pconst
let p_var = ident >>| pvar
let p_cons = token "::" *> return pcons
let p_any = token "_" *> skip_whitespace *> return pany
let tuple ident f = lift2 (fun h tl -> f @@ (h :: tl)) ident (many1 (token "," *> ident))
let p_tuple pat = parens (tuple pat ptuple) <|> tuple pat ptuple
let p_type_annotation = token ":" *> p_core_type

let pattern =
  fix (fun pattern ->
    let term = choice [ parens pattern; p_const; p_var; p_any ] in
    let term = p_tuple term <|> term in
    let cons = chainr1 term p_cons in
    let with_tp = parens @@ lift2 pconstraint pattern p_type_annotation in
    cons <|> term <|> with_tp)
;;

(*===================== Expressions =====================*)

let e_const = const >>| econst
let e_val = choice [ ident; parens infix_op; parens un_op ] >>| eval
let e_cons = token "::" *> return econs

let e_list_basic expr =
  let rec create_cons = function
    | [] -> econst cnil
    | h :: tl -> econs h (create_cons tl)
  in
  sbrcts @@ sep_by (token ";") expr >>| create_cons
;;

let e_list_cons expr = chainr1 (expr <|> e_list_basic expr) e_cons
let e_tuple expr = tuple expr etuple
let e_app expr = chainl1 expr (return eapp)
let e_ite b t e = lift3 eite (token "if" *> b) (token "then" *> t) (token "else" *> e)
let p_type_annotation_opt = p_type_annotation >>| (fun tp -> Some tp) <|> return None

let e_fun p_expr =
  let pars_args = many1 pattern in
  let pars_fun =
    token "fun"
    *> lift4
         (fun args tp_opt _ expr -> args, tp_opt, expr)
         pars_args
         p_type_annotation_opt
         (token "->")
         p_expr
  in
  pars_fun
  >>= fun (args, tp_opt, expr) ->
  (match List.rev args, tp_opt with
   | h :: tl, None -> return (h, tl)
   | h :: tl, Some tp -> return (pconstraint h tp, tl)
   | _ -> fail "The function must have at least one argument")
  >>| fun (h, tl) -> List.fold_left ~init:(efun h expr) ~f:(fun acc x -> efun x acc) tl
;;

let rec constr_nested_expr t = function
  | Exp_function (p, e) -> efun p (constr_nested_expr t e)
  | expr -> etype expr t
;;

let e_value_binding pexpr =
  let pars_main_p =
    choice [ ptoken pattern; parens infix_op >>| pvar; parens un_op >>| pvar ]
  in
  let pars_args = skip_whitespace *> many pattern in
  let validate_main_p main_p args =
    match main_p, args with
    | Pat_constraint _, _ :: _ ->
      fail
        "Explicitly specifying the binding type is only available before '='. Syntax \
         error: let (a: int) b c ... = ..."
    | pat, _ -> return pat
  in
  let collect_main_p tp_opt exp =
    match tp_opt with
    | Some tp -> constr_nested_expr tp exp
    | None -> exp
  in
  let collect_expr args expr =
    let f acc x = efun x acc in
    match List.rev args with
    | h :: tl -> List.fold_left ~init:(efun h expr) ~f tl
    | _ -> expr
  in
  let construct_value_binding (main_p, args, tp_opt, expr) =
    validate_main_p main_p args
    >>= fun main_valid_p ->
    return (collect_expr args expr)
    >>| collect_main_p tp_opt
    >>| fun expr -> evalue_binding main_valid_p expr
  in
  lift4
    (fun main_p args tp_opt expr -> main_p, args, tp_opt, expr)
    pars_main_p
    pars_args
    p_type_annotation_opt
    (token "=" *> pexpr)
  >>= construct_value_binding
;;

let e_decl pexpr =
  let pars_decl =
    let is_rec_flag = function
      | "rec" -> return Recursive
      | _ -> fail "Nonrec"
    in
    let is_and_flag = function
      | "and" -> return true
      | _ -> fail "No mutual recursion"
    in
    let pars_d_rec = unchecked_ident >>= is_rec_flag <|> return Nonrecursive in
    let pars_let = token "let" in
    let pars_secondary_vb = (unchecked_ident >>= is_and_flag) *> e_value_binding pexpr in
    pars_let *> pars_d_rec
    >>= fun rflag ->
    e_value_binding pexpr
    >>= fun first_vb ->
    many pars_secondary_vb >>| fun secondary_vbs -> rflag, first_vb :: secondary_vbs
  in
  let validate_decl (rflag, vb_list) =
    match rflag, vb_list with
    | Nonrecursive, _ :: _ :: _ -> fail "Using 'and' available only with 'rec' flag"
    | x -> return x
  in
  let construct_decl (rflag, vb_list) = return @@ edecl rflag vb_list in
  pars_decl >>= validate_decl >>= construct_decl
;;

let e_ptrn_matching pexpr = lift2 (fun k v -> k, v) (pattern <* token "->") pexpr

let e_match pexpr =
  token "match"
  *> lift2
       ematch
       (pexpr <* token "with")
       (e_ptrn_matching pexpr
        <|> token "|" *> e_ptrn_matching pexpr
        >>= fun p -> many (token "|" *> e_ptrn_matching pexpr) >>| fun ps -> p :: ps)
;;

let e_let pexpr = lift2 elet (e_decl pexpr) (token "in" *> pexpr)

(*===================== Binary/Unary operators =====================*)

let bin_op chain1 e ops = chain1 e (ops >>| ebinop)
let lbo = bin_op chainl1
let rbo = bin_op chainr1
let op l = choice (List.map ~f:(fun o -> token o >>| eval) l)
let mul_div = op [ op_mul; op_div ]
let add_sub = op [ op_plus; op_minus ]
let cmp = op [ op_less_eq; op_less; op_more_eq; op_more; op_2eq; op_eq; op_not_eq ]
let andop = op [ op_and ]
let orop = op [ op_or ]

let unop l =
  choice
    (List.map ~f:(fun o -> token o >>| fun un_name -> eval (un_op_prefix ^ un_name)) l)
;;

let neg = unop [ un_op_not; un_op_minus ]

let expr =
  fix (fun pexpr ->
    let etp = parens @@ lift2 etype pexpr p_type_annotation in
    let term = choice [ etp; parens pexpr; e_const; e_val ] in
    let term = e_list_basic term <|> term in
    let term = e_app term in
    let term = lbo (term <|> lift2 eunop neg term) mul_div in
    let term = e_list_cons term <|> term in
    let term = lbo term add_sub in
    let term = lbo term cmp in
    let term = rbo term andop in
    let term = rbo term orop in
    let term = e_tuple term <|> term in
    choice [ e_ite pexpr pexpr pexpr; e_let pexpr; e_match pexpr; e_fun pexpr; term ])
;;

let del = (dsmcln <|> skip_whitespace) *> skip_whitespace
let decl = ptoken (e_decl expr)
let str_item = expr >>| streval <|> (decl >>| strval)
let program = del *> many1 (str_item <* del)
let parse_syntax_err msg = Common.Errors.Parser (Syntax_error msg)

let parse s =
  match parse_string ~consume:All program s with
  | Ok v -> Ok v
  | Error _ -> Error (parse_syntax_err "Syntax error")
;;

let parse_prefix s =
  match parse_string ~consume:Prefix program s with
  | Ok v -> Ok v
  | Error _ -> Error (parse_syntax_err "Syntax error")
;;

module PP = struct
  let pp_error ppf = function
    | Common.Errors.Syntax_error msg -> Stdlib.Format.fprintf ppf "%s" msg
  ;;
end
