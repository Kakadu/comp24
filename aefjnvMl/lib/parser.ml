(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

open Angstrom
open Base
open Ast
(* open Errors *)

let id s = s

(*===================== const =====================*)

(* | "*" | "/" | "+" | "-" | "<=" | "<" | ">" | ">=" | "=" | "!=" | "&&" | "||" *)

let op_mul = "*"
let op_div = "/"
let op_plus = "+"
let op_minus = "-"
let op_less_eq = "<="
let op_more_eq = ">="
let op_less = "<"
let op_more = ">"
let op_eq = "="
let op_not_eq = "!="
let op_and = "&&"
let op_or = "||"
let un_op_minus = "-"
let un_op_not = "not"

(*===================== const =====================*)

let cint n = Const_int n
let cbool b = Const_bool b
let cnil = Const_nil

(*===================== core_type =====================*)

let ptint = Ptyp_int
let ptbool = Ptyp_bool
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
let pnil = pconst cnil

(*===================== expression =====================*)

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
let edecl d_rec d_pat d_expr = { d_rec; d_pat; d_expr }
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
  | "in" -> true
  | _ -> false
;;

let is_alpha c = is_upper c || is_lower c
let is_ident c = is_alpha c || Char.equal '_' c

(*===================== Control characters =====================*)

let skip_whitespace = take_while is_whitespace
let skip_whitespace1 = take_while1 is_whitespace
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
  e >>= fun init -> go init
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
let const = choice [ c_int; c_bool; c_nil ]

(*===================== Identifiers =====================*)

let check_ident i =
  if is_keyword i
  then fail "keyword"
  else if String.equal "_" i
  then fail "wildcard not expected"
  else return (id i)
;;

let ident =
  ptoken peek_char
  >>= (function
   | Some x when Char.equal x '_' || is_lower x -> return x
   | _ -> fail "not an identifier")
  >>= fun _ -> take_while is_ident >>= fun s -> check_ident s
;;

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
    ; token op_not_eq
    ; token op_and
    ; token op_or
    ]
;;

(*===================== Core types =====================*)

let p_tint = token "int" *> return ptint
let p_tbool = token "bool" *> return ptbool
let p_tvar = ident >>| ptvar

let p_ttuple p_core_tp =
  lift2 (fun h tl -> pttuple (h :: tl)) p_core_tp (many1 (token "*" *> p_core_tp))
;;

let p_tarrow p_core_tp =
  lift3 (fun lside _ rside -> ptarrow lside rside) p_core_tp (token "->") p_core_tp
;;

let p_core_type =
  fix (fun core_type ->
    let p_type =
      choice
        [ parens core_type
        ; p_tvar
        ; p_tint
        ; p_tbool
        ; p_ttuple core_type
        ; p_tarrow core_type
        ]
        (* TODO: problem with left recursion *)
        (* [ parens core_type
           ; p_tarrow core_type
           ; p_ttuple core_type
           ; p_tint
           ; p_tbool
           ; p_tvar
           ] 
        *)
    in
    p_type)
;;

(*===================== Patterns =====================*)

let p_const = const >>| fun p -> pconst p
let p_var = (ident <|> parens infix_op) >>| pvar
let p_cons = token "::" *> return pcons
let p_any = token "_" *> skip_whitespace *> return pany
let fold_plist = List.fold_right ~f:(fun p1 p2 -> pcons p1 p2) ~init:pnil

let p_list =
  let item = p_const <|> p_var in
  sbrcts @@ sep_by (token ";") item >>| fold_plist
;;

let tuple ident f = lift2 (fun h tl -> f @@ (h :: tl)) ident (many1 (token "," *> ident))
let p_tuple pat = parens (tuple pat ptuple)

let pattern =
  fix (fun pattern ->
    let term = choice [ parens pattern; p_const; p_any; p_var; p_tuple pattern ] in
    let cons = parens @@ chainr1 term p_cons in
    let with_tp =
      parens @@ lift3 (fun p _ tp -> pconstraint p tp) pattern (token ":") p_core_type
    in
    with_tp <|> cons <|> term)
;;

(*===================== Expressions =====================*)

let e_const = const >>| fun c -> econst c
let e_val = (ident <|> parens infix_op) >>| eval
let e_cons = token "::" *> return econs

let e_list expr =
  let rec create_cons = function
    | [] -> econst cnil
    | h :: tl -> econs h (create_cons tl)
  in
  let basic_list = sbrcts @@ sep_by (token ";") expr >>| create_cons in
  let cons_list = chainr1 (expr <|> basic_list) e_cons in
  basic_list <|> cons_list
;;

let e_tuple expr = tuple expr etuple
let e_app expr = chainl1 expr (return eapp)
let e_ite b t e = lift3 eite (token "if" *> b) (token "then" *> t) (token "else" *> e)
let pars_fun_type_opt = token ":" *> p_core_type >>| (fun tp -> Some tp) <|> return None

let e_fun p_expr =
  let pars_args = many1 pattern in
  let pars_fun =
    token "fun"
    *> lift4
         (fun args tp_opt _ expr -> args, tp_opt, expr)
         pars_args
         pars_fun_type_opt
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

let lift5 f a b c d e =
  lift4 (fun ra rb rc rd -> f ra rb rc rd) a b c d >>= fun f_new -> e >>| f_new
;;

(* TODO: refactor it *)
let e_decl pexpr =
  let pars_args = skip_whitespace *> many pattern in
  let pars_d_rec = token "rec" *> return Recursive <|> return Nonrecursive in
  let pars_decl =
    token "let" *> pars_d_rec
    >>= fun rflag ->
    lift5
      (fun main_p args tp_opt _ expr -> rflag, main_p, args, tp_opt, expr)
      (ptoken pattern)
      pars_args
      pars_fun_type_opt
      (token "=")
      pexpr
  in
  let construct_decl (rflag, main_p, args, tp_opt, expr) =
    return
    @@ edecl
         rflag
         main_p
         (match args, tp_opt with
          | h :: tl, None ->
            List.fold_left ~init:(efun h expr) ~f:(fun acc x -> efun x acc) tl
          | h :: tl, Some tp ->
            List.fold_left
              ~init:(efun (pconstraint h tp) expr)
              ~f:(fun acc x -> efun x acc)
              tl
          | _ -> expr)
  in
  pars_decl >>= construct_decl
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

let bin_op chain1 e ops = chain1 e (ops >>| fun o l r -> ebinop o l r)
let lbo = bin_op chainl1
let rbo = bin_op chainr1
let op l = choice (List.map ~f:(fun o -> token o >>| eval) l)
let mul_div = op [ op_mul; op_div ]
let add_sub = op [ op_plus; op_minus ]
let cmp = op [ op_less_eq; op_less; op_more_eq; op_more; op_eq; op_not_eq ]
let andop = op [ op_and ]
let orop = op [ op_or ]
let neg = op [ un_op_not; un_op_minus ]

let expr =
  fix (fun pexpr ->
    let sube = choice [ parens pexpr; e_const; e_val ] in
    let term = e_app sube in
    let term = lbo (term <|> lift2 eunop neg term) mul_div in
    let term = lbo term add_sub in
    let term = e_list term <|> term in
    let term = lbo term cmp in
    let term = rbo term andop in
    let term = rbo term orop in
    let term = e_tuple term <|> term in
    choice [ e_ite pexpr pexpr pexpr; e_let pexpr; e_match pexpr; e_fun pexpr; term ])
;;

let del = (dsmcln <|> skip_whitespace) *> skip_whitespace
let decl = ptoken (e_decl expr)
let str_item = expr >>| streval <* dsmcln <|> (decl >>| strval)
let program = del *> many1 (str_item <* del)

let parse_syntax_err msg = Errors.Parser (Syntax_error msg)

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

let parse_prefix_with p s =
  match parse_string ~consume:Prefix p s with
  | Ok v -> Ok v
  | Error _ -> Error (parse_syntax_err "Syntax error")
;;
