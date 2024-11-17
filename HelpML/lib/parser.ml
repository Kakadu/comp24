open Angstrom
open Ast
open Base
module Format = Stdlib.Format

let parse_str p s = parse_string ~consume:All p s

let is_whitespace = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_aletter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_cletter = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_underscore = function
  | c -> Char.equal c '_'
;;

let is_id_char = function
  | c -> is_digit c || is_aletter c || is_cletter c || is_underscore c
;;

let is_keyword = function
  | "let" | "rec" | "fun" | "in" | "if" | "then" | "else" | "true" | "false" -> true
  | _ -> false
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let empty = take_while is_whitespace
let empty1 = take_while1 is_whitespace
let token s = empty *> s
let token1 s = empty1 *> s
let str_token s = empty *> string s
let str_token1 s = empty1 *> string s
let pparens p = str_token "(" *> p <* str_token ")"
let parrow = str_token "->"
let pbinding = str_token "let"
let pwild = str_token "_"

let pcint = 
  let ps = token (option "" (str_token "-" <|> str_token "+")) in
  let pd = take_while1 is_digit in
  lift2 (fun sign digit -> constr_cint (Int.of_string @@ sign ^ digit)) ps pd
;;

let pcbool = 
  lift (fun b -> constr_cbool @@ Bool.of_string b) (str_token "false" <|> str_token "true")
;;

let pcunit = pparens (return CUnit)
let pconst = token (choice [ pcint; pcbool; pcunit ])

let pident =
  let is_id_head = function
    | c -> is_aletter c || is_underscore c
  in
  let pchar = satisfy is_id_char in
  empty *> satisfy is_id_head
  >>= fun h ->
  many pchar
  >>= fun tl ->
  let id = String.of_char_list (h :: tl) in
  if is_keyword id
  then fail ("Unexpected keyword '" ^ id ^ "' in binding")
  else if String.equal id "_"
  then fail "Wildcard \"_\" not expected"
  else return id
;;

let ppwild = constr_pwild <$> pwild
let ppconst = constr_pconst <$> pconst
let ppvar = constr_pvar <$> pident
let pattern = fix @@ fun m -> choice [ ppwild; ppconst; ppvar ] <|> pparens m

let pop ch op = str_token ch *> return (constr_ebinop op)
let pmulti = choice [ pop "*" Mul; pop "/" Div; pop "%" Mod ]
let padd = pop "+" Add <|> pop "-" Sub
let pcomp = choice [ pop ">=" Geq; pop ">" Gre; pop "<=" Leq; pop "<" Les ]
let peq = pop "=" Equ <|> pop "<>" Neq
let pconj = pop "&&" And
let pdisj = pop "||" Dsj

type edispatch =
  { evar : edispatch -> expr t
  ; econst : edispatch -> expr t
  ; op : edispatch -> expr t
  ; condition : edispatch -> expr t
  ; func : edispatch -> expr t
  ; bind_in : edispatch -> expr t
  ; app : edispatch -> expr t
  ; expr : edispatch -> expr t
  }

let peconst = constr_econst <$> pconst
let pevar = pident >>| constr_evar
let pfun_args = fix @@ fun p -> many pattern <|> pparens p
let pfun_args1 = fix @@ fun p -> many1 pattern <|> pparens p
let pparens_only ps = pparens @@ choice ps

let plet_body pargs pexpr =
  token1 pargs
  >>= fun args -> str_token "=" *> pexpr >>| fun expr -> constr_efun args expr
;;

let plet_body_without_args pexpr =
  str_token "=" *> pexpr >>| fun expr -> constr_efun [] expr
;;

let pbind_with_option_rec = pbinding *> option false (str_token1 "rec " >>| fun _ -> true)
let pbody_with_args pack = plet_body pfun_args (pack.expr pack)

let pack =
  let expr pack =
    choice
      [ pack.op pack
      ; pack.app pack
      ; pack.condition pack
      ; pack.func pack
      ; pack.bind_in pack
      ]
  in
  let econst pack = fix @@ fun _ -> peconst <|> pparens @@ pack.econst pack in
  let evar pack = fix @@ fun _ -> pevar <|> pparens @@ pack.evar pack in
  let op_parsers pack =
    choice
      [ pack.bind_in pack
      ; pack.app pack
      ; pparens_only [ pack.op pack; pack.condition pack ]
      ; pack.evar pack
      ; pack.econst pack
      ]
  in
  let cond_bool_parsers pack =
    choice [ pack.op pack; pack.bind_in pack; pack.app pack; pack.condition pack ]
  in
  let app_func_parsers pack =
    choice
      [ pack.evar pack
      ; pparens_only
          [ pack.condition pack; pack.func pack; pack.app pack; pack.bind_in pack ]
      ]
  in
  let app_args_parsers pack =
    choice
      [ pparens_only
          [ pack.op pack
          ; pack.condition pack
          ; pack.func pack
          ; pack.app pack
          ; pack.bind_in pack
          ]
      ; pack.evar pack
      ; pack.econst pack
      ]
  in
  let op pack =
    fix
    @@ fun _ ->
    let multi = chainl1 (op_parsers pack) pmulti in
    let add = chainl1 multi padd in
    let comp = chainl1 add pcomp in
    let eq = chainl1 comp peq in
    let conj = chainl1 eq pconj in
    chainl1 conj pdisj <|> pparens @@ pack.op pack
  in
  let condition pack =
    fix
    @@ fun _ ->
    lift3
      constr_eif
      (str_token "if" *> cond_bool_parsers pack)
      (str_token1 "then" *> expr pack)
      (str_token1 "else" *> expr pack)
    <|> pparens @@ pack.condition pack
  in
  let func pack =
    fix
    @@ fun _ ->
    lift2 constr_efun (str_token "fun" *> pfun_args1) (parrow *> expr pack)
    <|> pparens @@ pack.func pack
  in
  let app pack =
    fix
    @@ fun _ ->
    lift2 constr_eapp (app_func_parsers pack) (many1 (token1 @@ app_args_parsers pack))
    <|> pparens @@ pack.app pack
  in
  let pbind_without_rec = pbinding *> return false in
  let punderscore_name = str_token1 "_" *> return "_" in
  let pbody_without_args pack = plet_body_without_args (expr pack) in
  let bind_in pack =
    fix
    @@ fun _ ->
    lift4
      constr_eletin
      pbind_with_option_rec
      pident
      (pbody_with_args pack)
      (str_token1 "in" *> expr pack)
    <|> lift4
          constr_eletin
          pbind_without_rec
          punderscore_name
          (pbody_without_args pack)
          (str_token1 "in" *> expr pack)
    <|> pparens @@ pack.bind_in pack
  in
  { evar; econst; op; condition; func; bind_in; app; expr }
;;

(* let expr = pack.expr pack *)

(**  Binding parser *)
let bind =
  fix
  @@ fun m ->
  lift3 constr_elet pbind_with_option_rec pident (pbody_with_args pack) <|> pparens m
;;

(**  Program parser *)
let pprogram = many1 (token bind <* choice [ token (str_token ";;"); empty ])

let parse str = parse_str pprogram (String.strip str)