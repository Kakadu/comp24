[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Angstrom

open LMisc
open LAst
open LTypes

(* ======= Utils ======= *)
let unit = return ()

let skip_ws = skip_while Char.is_whitespace
let skip_comments =
  let scomment = string "(*" *> many_till any_char (string "*)") in
  sep_by skip_ws scomment *> unit

let ws = skip_ws *> skip_comments *> skip_ws
let ws1 =
  let paren = peek_char_fail >>= function '(' -> unit | _ -> fail "" in
  choice [skip Char.is_whitespace *> ws; paren *> skip_comments]

let ident s = string s >>| fun x -> Id.I x

let parens p = char '(' *> ws *> p <* ws <* char ')'
let spaced p = ws1 *> p <* ws1
let opt p = option None (p >>| Option.some)

(* ======= Identifiers ======= *)
(* https://ocaml.org/manual/5.2/lex.html#sss:lex:identifiers *)

let is_keyword = function
  (* https://ocaml.org/manual/5.2/lex.html#sss:keywords *)
  | "true"
  | "false"
  | "match"
  | "with"
  | "let"
  | "rec"
  | "and"
  | "in"
  | "type"
  | "function"
  | "fun"
  | "if"
  | "then"
  | "else" ->
      true
  | _ ->
      false

let pident flag =
  let is_first =
    match flag with
    | `LowerCase -> (
        function 'a' .. 'z' | '_' -> true | _ -> false )
    | `Capitalized -> (
        function 'A' .. 'Z' -> true | _ -> false )
  in

  let* first = satisfy is_first >>| String.of_char in
  let* rest =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' ->
          true
      | _ ->
          false )
  in

  let id = first ^ rest in
  if is_keyword id then fail "keyword" else return (Id.I id)

let pconstruct_id =
  let keywords =
    choice
      [ident "true"; ident "false"; ident "()"; ident "[]"; parens (ident "::")]
  in
  pident `Capitalized <|> keywords

let is_keyword_op = function "|" | "->" -> true | _ -> false

let pinfix_id ?starts () =
  let* first =
    Option.value_map starts ~f:string
      ~default:(satisfy is_op_first_char >>| Char.to_string)
  in
  let* rest = take_while is_op_char in

  let id = first ^ rest in
  if is_keyword_op id then fail "keyword" else return (Id.I id)

let pprefix_id =
  let* first = string "!" in
  let* rest = take_while is_op_char in

  let id = first ^ rest in
  if is_keyword_op id then fail "keyword" else return (Id.I id)

let pvalue_id =
  let pop_id = pinfix_id () <|> pprefix_id in
  pident `LowerCase <|> parens pop_id

let pty_con_id = pident `LowerCase

let pty_var =
  char '\'' *> ws *> (pident `LowerCase <|> pident `Capitalized)
  >>| fun (Id.I var) -> Var.V var

(* ======= Constants ======= *)

let pconst =
  let pint =
    let* num = take_while Char.is_digit in
    match Int.of_string_opt num with
    | None ->
        fail "not an integer"
    | Some x ->
        return (Const.Int x)
  in

  let pchar = char '\'' *> any_char <* char '\'' >>| fun x -> Const.Char x in

  let pstring =
    char '"' *> take_till (Char.( = ) '"')
    <* advance 1
    >>| fun x -> Const.String x
  in

  choice [pint; pchar; pstring]

(* ====== Value bindings ====== *)

(**
  [let [rec] P1 [: Ty] = E1 and P2 = E2 and ...]
  [let [rec] ValId1 PArg1 [: Ty] = E1 and P1 = E2 and ...]
*)
let plet pexpr ppat pty =
  let pbinding =
    let pfun =
      let* id = pvalue_id in
      let* args = ws1 *> sep_by1 ws1 ppat >>| List1.of_list_exn in
      let* ty =
        opt (ws *> char ':')
        >>= function None -> return None | Some _ -> pty >>| Option.some
      in
      let* expr = ws *> char '=' *> pexpr in

      let expr =
        match ty with None -> expr | Some ty -> Expr.Constraint (expr, ty)
      in
      return Expr.{pat= Pat.Var id; expr= Expr.Fun (args, expr)}
    in

    let psimple =
      let* pat =
        ppat
        >>= fun pat ->
        opt (ws *> char ':')
        >>= function
        | None ->
            return pat
        | Some _ ->
            pty >>| fun ty -> Pat.Constraint (pat, ty)
      in
      let* expr = ws *> char '=' *> pexpr in
      return Expr.{pat; expr}
    in

    pfun <|> psimple
  in

  let* rec_flag =
    string "let"
    *> choice
         [spaced (string "rec") *> return Expr.Rec; ws1 *> return Expr.Nonrec]
  in
  let* bindings =
    sep_by1 (spaced (string "and")) pbinding >>| List1.of_list_exn
  in

  return (rec_flag, bindings)

(* ======= Operators ======= *)

type 'oprnd operator =
  | Prefix of ('oprnd -> 'oprnd) t
  | InfixN of ('oprnd List2.t -> 'oprnd) t
  | InfixL of ('oprnd -> 'oprnd -> 'oprnd) t
  | InfixR of ('oprnd -> 'oprnd -> 'oprnd) t

let pprefix pop prhs =
  let pop_many =
    let* hd, tl = many1 pop >>| List1.of_list_exn in
    return @@ List.fold_left tl ~init:hd ~f:(fun acc f x -> acc (f x))
  in
  let* apply = option Fn.id pop_many in
  let* rhs = prhs in
  return (apply rhs)

let pinfixn (pop : ('oprnd List2.t -> 'oprnd) t) prhs fst =
  let* apply = pop in
  let* snd, tl = sep_by1 pop prhs >>| List1.of_list_exn in
  return @@ apply (fst, snd, tl)

let rec pinfixl pop prhs lhs =
  let* apply = pop in
  let* rhs = prhs in
  let lhs' = apply lhs rhs in
  pinfixl pop prhs lhs' <|> return lhs'

let rec pinfixr pop prhs lhs =
  let* apply = pop in
  let* rhs = prhs >>= fun lhs' -> pinfixr pop prhs lhs' <|> return lhs' in
  return (apply lhs rhs)

let plvl poprnd ops =
  let prefixs, infixns, infixls, infixrs =
    List.fold_left ops ~init:([], [], [], [])
      ~f:(fun (prefixs, infixns, infixls, infixrs) -> function
      | Prefix p ->
          (p :: prefixs, infixns, infixls, infixrs)
      | InfixN p ->
          (prefixs, p :: infixns, infixls, infixrs)
      | InfixL p ->
          (prefixs, infixns, p :: infixls, infixrs)
      | InfixR p ->
          (prefixs, infixns, infixls, p :: infixrs) )
  in

  let pprefix = pprefix (choice prefixs) poprnd in
  let pinfixn = pinfixn (choice infixns) pprefix in
  let pinfixl = pinfixl (choice infixls) pprefix in
  let pinfixr = pinfixr (choice infixrs) pprefix in

  let* lhs = pprefix in
  choice [pinfixr lhs; pinfixl lhs; pinfixn lhs; return lhs]

let poperators ~table ~poprnd = List.fold_left table ~init:poprnd ~f:plvl
