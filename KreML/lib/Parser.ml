open Angstrom
open Ast

type parse_result =
  | Ok of Ast.expr
  | Err of string

let show_res ~input:i ~parser:p ~to_string:ts =
      match Angstrom.parse_string ~consume:Consume.All p i with Ok rest -> ts rest | Error b  -> Format.sprintf "Parsing error, rest: %s" b

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_uc_letter = function | 'A'..'Z' -> true | _ -> false
let is_lc_letter = function | 'a'..'z' -> true | _ -> false
let is_letter l = is_lc_letter l || is_uc_letter l

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_keyword = function
  | "let" | "rec" | "in" | "match" | "with" | "fun" | "if" | "then" | "else"
  | "int" | "string" | "and"
  | "true" | "false" -> true
  | _ -> false
let ws = take_while is_whitespace

let number =
  ws *> let* sign = peek_char_fail >>| (function
    | '-' -> advance 1 |> ignore; "-"
    | _ -> "") in 
  let* value = take_while1 is_digit in
  sign ^ value |> int_of_string |> return

let string =
  ws *> char '"' *> take_while (fun c -> c <> '"') <* char '"'

let parens p = ws *> char '(' *> p <* char ')'

let ident =
    ws *>
    let* i =
      let* h = satisfy is_lc_letter in
      let allowed_char c = is_digit c || is_letter c || c = '_' in
      let* tail = many (satisfy allowed_char) in
      return (Base.String.of_char_list  (h::tail))
    in
    if is_keyword i then
       fail (Format.sprintf "Expected identifier, got keyword %s" i)
    else return i

let stoken s = ws *> Angstrom.string s

let keyword k =
  let* _ = ws *> Angstrom.string k in
  let* next = peek_char in
  match next with
  | Some v when is_letter v -> fail (Format.sprintf "Not a keyword %s" k)
  | _ -> return ()

(** Helpers **)
let chainl1 e op =
  let rec go acc =
    (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc
  in 
  e >>= fun init -> go init

let rec chainr1 e op =
  e >>= fun lhs -> op >>= (fun o -> chainr1 e op >>| o lhs) <|> return lhs

(** Patterns **)

let const =
  (string >>| fun s -> Const_string s)
  <|> (number >>| fun n -> Const_int n)
  <|> (keyword "true" *> (Const_bool true |> return ))
  <|> (keyword "false" *> (Const_bool false |> return ))

let pattern =
  fix (fun self ->
        let atom =
          ws *> (parens self (* parens *)
          <|> (const >>| pconst)  (* const *)
          <|> (ident >>| fun i -> Id i |> pvar) (* identifier *)
          <|> (keyword "_"  *> return Pat_wildcard)) in (* wildcard *)
        let cons_pat = chainr1 atom (stoken "::" *> return pcons) in
        let tuple_pat = 
          lift3 (fun fst snd rest -> ptuple fst snd rest)
          cons_pat
          (stoken "," *> cons_pat)
          (many (stoken "," *> cons_pat)) in
        tuple_pat <|> cons_pat
    )

(** Arithmetic  **)

let bin_expr op_string op_expr = ws *> Angstrom.string op_string <*ws >>= fun _ -> return op_expr
let op_list_to_parser l =
   List.map (fun (str, expr_op) -> bin_expr str expr_op) l

let prio = [["*", mul; "/", div]
          ;["+", add; "-", sub]
          ; ["=", eqq; ">=", geq; ">", ge; "<=", leq; "<", le]
          ; ["&&", eland]
          ; ["||", elor]]
  |> List.map (fun ops -> op_list_to_parser ops)

let ident_as_expr = ws *> parens ident <|> ident >>| (fun i -> Expr_var (Id i))

let expr_with_ops p =
   fix (fun self ->
    let atom = parens self <|> p in
    List.fold_left (fun acc ops -> chainl1 acc (choice ops)) atom prio
  )  

(** Expressions **)

let elist p =
  ws *> char '[' *> sep_by (ws *> char ';' *> ws) p <* char ']' >>| fun elems ->
    List.fold_right econs elems enil

let ematch expr =
  let* matching_expr = keyword "match" *> ws *> expr <* ws <* keyword "with" in
  let case = stoken "|" *> ws *> pattern >>= fun p -> stoken "->" *> ws *> expr >>= fun e -> return (p, e) in
  let* cases = many1 case in
  ematch matching_expr cases |> return

let eite expr =
  let* cond = keyword "if" *> ws *> expr in
  let* t = keyword "then" *> ws *> expr in
  let* e = keyword "else" *> ws *> expr in
  eite cond t e |> return

let letdef erhs =
  lift4 (fun is_rec name params rhs -> is_rec, name, List.fold_right efun params rhs)
  (keyword "let" *> option NonRecursive (keyword "rec" *> return Recursive))
  pattern
  (many pattern)
  (stoken "=" *> ws *> erhs)

(* let list_expr t = (5 + 5 - 8) + (6 - 5) / 2::[] *)

let anonymous_fun expr =
  lift2 (fun args body -> List.fold_right efun args body)
  (stoken "fun" *> many1 (ws *> pattern))
  (stoken "->" *> ws *> expr)
;;

let expr =
  fix (fun self ->
    let ident = (ident >>= fun i -> Expr_var (Id i) |> return) in
    let const = (const >>= fun c -> Expr_const c |> return) in
    let list = elist self in
    let app =
      (lift3 (fun i fst_arg rest_args -> eapp i (fst_arg, rest_args))
      ident
      self
      (many self)) in
    let simple_value = choice [app; ident; const; list; parens self; anonymous_fun self;] in
    let simple_value_ops = expr_with_ops simple_value in
    let complex_value = chainr1 simple_value_ops (stoken "::" *> return econs) in (* cons*)
    let complex_value = (* tuple *)
      (lift3 (fun fst snd rest -> etuple fst snd rest)
      complex_value
      (stoken "," *> ws *> complex_value)
      (many (stoken "," *> ws *> complex_value ))) 
      <|> complex_value
    in
    parens self
    <|> eite self (* ite *)
    <|> ematch self (* match *)
    <|> (letdef self >>= fun (rec_flag, name, value) -> (* local binding *)
      stoken "in" *> self >>= fun scope -> elet ~rec_flag (name, value) scope |> return)
    <|> complex_value (* atom *) 
    <|> fail "undefined"
  )

let program =
  let str_item =
    let* (is_rec, _, _) as fst = letdef expr in
    let* rest = many (keyword "and" *> letdef expr) in
    let bindings = Str_value(is_rec, (fst::rest) |> List.map (fun (_, name, e) -> name, e)) in
    return bindings
  in
  many1 str_item >>= return
