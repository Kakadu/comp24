open Angstrom
open Ast

let show_res ~input:i ~parser:p ~to_string:ts =
  match Angstrom.parse_string ~consume:Consume.All p i with
  | Ok rest -> ts rest
  | Error b -> Format.sprintf "Parsing error, rest: %s" b
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_uc_letter = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_lc_letter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_letter l = is_lc_letter l || is_uc_letter l

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "rec"
  | "in"
  | "match"
  | "with"
  | "fun"
  | "if"
  | "then"
  | "else"
  | "int"
  | "and"
  | "true"
  | "false" -> true
  | _ -> false
;;

let ws = take_while is_whitespace

let number =
  ws
  *> let* sign =
       peek_char_fail
       >>| function
       | '-' ->
         let _ = advance 1 in
         "-"
       | _ -> ""
     in
     let* value = take_while1 is_digit in
     sign ^ value |> int_of_string |> return
;;

let stoken s = ws *> string s
let parens p = ws *> stoken "(" *> p <* stoken ")"
let braces p = ws *> char '[' *> p <* char ']'

let ident =
  ws
  *>
  let* i =
    let* h = satisfy (fun c -> is_lc_letter c || c = '_') in
    let allowed_char c = is_digit c || is_letter c || c = '_' in
    let* tail =
      if h = '_' then many1 (satisfy allowed_char) else many (satisfy allowed_char)
    in
    return (Base.String.of_char_list (h :: tail))
  in
  if is_keyword i
  then fail (Format.sprintf "Expected identifier, got keyword %s" i)
  else return i
;;

let keyword k =
  let* _ = ws *> string k in
  let* next = peek_char in
  match next with
  | Some v when is_letter v -> fail (Format.sprintf "Not a keyword %s" k)
  | _ -> return ()
;;

(** Helpers **)
let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let rec chainr1 e op =
  e >>= fun lhs -> op >>= (fun o -> chainr1 e op >>| o lhs) <|> return lhs
;;

(** Patterns **)

let const =
  number
  >>| (fun n -> Const_int n)
  <|> keyword "true" *> (Const_bool true |> return)
  <|> keyword "false" *> (Const_bool false |> return)
;;

let tuple sep p =
  lift3 (fun fst snd rest -> fst, snd, rest) p (sep *> ws *> p) (many (sep *> ws *> p))
;;

let arrow p =
  let make_arrow a b = Typ_fun (a, b) in
  chainr1 p (stoken "->" *> return make_arrow)
;;

let typ =
  fix (fun self ->
    let int = keyword "int" >>| fun _ -> Typ_int in
    let bool = keyword "bool" >>| fun _ -> Typ_bool in
    let unit = keyword "unit" >>| fun _ -> Typ_unit in
    let atom = choice [ int; bool; unit; parens self ] in
    let typ =
      lift2
        (fun elem dims -> List.fold_left (fun t _ -> Typ_list t) elem dims)
        atom
        (many (stoken "list" <* ws))
    in
    let typ =
      tuple (stoken "*") typ
      >>| (fun (fst, snd, rest) -> Typ_tuple (fst, snd, rest))
      <|> typ
    in
    arrow typ)
;;

let typed_pattern =
  fix (fun self ->
    let atom =
      ws *> (parens ws >>| fun _ -> Pat_unit) (* unit *)
      <|> (braces ws >>| fun _ -> pnil) (* nil *)
      <|> parens self (* parens *)
      <|> (const >>| pconst) (* const *)
      <|> (ident >>| pvar) (* identifier *)
      <|> keyword "_" *> return Pat_wildcard
    in
    (* wildcard *)
    let pattern = chainr1 atom (stoken "::" *> return pcons) in
    (* cons *)
    let pattern =
      (* try tuple *)
      tuple (stoken ",") pattern
      >>| (fun (fst, snd, rest) -> ptuple fst snd rest)
      <|> pattern
    in
    let pattern =
      pattern
      >>= (fun p -> stoken ":" *> typ >>= fun t -> Pat_constrained (p, t) |> return)
            (* try type *)
      <|> pattern
    in
    pattern <* ws)
;;

(* we need this because typing of fun params is allowed only in parens,
   otherwise type constraint belongs to expression
   for example, fun (x: int) : int -> ...
   let x : int = ... *)
let untyped_pattern =
  fix (fun self ->
    let atom =
      ws *> (parens ws >>| fun _ -> Pat_unit) (* unit *)
      <|> (braces ws >>| fun _ -> pnil) (* nil *)
      <|> parens self (* parens *)
      <|> (const >>| pconst) (* const *)
      <|> (ident >>| pvar) (* identifier *)
      <|> keyword "_" *> return Pat_wildcard
    in
    (* wildcard *)
    let pattern = chainr1 atom (stoken "::" *> return pcons) in
    (* cons *)
    tuple (stoken ",") pattern
    >>| (fun (fst, snd, rest) -> ptuple fst snd rest) (* try tuple *)
    <|> pattern
    <* ws)
;;

let fun_arg = parens typed_pattern <|> untyped_pattern

(** Arithmetic  **)

let bin_expr op_string op_expr = ws *> string op_string <* ws >>= fun _ -> return op_expr
let op_list_to_parser l = List.map (fun (str, expr_op) -> bin_expr str expr_op) l

let prio =
  [ [ "*", mul; "/", div ]
  ; [ "+", add; "-", sub ]
  ; [ "=", eqq; ">=", geq; ">", ge; "<=", leq; "<", le ]
  ; [ "&&", eland ]
  ; [ "||", elor ]
  ; [ ("", fun f a -> eapp f [ a ]) ]
  ]
  |> List.map op_list_to_parser
;;

let ident_as_expr = ws *> parens ident <|> ident >>| fun i -> Expr_var i

let expr_with_ops p =
  fix (fun self ->
    let atom = parens self <|> p in
    List.fold_left (fun acc ops -> chainl1 acc (choice ops)) atom prio)
;;

(** Expressions **)

let elist p =
  ws *> char '[' *> sep_by (ws *> char ';' *> ws) p
  <* char ']'
  >>| fun elems -> List.fold_right econs elems enil
;;

let ematch expr =
  let* matching_expr = keyword "match" *> ws *> expr <* ws <* keyword "with" in
  let fst_case =
    (stoken "|" <|> ws) *> fun_arg
    >>= fun p -> stoken "->" *> ws *> expr >>= fun e -> return (p, e)
  in
  let case =
    stoken "|" *> ws *> fun_arg
    >>= fun p -> stoken "->" *> ws *> expr >>= fun e -> return (p, e)
  in
  let* fst_case = fst_case in
  let* rest_cases = many case in
  ematch matching_expr (fst_case :: rest_cases) |> return
;;

let eite expr =
  let* cond = keyword "if" *> ws *> expr in
  let* t = keyword "then" *> ws *> expr in
  let* e = keyword "else" *> ws *> expr in
  eite cond t e |> return
;;

let letdef kw erhs =
  (fun is_rec name params typ_constr rhs ->
    let rhs =
      match typ_constr with
      | Some t -> Expr_constrained (rhs, t)
      | None -> rhs
    in
    is_rec, name, List.fold_right efun params rhs)
  <$> kw *> option NonRecursive (keyword "rec" *> return Recursive)
  <*> fun_arg
  <*> many fun_arg
  <*> option None (stoken ":" *> typ >>| fun t -> Some t)
  <*> stoken "=" *> ws *> erhs
;;

let anonymous_fun expr =
  lift3
    (fun args typ_constr body ->
      match typ_constr with
      | Some t -> List.fold_right efun args (Expr_constrained (body, t))
      | None -> List.fold_right efun args body)
    (keyword "fun" *> many1 (ws *> fun_arg))
    (option None (stoken ":" *> typ >>| fun t -> Some t))
    (stoken "->" *> ws *> expr)
;;

let prefix_ops =
  choice
    [ stoken "+"
    ; stoken "-"
    ; stoken "*"
    ; stoken "/"
    ; stoken "="
    ; stoken ">"
    ; stoken ">="
    ; stoken "<"
    ; stoken "<="
    ; stoken "||"
    ; stoken "&&"
    ]
  >>| fun id -> Expr_var id
;;

let expr =
  fix (fun self ->
    let ident = ident >>| fun i -> Expr_var i in
    let const = const >>| fun c -> Expr_const c in
    let unit = parens ws >>| fun _ -> Expr_unit in
    let list = elist self in
    let atom = choice [ parens self; ident; const; unit; list; anonymous_fun self ] in
    let expr = chainl1 atom (ws *> return (fun f a -> eapp f [ a ])) in
    let expr = expr_with_ops expr in
    (* should come after apply cause of [n * f (n-1)] *)
    let expr = prefix_ops <|> expr in
    let expr = chainr1 expr (stoken "::" *> return econs) in
    (* cons *)
    let expr =
      (* tuple *)
      tuple (stoken ",") expr >>| (fun (fst, snd, rest) -> etuple fst snd rest) <|> expr
    in
    let expr =
      expr
      >>= (fun a -> stoken ":" *> typ >>= fun t -> Expr_constrained (a, t) |> return)
            (* try type *)
      <|> expr
    in
    expr (* atom *)
    <|> eite self (* ite *)
    <|> ematch self (* match *)
    <|> (let* rec_flag, name, value = letdef (keyword "let") self in
         (* local binding *)
         let* scope = stoken "in" *> self in
         elet ~rec_flag (name, value) scope |> return)
    <* ws)
;;

let program : structure t =
  let str_item =
    let* ((is_rec, _, _) as fst) = letdef (keyword "let") expr in
    let* rest = many (ws *> letdef (keyword "and") expr) in
    let bindings =
      Str_value (is_rec, fst :: rest |> List.map (fun (_, name, e) -> name, e))
    in
    return bindings
  in
  many1 str_item <* ws
;;
