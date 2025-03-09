(** Copyright 2023-2024, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Angstrom
open Ast

let is_whitespace = function
  | ' ' -> true
  | _ -> false

let is_tab = function
  | '\t' -> true
  | _ -> false

let is_newline = function
  | '\n' | '\r' -> true
  | _ -> false

let is_empty_char c = is_tab c || is_whitespace c || is_newline c

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let is_keyword = function
  | "fun" | "let" | "in" | "if" | "then" | "else" | "true" | "false" | "not" | "rec" -> true
  | _ -> false
;;

let skip_empty = skip_while is_empty_char
let take_empty1 = take_while1 is_empty_char

let parens p = char '(' *> p <* char ')';;

let integer =
  let minus = skip_empty *> string "-" *> return (-1) in
  (* let plus = skip_empty *> (string "+" <|> string "") *> return 1 in *)
  let number sign = skip_empty *> take_while1 is_digit >>= (fun num -> return (CInt (sign * int_of_string num))) in
  (* (minus <|> plus) >>= number *)
  number 1 <|> skip_empty *> parens ((minus >>= number) <* skip_empty)

let boolean1 =
  let true_w = take_empty1 *> string "true" *> return (CBool true) in
  let false_w = take_empty1 *> string "false" *> return (CBool false) in
  true_w <|> false_w

let boolean =
  let true_w = skip_empty *> string "true" *> return (CBool true) in
  let false_w = skip_empty *> string "false" *> return (CBool false) in
  true_w <|> false_w

let unit_c = parens @@ string "" *> return CUnit

let identifier = 
  let word = skip_empty *> take_while1 (fun c -> Char.equal c '_' || is_digit c || is_letter c) in
  let checker word = 
    if is_digit (String.get word 0) || is_keyword word 
    then fail "Not correct identifier"
    else return word
  in
  word >>= checker

let tuple el =
  skip_empty *> parens (lift2 (fun lst el -> List.concat [lst; [el]]) (many (el <* skip_empty <* char ',')) (el <* skip_empty))

let arguments =
  tuple identifier <|> (many identifier)

let arguments1 =
  tuple identifier <|> (many1 identifier)

let declaration is_rec =
  skip_empty *> identifier >>= 
    fun id -> skip_empty *> arguments >>= 
    fun arg_lst -> 
      if is_rec 
        then return (DeclRec (id, arg_lst))
        else return (Decl (id, arg_lst))
;;

let add = skip_empty *> char '+' *> return (fun e1 e2 -> Add (e1, e2))
let sub = skip_empty *> char '-' *> return (fun e1 e2 -> Sub (e1, e2))
let mul = skip_empty *> char '*' *> return (fun e1 e2 -> Mul (e1, e2))
let div = skip_empty *> char '/' *> return (fun e1 e2 -> Div (e1, e2))

let and_o = skip_empty *> string "&&" *> return (fun e1 e2 -> And (e1, e2))
let or_o = skip_empty *> string "||" *> return (fun e1 e2 -> Or (e1, e2))

let not_op =
  let is_parens = peek_char_fail >>= function | '(' -> return "" | _ -> take_empty1 in
  let not_o = (skip_empty *> string "not" <* is_parens) *> return (fun e -> Not e) in
  let empty_o = skip_empty *> return (fun e -> e) in
  (not_o <|> empty_o)

let compare_op =
  let gt = skip_empty *> string ">" *> return (fun e1 e2 -> Gt (e1, e2)) in
  let lt = skip_empty *> string "<" *> return (fun e1 e2 -> Lt (e1, e2)) in
  let gte = skip_empty *> string ">=" *> return (fun e1 e2 -> Gte (e1, e2)) in
  let lte = skip_empty *> string "<=" *> return (fun e1 e2 -> Lte (e1, e2)) in
  let eq = skip_empty *> string "=" *> return (fun e1 e2 -> Eq (e1, e2)) in
  let neq = skip_empty *> (string "<>" <|> string "!=") *> return (fun e1 e2 -> Not (Eq (e1, e2))) in
  (eq <|> neq <|> gte <|> lte <|> gt <|> lt)
;;

let chainl1 e op =
  let rec go acc =
    (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
  e >>= fun init -> go init
;;

let identifier_expr = identifier >>= (fun id -> return @@ Id id);;

let parse_math id_f =
  fix (fun expr ->
    let not_e ex = not_op >>= (fun f -> lift f ex) in
    let integer_e = integer >>= (fun c -> return @@ Const c) in
    let id_e = id_f in
    let bool_e = boolean >>= fun b -> return @@ Const b in
    let factor = skip_empty *> parens expr  <|> bool_e <|> integer_e <|> parens id_e <|> identifier_expr in
    let term = not_e factor in
    let term = chainl1 term (mul <|> div) in
    let term = chainl1 term (add <|> sub) in
    let term = chainl1 term compare_op in
    let term = chainl1 term and_o in
    let term = chainl1 term or_o in
    term <* skip_empty)
;;

let parse_expr =
  fix (fun expr ->
      let unit_e = unit_c >>= (fun c -> return @@ Const c) in
      let if_ex =
        let if_e ex = (skip_empty *> string "if" <* take_empty1) *> ex in
        let then_e ex = (skip_empty *> string "then" <* take_empty1) *> ex in
        let else_e ex = (skip_empty *> string "else" <* take_empty1) *> ex in
        lift3 (fun i t e -> If (i, t, e)) (if_e expr) (then_e expr) (else_e expr <|> return (Const CUnit))
      in
      let let_ex =
        let let_d = (skip_empty *> string "let" <* take_empty1) *> declaration false in
        let let_rd = (skip_empty *> string "let" *> take_empty1 *> string "rec" <* take_empty1) *> declaration true in
        let eq_e ex = skip_empty *> string "=" *> ex in
        lift2 (fun le eq -> Let (le, eq)) (let_rd <|> let_d) (eq_e expr)
      in
      let let_in_ex =
        let let_d = (skip_empty *> string "let" <* take_empty1) *> declaration false in
        let let_rd = (skip_empty *> string "let" *> take_empty1 *> string "rec" <* take_empty1) *> declaration true in
        let eq_e ex = skip_empty *> string "=" *> ex in
        let in_e ex = (skip_empty *> string "in" <* take_empty1) *> ex in
        lift3 (fun le eq i -> LetIn (le, eq, i)) (let_rd <|> let_d) (eq_e expr) (in_e expr)
      in
      let fun_ex e = 
        let fun_a = (skip_empty *> string "fun" <* take_empty1) *> arguments1 in
        let arrow_e ex = skip_empty *> string "->" *> ex in
        lift2 (fun a f -> Fun (a, f)) fun_a (arrow_e e)
      in
      let app_ex e = 
        let args_app =
          many1 @@ ((parens@@fun_ex e) <|> e)
        in
        let id = skip_empty *> (parens@@fun_ex e) <|> identifier_expr in
        let args = skip_empty *> args_app in
        lift2 (fun id args -> App(id, args)) id args
      in
      let parse_math2 =
        fix (fun m_expr ->
          let not_e ex = not_op >>= (fun f -> lift f ex) in
          let integer_e = integer >>= (fun c -> return @@ Const c) in
          let bool_e = boolean >>= fun b -> return @@ Const b in
          let factor = take_empty1 *> m_expr <|> parens m_expr <|> bool_e <|> integer_e <|> 
            (parens@@app_ex m_expr) <|> (parens@@fun_ex expr) <|> identifier_expr 
          in
          let term = not_e factor in
          let term = chainl1 term (mul <|> div) in
          let term = chainl1 term (add <|> sub) in
          let term = chainl1 term compare_op in
          let term = chainl1 term and_o in
          let term = chainl1 term or_o in
          term <* skip_empty)
      in
      take_empty1 *> expr <|> parens expr <|> 
      let_in_ex <|> let_ex <|> if_ex <|> 
        parse_math2 <|> (app_ex expr) <|> (fun_ex expr) <|> unit_e
       (* <|> (parse_math identifier_expr) *)
    )
;;

let parse_exprs = 
  many (parse_expr <* (string ";;" <|> string "")) <* skip_empty
;;

let parser str = parse_string ~consume:Consume.All parse_exprs str;;

(*=============================*)
(*============TESTS============*)
(*=============================*)

let test_ok parser input expected =
  match parse_string ~consume:Consume.All parser input with
  | Ok res when res = expected -> true
  | Ok _ ->
    Printf.printf "%s\n" input;
    false
  | Error e ->
    Printf.printf "%s\n" e;
    false
;;

let test_fail parser input =
  match parse_string ~consume:Consume.All parser input with
  | Ok _ ->
    Printf.printf "%s\n" input;
    false
  | Error _ -> true
;;


(*== Test parse integer ==*)
let parse_ok = test_ok integer
let parse_fail = test_fail integer

let%test _ = parse_ok " 1" (CInt 1)
let%test _ = parse_ok " (-1234567890)" (CInt (-1234567890))
let%test _ = parse_ok " 0901" (CInt 901)
let%test _ = parse_ok "( - 0001000 )" (CInt (-1000))

let%test _ = parse_fail ""
let%test _ = parse_fail "aa"
let%test _ = parse_fail " "


(*== Test parse boolean ==*)
let parse_ok = test_ok boolean1
let parse_fail = test_fail boolean1

let%test _ = parse_ok " true" (CBool true)
let%test _ = parse_ok " false" (CBool false)

let%test _ = parse_fail "1"
let%test _ = parse_fail "aa"
let%test _ = parse_fail " "
let%test _ = parse_fail ""


(*== Test parse identifier ==*)
let parse_ok = test_ok identifier
let parse_fail = test_fail identifier

let%test _ = parse_ok " _" "_"
let%test _ = parse_ok " a" "a"
let%test _ = parse_ok "qwertyuiopasdfghjklzxcvbnm1234567890_" "qwertyuiopasdfghjklzxcvbnm1234567890_"
let%test _ = parse_ok "true_" "true_"
let%test _ = parse_ok "false_" "false_"
let%test _ = parse_ok "fun_" "fun_"
let%test _ = parse_ok "let_" "let_"
let%test _ = parse_ok "if_" "if_"
let%test _ = parse_ok "then_" "then_"
let%test _ = parse_ok "else_" "else_"
let%test _ = parse_ok "in_" "in_"
let%test _ = parse_ok "not_" "not_"
let%test _ = parse_ok "rec_" "rec_"

let%test _ = parse_fail "1"
let%test _ = parse_fail "1a"
let%test _ = parse_fail "true"
let%test _ = parse_fail "false"
let%test _ = parse_fail "fun"
let%test _ = parse_fail "let"
let%test _ = parse_fail "if"
let%test _ = parse_fail "then"
let%test _ = parse_fail "else"
let%test _ = parse_fail "in"
let%test _ = parse_fail "not"
let%test _ = parse_fail "rec"
let%test _ = parse_fail " "
let%test _ = parse_fail ""

(*== Test parse tuple ==*)
let common_tuple =
  let constant_e = (boolean <|> integer <|> unit_c) >>= (fun c -> return @@ Const c) in
  let identifier_e = identifier >>= (fun id -> return @@ Id id) in
  let tuple_el = skip_empty *> (constant_e <|> identifier_e) in
  tuple tuple_el

let parse_ok = test_ok common_tuple
let parse_fail = test_fail common_tuple

let%test _ = parse_ok "(())" [(Const CUnit)]
let%test _ = parse_ok "(true)" [(Const(CBool true))]
let%test _ = parse_ok "(1)" [(Const(CInt 1))]
let%test _ = parse_ok "(a)" [(Id "a")]
let%test _ = parse_ok " ( a )" [(Id "a")]
let%test _ = parse_ok "(a, b)" [(Id "a"); (Id "b")]
let%test _ = parse_ok " ( a , b )" [(Id "a"); (Id "b")]

let%test _ = parse_fail "(a b)"
let%test _ = parse_fail "(a, )"
let%test _ = parse_fail "()"

(*== Test parse argiments ==*)
(* TODO: arguments can accept Const(CUnit, CInt, CBool) and Id(string) *)

let parse_ok = test_ok arguments
let parse_fail = test_fail arguments

let%test _ = parse_ok " _" ["_"]
let%test _ = parse_ok " _ _" ["_"; "_"]
let%test _ = parse_ok "a b" ["a"; "b"]
let%test _ = parse_ok "" []
let%test _ = parse_ok "(a)" ["a"]
let%test _ = parse_ok "(a, b)" ["a"; "b"]

let%test _ = parse_fail "(a b)"
let%test _ = parse_fail "(a, )"
let%test _ = parse_fail "()"

(*== Test parse declaration ==*)
let parse_ok flag = test_ok @@ declaration flag
let parse_fail flag = test_fail @@ declaration flag

let%test _ = parse_ok true "_" (DeclRec ("_", []))
let%test _ = parse_ok false "a" (Decl ("a", []))
let%test _ = parse_ok true "a b c" (DeclRec ("a", ["b"; "c"]))
let%test _ = parse_ok false "a b c" (Decl ("a", ["b"; "c"]))

let%test _ = parse_fail true ""
let%test _ = parse_fail false ""


(*== Test parse math operations ==*)
let parse_ok = test_ok (parse_math identifier_expr)
let parse_fail = test_fail (parse_math identifier_expr)

let%test _ = parse_ok " ( a )" (Id "a")
let%test _ = parse_ok " a" (Id "a")
let%test _ = parse_ok "(a)" (Id "a")
let%test _ = parse_ok " (1)" (Const (CInt 1))
let%test _ = parse_ok " 1" (Const (CInt 1))
let%test _ = parse_ok "(1)" (Const (CInt 1))

let%test _ = parse_ok " a + 2" (Add (Id "a", Const(CInt 2)))
let%test _ = parse_ok " a - 2" (Sub (Id "a", Const(CInt 2)))
let%test _ = parse_ok " a * 2" (Mul (Id "a", Const(CInt 2)))
let%test _ = parse_ok " a / 2" (Div (Id "a", Const(CInt 2)))

let%test _ = parse_ok " not a" (Not (Id "a"))
let%test _ = parse_ok " not (a)" (Not (Id "a"))
let%test _ = parse_ok "not(a)" (Not (Id "a"))
let%test _ = parse_ok " a && b" (And (Id "a", Id "b"))
let%test _ = parse_ok " a || b" (Or (Id "a", Id "b"))

let%test _ = parse_ok " a = b" (Eq (Id "a", Id "b"))
let%test _ = parse_ok " a != b" (Not (Eq (Id "a", Id "b")))
let%test _ = parse_ok " a <> b" (Not (Eq (Id "a", Id "b")))
let%test _ = parse_ok " not (a = b)" (Not (Eq (Id "a", Id "b")))
let%test _ = parse_ok " a > b" (Gt (Id "a", Id "b"))
let%test _ = parse_ok " a >= b" (Gte (Id "a", Id "b"))
let%test _ = parse_ok " a < b" (Lt (Id "a", Id "b"))
let%test _ = parse_ok " a <= b" (Lte (Id "a", Id "b"))
(*Test parse math priority*)
let%test _ = parse_ok " a - 2+b" (Add(Sub (Id "a", Const(CInt 2)), Id "b"))
let%test _ = parse_ok " a + 2-b" (Sub(Add (Id "a", Const(CInt 2)), Id "b"))
let%test _ = parse_ok " a + 2*b" (Add (Id "a", Mul(Const(CInt 2), Id "b")))
let%test _ = parse_ok " a / 2*b" (Mul(Div (Id "a", Const(CInt 2)), Id "b"))
let%test _ = parse_ok "\n\n a*2/b" (Div(Mul (Id "a", Const(CInt 2)), Id "b"))
let%test _ = parse_ok " (a + 2)*b" (Mul(Add (Id "a", Const(CInt 2)), Id "b"))
let%test _ = parse_ok " not(not (a) + 2)*b" (Mul(Not (Add (Not (Id "a"), Const(CInt 2))), Id "b"))
let%test _ = parse_ok " not(not((a+(3/2-1)))+2)*b" 
  (Mul(Not (Add (Not (Add(Id "a", Sub(Div(Const(CInt 3), Const(CInt 2)), Const(CInt 1)))), Const(CInt 2))), Id "b"))
let%test _ = parse_ok " a + 2 = b * 3" (Eq (Add(Id "a", Const (CInt 2)), Mul(Id "b", Const (CInt 3))))
let%test _ = parse_ok " a \n+ 2 > b * 3" (Gt (Add(Id "a", Const (CInt 2)), Mul(Id "b", Const (CInt 3))))
let%test _ = parse_ok " a + 2\n >= b * 3" (Gte (Add(Id "a", Const (CInt 2)), Mul(Id "b", Const (CInt 3))))
let%test _ = parse_ok " a + 2 != b * 3" (Not (Eq (Add(Id "a", Const (CInt 2)), Mul(Id "b", Const (CInt 3)))))
let%test _ = parse_ok " a + 2 < b \t* 3" (Lt (Add(Id "a", Const (CInt 2)), Mul(Id "b", Const (CInt 3))))
let%test _ = parse_ok " a + 2 \n\n<= b * 3" (Lte (Add(Id "a", Const (CInt 2)), Mul(Id "b", Const (CInt 3))))
let%test _ = parse_ok " a < 2 && b = 3" (And(Lt (Id "a", Const(CInt 2)), Eq(Id "b", Const(CInt 3))))
let%test _ = parse_ok " a < 2 || b = 3" (Or(Lt (Id "a", Const(CInt 2)), Eq(Id "b", Const(CInt 3))))
let%test _ = parse_ok "a && b||c" (Or(And(Id "a", Id "b"), Id "c"))

let%test _ = parse_fail "(a && b||c"
let%test _ = parse_fail "a + "
let%test _ = parse_fail " + "
let%test _ = parse_fail " + a"
let%test _ = parse_fail "not"
let%test _ = parse_fail "a && b||c)"

(*== Test parse apply ==*)

let parse_ok = test_ok parse_expr
let%test _ = parse_ok "let c = (a 1 b w c (fun x -> x))" 
  (Let(Decl("c",[]), 
    App (Id "a", [Const(CInt 1); Id "b"; Id "w"; Id "c"; Fun(["x"], Id "x")])))
(* let parse_ok_anon = test_ok (parse_expr) *)
(* let%test _ = parse_ok_anon "(fun x -> x)" (Fun(["x"], Id "x")) *)


(*== Test parse common expretion ==*)
let parse_ok = test_ok parse_expr
let parse_fail = test_fail parse_expr

let%test _ = parse_ok " a + 2 \n\n<= b * 3" (Lte (Add(Id "a", Const (CInt 2)), Mul(Id "b", Const (CInt 3))))
let%test _ = parse_ok "a < 2 && b = 3" (And(Lt (Id "a", Const(CInt 2)), Eq(Id "b", Const(CInt 3))))
let%test _ = parse_ok " a < 2 || b = 3" (Or(Lt (Id "a", Const(CInt 2)), Eq(Id "b", Const(CInt 3))))
let%test _ = parse_ok " a && b||c" (Or(And(Id "a", Id "b"), Id "c"))

(* let%test _ = parse_ok "(a b) * 3 + (b 1 (a f 2) (1 + a))" (Id "") *)
let%test _ = parse_ok "(a b 2 1+3 * b d (-2) (r f)) + 3" 
  (Add(App(Id "a", [Id "b"; Const(CInt 2); Add(Const(CInt 1), Mul(Const(CInt 3), Id "b")); Id "d"; Const(CInt (-2)); App(Id "r", [Id "f"])]), Const(CInt 3)))
let%test _ = parse_ok "a + (f 2 x) * 3" (Add(Id "a", Mul(App(Id "f", [Const(CInt 2); Id "x"]), Const(CInt 3))))
let%test _ = parse_ok "(a + (f 2 x (g 3 y)) * 3)" (Add(Id "a", Mul(App(Id "f", [Const(CInt 2); Id "x"; App(Id "g", [Const(CInt 3); Id "y"])]), Const(CInt 3))))
(* (a + (f 2 x (g (3*z) y)) * 3) - not work *)
let%test _ = parse_ok "(a + (f 2 x (g 3*z y)) * 3)" 
  (Add(Id "a", Mul(App(Id "f", [Const(CInt 2); Id "x"; App(Id "g", [Mul(Const(CInt 3), Id "z"); Id "y"])]), Const(CInt 3))))
let%test _ = parse_ok "true && (a + (f false (g 3 y)) = 3  || 2)" 
  (And(Const(CBool true),
      Or(Eq((Add(Id "a", App(Id "f", [Const(CBool false); App(Id "g", [Const(CInt 3); Id "y"])]))), Const(CInt 3)),
      Const(CInt 2))))


let%test _ = parse_ok "()" (Const CUnit)
let%test _ = parse_ok "fun \n( a , c ) -> b" (Fun (["a"; "c"], Id "b"))
let%test _ = parse_ok "(fun a -> b)" (Fun (["a"], Id "b"))
let%test _ = parse_ok "let a (c, d) = (b)" (Let (Decl("a", ["c"; "d"]), Id "b"))
let%test _ = parse_ok "fun (a, b) -> c" (Fun (["a"; "b"], Id "c"))
let%test _ = parse_ok " let rec a = ()" (Let (DeclRec("a", []), Const CUnit))
let%test _ = parse_ok "let a = (b) in c" (LetIn (Decl("a", []), Id "b", Id "c"))
let%test _ = parse_ok "\nlet rec a = b in (c)" (LetIn (DeclRec("a", []), Id "b", Id "c"))
let%test _ = parse_ok "\nif a then b else c" (If (Id "a", Id "b", Id "c"))
let%test _ = parse_ok "if a then b" (If (Id "a", Id "b", Const CUnit))
let%test _ = parse_ok "\n(let a = b)" (Let (Decl("a", []), Id "b"))
let%test _ = parse_ok "let a = \nlet b = 1 in\n\t let c = b in\n\t c" 
  (Let (Decl("a", []), LetIn (Decl("b", []), Const (CInt 1), LetIn (Decl("c", []), Id "b", Id "c"))))
let%test _ = parse_ok "let a = let b = 1 in let c = b in c" 
  (Let (Decl("a", []), LetIn (Decl("b", []), Const (CInt 1), LetIn (Decl("c", []), Id "b", Id "c"))))


let%test _ = parse_fail "fun -> b"
let%test _ = parse_fail "(let a = b"
let%test _ = parse_fail "let a = b)"
let%test _ = parse_fail "let = b"
let%test _ = parse_fail "let a = "
let%test _ = parse_fail "let (a) = b"
let%test _ = parse_fail "let () = b"
let%test _ = parse_fail "let rec = b"
let%test _ = parse_fail "let rec a = "
let%test _ = parse_fail "let = b in c"
let%test _ = parse_fail "let a =  in c"
let%test _ = parse_fail "let a = b in "
let%test _ = parse_fail "let rec = b in c"
let%test _ = parse_fail "let rec a = in c"
let%test _ = parse_fail "let rec a = b in "
let%test _ = parse_fail "if a else b"


(*== Test parse ==*)
let parse_ok = test_ok parse_exprs
(*let parse_fail = test_fail parse_exprs*)

let%test _ = parse_ok "let a = \nlet b = 1 in\n\t let c = b in\n\t c" 
  [(Let (Decl("a", []), LetIn (Decl("b", []), Const (CInt 1), LetIn (Decl("c", []), Id "b", Id "c"))))]
let%test _ = parse_ok "let a = \nlet b = 1 in\n\t let c = b in\n\t c;;" 
  [(Let (Decl("a", []), LetIn (Decl("b", []), Const (CInt 1), LetIn (Decl("c", []), Id "b", Id "c"))))]
let%test _ = parse_ok "\n\t \n" []
let%test _ = parse_ok "" []
let%test _ = parse_ok "let a = (b) let a = b;;" 
  [(Let (Decl("a", []), Id "b"));
    (Let (Decl("a", []), Id "b"))]
let%test _ = parse_ok "let a = b;; let a c d = b" 
  [(Let (Decl("a", []), Id "b")); 
    (Let (Decl("a", ["c"; "d"]), Id "b"))]
let%test _ = parse_ok "let a = b in c;; let a = b let a = b in c" 
  [LetIn (Decl("a", []), Id "b", Id "c"); 
    Let (Decl("a", []), Id "b"); 
    LetIn (Decl("a", []), Id "b", Id "c")]
let%test _ = parse_ok "let a b c = 1;; (a b c 1 (c + 2));; let b = (a c)" 
  [Let(Decl("a", ["b"; "c"]), Const(CInt 1)); 
  App(Id "a", [Id "b"; Id "c"; Const(CInt 1); Add(Id "c", Const(CInt 2))]); 
  Let(Decl("b", []), App(Id "a", [Id "c"]))]
let%test _ = parse_ok "let a = let b = 1 in let c = 2 in (d 3)" 
  [Let(Decl("a", []), 
    LetIn(Decl("b", []), 
      Const(CInt 1), 
      LetIn(Decl("c",[]), 
        Const(CInt 2), 
        App(Id "d", [Const(CInt 3)]))))]

let%test _ = parse_fail ";;"
