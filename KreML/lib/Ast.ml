type ident = string
[@@deriving show]

type type_id = int
[@@deriving show]

type typ =
  | Typ_int
  | Typ_bool
  | Typ_var of type_id
  | Typ_list of typ
  | Typ_tuple of typ * typ * typ list
  | Typ_fun of typ * typ
  | Typ_unit
[@@deriving show]

let tfun l r = Typ_fun(l, r)

type const =
    | Const_int of int
    | Const_bool of bool
[@@deriving show]


type pattern =
    | Pat_const of const
    | Pat_var of ident
    | Pat_cons of pattern * pattern
    | Pat_nil
    | Pat_tuple of pattern * pattern * pattern list
    | Pat_wildcard
    | Pat_constrained of pattern * typ
    | Pat_unit
[@@deriving show]

let pconst c = Pat_const c
let pvar v = Pat_var v
let pcons x xs = Pat_cons(x, xs)
let pnil = Pat_nil
let ptuple a b rest = Pat_tuple(a, b, rest)

type rec_flag = Recursive | NonRecursive
[@@deriving show]


type expr =
    | Expr_const of const
    | Expr_var of ident
    | Expr_cons of expr * expr
    | Expr_nil
    | Expr_tuple of expr * expr * expr list
    | Expr_let of rec_flag * binding * expr
    | Expr_ite of expr * expr * expr
    | Expr_fun of pattern * expr
    | Expr_match of expr * case list
    | Expr_app of expr * expr
    | Expr_constrained of expr * typ
    | Expr_unit
[@@deriving show]



and binding = pattern * expr
[@@deriving show]

and case = pattern * expr
[@@deriving show]

type structure_item =
| Str_value of rec_flag * binding list
[@@deriving show]

type structure = structure_item list
[@@deriving show]

let eapp func args =
    (* let fmt = Stdlib.Format.std_formatter in *)
    (* let() = Format.fprintf fmt "fun:" in 
    let() =  pp_expr  fmt func in
    let() = Format.fprintf fmt "args:" in 
    let () = List.iter (fun arg -> (pp_expr fmt arg)) args in *)
    Base.List.fold_left args ~init:func ~f:(fun acc arg -> Expr_app(acc, arg))

let econs x y = Expr_cons(x, y)
let enil = Expr_nil
let etuple fst snd rest = Expr_tuple(fst, snd, rest)
let eite c e t = Expr_ite(c, e, t)
let efun p body = Expr_fun(p, body)
let elet ?(rec_flag = NonRecursive) (pattern, binding) where =
     Expr_let(rec_flag, (pattern, binding), where)
let ematch expr bindings = Expr_match(expr, bindings)
let eland x y = eapp (Expr_var "&&") [x; y]
let elor x y = eapp (Expr_var "||") [x; y]
let add x y = eapp (Expr_var "+") [x; y]
let mul x y = eapp (Expr_var "*") [x; y]
let div x y = eapp (Expr_var "/") [x; y]
let sub x y = eapp (Expr_var "-") [x; y]
let eqq x y = eapp (Expr_var "=") [x; y]
let ge x y = eapp (Expr_var ">") [x; y]
let le x y = eapp (Expr_var "<") [x; y]
let geq x y = eapp (Expr_var  ">=") [x; y]
let leq x y = eapp (Expr_var "<=") [x; y]



open Stdlib.Format

let kwd ppf s = fprintf ppf "%s" s

let rec pp_tuple tuple_elems pp_elem pp_delim =
    match tuple_elems with
    | [] -> failwith "111"
    | [e] -> pp_elem e
    | e::es ->
        pp_elem e;
        pp_delim();
        pp_tuple es pp_elem pp_delim


let rec pp_typ fmt = function
  | Typ_bool -> fprintf fmt "bool"
  | Typ_int -> fprintf fmt "int"
  | Typ_unit -> fprintf fmt "unit"
  | Typ_var id -> fprintf fmt "%d" id
  | Typ_fun(Typ_fun(_, _) as farg, y) -> fprintf fmt "(%a) -> %a" pp_typ farg pp_typ y;
  | Typ_fun(x, y) -> fprintf fmt "%a -> %a" pp_typ x pp_typ y;
  | Typ_list x -> fprintf fmt "%a list" pp_typ x
  | Typ_tuple(fst, snd, rest) ->
    pp_tuple (fst::snd::rest)
    (fun e -> fprintf fmt "%a" pp_typ e)
    (fun () -> fprintf fmt " * ")

let rec pp_pat ppf = function
    | Pat_nil -> fprintf ppf "[]"
    | Pat_unit -> fprintf ppf "()"
    | Pat_wildcard -> fprintf ppf "_"
    | Pat_var id -> fprintf ppf "%s" id
    | Pat_const(Const_bool b) -> fprintf ppf "%b" b
    | Pat_const(Const_int i) -> fprintf ppf "%i" i
    | Pat_tuple(fst, snd, rest) ->
         pp_tuple (fst::snd::rest)
         (fun e -> fprintf ppf "%a" pp_pat e)
         (fun () -> fprintf ppf ", ")
    | Pat_cons(x, xs) -> fprintf ppf "@[(%a::%a)@]" pp_pat x pp_pat xs
    | Pat_constrained(x, typ) -> fprintf ppf "@[%a @:@ %a @]" pp_pat x pp_typ typ

let rec pp_expr ppf = function
    | Expr_nil -> fprintf ppf "[]"
    | Expr_unit -> fprintf ppf "()"
    | Expr_var id -> fprintf ppf "%s" id
    | Expr_const(Const_bool b) -> fprintf ppf "%b" b
    | Expr_const(Const_int i) -> fprintf ppf "%i" i
    | Expr_cons(x, xs) -> fprintf ppf "@[%a::%a @]" pp_expr x pp_expr xs
    | Expr_fun(f, arg) -> fprintf ppf "@[%a @ (%a) @ %a @ %a]"
        kwd "fun" 
        pp_pat f
        kwd "->"
        pp_expr arg
    | Expr_tuple(fst, snd, rest) ->
            pp_tuple (fst::snd::rest)
            (fun e -> fprintf ppf "%a" pp_expr e)
            (fun () -> fprintf ppf ", ")
    | Expr_ite(c, t, e) ->
        fprintf ppf "@[if %a then %a @, else %a @]" pp_expr c pp_expr t pp_expr e
    | Expr_app(f, (Expr_nil | Expr_unit | Expr_var _ | Expr_const _ as arg)) ->
        fprintf ppf "@[%a %a]" pp_expr f pp_expr arg
    | Expr_app(f, arg) -> fprintf ppf "@[%a (%a)]" pp_expr f pp_expr arg
    | Expr_let(NonRecursive, (p, e), scope) ->
        fprintf ppf "@[let %a = %a in @, %a @]"
        pp_pat p
        pp_expr e
        pp_expr scope
    | Expr_let(Recursive, (p, e), scope) ->
        fprintf ppf "@[let rec %a = %a in @, %a @]"
        pp_pat p
        pp_expr e
        pp_expr scope
    | Expr_constrained(e, t) -> fprintf ppf "@[%a : %a@]" pp_expr e pp_typ t
    | Expr_match(e, cases) ->
        fprintf ppf "@[match %a with @,@]" pp_expr e;
        List.iter
            (fun (p, e) -> fprintf ppf "@[| %a -> %a @,@]" pp_pat p pp_expr e)
            cases

let pp_structure ppf s =
    let item_printer = function
    | Str_value(NonRecursive, [id, e]) ->
        fprintf ppf "@[let %a = @, %a @]" pp_pat id pp_expr e
    | Str_value(Recursive, (p, e)::rest) ->
        fprintf ppf "@[let %a = @, %a @,@]" pp_pat p pp_expr e;
        List.iter
         (fun (p, e) -> fprintf ppf "@[and %a = %a @,@]" pp_pat p pp_expr e)
         rest
    | _ -> Utils.unreachable in
    List.iter item_printer s



