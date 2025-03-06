(** Copyright 2024-2025, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** (a-z|_) {a-z|0-9|_}*)
type id = string [@@deriving show { with_path = false }]

(** Used in {let} expr *)
type rec_flag =
  | Rec
  | Nonrec
[@@deriving show { with_path = false }]

type const =
  | CInt of int (** 123 *)
  | CBool of bool (** true | false *)
  | CString of string (** "string" *)
  | CUnit (** () *)
  | CNil (** [] *)
[@@deriving show { with_path = false }]

type type_annot =
  | AInt (** x : int *)
  | ABool (** b : bool *)
  | AString (** s : string *)
  | AUnit (** u : unit*)
  | AList of type_annot (** l : int list *)
  | AFun of type_annot * type_annot (** f : int -> int list *)
  | ATuple of type_annot list (** t : int * int *)
  | AVar of id (** x : 'a *)
[@@deriving show { with_path = false }]

type pattern =
  | PAny (** _ *)
  | PConst of const (** 123, true, "string" *)
  | PVar of id (** x *)
  | PTuple of pattern list (** p1,..., pn *)
  | PCons of pattern * pattern (** p1 :: p2 *)
  | PConstraint of pattern * type_annot (** p : int list *)
[@@deriving show { with_path = false }]

type expr =
  | EConst of const (** 123, true, "string" *)
  | EVar of id (** x *)
  | EIf of expr * expr * expr (** if e1 then e2 else e3 *)
  | EMatch of expr * case list (** match e with p1 -> e1 |...| pn -> en *)
  | ELet of rec_flag * binding * expr (** let x = e1 in e2 *)
  | EFun of pattern * expr (** fun p -> e *)
  | ETuple of expr list (** a, b, c *)
  | ECons of expr * expr (** x :: xs | [x1; x2]*)
  | EApply of expr * expr (** f e *)
  | EConstraint of expr * type_annot (** e : int list *)
[@@deriving show { with_path = false }]

(** Used in {match} expr *)
and case = pattern * expr [@@deriving show { with_path = false }]

(** Used in {let} expr *)
and binding = pattern * expr [@@deriving show { with_path = false }]

type str_item =
  | SValue of rec_flag * binding list (** let [rec] p1 = e1 and p2 = e2 and ... *)
[@@deriving show { with_path = false }]

let constr_apply f args = List.fold_left (fun f arg -> EApply (f, arg)) f args
let constr_fun pl e = List.fold_right (fun p e -> EFun (p, e)) pl e

(** Sequence of structure items *)
type structure = str_item list [@@deriving show { with_path = false }]

let fmt_const ppf c =
  let fprintf x = Format.fprintf ppf x in
  match c with
  | CInt i -> fprintf "%d" i
  | CBool false -> fprintf "false"
  | CBool true -> fprintf "true"
  | CNil -> fprintf "[]"
  | CUnit -> fprintf "()"
  | CString s -> fprintf "\"%s\"" s
;;

let fmt_list pp_elem ppf lst =
  List.iteri
    (fun i pat ->
      if i <> 0 then Format.fprintf ppf ", " else ();
      pp_elem ppf pat)
    lst
;;

let rec fmt_pattern ppf pat =
  let fprintf x = Format.fprintf ppf x in
  match pat with
  | PAny -> fprintf "_"
  | PCons (h_pat, t_pat) -> fprintf "(%a :: %a)" fmt_pattern h_pat fmt_pattern t_pat
  | PVar x -> fprintf "%s" x
  | PTuple lst -> fprintf "(%a)" (fmt_list fmt_pattern) lst
  | PConst c -> fmt_const ppf c
  | PConstraint (pat, tp) -> fprintf "(%a : %a)" fmt_pattern pat pp_type_annot tp
;;

let fmt_rec_flag ppf = function
  | Rec -> Format.fprintf ppf " rec"
  | Nonrec -> ()
;;

let rec fmt_expr ppf exp =
  let fprintf x = Format.fprintf ppf x in
  match exp with
  | EConst c -> fmt_const ppf c
  | EVar s -> fprintf "%s" s
  | EFun (pat, exp) -> fprintf "(fun %a -> %a)" fmt_pattern pat fmt_expr exp
  | EApply (exp1, exp2) -> fprintf "(%a %a)" fmt_expr exp1 fmt_expr exp2
  | EIf (exp_cond, exp_then, exp_else) ->
    fprintf
      "(if %a then %a else %a)"
      fmt_expr
      exp_cond
      fmt_expr
      exp_then
      fmt_expr
      exp_else
  | ELet (rec_f, (pat, exp_val), exp_body) ->
    fprintf
      "(let%a %a = %a in %a)"
      fmt_rec_flag
      rec_f
      fmt_pattern
      pat
      fmt_expr
      exp_val
      fmt_expr
      exp_body
  | ETuple lst -> fprintf "(%a)" (fmt_list fmt_expr) lst
  | EMatch (pat_head, case_list) ->
    fprintf "(match %a with" fmt_expr pat_head;
    List.iter
      (fun (pat, exp) -> fprintf "\n| %a -> %a" fmt_pattern pat fmt_expr exp)
      case_list;
    fprintf ")"
  | EConstraint (exp, tp) -> fprintf "(%a : %a)" fmt_expr exp pp_type_annot tp
  | ECons (exp1, exp2) -> fprintf "(%a :: %a)" fmt_expr exp1 fmt_expr exp2
;;

let fmt_binding ppf ((pat, exp) : binding) =
  Format.fprintf ppf "%a = %a" fmt_pattern pat fmt_expr exp
;;

let fmt_str_item ppf decl =
  let fprintf x = Format.fprintf ppf x in
  match decl with
  | SValue (flag, [ binding ]) ->
    fprintf "let%a %a\n" fmt_rec_flag flag fmt_binding binding
  | SValue (flag, binding_list) ->
    fprintf "let%a " fmt_rec_flag flag;
    List.iteri
      (fun i binding ->
        if i <> 0 then fprintf " and " else ();
        fmt_binding ppf binding)
      binding_list;
    fprintf "\n"
;;

let fmt_structure ppf decls = List.iter (fmt_str_item ppf) decls
let print_structure structure = Format.asprintf "%a" fmt_structure structure
