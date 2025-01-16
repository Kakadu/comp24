open Ast

module StringSet = struct
  include Stdlib.Set.Make(String)
  let pp fmt s =
    let open Stdlib.Format in
    fprintf fmt "[ ";
    iter (fprintf fmt "%s; ") s;
    fprintf fmt "]"
  ;;
end


type lambda =
  | Lconst of const
  | Lvar of ident
  | Lclosure of closure
  | Lite of lambda * lambda * lambda
  | LApp of lambda * lambda
  | Llet of rec_flag * ident * lambda * lambda
  | Lswitch (* TODO *)
[@@deriving show]

and closure =
  { body : lambda
  ; env : closure_env
  ; total_args : int
  ; applied_count : int
  ; freevars : StringSet.t
  }
[@@deriving show]

and closure_env = (int * lambda) list [@@deriving show]

and global_value =
  | Fun of closure
  | Var of lambda
[@@deriving show]

type lprogram = (ident * global_value) list [@@deriving show]
let apply_arg ({ applied_count; env; _ } as c) arg =
  Lclosure { c with applied_count = applied_count + 1; env = (applied_count, arg) :: env }
;;

let iconst i = Lconst (Const_int i)

let lclosure body total_args freevars =
  Lclosure { body; total_args; applied_count = 0; env = []; freevars }
;;

let lvar v = Lvar v
let lapp f a = LApp (f, a)
let llet id expr scope = Llet (NonRecursive, id, expr, scope)
(* let lletrec id expr scope = Llet (Recursive, id, expr, scope) *)

open Stdlib.Format
let rec pp_lambda ppf = function
| Lvar id -> fprintf ppf "%s" id
| Lconst (Const_int i) -> fprintf ppf "%i" i
| Lconst (Const_bool b) -> fprintf ppf "%b" b
| Lconst (Const_unit) -> fprintf ppf "unit"
| Lite(c, t, e) -> fprintf ppf "@[if %a then %a @, else %a @]@." pp_lambda c pp_lambda t pp_lambda e
| Llet(_, id, e, scope) ->
  fprintf ppf "@[let %s = %a in @, %a @]@." id pp_lambda e pp_lambda scope
| LApp(f, a) -> fprintf ppf "@[(%a %a)@]" pp_lambda f pp_lambda a
| Lclosure {body; env; total_args; applied_count; freevars} ->
  fprintf ppf "@[<2>{body: %a @, env: %a @, total args: %i @, applied_count: %i @, free_vars: %a } @]@."
  pp_lambda body pp_clos_env env total_args applied_count StringSet.pp freevars
| Lswitch -> Utils.internalfail "TODO"
and pp_clos_env ppf ce =
  fprintf ppf "[ ";
  List.iter (fun (id, v) -> fprintf ppf "@[ (%i,%a) @]" id pp_lambda v ) ce;
  fprintf ppf "]"

let pp ppf lprogram =
  let item_printer (id, gv) =
    match gv with
    | Fun closure ->
      let lam = Lclosure closure in
      fprintf ppf "@[let %s = %a @]@." id pp_lambda lam
    | Var l -> fprintf ppf "@[let %s = %a]@." id pp_lambda l
     in
  List.iter item_printer lprogram