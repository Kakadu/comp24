(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Sexplib0.Sexp

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Result : MONADERROR with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let return = Result.ok
  let ( let* ) = ( >>= )
  let error = Result.error
end

module Mapper (M : MONADERROR) = struct
  open M

  let rec map1 f = function
    | [] -> return []
    | h :: tl -> f h >>= fun c -> map1 f tl >>= fun lst -> return (c :: lst)
  ;;

  let rec type_to_sexpr = function
    | TUnit -> Atom "unit"
    | TInt -> Atom "int"
    | TBool -> Atom "bool"
    | TPoly name -> Atom (Printf.sprintf "'%s" name) (* Handling TPoly *)
    | TTuple lst -> List (Atom "tuple" :: List.map type_to_sexpr lst)
    | TFunction (tp_arg, tp_ret) ->
      List [ Atom "function"; type_to_sexpr tp_arg; type_to_sexpr tp_ret ]
    | TList tp -> List [ Atom "list"; type_to_sexpr tp ]
  ;;

  let constant_to_sexpr = function
    | CInt n -> List [ Atom "int"; Atom (string_of_int n) ]
    | CBool b -> List [ Atom "bool"; Atom (string_of_bool b) ]
    | CNil -> Atom "nil"
    | CUnit -> Atom "unit"
  ;;

  let rec pattern_to_sexpr = function
    | PWildCard -> Atom "_"
    | PCons (p1, p2) -> List [ Atom "cons"; pattern_to_sexpr p1; pattern_to_sexpr p2 ]
    | PIdentifier id -> Atom id
    | PTuple pats -> List (Atom "tuple" :: List.map pattern_to_sexpr pats)
    | PConstant c -> List [ Atom "constant"; constant_to_sexpr c ]
    | PConstraint (p, t) ->
      List [ Atom "constraint"; pattern_to_sexpr p; type_to_sexpr t ]
  ;;

  let rec expr_to_sexpr = function
    | EConstant c -> List [ Atom "constant"; constant_to_sexpr c ]
    | EIdentifier id -> Atom id
    | EFunction (p, e) -> List [ Atom "function"; pattern_to_sexpr p; expr_to_sexpr e ]
    | EApplication (e1, e2) ->
      List [ Atom "application"; expr_to_sexpr e1; expr_to_sexpr e2 ]
    | EIfThenElse (e1, e2, e3) ->
      List [ Atom "if"; expr_to_sexpr e1; expr_to_sexpr e2; expr_to_sexpr e3 ]
    | ELetIn (flag, p, e1, e2) ->
      List
        [ Atom "let"
        ; rec_flag_to_sexpr flag
        ; pattern_to_sexpr p
        ; expr_to_sexpr e1
        ; expr_to_sexpr e2
        ]
    | ETuple es -> List (Atom "tuple" :: List.map expr_to_sexpr es)
    | EMatch (p, cases) ->
      List
        [ Atom "match"
        ; pattern_to_sexpr p
        ; List
            (List.map (fun (p, e) -> List [ pattern_to_sexpr p; expr_to_sexpr e ]) cases)
        ]
    | EConstraint (e, t) -> List [ Atom "constraint"; expr_to_sexpr e; type_to_sexpr t ]

  and rec_flag_to_sexpr = function
    | Rec -> Atom "rec"
    | NotRec -> Atom "notrec"
  ;;

  let single_let_to_sexpr (DLet (p, e)) =
    List [ Atom "let"; pattern_to_sexpr p; expr_to_sexpr e ]
  ;;

  let sexpr_of_declaration = function
    | DSingleLet (flag, let_expr) ->
      List [ Atom "single_let"; rec_flag_to_sexpr flag; single_let_to_sexpr let_expr ]
    | DMutualRecDecl (flag, lets) ->
      List
        [ Atom "mutual_rec_decl"
        ; rec_flag_to_sexpr flag
        ; List (List.map single_let_to_sexpr lets)
        ]
  ;;

  let sexpr_of_declarations decls = List.map sexpr_of_declaration decls

  let rec type_of_sexpr = function
    | Atom "unit" -> return TUnit
    | Atom "int" -> return TInt
    | Atom "bool" -> return TBool
    | Atom s when s.[0] = '\'' -> return (TPoly (String.sub s 1 (String.length s - 1)))
    | List (Atom "tuple" :: ts) ->
      let* ts' = map1 type_of_sexpr ts in
      return (TTuple ts')
    | List [ Atom "function"; arg; ret ] ->
      let* tp_arg = type_of_sexpr arg in
      let* tp_ret = type_of_sexpr ret in
      return (TFunction (tp_arg, tp_ret))
    | List [ Atom "list"; tp ] ->
      let* tp' = type_of_sexpr tp in
      return (TList tp')
    | _ -> error "Invalid S-expression for type"
  ;;

  let constant_of_sexpr = function
    | List [ Atom "int"; Atom n ] -> return (CInt (int_of_string n))
    | List [ Atom "bool"; Atom b ] -> return (CBool (bool_of_string b))
    | Atom "nil" -> return CNil
    | Atom "unit" -> return CUnit
    | _ -> error "Invalid S-expression for constant"
  ;;

  let rec pattern_of_sexpr = function
    | Atom "_" -> return PWildCard
    | List [ Atom "cons"; p1; p2 ] ->
      let* p1' = pattern_of_sexpr p1 in
      let* p2' = pattern_of_sexpr p2 in
      return (PCons (p1', p2'))
    | Atom id -> return (PIdentifier id)
    | List (Atom "tuple" :: pats) ->
      let* pats' = map1 pattern_of_sexpr pats in
      return (PTuple pats')
    | List [ Atom "constant"; c ] ->
      let* c' = constant_of_sexpr c in
      return (PConstant c')
    | List [ Atom "constraint"; p; t ] ->
      let* p' = pattern_of_sexpr p in
      let* t' = type_of_sexpr t in
      return (PConstraint (p', t'))
    | _ -> error "Invalid S-expression for pattern"
  ;;

  let rec expr_of_sexpr = function
    | List [ Atom "constant"; c ] ->
      let* c' = constant_of_sexpr c in
      return (EConstant c')
    | Atom id -> return (EIdentifier id)
    | List [ Atom "function"; p; e ] ->
      let* p' = pattern_of_sexpr p in
      let* e' = expr_of_sexpr e in
      return (EFunction (p', e'))
    | List [ Atom "application"; e1; e2 ] ->
      let* e1' = expr_of_sexpr e1 in
      let* e2' = expr_of_sexpr e2 in
      return (EApplication (e1', e2'))
    | List [ Atom "if"; e1; e2; e3 ] ->
      let* e1' = expr_of_sexpr e1 in
      let* e2' = expr_of_sexpr e2 in
      let* e3' = expr_of_sexpr e3 in
      return (EIfThenElse (e1', e2', e3'))
    | List [ Atom "let"; flag; p; e1; e2 ] ->
      let* flag' = rec_flag_of_sexpr flag in
      let* p' = pattern_of_sexpr p in
      let* e1' = expr_of_sexpr e1 in
      let* e2' = expr_of_sexpr e2 in
      return (ELetIn (flag', p', e1', e2'))
    | List (Atom "tuple" :: es) ->
      let* es' = map1 expr_of_sexpr es in
      return (ETuple es')
    | List [ Atom "match"; pat; List cases ] ->
      let* pat' = pattern_of_sexpr pat in
      let* cases' =
        map1
          (fun case ->
             match case with
             | List [ p; e ] ->
               let* p' = pattern_of_sexpr p in
               let* e' = expr_of_sexpr e in
               return (p', e')
             | _ -> error "Invalid case in match")
          cases
      in
      return (EMatch (pat', cases'))
    | List [ Atom "constraint"; e; t ] ->
      let* e' = expr_of_sexpr e in
      let* t' = type_of_sexpr t in
      return (EConstraint (e', t'))
    | _ -> error "Invalid S-expression for expression"

  and rec_flag_of_sexpr = function
    | Atom "rec" -> return Rec
    | Atom "notrec" -> return NotRec
    | _ -> error "Invalid S-expression for rec flag"
  ;;

  let single_let_of_sexpr = function
    | List [ Atom "let"; p; e ] ->
      let* p' = pattern_of_sexpr p in
      let* e' = expr_of_sexpr e in
      return (DLet (p', e'))
    | _ -> error "Invalid S-expression for single let"
  ;;

  let let_declaration_of_sexpr = function
    | List [ Atom "single_let"; flag; let_expr ] ->
      let* flag' = rec_flag_of_sexpr flag in
      let* let_expr' = single_let_of_sexpr let_expr in
      return (DSingleLet (flag', let_expr'))
    | List [ Atom "mutual_rec_decl"; flag; List lets ] ->
      let* flag' = rec_flag_of_sexpr flag in
      let* lets' = map1 single_let_of_sexpr lets in
      return (DMutualRecDecl (flag', lets'))
    | _ -> error "Invalid S-expression for let declaration"
  ;;

  let declarations_of_sexpr sexprs =
    let* decls' = map1 let_declaration_of_sexpr sexprs in
    return decls'
  ;;
end

open Mapper (Result)

let rec show_sexp sexp : string =
  match sexp with
  | Atom s -> Printf.sprintf "Atom %S" s
  | List l ->
    let inner = String.concat "; " (List.map show_sexp l) in
    Printf.sprintf "List [ %s ]" inner
;;

let sexpr_of_ast_test_parse prog =
  match Parser.parse_program prog with
  | Ok ast ->
    let sexprs = sexpr_of_declarations ast in
    List.iter (fun sexpr -> Printf.printf "%s\n" (show_sexp sexpr)) sexprs
  | Error err -> Printf.printf "%s\n" err
;;

let%expect_test "Test list type" =
  sexpr_of_ast_test_parse "let x = (a + 0) * 1 - 0 / 1";
  [%expect
    {|
    List [ Atom "single_let"; Atom "notrec"; List [ Atom "let"; Atom "x"; List [ Atom "application"; List [ Atom "application"; Atom "( - )"; List [ Atom "application"; List [ Atom "application"; Atom "( * )"; List [ Atom "application"; List [ Atom "application"; Atom "( + )"; Atom "a" ]; List [ Atom "constant"; List [ Atom "int"; Atom "0" ] ] ] ]; List [ Atom "constant"; List [ Atom "int"; Atom "1" ] ] ] ]; List [ Atom "application"; List [ Atom "application"; Atom "( / )"; List [ Atom "constant"; List [ Atom "int"; Atom "0" ] ] ]; List [ Atom "constant"; List [ Atom "int"; Atom "1" ] ] ] ] ] ] |}]
;;
