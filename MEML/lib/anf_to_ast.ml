open Anfast
open Ast
open Base

let aconst_to_const = function
  | AInt i -> CInt i
  | ABool b -> CBool b
  | ANil -> CNil
;;

let rec a_pattern = function
  | AVar "_" -> PWild
  | AVar v -> PVar (v, TUnknown)
  | ATuple t ->
    let anf_t = List.rev @@ List.fold ~init:[] ~f:(fun acc le -> a_pattern le :: acc) t in
    PTuple anf_t
  | AConst c ->
    let pc = aconst_to_const c in
    PConst pc
  | _ -> failwith "Not pattern"
;;

let rec aexpression_to_expression = function
  | AVar v -> EVar (v, TUnknown)
  | AVars (l, r) ->
    let el = aexpression_to_expression l in
    let er = aexpression_to_expression r in
    ELetIn (Notrec, "()", el, er)
  | AConst c ->
    let ec = aconst_to_const c in
    EConst ec
  | AIfElse (i, t, e) ->
    let ei = aexpression_to_expression i in
    let et = aexpression_to_expression t in
    let ee = aexpression_to_expression e in
    EIfElse (ei, et, ee)
  | ABinOp (op, l, r) ->
    let el = aexpression_to_expression l in
    let er = aexpression_to_expression r in
    EBinaryOp (op, el, er)
  | APatLetIn (names, e, ine) ->
    let ee = aexpression_to_expression e in
    let eine = aexpression_to_expression ine in
    ELetPatIn (names, ee, eine)
  | AApp (l, r) ->
    let el = aexpression_to_expression l in
    let er = aexpression_to_expression r in
    EApp (el, er, TUnknown)
  | ATuple t ->
    ETuple
      (List.rev
       @@ List.fold
            ~init:[]
            ~f:(fun acc a ->
              let e = aexpression_to_expression a in
              e :: acc)
            t)
  | AList (l, r) ->
    let el = aexpression_to_expression l in
    let er = aexpression_to_expression r in
    EList (el, er)
  | ALetIn (n, ae, aine) ->
    let e = aexpression_to_expression ae in
    let ine = aexpression_to_expression aine in
    ELetIn (Notrec, n, e, ine)
;;

let name_to_pattern = function
  | "_" -> PWild
  | n -> PVar (n, TUnknown)
;;

let rec args_to_fun e = function
  | [] -> e
  | hd :: tl ->
    let n = name_to_pattern hd in
    EFun (n, args_to_fun e tl)
;;

let abindings_to_ebindings = function
  | ALet (r, n, args, e) ->
    let ee = aexpression_to_expression e in
    Let (r, [ name_to_pattern n, args_to_fun ee args ])
  | ALetPat (p, e) ->
    let ee = aexpression_to_expression e in
    Let (Notrec, [ p, ee ])
  | AExpression e -> Expression (aexpression_to_expression e)
;;

let anf_to_ast anf =
  List.rev @@ List.fold ~init:[] ~f:(fun acc a -> abindings_to_ebindings a :: acc) anf
;;
