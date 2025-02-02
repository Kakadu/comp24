(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ego.Basic
open Sexplib0.Sexp
open Ast_mapper.Mapper (Ast_mapper.Result)
open Ast_mapper.Result

let graph = EGraph.init ()

let add_rule from_list into_list =
  let from = Query.of_sexp from_list in
  let into = Query.of_sexp into_list in
  let rule = Rule.make ~from ~into in
  let _ =
    match rule with
    | Some r -> EGraph.run_until_saturation graph [ r ]
    | None -> EGraph.run_until_saturation graph []
  in
  ()
;;

let cost_function score (sym, children) =
  let node_score =
    match Symbol.to_string sym with
    | "( * )" -> 5.
    | "( / )" -> 5.
    | "( + )" -> 3.
    | "( - )" -> 5.
    | "0" -> 1.
    | "1" -> 2.
    | "?a" -> 0.
    | _ -> 0.
  in
  node_score +. List.fold_left (fun acc vl -> acc +. score vl) 0. children
;;

let transform_sexp sexp =
  let graph_expr = EGraph.add_sexp graph sexp in
  (* (a1 + a2) / a3 = (a1 / a3) + (a2 / a3) *)
  add_rule
    (List
       [ Atom "application"
       ; List
           [ Atom "application"
           ; Atom "( / )"
           ; List
               [ Atom "application"
               ; List [ Atom "application"; Atom "( + )"; Atom "?a" ]
               ; Atom "?b"
               ]
           ]
       ; Atom "?c"
       ])
    (List
       [ Atom "application"
       ; List
           [ Atom "application"
           ; Atom "( + )"
           ; List
               [ Atom "application"
               ; List [ Atom "application"; Atom "( / )"; Atom "?a" ]
               ; Atom "?c"
               ]
           ]
       ; List
           [ Atom "application"
           ; List [ Atom "application"; Atom "( / )"; Atom "?b" ]
           ; Atom "?c"
           ]
       ]);
  (* (a1 * a2) / a3 = a1 * (a2 / a3)*)
  add_rule
    (List
       [ Atom "application"
       ; List
           [ Atom "application"
           ; Atom "( / )"
           ; List
               [ Atom "application"
               ; List [ Atom "application"; Atom "( * )"; Atom "?a" ]
               ; Atom "?b"
               ]
           ]
       ; Atom "?c"
       ])
    (List
       [ Atom "application"
       ; List [ Atom "application"; Atom "( * )"; Atom "?a" ]
       ; List
           [ Atom "application"
           ; List [ Atom "application"; Atom "( / )"; Atom "?b" ]
           ; Atom "?c"
           ]
       ]);
  (* a1 + a2 = a2 + a1 *)
  add_rule
    (List
       [ Atom "application"
       ; List [ Atom "application"; Atom "( + )"; Atom "?a" ]
       ; Atom "?b"
       ])
    (List
       [ Atom "application"
       ; List [ Atom "application"; Atom "( + )"; Atom "?b" ]
       ; Atom "?a"
       ]);
  (* a1 * a2 = a2 * a1 *)
  add_rule
    (List
       [ Atom "application"
       ; List [ Atom "application"; Atom "( * )"; Atom "?a" ]
       ; Atom "?b"
       ])
    (List
       [ Atom "application"
       ; List [ Atom "application"; Atom "( * )"; Atom "?b" ]
       ; Atom "?a"
       ]);
  (* a / 1 = a *)
  add_rule
    (List
       [ Atom "application"
       ; List [ Atom "application"; Atom "( / )"; Atom "?a" ]
       ; List [ Atom "constant"; List [ Atom "int"; Atom "1" ] ]
       ])
    (Atom "?a");
  (* a - 0 = a *)
  add_rule
    (List
       [ Atom "application"
       ; List [ Atom "application"; Atom "( - )"; Atom "?a" ]
       ; List [ Atom "constant"; List [ Atom "int"; Atom "0" ] ]
       ])
    (Atom "?a");
  (* a + 0 = a *)
  add_rule
    (List
       [ Atom "application"
       ; List [ Atom "application"; Atom "( + )"; Atom "?a" ]
       ; List [ Atom "constant"; List [ Atom "int"; Atom "0" ] ]
       ])
    (Atom "?a");
  (* 0 + a = a *)
  add_rule
    (List
       [ Atom "application"
       ; List
           [ Atom "application"
           ; Atom "( + )"
           ; List [ Atom "constant"; List [ Atom "int"; Atom "0" ] ]
           ]
       ; Atom "?a"
       ])
    (Atom "?a");
  (* a - a = 0 *)
  add_rule
    (List
       [ Atom "application"
       ; List [ Atom "application"; Atom "( - )"; Atom "?a" ]
       ; Atom "?a"
       ])
    (List [ Atom "constant"; List [ Atom "int"; Atom "0" ] ]);
  (* a / a = 1 *)
  add_rule
    (List
       [ Atom "application"
       ; List [ Atom "application"; Atom "( / )"; Atom "?a" ]
       ; Atom "?a"
       ])
    (List [ Atom "constant"; List [ Atom "int"; Atom "1" ] ]);
  (* a * 1 = a *)
  add_rule
    (List
       [ Atom "application"
       ; List [ Atom "application"; Atom "( * )"; Atom "?a" ]
       ; List [ Atom "constant"; List [ Atom "int"; Atom "1" ] ]
       ])
    (Atom "?a");
  (* 1 * a = a *)
  add_rule
    (List
       [ Atom "application"
       ; List
           [ Atom "application"
           ; Atom "( * )"
           ; List [ Atom "constant"; List [ Atom "int"; Atom "1" ] ]
           ]
       ; Atom "?a"
       ])
    (Atom "?a");
  (* a * 2 = a + a *)
  add_rule
    (List
       [ Atom "application"
       ; List [ Atom "application"; Atom "( * )"; Atom "?a" ]
       ; List [ Atom "constant"; List [ Atom "int"; Atom "2" ] ]
       ])
    (List
       [ Atom "application"
       ; List [ Atom "application"; Atom "( + )"; Atom "?a" ]
       ; Atom "?a"
       ]);
  let sexp_result = EGraph.extract cost_function graph graph_expr in
  sexp_result
;;

let simplify =
  map1 (fun decl ->
    let sexp = sexpr_of_declaration decl in
    match let_declaration_of_sexpr (transform_sexp sexp) with
    | Ok decl -> return decl
    | Error _ -> error "Error while applying rewrites")
;;

let test_egraph_optimization prog =
  match Parser.parse_program prog with
  | Ok ast ->
    (match simplify ast with
     | Ok simplified ->
       Printf.printf "%s" (Restore_src.RestoreSrc.restore_declarations simplified)
     | Error msg -> Printf.printf "%s" msg)
  | Error msg -> Printf.printf "%s" msg
;;
