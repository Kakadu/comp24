(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Res
open String

(* open Pprint_ast *)
open Pprint_cc
open Pprint_ll
open Pprint_anf
open Pprint_riscv

open Ast

open Clos_conv
open Lambda_lifting
open Anf_conv
open Riscv



(* let p = String.concat "\n" [
  "let fac n = ";
  "let rec fack n f = ";
  "if (n <= 1) then (f 1)";
  "else (fack n f)";
  "in";
  "(fack (n-1) (fun x -> x));;";
  "let main = (print_int (fac 3));;"
]
;;
let _ = String.concat "\n" ["let c=(a 1 b w c (fun x->x));;"]
;;
let _ = String.concat "\n" ["((fun x -> x) 10)"]
;; 
Pprint_ast.print_expr p;; *)



let pp_clos_conv ast = 
  match clos_conv ast with
  | Result cc_ast -> print_string@@concat "\n" (List.map pp_cc_expr cc_ast)
  | Error e -> print_string e
;;

let pp_lambda_lifting ast = 
  match lambda_lifting ast with
  | Result ll_ast -> print_string@@ concat "\n" (List.map pp_gl_expr ll_ast)
  | Error e -> print_string e
;;

let pp_anf ast = print_string@@ concat "\n" (List.map pp_anf_afun (anf ast))

(* let pp_cc_ll ast = 
  match clos_conv ast with
  | Result cc_ast -> pp_lambda_lifting cc_ast
  | Error e -> print_string e
;; *)


let pp_cc_ll_anf ast = 
  match ((clos_conv ast) >>= (fun ast -> lambda_lifting ast)) with
  | Result ll_ast -> pp_anf ll_ast
  | Error e -> print_string e
;;


let ast1 = (Let(Decl("a", ["b"]), 
    LetIn(Decl("c", ["d";"e"]), 
      Add(Id "e", Add(Id "d", Id "b")), 
      App(Id "c", [Const(CInt 2); Id "b"]))
  ))
;;
let ast2 = (Let(Decl("fac", ["n"]),
    LetIn(DeclRec("fack", ["n"; "k"]),
      If(Lte(Id "n", Const(CInt 1)), 
      App(Id "k", [Const(CInt 1)]), 
      App(Id "fack", [
        Sub(Id "n", Const(CInt 1)); 
        Fun(["m"], App(Id "k", [Mul(Id "m", Id "n")]))
        ])),
      App(Id "fack", [Id "n"; Fun(["x"], Id "x")])
  )))
;;
let ast3 = (Let(Decl("fac", ["n"]),
    LetIn(DeclRec("fack", ["k"]),
      If(Lte(Id "n", Const(CInt 1)), 
      App(Id "k", [Const(CInt 1)]), 
      App(Id "fack", [
        Sub(Id "n", Const(CInt 1)); 
        Fun(["m"], App(Id "k", [Mul(Id "m", Id "n")]))
        ])),
      App(Id "fack", [Fun(["x"], Id "x")])
  )))
;;

(* 
let anf1 = [
AFun("anon$1#fack#fac",["k";"n";"m"],
  ALet("anf_op#1",AMul(AId"m",AId"n"),
  ALet("anf_op#2",AMul(AId"k",AId"anf_op#1"),
  ACExpr(CImmExpr(AId"anf_op#2"))
  )));
AFun("fack#fac",["n";"k"],
  ALet("anf_op#3",ALte(AId"n",AInt 1),
  ALet("anf_if#4",AIf(AId"anf_op#3",
    ALet("anf_op#5",ASub(AId"n",AInt 1),
    ALet("anf_app#6",AApp(AId"n", [AId"anf_op#5"]),
    ACExpr(CImmExpr(AId"anf_app#6"))
    )),
    ALet("anf_app#7",AApp(AId"anon$1#fack#fac",[AId"k";AId"n"]),
    ACExpr(CImmExpr(AId"anf_app#7"))
    )
    ),
  ACExpr(CImmExpr(AId"anf_if#4"))
  )))
(* AFun("anon$2#fac",["x"],ACExpr(CImmExpr(AId"x")));
AFun("fac",["n"],
  ALet("anf_app#8",AApp(AId"fack#fac",[AId"n";AId"anon$2#fac"]),
  ACExpr(CImmExpr(AId "anf_app#8"))
  )) *)
];; *)

(* 
let ast = ast2
;;
pp_ast ast;;
print_string "\n\n"
;;
pp_clos_conv ast;;
print_string "\n\n"
;;
pp_cc_ll ast;;
print_string "\n\n"
;;
pp_cc_ll_anf ast;;
print_string "\n\n\n=====\n\n\n"
;; 
*)
(* pp_clos_conv Clos_conv.ast1;;
print_string@@pp_cc_expr Clos_conv.cc1;; *)


let pp_asm asm = match asm with
| Error e -> print_string e
| Result prog -> print_string @@ String.concat "\n" (List.map (pp_instruction "\t") prog)
;;


let pp_cc_ll_anf_asm ast = 
  match ((clos_conv ast) >>= (fun ast -> lambda_lifting ast)) with
  | Result ll_ast -> pp_asm (asm (anf ll_ast))
  | Error e -> print_string e
;;
(* pp_cc_ll_anf_asm [];; *)

let ast_fac = [(Let(DeclRec("fac", ["n"]), 
    If(Gt(Id "n", Const(CInt 1)),
      Const(CInt 1),
      Mul(Id"n", App(Id "fac",[Sub(Id"n",Const(CInt 1))]))
    )
  ))]
;;
let ast_fac_1 = [
Let(Decl("fac", ["n"]),
LetIn(DeclRec("fack", ["n"; "k"]),
  If(Lte(Id"n",Const(CInt(1))), 
    App(Id "k", [Const(CInt 1)]), 
    App(Id "fack", [Sub(Id"n",Const(CInt 1)); Fun(["x"], Mul(Id"x",App(Id"k",[Id"n"])))])),
App(Id"fack",[Id"n";Fun(["x"],Id"x")])));
Let(Decl("main",[]),
  App(Id"print_int", [App(Id"fac", [Const(CInt 6)])])
)
]
;;


(* pp_cc_ll_anf [];; *)
(* print_string "\n\n\n";;
pp_cc_ll_anf_asm ast_fac_1
;; *)
(* pp_cc_ll_anf ast_fac_1
;;  *)


