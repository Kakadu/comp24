[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

open LMiddle
open LBack

module Format = Stdlib.Format

type err = ParseErr | MiddleErr of MSimpl.err | CodegenErr of BCodegen.err

let run' s =
  let open Result in
  let ( let* ) = ( >>= ) in

  let* structure =
    LParse.parse s |> Option.value_map ~default:(fail ParseErr) ~f:return
  in

  let* sim =
    MSimpl.from_structure structure |> map_error ~f:(fun err -> MiddleErr err)
  in
  let opt = MOpt.opt sim in

  let module LLModule = struct
    open Llvm
    let lctx = create_context ()
    let lmod = create_module lctx "neml"
  end in
  let module LLRuntime = LLRuntime.Make (LLModule) in
  let module Builtin = LBuiltin.Make (LLModule) (LLRuntime) in
  let globals, builtins =
    List.fold_right Builtin.builtins ~init:(IdSet.empty, [])
      ~f:(fun (id, _, bltn) (globals, builtins) ->
        (Set.add globals id, bltn :: builtins) )
  in

  let cls = MCLess.from_simpl ~globals opt in
  let anf = MAnf.from_cless cls in

  PPrint.ToChannel.pretty 1. 80 stdout
    (LPrint.pp_structure (MAnf.to_structure anf)) ;
  let module CodeGen = BCodegen.LLCodeGen (LLModule) (LLRuntime) in
  let* () =
    CodeGen.gen ~builtins anf |> map_error ~f:(fun err -> CodegenErr err)
  in

  Format.printf "\n\n%s" (Llvm.string_of_llmodule LLModule.lmod) ;
  return ()

let run s =
  match run' s with
  | Error ParseErr ->
      print_endline "syntax error"
  | Error (MiddleErr err) ->
      MSimpl.pp_err Format.std_formatter err
  | Error (CodegenErr err) ->
      BCodegen.pp_err Format.std_formatter err
  | Ok () ->
      ()

let run path = In_channel.read_all path |> run

let%expect_test _ =
  run "manytests/typed/002fac.ml" ;
  [%expect
    {|
    let f0 = fun k n p -> let v0 = ( * ) p n in k v0;;
    let rec f1 =
      fun n k ->
        let v0 = ( = ) n 1 in
        if v0 then k 1 else let v2 = f0 k n in let v3 = ( - ) n 1 in f1 v3 v2;;
    let f2 = fun print_int -> print_int;;
    let f3 = fun main -> ();;
    let f4 = fun fac_cps -> let v0 = fac_cps 4 f2 in let v1 = print_int v0 in f3 0;;
    f4 f1

    ; ModuleID = 'neml'
    source_filename = "neml"

    declare i64 @neml_print_int(i64)

    declare i64 @neml_apply_closure(i64, i64, ...)

    declare i64 @neml_create_closure(i64, i64)

    ; Function Attrs: alwaysinline
    define i64 @u43(i64 %0, i64 %1) #0 {
    entry:
      %r = add i64 %0, %1
      ret i64 %r
    }

    ; Function Attrs: alwaysinline
    define i64 @u45(i64 %0, i64 %1) #0 {
    entry:
      %r = sub i64 %0, %1
      ret i64 %r
    }

    ; Function Attrs: alwaysinline
    define i64 @u42(i64 %0, i64 %1) #0 {
    entry:
      %r = mul i64 %0, %1
      ret i64 %r
    }

    ; Function Attrs: alwaysinline
    define i64 @u47(i64 %0, i64 %1) #0 {
    entry:
      %r = sdiv i64 %0, %1
      ret i64 %r
    }

    ; Function Attrs: alwaysinline
    define i64 @u61(i64 %0, i64 %1) #0 {
    entry:
      %r = icmp eq i64 %0, %1
      %r1 = sext i1 %r to i64
      ret i64 %r1
    }

    ; Function Attrs: alwaysinline
    define i64 @u60(i64 %0, i64 %1) #0 {
    entry:
      %r = icmp slt i64 %0, %1
      %r1 = sext i1 %r to i64
      ret i64 %r1
    }

    ; Function Attrs: alwaysinline
    define i64 @u6061(i64 %0, i64 %1) #0 {
    entry:
      %r = icmp sle i64 %0, %1
      %r1 = sext i1 %r to i64
      ret i64 %r1
    }

    ; Function Attrs: alwaysinline
    define i64 @uprint95int(i64 %0) #0 {
    entry:
      %r = call i64 @neml_print_int(i64 %0)
      ret i64 %r
    }

    define i64 @gf0(i64 %uk, i64 %un, i64 %up) {
    entry:
      %gv0 = call i64 @u42(i64 %up, i64 %un)
      %r = call i64 (i64, i64, ...) @neml_apply_closure(i64 %uk, i64 1, i64 %gv0)
      ret i64 %r
    }

    define i64 @gf1(i64 %un, i64 %uk) {
    entry:
      %gv0 = call i64 @u61(i64 %un, i64 1)
      %cond = trunc i64 %gv0 to i1
      br i1 %cond, label %then, label %else

    then:                                             ; preds = %entry
      %r = call i64 (i64, i64, ...) @neml_apply_closure(i64 %uk, i64 1, i64 1)
      br label %cont

    else:                                             ; preds = %entry
      %r1 = call i64 @neml_create_closure(ptr @gf0, i64 3)
      %gv2 = call i64 (i64, i64, ...) @neml_apply_closure(i64 %r1, i64 2, i64 %uk, i64 %un)
      %gv3 = call i64 @u45(i64 %un, i64 1)
      %r4 = call i64 @gf1(i64 %gv3, i64 %gv2)
      br label %cont

    cont:                                             ; preds = %else, %then
      %r5 = phi i64 [ %r, %then ], [ %r4, %else ]
      ret i64 %r5
    }

    define i64 @gf2(i64 %uprint95int) {
    entry:
      ret i64 %uprint95int
    }

    define i64 @gf3(i64 %umain) {
    entry:
      ret i64 0
    }

    define i64 @gf4(i64 %ufac95cps) {
    entry:
      %r = call i64 @neml_create_closure(ptr @gf2, i64 1)
      %gv0 = call i64 (i64, i64, ...) @neml_apply_closure(i64 %ufac95cps, i64 2, i64 4, i64 %r)
      %gv1 = call i64 @uprint95int(i64 %gv0)
      %r3 = call i64 @gf3(i64 0)
      ret i64 %r3
    }

    define i64 @main() {
    entry:
      %r = call i64 @neml_create_closure(ptr @gf1, i64 2)
      %r1 = call i64 @gf4(i64 %r)
      ret i64 0
    }

    attributes #0 = { alwaysinline }
    |}]

let%expect_test _ =
  run "manytests/typed/004manyargs.ml" ;
  [%expect
    {|
    let f0 = fun f -> let v0 = ( = ) 1 1 in if v0 then f else f;;
    let f1 = fun c -> 0;;
    let f2 = fun c b -> let v0 = print_int c in f1 v0;;
    let f3 = fun b c a -> let v0 = print_int b in f2 c v0;;
    let f4 = fun a b c -> let v0 = print_int a in f3 b c v0;;
    let f5 =
      fun a b c d e f g h i j ->
        let v0 = ( + ) a b in
        let v1 = ( + ) v0 c in
        let v2 = ( + ) v1 d in
        let v3 = ( + ) v2 e in
        let v4 = ( + ) v3 f in
        let v5 = ( + ) v4 g in
        let v6 = ( + ) v5 h in let v7 = ( + ) v6 i in ( + ) v7 j;;
    let f6 = fun temp2 -> 0;;
    let f7 =
      fun test3 wrap rez ->
        let v0 = print_int rez in let v1 = wrap test3 1 10 100 in f6 v1;;
    let f8 = fun main -> ();;
    let f9 =
      fun test3 wrap test10 ->
        let v0 =
          wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
            1000000000
        in
        let v1 = f7 test3 wrap v0 in f8 v1;;
    let f10 = fun wrap test3 -> f9 test3 wrap f5;;
    let f11 = fun wrap -> f10 wrap f4;;
    f11 f0

    ; ModuleID = 'neml'
    source_filename = "neml"

    declare i64 @neml_print_int(i64)

    declare i64 @neml_apply_closure(i64, i64, ...)

    declare i64 @neml_create_closure(i64, i64)

    ; Function Attrs: alwaysinline
    define i64 @u43(i64 %0, i64 %1) #0 {
    entry:
      %r = add i64 %0, %1
      ret i64 %r
    }

    ; Function Attrs: alwaysinline
    define i64 @u45(i64 %0, i64 %1) #0 {
    entry:
      %r = sub i64 %0, %1
      ret i64 %r
    }

    ; Function Attrs: alwaysinline
    define i64 @u42(i64 %0, i64 %1) #0 {
    entry:
      %r = mul i64 %0, %1
      ret i64 %r
    }

    ; Function Attrs: alwaysinline
    define i64 @u47(i64 %0, i64 %1) #0 {
    entry:
      %r = sdiv i64 %0, %1
      ret i64 %r
    }

    ; Function Attrs: alwaysinline
    define i64 @u61(i64 %0, i64 %1) #0 {
    entry:
      %r = icmp eq i64 %0, %1
      %r1 = sext i1 %r to i64
      ret i64 %r1
    }

    ; Function Attrs: alwaysinline
    define i64 @u60(i64 %0, i64 %1) #0 {
    entry:
      %r = icmp slt i64 %0, %1
      %r1 = sext i1 %r to i64
      ret i64 %r1
    }

    ; Function Attrs: alwaysinline
    define i64 @u6061(i64 %0, i64 %1) #0 {
    entry:
      %r = icmp sle i64 %0, %1
      %r1 = sext i1 %r to i64
      ret i64 %r1
    }

    ; Function Attrs: alwaysinline
    define i64 @uprint95int(i64 %0) #0 {
    entry:
      %r = call i64 @neml_print_int(i64 %0)
      ret i64 %r
    }

    define i64 @gf0(i64 %uf) {
    entry:
      %gv0 = call i64 @u61(i64 1, i64 1)
      %cond = trunc i64 %gv0 to i1
      br i1 %cond, label %then, label %else

    then:                                             ; preds = %entry
      br label %cont

    else:                                             ; preds = %entry
      br label %cont

    cont:                                             ; preds = %else, %then
      %r = phi i64 [ %uf, %then ], [ %uf, %else ]
      ret i64 %r
    }

    define i64 @gf1(i64 %uc) {
    entry:
      ret i64 0
    }

    define i64 @gf2(i64 %uc, i64 %ub) {
    entry:
      %gv0 = call i64 @uprint95int(i64 %uc)
      %r = call i64 @gf1(i64 %gv0)
      ret i64 %r
    }

    define i64 @gf3(i64 %ub, i64 %uc, i64 %ua) {
    entry:
      %gv0 = call i64 @uprint95int(i64 %ub)
      %r = call i64 @gf2(i64 %uc, i64 %gv0)
      ret i64 %r
    }

    define i64 @gf4(i64 %ua, i64 %ub, i64 %uc) {
    entry:
      %gv0 = call i64 @uprint95int(i64 %ua)
      %r = call i64 @gf3(i64 %ub, i64 %uc, i64 %gv0)
      ret i64 %r
    }

    define i64 @gf5(i64 %ua, i64 %ub, i64 %uc, i64 %ud, i64 %ue, i64 %uf, i64 %ug, i64 %uh, i64 %ui, i64 %uj) {
    entry:
      %gv0 = call i64 @u43(i64 %ua, i64 %ub)
      %gv1 = call i64 @u43(i64 %gv0, i64 %uc)
      %gv2 = call i64 @u43(i64 %gv1, i64 %ud)
      %gv3 = call i64 @u43(i64 %gv2, i64 %ue)
      %gv4 = call i64 @u43(i64 %gv3, i64 %uf)
      %gv5 = call i64 @u43(i64 %gv4, i64 %ug)
      %gv6 = call i64 @u43(i64 %gv5, i64 %uh)
      %gv7 = call i64 @u43(i64 %gv6, i64 %ui)
      %r = call i64 @u43(i64 %gv7, i64 %uj)
      ret i64 %r
    }

    define i64 @gf6(i64 %utemp2) {
    entry:
      ret i64 0
    }

    define i64 @gf7(i64 %utest3, i64 %uwrap, i64 %urez) {
    entry:
      %gv0 = call i64 @uprint95int(i64 %urez)
      %gv1 = call i64 (i64, i64, ...) @neml_apply_closure(i64 %uwrap, i64 4, i64 %utest3, i64 1, i64 10, i64 100)
      %r = call i64 @gf6(i64 %gv1)
      ret i64 %r
    }

    define i64 @gf8(i64 %umain) {
    entry:
      ret i64 0
    }

    define i64 @gf9(i64 %utest3, i64 %uwrap, i64 %utest10) {
    entry:
      %gv0 = call i64 (i64, i64, ...) @neml_apply_closure(i64 %uwrap, i64 11, i64 %utest10, i64 1, i64 10, i64 100, i64 1000, i64 10000, i64 100000, i64 1000000, i64 10000000, i64 100000000, i64 1000000000)
      %gv1 = call i64 @gf7(i64 %utest3, i64 %uwrap, i64 %gv0)
      %r = call i64 @gf8(i64 %gv1)
      ret i64 %r
    }

    define i64 @gf10(i64 %uwrap, i64 %utest3) {
    entry:
      %r = call i64 @neml_create_closure(ptr @gf5, i64 10)
      %r1 = call i64 @gf9(i64 %utest3, i64 %uwrap, i64 %r)
      ret i64 %r1
    }

    define i64 @gf11(i64 %uwrap) {
    entry:
      %r = call i64 @neml_create_closure(ptr @gf4, i64 3)
      %r1 = call i64 @gf10(i64 %uwrap, i64 %r)
      ret i64 %r1
    }

    define i64 @main() {
    entry:
      %r = call i64 @neml_create_closure(ptr @gf0, i64 1)
      %r1 = call i64 @gf11(i64 %r)
      ret i64 0
    }

    attributes #0 = { alwaysinline }
    |}]
