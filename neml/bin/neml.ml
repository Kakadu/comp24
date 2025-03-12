[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

open LMiddle
open LBack

type err =
  | ParseErr
  | InferErr of LInfer.IError.t
  | SimplErr of MSimpl.err
  | CodegenErr of BCodegen.err

open Result
let ( let* ) = ( >>= )

let run' s out_path =
  let* structure =
    LParse.parse s |> Option.value_map ~default:(fail ParseErr) ~f:return
  in

  let open Llvm in
  let module LLModule = struct
    let lctx = create_context ()
    let lmod = create_module lctx "neml"
  end in
  let module LLRuntime = LLRuntime.Make (LLModule) in
  let module Builtin = LBuiltin.Make (LLModule) (LLRuntime) in
  let globals, tyenv, llbuiltins =
    List.fold_right Builtin.builtins ~init:(IdSet.empty, LInfer.Env.empty, [])
      ~f:(fun (id, ty, llbltn) (globals, (tyenv : LInfer.Env.t), llbltns) ->
        ( Set.add globals id
        , {tyenv with bounds= Map.set tyenv.bounds ~key:id ~data:ty}
        , llbltn :: llbltns ) )
  in

  let* _ =
    let print_ty id ty =
      let open PPrint in
      let doc =
        group @@ LPrint.pp_id id ^^ string ": " ^^ LPrint.pp_ty ty ^^ hardline
      in
      ToChannel.pretty 1. 80 stdout doc
    in

    List.fold_result structure ~init:tyenv ~f:(fun env item ->
        let* {ty; env; bounds} = LInfer.infer env item in

        Option.iter ty ~f:(print_ty (I "_")) ;
        List.iter bounds ~f:(fun id ->
            Map.find_exn env.bounds id |> print_ty id ) ;

        return env )
    |> map_error ~f:(fun err -> InferErr err)
  in

  let* sim =
    MSimpl.from_structure structure |> map_error ~f:(fun err -> SimplErr err)
  in
  let opt = MOpt.opt sim in
  let cls = MCLess.from_simpl ~globals opt in
  let anf = MAnf.from_cless cls in

  let module LLCodegen = BCodegen.LLCodeGen (LLModule) (LLRuntime) in
  let* () =
    LLCodegen.gen ~builtins:llbuiltins anf
    |> map_error ~f:(fun err -> CodegenErr err)
  in

  return (print_module out_path LLModule.lmod)

module Format = Stdlib.Format

let run code out_path =
  match run' code out_path with
  | Error ParseErr ->
      print_endline "syntax error" ;
      -1
  | Error (InferErr err) ->
      LInfer.IError.pp Format.std_formatter err ;
      -1
  | Error (SimplErr err) ->
      MSimpl.pp_err Format.std_formatter err ;
      -1
  | Error (CodegenErr err) ->
      BCodegen.pp_err Format.std_formatter err ;
      -1
  | Ok () ->
      0

let main path out_path =
  let code = Stdio.In_channel.read_all path in
  (* XXX: hardcode *)
  let code = "type unit = ();; type bool = true | false;;" ^ code in
  run code out_path

let () =
  let argv = Sys.get_argv () in
  Stdlib.exit @@ main argv.(1) argv.(2)
