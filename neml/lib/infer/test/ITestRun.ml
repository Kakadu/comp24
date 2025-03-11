[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

module Format = Stdlib.Format

type err = ParseError | InferError of LInfer.IError.t

let run' ?(silent = false) (env : LInfer.Env.t) s =
  let print id ty =
    let open PPrint in
    let doc =
      group @@ LPrint.pp_id id ^^ string ": " ^^ LPrint.pp_ty ty ^^ hardline
    in
    ToChannel.pretty 1. 80 stdout doc
  in

  let open Result in
  let ( let* ) = ( >>= ) in

  let* structure =
    LParse.parse s |> Option.value_map ~default:(fail ParseError) ~f:return
  in

  List.fold_result structure ~init:env ~f:(fun env item ->
      let* {ty; env; bounds} = LInfer.infer env item in

      (* print results *)
      if not silent then (
        Option.iter ty ~f:(print (I "_")) ;
        List.iter bounds ~f:(fun id -> Map.find_exn env.bounds id |> print id) ) ;

      return env )
  |> map_error ~f:(fun err -> InferError err)

let env =
  run' ~silent:true LInfer.Env.empty
    {| type int;;
       type string;;
       type unit = ();;
       type bool = true | false;;
       type 'a option = Some of 'a | None;;
       type ('a, 'err) result = Ok of 'a | Err of 'err;;
       type 'a list = [] | (::) of 'a * 'a list;;
       let ( + ) (x: int) (y: int) = 0;;
       let ( - ) (x: int) (y: int) = 0;;
       let ( * ) (x: int) (y: int) = 0;;
       let ( / ) (x: int) (y: int) = 0;;
       let ( = ) (x: int) (y: int) = false;;
       let ( < ) (x: int) (y: int) = false;;
       let ( <= ) (x: int) (y: int) = false;;
       let id x = x;;
       let print_int (x: int) = () |}
  |> Result.ok |> Option.value_exn

let run s =
  match run' env s with
  | Error ParseError ->
      print_endline "syntax error"
  | Error (InferError err) ->
      LInfer.IError.pp Format.std_formatter err
  | Ok _ ->
      ()
