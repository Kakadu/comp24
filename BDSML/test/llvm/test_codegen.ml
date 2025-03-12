open Parser
open Typing
open Middleend
open My_llvm

let () =
  let str = Stdio.In_channel.input_all Stdlib.stdin in
  match Main_parser.parse str with
  | Result.Error err -> Format.printf "Parsing error%s\n" err
  | Result.Ok ast ->
    (match Inference.infer_program ast with
     | Result.Error e -> Format.printf "Type inference error: %s\n" e
     | Result.Ok _ ->
       let ( >>= ) = Result.bind in
       let res =
         Pattern_remover.remove_patterns ast
         >>= Alpha_conversion.alpha_conversion
         |> Result.map Closure_conversion.closure_convert
         >>= Alpha_conversion.alpha_conversion
         >>= Lambda_lifting.ll
         |> Result.map_error Middleend_utils.exp_to_string
       in
       (match res with
        | Result.Error e -> Format.printf "Middleend error: %s\n" e
        | Result.Ok res ->
          (match Anf.rast_to_anf res with
           | Result.Error _ -> Format.printf "Converting to anf error\n"
           | Result.Ok res -> Codegen.compile_program ~verbose:true res)))
;;
