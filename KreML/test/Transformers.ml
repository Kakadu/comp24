open Kreml_lib.Inferencer
open Kreml_lib.Ast_transformer

let () =
  let open Stdlib.Format in
  let open Kreml_lib.Ast_printer in
  let input = In_channel.input_all stdin in
  match Kreml_lib.Parser.run input with
  | Ok structure ->
    (match Kreml_lib.Inferencer.run structure with
     | Ok env ->
       let anf_structure = transform_structure structure in
       (match Kreml_lib.Inferencer.run anf_structure with
        | Ok anf_env when TypeEnv.alpha_equals env anf_env -> ()
        | Ok anf_env ->
          fprintf
            std_formatter
            "expected: %a\n anf:%a\n"
            pp_structure
            structure
            pp_structure
            anf_structure;
          fprintf
            std_formatter
            "Type environment changed after applying anf:\n expected: %a\n actual:%a"
            TypeEnv.pp
            env
            TypeEnv.pp
            anf_env
        | Error e ->
          fprintf
            std_formatter
            "An error %a occured while inferencing program in ANF.\n %a"
            pp_error
            e
            pp_structure
            anf_structure)
     | Error error ->
       fprintf std_formatter "An error occured while type checking: %a" pp_error error)
  | Error _ -> fprintf std_formatter "Could not parse the program %s" input
;;
