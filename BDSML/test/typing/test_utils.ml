open Typing

let ( >>= ) a b =
  match a with
  | Ok x -> b x
  | Error _ as e -> e
;;

let test str =
  Parser.Main_parser.parse str
  >>= Inference.infer_program
  |> function
  | Result.Ok res ->
    let rec helper acc = function
      | (id, v) :: tl ->
        acc
        ^ (if id = "" then "" else "val " ^ id ^ " : ")
        ^ Types.show_type_val v
        ^ "\n"
        ^ helper acc tl
      | _ -> acc
    in
    Format.print_string @@ helper "" res
  | Result.Error s -> Format.eprintf "Error%s" s
;;
