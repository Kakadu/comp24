open SlarnML_lib.Res

let input_file = try Sys.argv.(1) with _ -> "main.ml"
let output_file = try Sys.argv.(2) with _ -> "main.S"
;;


let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []
;;

let parse_to_riscv _ output_file = 
  let program = (String.concat "\n" (read_lines input_file))^"\n" in
  print_string program;
  let ast = SlarnML_lib.Parser.parser program in
  let result = match ast with
  | Ok ast -> SlarnML_lib.Clos_conv.clos_conv ast >>= 
  (fun ast -> SlarnML_lib.Lambda_lifting.lambda_lifting ast) >>=
  (fun ast -> Result (SlarnML_lib.Anf_conv.anf ast)) >>=
  (fun anf -> SlarnML_lib.Riscv.asm anf) >>=
  (fun prog -> Result (String.concat "\n" (List.map (SlarnML_lib.Pprint_riscv.pp_instruction "\t") prog)))
  | Error e -> SlarnML_lib.Res.Error ("there was an error: "^e^"\n")
  in
  match result with
  | SlarnML_lib.Res.Result r -> let oc = open_out output_file in 
  output_string oc (r^"\n"); close_out oc;
  | Error e -> Printf.eprintf "%s" e;
;;


let () = parse_to_riscv input_file output_file
;;

