open Base

let extern =
  let open Std in
  List.map stdlib ~f:(fun x -> Format.sprintf "    .extern %s" x.extern)
  |> String.concat_lines
;;

let test =
  Format.sprintf
    {|
    .section .data
%s

_start:
    call main

    # syscall exit 0
    li a7, 93
    li a0, 0
    ecall

main:
    li a0, 0
    ret
|}
    extern
;;

let compile () =
  let open Format in
  Stdio.Out_channel.with_file "/tmp/out.s" ~f:(fun out ->
    let fmt = Format.formatter_of_out_channel out in
    let print = Format.fprintf fmt "%s" in
    print test;
    ());
  Ok ()
;;

compile ()
