open Base
open Machine

let stack_size = 96 (* TODO: <- *)
let stack_pos = ref 0
let code : string Queue.t = Queue.create ()

let emit ?(comm = "") instr =
  instr (fun i ->
    Queue.enqueue
      code
      (Format.asprintf
         "%a%s"
         pp_instr
         i
         (if String.(comm <> "") then Format.sprintf " # %s\n" comm else "\n")))
;;

let emit_str s = Queue.enqueue code s

let emit_store reg =
  let stack_loc = (Offset (fp, !stack_pos)) in
  emit sd reg stack_loc;
  stack_pos := !stack_pos - 8;
  stack_loc
;;
(* let emit_load value = () ;; *)


let emit_fn_decl name (args : Ast.id list) =
  if List.length args > 8
  then failwith "TODO: stack arguments"
  else (
    emit_str
      (Format.sprintf {|
    .globl %s
    .type %s, @function
%s:
|} name name name);
    emit addi sp sp (-stack_size);
    emit sd ra (Offset (sp, stack_size - 8));
    emit sd fp (Offset (sp, stack_size - 16));
    emit addi fp sp stack_size)
;;

let emit_fn_ret () =
  emit ld fp (Offset (sp, stack_size - 16));
  emit ld ra (Offset (sp, stack_size - 8));
  emit addi sp sp stack_size;
  emit ret
;;

(* let extern =
  let open Std in
  stdlib @ runtime
  |> List.map ~f:(fun x -> Format.sprintf "    .extern %s" x.extern)
  |> String.concat_lines
;; *)
