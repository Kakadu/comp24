open AefjnvMl_lib
open Inferencer
open Inferencer.PP
open Base.Result

let run_infer input =
  match Parser.parse input >>= check_program with
  | Ok env -> pp_program Format.std_formatter env
  | Error _ -> Format.printf "o_0\n"
;;

let%expect_test _ =
  let () = run_infer {|let a = (true, 55 - 892, [(1, 2); (3, 5)])|} in
  [%expect {| val a: bool * int * (int * int) list |}]
;;
