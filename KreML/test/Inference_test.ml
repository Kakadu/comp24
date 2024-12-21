open Kreml_lib.Inferencer
open Kreml_lib.Parser

let%expect_test "binary" =
  let example = "a + b" in
  