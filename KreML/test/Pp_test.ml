open Angstrom
open Kreml_lib.Parser
open Kreml_lib.Ast
open Kreml_lib.Ast_printer
open Stdlib.Format

let fmt = std_formatter

let check_ast input =
  match parse_string ~consume:Consume.All program input with
  | Ok original_struct ->
    let string = structure_to_code original_struct in
    (match parse_string ~consume:Consume.All program string with
     | Ok struct_from_ppf when original_struct = struct_from_ppf -> fprintf fmt "Ok"
     | Ok struct_from_ppf ->
       fprintf
         fmt
         "Ast do not match. Expected %s, but got %s"
         (show_structure original_struct)
         (show_structure struct_from_ppf)
     | Error _ -> fprintf fmt "Could not parse pretting print program: \n %s" string)
  | Error _ -> fprintf fmt "Could not parse input: \n %s" input
;;

let%expect_test "map" =
  let input =
    "\n\
    \     let rec map f xs =\n\
    \      match xs with\n\
    \      | [] -> []\n\
    \      | a::[] -> [f a]\n\
    \      | a::b::[] -> [f a; f b]\n\
    \      | a::b::c::[] -> [f a; f b; f c]\n\
    \      | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl\n\
    \      \n\
    \     let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter f \
     tl\n\n\
    \      "
  in
  check_ast input;
  [%expect {|
    Ok |}]
;;

let%expect_test "fold" =
  let input =
    "let rec fold l folder init =\n\
    \         match l with\n\
    \         | x::xs ->\n\
    \            let acc = folder init x in\n\
    \            fold xs folder acc\n\
    \         | [] -> init"
  in
  check_ast input;
  [%expect {| Ok |}]
;;

let%expect_test "factorial" =
  let input = "let f n = if n > 0 then\n          n * f (n - 1)\n          else 1" in
  check_ast input;
  [%expect {| Ok |}]
;;

let%expect_test "even_odd" =
  let input =
    "let rec is_even n =\n\
    \         if n = 0 then true\n\
    \         else if n = 1 then false\n\
    \         else is_odd (n - 1)\n\
    \      and is_odd n =\n\
    \         if n = 1 then true\n\
    \         else if n = 0 then false\n\
    \         else is_odd (n - 1)"
  in
  check_ast input;
  [%expect {| Ok |}]
;;

let%expect_test "typed" =
  let input =
    "let rec fold : int list -> int -> (int -> int -> int) -> int =\n\
    \      fun l acc f ->\n\
    \      match l with\n\
    \      | [] -> acc\n\
    \      | x::xs -> fold xs (f acc x) f\n\n\
    \      let somefun \n\
    \        (a : int)\n\
    \        (b: int * int * bool)\n\
    \        (c: (int * int) list)\n\
    \        (f : (int -> int) -> int -> int) : int =\n\
    \         todo"
  in
  check_ast input;
  [%expect {|
    Ok |}]
;;
