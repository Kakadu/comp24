open Angstrom
open Kreml_lib.Parser
open Kreml_lib.Ast
open Kreml_lib.Ast_printer
open Stdlib.Format

let fmt = std_formatter

let print_ast input =
      match parse_string ~consume:Consume.All program input with
      | Ok original_struct ->
        let string = structure_as_string original_struct in
        (match parse_string ~consume:Consume.All program string with
        | Ok struct_from_ppf when original_struct = struct_from_ppf -> fprintf fmt "Ok"
        | Ok struct_from_ppf ->
          fprintf fmt "Ast do not match. Expected %s, but got %s"
           (show_structure original_struct)
           (show_structure struct_from_ppf)
        | Error _ -> fprintf fmt "Could not parse pretting print program: \n %s" string)
      | Error _ -> fprintf fmt "Could not parse input: \n %s" input
          

        
let%expect_test "map" =
   let input = "
     let rec map f xs =
      match xs with
      | [] -> []
      | a::[] -> [f a]
      | a::b::[] -> [f a; f b]
      | a::b::c::[] -> [f a; f b; f c]
      | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl
      
     let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter f tl

      " in
    print_ast input;
  [%expect {|
    Ok |}]




let%expect_test "fold" =
   let input = 
      "let rec fold l folder init =
         match l with
         | x::xs ->
            let acc = folder init x in
            fold xs folder acc
         | [] -> init"
   in
   print_ast input;
  [%expect {| Ok |}]

let%expect_test "factorial" =
   let input =
       "let f n = if n > 0 then
          n * f (n - 1)
          else 1" in
   print_ast input;
  [%expect {| Ok |}]


let%expect_test "even_odd" =
   let input =
      "let rec is_even n =
         if n = 0 then true
         else if n = 1 then false
         else is_odd (n - 1)
      and is_odd n =
         if n = 1 then true
         else if n = 0 then false
         else is_odd (n - 1)" in
   print_ast input;
  [%expect {| Ok |}]

let%expect_test "typed" =
   let input = "let rec fold : int list -> int -> (int -> int -> int) -> int =
      fun l acc f ->
      match l with
      | [] -> acc
      | x::xs -> fold xs (f acc x) f

      let somefun 
        (a : int)
        (b: int * int * bool)
        (c: (int * int) list)
        (f : (int -> int) -> int -> int) : int =
         todo";
     in
   print_ast input;
  [%expect {|
    Ok |}]
