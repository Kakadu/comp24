open Angstrom
open Kreml_lib.Parser
open Kreml_lib.Ast
open Kreml_lib.Ast_printer
open Stdlib.Format

let fmt = std_formatter

let check_ast input =
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
    check_ast input;
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
   check_ast input;
  [%expect {| Ok |}]

let%expect_test "factorial" =
   let input =
       "let f n = if n > 0 then
          n * f (n - 1)
          else 1" in
   check_ast input;
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
   check_ast input;
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
   check_ast input;
  [%expect {|
    Ok |}]

(* let%expect_test "tuples" =
    let tuples = "let rec fix f x = f (fix f) x
  let map f p = let (a,b) = p in (f a, f b)
  let fixpoly l =
    fix (fun self l -> map (fun li x -> li (self l) x) l) l
  let feven p n =
    let (e, o) = p in
    if n = 0 then 1 else o (n - 1)
  let fodd p n =
    let (e, o) = p in
    if n = 0 then 0 else e (n - 1)
  let tie = fixpoly (feven, fodd)

  let rec meven n = if n = 0 then 1 else modd (n - 1)
  and modd n = if n = 0 then 1 else meven (n - 1)
  let main =
    let () = print_int (modd 1) in
    let () = print_int (meven 2) in
    let (even,odd) = tie in
    let () = print_int (odd 3) in
    let () = print_int (even 4) in
    0" in
  check_ast tuples; *)
