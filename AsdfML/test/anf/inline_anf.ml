open Lib
open Base
open Format

let test code =
  let ast =
    match Parser.parse_program code with
    | Error e -> failwith e
    | Ok ast ->
      (match Inferencer.inference_program ast with
       | Error e -> failwith (asprintf "%a" Pp_typing.pp_error e)
       | Ok ast ->
         ast
         |> Remove_patterns.remove_patterns
         |> Remove_match.remove_match
         |> Closure_conversion.closure_conversion
         |> Lambda_lifting.lambda_lifting
         |> Anf.anf)
  in
  printf "%a" Anf_ast.pp_program ast
;;

let%expect_test _ =
  test {|
    let a = 42
  |};
  [%expect {| 
    let a = 42
  |}]
;;

let%expect_test _ =
  test {|
    let a = 1 + 2 - 42
  |};
  [%expect
    {|
    let a = let a3 = ( + ) 1 in
      let a2 = a3 2 in
      let a1 = ( - ) a2 in
      a1 42
    |}]
;;

let%expect_test _ =
  test {|
    let a = 
      let one = 1 in 
      let two = 2 in
      one + two
  |};
  [%expect
    {|
    let a = let a0 = 1 in
      let a1 = 2 in
      let a3 = ( + ) a0 in
      a3 a1
    |}]
;;

let%expect_test _ =
  test {|
    let rec fact = fun x -> if x < 2 then 1 else x * fact (x - 1)
    let main = print_int (fact 5)
  |};
  [%expect {|
    FVs [] in fun
    (fun x -> if (( < ) x 2) then 1 else (( * ) x (fact (( - ) x 1))))
    FV in fact: {}

    let fact x =
      let a7 = ( < ) x in
      let a1 = a7 2 in
      if a1
      then 1
      else
        let a3 = ( * ) x in
        let a6 = ( - ) x in
        let a5 = a6 1 in
        let a4 = fact a5 in
        a3 a4
    let main = let a9 = fact 5 in
      print_int a9
    |}]
;;

let%expect_test _ =
  test
    {|
      let fact n =
        let rec helper n cont =
          if n <= 1 then 
            cont 1
          else 
            helper (n - 1) (fun res -> cont (n * res)) 
        in 
        helper n (fun x -> x)
    |};
  [%expect
    {|
    FVs [] in fun
    (fun n -> let rec helper = (fun n cont -> if (( <= ) n 1) then (cont 1) else (helper (( - ) n 1) (fun res -> (cont (( * ) n res)))))
     in (helper n (fun x -> x)))
    FVs [] in fun
    (fun n cont -> if (( <= ) n 1) then (cont 1) else (helper (( - ) n 1) (fun res -> (cont (( * ) n res)))))
    FVs [cont; n] in fun
    (fun res -> (cont (( * ) n res)))
    Creating (apply:true) closure with FVs [cont; n]
    and body (cont (( * ) n res))
    FV in helper: {}
    FVs [] in fun
    (fun x -> x)
    FV in helper: {}

    let `ll_2 cont n res = let a2 = ( * ) n in
      let a1 = a2 res in
      cont a1
    let `helper_1 n cont =
      let a12 = ( <= ) n in
      let a4 = a12 1 in
      if a4
      then cont 1
      else
        let a11 = ( - ) n in
        let a10 = a11 1 in
        let a7 = `helper_1 a10 in
        let a9 = `ll_2 cont in
        let a8 = a9 n in
        a7 a8
    let `ll_3 x = x
    let fact n = let a13 = `helper_1 in
      let a15 = a13 n in
      a15 `ll_3
    |}]
;;

let%expect_test _ =
  test
    {|
    let rec map f list = match list with
      | hd::tl -> f hd :: map f tl
      | _ -> []
    |};
  [%expect
    {|
    FVs [] in fun
    (fun f list -> if (TODO: check cons pattern) then let hd = (`list_hd list)
     in let tl = (`list_tl list)
     in (( :: ) (f hd) (map f tl)) else [])
    FV in map: {}

    let map f list =
      if (TODO: check cons pattern)
      then
        let a2 = `list_hd list in
        let a4 = `list_tl list in
        let a9 = f a2 in
        let a6 = ( :: ) a9 in
        let a8 = map f in
        let a7 = a8 a4 in
        a6 a7
      else []
    |}]
;;

let%expect_test _ =
  test
    {|
    let cross (x1, y1, z1) (x2, y2, z2) =
      let x = (y1 * z2) - (z1 * y2) in
      let y = (z1 * x2) - (x1 * z2) in
      let z = (x1 * y2) - (y1 * x2) in
      (x, y, z)
    
    let main = 
      let a = (1, 2, 3) in
      let b = (4, 5, 6) in
      let c = cross a b in
      print_tuple c
    |};
  [%expect
    {|
     FVs [] in fun
     (fun `arg_8 `arg_9 -> let `tuple = `arg_9
      in let z2 = (`get_tuple_field `tuple 2)
      in let y2 = (`get_tuple_field `tuple 1)
      in let x2 = (`get_tuple_field `tuple 0)
      in let `tuple = `arg_8
      in let z1 = (`get_tuple_field `tuple 2)
      in let y1 = (`get_tuple_field `tuple 1)
      in let x1 = (`get_tuple_field `tuple 0)
      in let x = (( - ) (( * ) y1 z2) (( * ) z1 y2))
      in let y = (( - ) (( * ) z1 x2) (( * ) x1 z2))
      in let z = (( - ) (( * ) x1 y2) (( * ) y1 x2))
      in (x, y, z))
 
     let cross `arg_8 `arg_9 =
       let a0 = `arg_9 in
       let a40 = `get_tuple_field a0 in
       let a2 = a40 2 in
       let a39 = `get_tuple_field a0 in
       let a4 = a39 1 in
       let a38 = `get_tuple_field a0 in
       let a6 = a38 0 in
       let a7 = `arg_8 in
       let a37 = `get_tuple_field a7 in
       let a9 = a37 2 in
       let a36 = `get_tuple_field a7 in
       let a11 = a36 1 in
       let a35 = `get_tuple_field a7 in
       let a13 = a35 0 in
       let a34 = ( * ) a11 in
       let a33 = a34 a2 in
       let a30 = ( - ) a33 in
       let a32 = ( * ) a9 in
       let a31 = a32 a4 in
       let a15 = a30 a31 in
       let a29 = ( * ) a9 in
       let a28 = a29 a6 in
       let a25 = ( - ) a28 in
       let a27 = ( * ) a13 in
       let a26 = a27 a2 in
       let a17 = a25 a26 in
       let a24 = ( * ) a13 in
       let a23 = a24 a4 in
       let a20 = ( - ) a23 in
       let a22 = ( * ) a11 in
       let a21 = a22 a6 in
       let a19 = a20 a21 in
       (a15, a17, a19)
     let main =
       let a41 = (1, 2, 3) in
       let a42 = (4, 5, 6) in
       let a46 = cross a41 in
       let a44 = a46 a42 in
       print_tuple a44
    |}]
;;

let%expect_test _ =
  test {|
    let rec map f list = match list with
    | [] -> []
    | hd::tl -> (f hd) :: (map f tl) 
  |};
  [%expect {|
    FVs [] in fun
    (fun f list -> if (`list_is_empty list) then [] else let hd = (`list_hd list)
     in let tl = (`list_tl list)
     in (( :: ) (f hd) (map f tl)))
    FV in map: {}

    let map f list =
      let a1 = `list_is_empty list in
      if a1
      then []
      else
        let a3 = `list_hd list in
        let a5 = `list_tl list in
        let a10 = f a3 in
        let a7 = ( :: ) a10 in
        let a9 = map f in
        let a8 = a9 a5 in
        a7 a8
    |}]
;;