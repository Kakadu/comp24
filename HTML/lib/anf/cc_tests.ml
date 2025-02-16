module CcTests = struct
  let cc_test s =
    match Parser.parse_program s with
    | Ok actual ->
      let prog = Cc.closure_convert actual in
      Format.printf "%a\n" AstLib.Pp_ast.pp_prog prog
    | Error err -> Format.printf "%s\n" err
  ;;
end

let%expect_test "sanity check" =
  CcTests.cc_test {|let test1 x = let test2 y = x + y in test2|};
  [%expect
    {|
    let fun-0 x y = (x + y);;
    let test1 x = let test2 = (fun-0 x)
    in test2 |}]
;;

(*
   fun-5 = fun n -> (fun-3, ( *, -, facCPS, fun-1, n))
   fun-3 = fun k -> (match n with
   + | 0 -> (k 1)
   + | n -> ((facCPS ((- n) 1)) (fun-1, (( *, k, n))))));;
     fun-1 = fun t -> (k ((( * ) n) t) )
*)

let%expect_test "sanity check" =
  CcTests.cc_test
    {|let rec fact_cps n cont =
  if (n = 0) then
    cont 1
   else
     fact_cps (n - 1) (fun acc ->  cont  (n * acc))
|};
  [%expect
    {|
    let fun-0 n cont acc = cont (n * acc);;
    let rec fact_cps n cont = if (n = 0) then (cont 1) else (fact_cps (n - 1) ((fun-0 n) cont)) |}]
;;

(*
   fun-5 = fun x -> (fun-3, (+, fun-1, x))
   fun-3 = fun y -> (fun-1, (+, x))
   fun-1 = fun z -> ((+ x) z)
*)

let%expect_test "sanity check" =
  CcTests.cc_test
    {|let nested1 = let nested2 = 5 in 
  let nested3 = 6 in
  let nested4 x = x + (fun i -> nested2 + nested3) 8 in nested4 55
|};
  [%expect
    {|
    let fun-0 nested3 nested2 i = (nested2 + nested3);;
    let fun-1 nested3 nested2 x = (x + (((fun-0 nested3) nested2) 8));;
    let nested1 = let nested2 = 5
    in let nested3 = 6
    in let nested4 = ((fun-1 nested3) nested2)
    in (nested4 55) |}]
;;

let%expect_test "sanity check" =
  CcTests.cc_test
    {|let length_tail =
  let rec helper acc xs =
  match xs with
  | [] -> acc
  | h::tl -> helper (acc + 1) tl
  in
  helper 0

|};
  [%expect
    {|
    let rec fun-0 acc xs = match xs with
    | [] -> acc
    | h :: tl -> (fun-0 (acc + 1) tl);;
    let length_tail = let helper = fun-0
    in (helper 0) |}]
;;

let%expect_test "sanity check" =
  CcTests.cc_test
    {|let rec meven n = if n = 0 then 1 else modd (n - 1)
and modd n = if n = 0 then 1 else meven (n - 1)

|};
  [%expect
    {|
    let rec meven n = if (n = 0) then 1 else modd (n - 1)
    and modd n = if (n = 0) then 1 else meven (n - 1) |}]
;;
