open Llvm
open Ast
open Anf

(* LLVM setup *)
let global_context = global_context ()
let builder = builder global_context
let the_module = create_module global_context "HamsterML_LLVM_Compiler"
let i64 = i16_type global_context
let lookup_function_exception id llmodule = Option.get @@ lookup_function id llmodule

let build_binary_operation = function
  | ADD -> build_add
  | SUB -> build_sub
  | MUL -> build_mul
  | DIV -> build_udiv
  | EQ -> build_icmp Icmp.Eq
  | NEQ -> build_icmp Icmp.Ne
  | GT -> build_icmp Icmp.Sgt
  | GTE -> build_icmp Icmp.Sge
  | LT -> build_icmp Icmp.Slt
  | LTE -> build_icmp Icmp.Sle
  | AND -> build_and
  | OR -> build_or
  | CONCAT ->
    fun lhs rhs name builder ->
      let concat_fn = lookup_function_exception "hamsterml_concat" the_module in
      let fnty = function_type i64 [| i64; i64 |] in
      build_call fnty concat_fn [| lhs; rhs |] name builder
  | ID_EQ -> failwith "'==' operation is not expected to be implemented"
;;

let build_unary_operation = function
  | UMINUS -> build_neg
  | NOT -> build_not
  | UPLUS -> fun value _ _ -> value (* unary plus is an identity operation *)
;;

let rec codegen_immexpr env =
  let list_helper lst =
    let allocated_list =
      build_call
        (function_type i64 [||])
        (lookup_function_exception "hamsterml_alloc_list" the_module)
        [||]
        "hamsterml_allc_list_n"
        builder
    in
    let add = lookup_function_exception "hamsterml_add_to_list" the_module in
    let fnty = function_type i64 [| i64; i64 |] in
    Base.List.fold (Base.List.rev lst) ~init:allocated_list ~f:(fun acc elem ->
      let elem = codegen_immexpr env elem in
      let acc = acc in
      build_call fnty add [| acc; elem |] "hamsterml_add_to_list" builder)
  in
  function
  | ImmInt i -> const_int i64 i
  | ImmBool b -> const_int i64 (Base.Bool.to_int b)
  | ImmList l -> list_helper l
  | ImmTuple tpl ->
    let allocated_tuple =
      build_call
        (function_type i64 [| i64 |])
        (lookup_function_exception "hamsterml_alloc_tuple" the_module)
        [| Base.List.length tpl |> const_int i64 |]
        "hamsterml_alloc_tuple_n"
        builder
    in
    Base.List.fold tpl ~init:allocated_tuple ~f:(fun acc elem ->
      let elem = codegen_immexpr env elem in
      let acc = acc in
      build_call
        (function_type i64 [| i64; i64 |])
        (lookup_function_exception "hamsterml_fill_tuple" the_module)
        [| acc; elem |]
        "hamsterml_fill_tuple_n"
        builder)
  | ImmString s ->
    Base.List.init (Base.String.length s) ~f:(Base.String.get s)
    |> Base.List.map ~f:(fun c -> ImmInt (Base.Char.to_int c))
    |> list_helper
  | ImmId id ->
    (match Base.Map.find env id with
     | Some value ->
       let llval = value in
       build_load i64 llval id builder
     | None ->
       let llv = lookup_function_exception id the_module in
       if params llv |> Base.Array.length = 0
       then (
         let fnty = function_type i64 [| i64 |] in
         let func = lookup_function_exception "hamsterml_apply0" the_module in
         let args = [| build_pointercast llv i64 "ptr_to_i64_n" builder |] in
         build_call fnty func args "hamsterml_apply0_n" builder)
       else llv)
  | ImmOperation op ->
    (match op with
     | Binary bop -> codegen_immexpr env (ImmId (BinOperator.to_string bop))
     | Unary uop -> codegen_immexpr env (ImmId (UnOperator.to_string uop)))
  | ImmUnit -> const_int i64 0
;;

let codegen_cexpr env = function
  | CImm ie -> codegen_immexpr env ie
  | CConstructList (ie1, ie2) ->
    let arg_value = codegen_immexpr env ie1 in
    let arg_list = codegen_immexpr env ie2 in
    let add_to_list = lookup_function_exception "hamsterml_add_to_list" the_module in
    let fnty = function_type i64 [| i64; i64 |] in
    let result =
      build_call
        fnty
        add_to_list
        [| arg_list; arg_value |]
        "hamsterml_add_to_list_n"
        builder
    in
    result
  | CApplication (ce1, ce2, ce_list) -> failwith "not yet implemented"
  | CIf (ie, ae, ae_opt) -> failwith "not yet implemented"
;;

(*
   TODO:
   let codegen_aexpr = failwith "not yet implemented"
   let codegen_global_scope_function = failwith "not yet implemented"
   let codegen = failwith "not yet implemented"
*)
