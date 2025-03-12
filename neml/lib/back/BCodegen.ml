[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Llvm

open LMisc
open LMiddle
open BCommon

type err = TypeError of string | NotImplemented of string
[@@deriving show {with_path= false}]

module FuncDefs = struct
  type t = (LLId.t, llvalue * lltype, LLId.comparator_witness) Map.t
  let empty : t = Map.empty (module LLId)
  let single x y : t = Map.singleton (module LLId) x y
end

module Locals = struct
  type t = (LLId.t, llvalue, LLId.comparator_witness) Map.t
  let empty : t = Map.empty (module LLId)
  let single x y : t = Map.singleton (module LLId) x y
end

module LLMonad : sig
  include MONAD

  val run : 'a t -> ('a, err) Result.t
  val fail : err -> 'a t

  val funcs : FuncDefs.t t
  val locals : Locals.t t

  val add_func : LLId.t -> func:llvalue -> typ:lltype -> unit t
  val extend_locals : Locals.t -> 'a t -> 'a t
end = struct
  type state = {funcs: FuncDefs.t; locals: Locals.t}

  include
    MakeSEMonad
      (struct
        type t = state
      end)
      (struct
        type t = err
      end)

  let run m = run m {funcs= FuncDefs.empty; locals= Locals.empty} |> snd

  let funcs =
    let* {funcs; _} = get in
    return funcs

  let locals =
    let* {locals; _} = get in
    return locals

  let add_func id ~func ~typ =
    let* st = get in
    put {st with funcs= Map.set st.funcs ~key:id ~data:(func, typ)}

  let extend_locals (locals : Locals.t) m =
    let* st = get in
    let* () =
      put
        { st with
          locals=
            Map.merge_skewed st.locals locals ~combine:(fun ~key:_ _ v2 -> v2)
        }
    in

    let* x = m in
    let* new_st = get in
    let* () = put {new_st with locals= st.locals} in
    return x
end

type builtin = llbuilder -> LLId.t * llvalue * lltype

module LLCodeGen (LLModule : sig
  val lmod : llmodule
end) (LLRuntime : sig
  val create_closure : func:llvalue -> argc:int -> llbuilder -> llvalue
  val apply_closure :
    closure:llvalue -> args:llvalue list -> llbuilder -> llvalue
end) : sig
  val gen : builtins:builtin list -> MAnf.t -> (unit, err) Result.t
end = struct
  let lmod = LLModule.lmod
  let ctx = module_context lmod
  let bld = builder ctx
  let i64 = i64_type ctx
  let i1 = i1_type ctx

  open LLMonad

  let lookup_function (LLId.LId id as lid) : (llvalue * lltype) t =
    let* funcs = funcs in
    let* func, func_type =
      Map.find funcs lid
      |> Option.value_map
           ~default:(fail (TypeError (id ^ " unbound")))
           ~f:return
    in
    return (func, func_type)

  let gen_imm : MAnf.imm -> llvalue t = function
    | Id tagged -> (
        let lid = LLId.from_tagged tagged in

        let process_global () =
          let* func, func_type = lookup_function lid in
          let closure =
            LLRuntime.create_closure ~func
              ~argc:(Array.length (param_types func_type))
              bld
          in
          return closure
        in

        let* locals = locals in
        match Map.find locals lid with
        | Some value ->
            return value
        | None ->
            process_global () )
    | Const (Int i) ->
        return (const_int i64 i)
    | Const Unit ->
        return (const_null i64)
    | Const (Bool b) ->
        return (const_int (i1_type ctx) (Bool.to_int b))
    | Const (String _ | Char _) ->
        fail (NotImplemented "weird constants")

  let gen_cmplx : MAnf.cmplx -> llvalue t = function
    | Imm imm ->
        gen_imm imm
    | Apply (Id tagged, args) -> (
        let lid = LLId.from_tagged tagged in
        let* args =
          List.fold_right (List1.to_list args) ~init:(return [])
            ~f:(fun arg acc ->
              let* acc = acc in
              let* value = gen_imm arg in
              return (value :: acc) )
        in

        let process_global () =
          let* func, func_type = lookup_function lid in
          let argc = Array.length (param_types func_type) in

          if List.length args = argc then
            return (build_call func_type func (List.to_array args) "r" bld)
          else
            let closure = LLRuntime.create_closure ~func ~argc bld in
            return (LLRuntime.apply_closure ~closure ~args bld)
        in

        let* locals = locals in
        match Map.find locals lid with
        | Some value ->
            return (LLRuntime.apply_closure ~closure:value ~args bld)
        | None ->
            process_global () )
    | Apply (Const _, _) ->
        fail (TypeError "application to constant")

  let rec gen_anf : MAnf.anf -> llvalue t = function
    | Cmplx cmplx ->
        gen_cmplx cmplx
    | Let (tagged, cmplx, anf) ->
        let (LId id as lid) = LLId.from_tagged tagged in
        let* val_cmplx = gen_cmplx cmplx in
        set_value_name id val_cmplx ;
        extend_locals
          (Map.of_alist_exn (module LLId) [(lid, val_cmplx)])
          (gen_anf anf)
    | If (icond, athen, aelse) ->
        let* val_cond = gen_imm icond in
        let val_cond = build_intcast val_cond i1 "cond" bld in

        let func = block_parent (insertion_block bld) in
        let bthen = append_block ctx "then" func in
        let belse = append_block ctx "else" func in
        let bcont = append_block ctx "cont" func in

        let _ = build_cond_br val_cond bthen belse bld in
        position_at_end bthen bld ;
        let* val_then = gen_anf athen in
        let _ = build_br bcont bld in

        position_at_end belse bld ;
        let* val_else = gen_anf aelse in
        let _ = build_br bcont bld in

        position_at_end bcont bld ;
        return @@ build_phi [(val_then, bthen); (val_else, belse)] "r" bld

  let gen_func (FuncDef.Func {recf= _; id= tagged; args; body}) =
    let (LId id as lid) = LLId.from_tagged tagged in
    let args = List1.to_list args in

    let func_type =
      function_type i64 (Array.init (List.length args) ~f:(fun _ -> i64))
    in
    let func = define_function id func_type lmod in
    let () = position_at_end (entry_block func) bld in
    let* () = add_func lid ~func ~typ:func_type in

    let locals =
      List.foldi args ~init:Locals.empty ~f:(fun i acc arg_id ->
          let (LId id as lid) = LLId.from_tagged (arg_id, User) in
          let arg_value = param func i in
          set_value_name id arg_value ;
          Map.set acc ~key:lid ~data:arg_value )
    in

    let* ret = extend_locals locals (gen_anf body) in
    let _ = build_ret ret bld in
    return ()

  let gen_builtins (builtins : builtin list) =
    List.fold_left builtins ~init:(return ()) ~f:(fun acc bltn ->
        let* () = acc in
        let id, func, typ = bltn bld in
        add_func id ~func ~typ )

  let gen ~(builtins : builtin list) ((defs, anf) : MAnf.t) =
    (let* () = gen_builtins builtins in

     let* () =
       List.fold_left defs ~init:(return ()) ~f:(fun acc def ->
           let* () = acc in
           gen_func def )
     in

     let main = define_function "main" (function_type i64 [||]) lmod in
     let () = position_at_end (entry_block main) bld in
     let* _ = gen_anf anf in

     let _ = build_ret (const_int i64 0) bld in
     return () )
    |> run
end
