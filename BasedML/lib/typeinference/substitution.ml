(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)
include Common.StateMonad

open Ast

module MapString = struct
  include Map.Make (String)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%S\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

type substitution_list = (string * typeName) list [@@deriving show { with_path = false }]
type subs_state = substitution_list [@@deriving show { with_path = false }]

let rec apply_subst (stv, stp) tp =
  let rec_call = apply_subst (stv, stp) in
  (* DEBUG PRINT
     let _ = Format.printf "[apply]: change %s in %s\n" stv (Ast.show_typeName tp) in
  *)
  match tp with
  | TPoly tv when stv = tv -> stp
  | TTuple t_lst -> TTuple (List.map rec_call t_lst)
  | TFunction (t1, t2) -> TFunction (rec_call t1, rec_call t2)
  | TList t -> TList (rec_call t)
  | _ as t -> t
;;

let apply_substs sub_lst tp =
  List.fold_right (fun subst tp -> apply_subst subst tp) sub_lst tp
;;

type occurs_status =
  | Found
  | NotFound

let or_occurs_status st1 st2 =
  match st1 with
  | Found -> Found
  | NotFound -> st2
;;

let rec occurs_check tv tp =
  let rec_call = occurs_check tv in
  match tp with
  | TPoly new_tv when new_tv = tv -> Found
  | TTuple t_lst ->
    List.fold_left (fun acc tp -> or_occurs_status acc (rec_call tp)) NotFound t_lst
  | TFunction (tp1, tp2) -> or_occurs_status (rec_call tp1) (rec_call tp2)
  | TList tp -> rec_call tp
  | TBool | TInt | TPoly _ -> NotFound
;;

let rec unify : typeName -> typeName -> (subs_state, typeName) t =
  fun tp1 tp2 ->
  match tp1, tp2 with
  | tp1, tp2 when tp1 = tp2 -> return tp1
  | TPoly tv, tp | tp, TPoly tv ->
    (match occurs_check tv tp with
     | NotFound ->
       let* ret_tp = insert_subst (tv, tp) in
       return ret_tp
     | Found ->
       fail (Format.sprintf "The type variable %s occurs inside %s" tv (show_typeName tp)))
  | TTuple t_lst1, TTuple t_lst2 ->
    let* new_t_lst =
      map_list (fun (tp1, tp2) -> unify tp1 tp2) (List.combine t_lst1 t_lst2)
    in
    return (TTuple new_t_lst)
  | TFunction (arg1, body1), TFunction (arg2, body2) ->
    let* new_arg_t = unify arg1 arg2 in
    let* new_body_t = unify body1 body2 in
    return (TFunction (new_arg_t, new_body_t))
  | TList tp1, TList tp2 -> unify tp1 tp2
  | _ ->
    fail
      (Format.sprintf
         "Can not unify `%s` and `%s`"
         (show_typeName tp1)
         (show_typeName tp2))

and insert_subst : string * typeName -> (subs_state, typeName) t =
  fun (stv, stp) ->
  let* sub_lst = read in
  match List.assoc_opt stv sub_lst with
  | Some tp ->
    let* tp = unify tp stp in
    return tp
  | None ->
    let stp = apply_substs sub_lst stp in
    let new_sub_lst =
      List.map
        (fun (tv, tp) ->
          let new_tp = apply_subst (stv, stp) tp in
          tv, new_tp)
        sub_lst
    in
    write ((stv, stp) :: new_sub_lst) *> return stp
;;
