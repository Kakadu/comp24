open Anf_ast
open Me_ast
open Common
open StateMonad


let const_to_immexp = function
  | Me_Cint i -> ImmInt i
  | Me_CBool b -> ImmBool b
;;

let rec to_cexp = function
  | Me_ETuple elems ->
    let* binds, imm_elems =
      RList.fold_left elems ~init:(return ([], [])) ~f:(fun (acc_binds, acc_elems) el ->
        let* binds_el, imm_el = check_hard_expr el in
        return (acc_binds @ binds_el, acc_elems @ [imm_el]))
    in
    return (binds, CImmExpr (ImmTuple imm_elems))

  | Me_ELet (NoRec, name, e1, e2) ->
    let* binds1, ce1 = to_cexp e1 in
    let* binds2, ce2 = to_cexp e2 in
    return (binds1 @ [ name, ce1 ] @ binds2, ce2)

  | Me_ECons (e1, e2) ->
    let* binds1, a1 = check_hard_expr e1 in
    let* binds2, a2 = check_hard_expr e2 in
    return (binds1 @ binds2, CECons (a1, a2))

  | Me_EConst c -> return ([], CImmExpr (const_to_immexp c))
  | Me_EIdentifier v -> return ([], CImmExpr (ImmIdentifier v))
  | Me_EUnit -> return ([], CImmExpr ImmUnit)
  | Me_ENill -> return ([], CImmExpr ImmNill)
  | _ -> failwith "See you later space cowboy"

(* для обработки сложных выражений в условиях *)
and check_hard_expr  e = match e with
  | Me_EIdentifier v -> return ([], ImmIdentifier v)
  | Me_EConst c -> return ([], const_to_immexp c)
  | Me_EUnit -> return ([], ImmUnit)
  | Me_ENill -> return ([], ImmNill)
    | e -> 
      let* id = fresh in
      let name = "anf" ^ Int.to_string id in
      let* binds1, ce = to_cexp e in
      return (binds1 @ [ name, ce ], (ImmIdentifier name))
;;

(* let anf prog =
  StateMonad.run
    (RList.fold_left prog ~init:(return []) ~f:(fun acc decl ->
       let* d = anf_decl decl in
       return (acc @ [ d ])))
;; *)
