(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type me_ident =
  | Me_unit
  | Me_name of string
[@@deriving eq, show { with_path = false }]

type m_expr =
  | MExp_constant of Common.Ast.const
  | MExp_ident of Common.Ast.ident
  | MExp_tuple of m_expr list
  | MExp_apply of m_expr * m_expr
  | MExp_list of m_expr * m_expr
  | MExp_ifthenelse of m_expr * m_expr * m_expr
  | MExp_function of me_ident * m_expr
  | MExp_let of m_decl * m_expr
[@@deriving show { with_path = false }]

and m_decl = MDecl of Common.Ast.rec_flag * value_binding list

and value_binding =
  { m_vb_pat : me_ident
  ; m_vb_expr : m_expr
  }

type m_program = m_decl list [@@deriving show { with_path = false }]
