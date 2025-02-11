(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

%{
  [@@@coverage exclude_file]
  open Ast
%}

%token <int> INT
%token <string> IDENT
%token <bool> BOOL
%token UNIT
%token NIL
%token WILDCARD

%token PLUS MINUS 
%token MUL DIV
%token EQ
%token LTGT
%token EQEQ
%token NE
%token GT LT GE LE
%token AND
%token OR
%token NOT

%token LPAREN RPAREN
%token LBRACK RBRACK
%token COLON COMMA
%token CONS

%token LET 
%token LETREC
%token IN
%token FUN ARROW
%token IF THEN ELSE
%token MATCH WITH BAR

%token SEMI SS
%token EOF

// Priorities (low to high)
%left GT LT GE LE
%left PLUS MINUS 
%left MUL DIV

%right AND OR

%type <program> program

%start program

%%

take_2_or_more(item):
| hd = item tl = nonempty_list(item)   { hd :: tl }

take_2_or_more_sep(item, sep):
| hd = item sep tl = separated_nonempty_list(sep, item)   { hd :: tl }


program : p = list(definition) EOF { p }

definition: 
| LETREC pat = pattern args = list(pattern) EQ e = expr option(SS) { 
    match args with 
    | [] -> DLet(Rec, pat, e)
    | xs -> DLet(Rec, pat, EFun(xs, e))
  }
| LET pat = pattern args = list(pattern) EQ e = expr option(SS) { 
    match args with 
    | [] -> DLet(NonRec, pat, e)
    | xs -> DLet(NonRec, pat, EFun(xs, e))
  }

expr: 
| c = constant { EConst(c) }
| v = identifier { EVar(v) }
| e = expr_binary { e }
| e = expr_unary { e }
| app = application { app }
| ifelse = ifelse { ifelse }
| lambda = lambda { lambda }
| let_in = let_in { let_in }
| t = tuple { t }
| l = list_ { l }
| m = match_ { m }
| LPAREN e = expr RPAREN { e }

ifelse:
| IF cond = expr THEN tbranch = expr ELSE fbranch = expr { EIfElse(cond, tbranch, fbranch) }
| LPAREN ifelse = ifelse RPAREN { ifelse }

lambda:
| FUN pat = nonempty_list(pattern) ARROW body = expr { EFun(pat, body) }
| LPAREN lambda = lambda RPAREN { lambda }

let_in:
| def = definition IN body = expr { ELetIn(def, body) }
| LPAREN let_in = let_in RPAREN { let_in }

application :
| l = application r = app_expr { EApp(l, r) }
| LPAREN a = let_in RPAREN { a }
| LPAREN a = lambda RPAREN { a }
| LPAREN a = ifelse RPAREN { a }
| LPAREN a = match_ RPAREN { a }
| a = app_expr { a }
| op = op_binary { EVar(op) }

app_expr: 
| LPAREN e = expr RPAREN { e }
| c = constant { EConst(c) }
| t = tuple { t }
| l = list_ { l }
| v = identifier { EVar(v) }

tuple:
| LPAREN es = take_2_or_more_sep(expr, COMMA) RPAREN { ETuple(es) }

list_:
| LBRACK es = separated_nonempty_list(SEMI, expr) RBRACK { EList(es) }

type_ann:
| LPAREN es = take_2_or_more_sep(type_ann, MUL) RPAREN { TATuple(es) }
| ann = type_ann id = identifier { 
    if String.equal id "list" 
    then TAList ann
    else (failwith "list type ann")
  }
| id = identifier { 
    match id with 
    | "int" -> TAInt
    | "bool" -> TABool
    | _ -> failwith "unknown type annotation"
  }
| UNIT { TAUnit }
| hd = type_ann ARROW tl = type_ann { TAFun(hd, tl) }
| LPAREN hd = type_ann ARROW tl = type_ann RPAREN { TAFun(hd, tl) }

pattern: 
| c = constant { 
    match c with 
    | CInt _ | CBool _ | CUnit -> PConst(c)
    | CNil -> PConst(CNil)
  }
| WILDCARD { PWild }
| id = identifier { PIdent(id) }
| LPAREN op = op_binary RPAREN { PIdent(op) }
| LPAREN pat = pattern COLON ty = type_ann RPAREN { PAnn(pat, ty) }
| LPAREN es = take_2_or_more_sep(pattern, COMMA) RPAREN { PTuple(es) }
| LBRACK es = separated_nonempty_list(SEMI, pattern) RBRACK { PList(es) }
| l = pattern CONS r = pattern { PCons(l, r) }
| LPAREN p = pattern RPAREN { p }

match_:
| MATCH e = expr WITH option(BAR) cs = separated_nonempty_list(BAR, case) { EMatch(e, cs) }
case:
| p = pattern ARROW e = expr { (p, e) }

expr_binary: 
| left = expr op = op_binary right = expr { EApp(EApp(EVar(op), left), right) }

%inline op_binary:
| MUL { "( * )" }
| DIV { "( / )" }
| PLUS { "( + )" }
| MINUS { "( - )" }
| EQ { "( = )" }
| LTGT { "( <> )" }
| EQEQ { "( = )" }  // <- todo
| NE { "( <> )" }
| GT { "( > )" }
| LT { "( < )" }
| GE { "( >= )" }
| LE { "( <= )" }
| AND { "( && )" }
| OR { "( || )" }
| CONS { "( :: )" }

expr_unary: 
| op = op_unary e = expr { 
    match op, e with
    | "[ - ]", EConst(CInt(x)) -> EConst(CInt(-x))
    | _ -> EApp(EVar(op), e)
  }

%inline op_unary:
| MINUS { "[ - ]" }
// | NOT { "[ not ]" }

constant: 
| i = INT { CInt i }
| MINUS i = INT { CInt (-i) }
| b = BOOL { CBool b }
| UNIT { CUnit }
| NIL { CNil }

identifier: 
| id = IDENT { id }
