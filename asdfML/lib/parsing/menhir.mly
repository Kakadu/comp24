%{
  [@@@coverage exclude_file]
  open Ast
%}

%token <int> INT
%token <string> IDENT
%token <bool> BOOL
%token UNIT
%token WILDCARD

%token PLUS 
%token MINUS 
%token MUL 
%token DIV
%token EQ
%token EQEQ
%token NE
%token GT
%token LT
%token GE
%token LE
%token AND
%token OR
%token NOT

%token LPAREN RPAREN
%token COLON

%token LET 
%token LETREC
%token IN
%token FUN ARROW
%token IF THEN ELSE

%token SS
%token EOF

%type <program> program

%start program

%%

program : p = list(definition) EOF { p }

definition: 
| LETREC pat = pattern EQ e = expr { DLet(Rec, pat, e) }
| LET pat = pattern EQ e = expr { DLet(NonRec, pat, e) }

expr: 
| IF cond = expr THEN tbranch = expr ELSE fbranch = expr { EIfElse(cond, tbranch, fbranch) }
| FUN pat = pattern ARROW body = expr { EFun(pat, body) }
| def = definition IN body = expr { ELetIn(def, body) }
| app = application { app }
| e = expr_unary { e }
| e = expr_binary { e }
| c = constant { EConst(c) }
| v = identifier { EVar(v) }
| LPAREN e = expr RPAREN { e }

type_ann:
| id = identifier { 
    match id with 
    | "int" -> TInt
    | "bool" -> TBool
    | "()" -> TUnit
    | _ -> failwith "TODO"
  }
| hd = type_ann ARROW tl = type_ann { TFun(hd, tl) }

pattern: 
| LPAREN id = identifier COLON ty = type_ann RPAREN { PIdent(id, Some(ty)) }
| WILDCARD { PWild }
| id = identifier { PIdent(id, None) }

application :
| l = application r = app_expr { EApp(l, r) }
| a = app_expr { a }

app_expr: 
| LPAREN e = expr RPAREN { e }
| c = constant { EConst(c) }
| v = identifier { EVar(v) }

expr_binary: 
| left = expr op = op_binary right = expr { EBinaryOp(op, left, right) }

op_binary:
| MUL {Mul}
| DIV {Div}
| PLUS {Add}
| MINUS {Sub}
| EQEQ { Eq }
| NE { Ne }
| GT { Gt }
| LT { Lt }
| GE { Ge }
| LE { Le }
| AND { And }
| OR { Or }

expr_unary:
| op = op_unary arg = constant {EUnaryOp (op, EConst(arg))}

op_unary: 
| MINUS { Neg }
| NOT { Not }

constant: 
| i = INT { CInt i }
| b = BOOL { CBool b }
| UNIT { CUnit }

identifier: 
| id = IDENT { id }
