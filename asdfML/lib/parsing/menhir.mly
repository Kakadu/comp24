%{
  [@@@coverage exclude_file]
  open Ast
%}

%token <int> INT
%token <string> IDENT
%token <bool> BOOL
%token UNIT
%token WILDCARD

%token PLUS MINUS 
%token MUL DIV
%token EQ
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

program : p = list(definition) EOF { p }

definition: 
| LETREC pat = pattern EQ e = expr { DLet(Rec, pat, e) }
| LET pat = pattern EQ e = expr { DLet(NonRec, pat, e) }

expr: 
| c = constant { EConst(c) }
| v = identifier { EVar(v) }
| e = expr_binary { e }
| e = expr_unary { e }
| app = application { app }
| IF cond = expr THEN tbranch = expr ELSE fbranch = expr { EIfElse(cond, tbranch, fbranch) }
| FUN pat = pattern ARROW body = expr { EFun(pat, body) }
| def = definition IN body = expr { ELetIn(def, body) }
| LPAREN es = separated_nonempty_list(COMMA, expr) RPAREN { ETuple(es) }
| LBRACK es = separated_nonempty_list(SEMI, expr) RBRACK { EList(es) }
| m = match_ { m }
| LPAREN e = expr RPAREN { e }

type_ann:
| id = identifier { 
    match id with 
    | "int" -> TAInt
    | "bool" -> TABool
    | "()" -> TAUnit
    | _ -> failwith "unknown type annotation"
  }
| hd = type_ann ARROW tl = type_ann { TAFun(hd, tl) }
| LPAREN es = separated_nonempty_list(COMMA, type_ann) RPAREN { TATuple(es) }

pattern: 
| c = constant { PConst(c) }
| WILDCARD { PWild }
| id = identifier { PIdent(id, None) }
| LPAREN op = op_binary RPAREN { PIdent(op, None) }
| LPAREN id = identifier COLON ty = type_ann RPAREN { PIdent(id, Some(ty)) }
| LPAREN es = separated_nonempty_list(COMMA, pattern) RPAREN { PTuple(es) }
| LBRACK es = separated_nonempty_list(SEMI, pattern) RBRACK { PList(es) }
| l = pattern CONS r = pattern { PCons(l, r) }

match_:
| MATCH p = pattern WITH cs = nonempty_list(case) { EMatch(p, cs) }
case:
| BAR p = pattern ARROW e = expr { (p, e) }

application :
| l = application r = app_expr { EApp(l, r) }
| a = app_expr { a }

app_expr: 
| LPAREN e = expr RPAREN { e }
| c = constant { EConst(c) }
| v = identifier { EVar(v) }

expr_binary: 
| left = expr op = op_binary right = expr { EApp(EApp(EVar(op), left), right) }

%inline op_binary:
| MUL { "( * )" }
| DIV { "( / )" }
| PLUS { "( + )" }
| MINUS { "( - )" }
| EQEQ { "( == )" }
| NE { "( != )" }
| GT { "( > )" }
| LT { "( < )" }
| GE { "( >= )" }
| LE { "( <= )" }
| AND { "( && )" }
| OR { "( || )" }
| CONS { "( :: )" }

expr_unary: 
| op = op_unary e = expr { EApp(EVar(op), e) }

%inline op_unary:
| MINUS { "[ - ]" }
// | NOT { "[ not ]" }

constant: 
| i = INT { CInt i }
| b = BOOL { CBool b }
| UNIT { CUnit }

identifier: 
| id = IDENT { id }
