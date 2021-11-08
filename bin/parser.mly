%{
    open Ast
%}

%token <int> INT
%token TRUE
%token FALSE
%token <string> STRING
%token UNIT

%token <string> IDENTIFIER
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token PLUS
%token TIMES
%token FUN
%token ARROW

%token EOF

%nonassoc IN
%nonassoc ELSE
%left PLUS
%left TIMES

%start <Ast.expr> program

%%

program:
    | e = expr; EOF { e }
    ;

expr:
    | x = INT { E_Int x }
    | x = STRING { E_String x}
    | TRUE { E_Bool true }
    | FALSE { E_Bool false }
    | x = IDENTIFIER { E_Variable x }
    | UNIT { E_Unit }
    | LPAREN; e = expr; RPAREN { e }
    | e1 = expr; PLUS; e2 = expr { E_Binop { op = Add; left = e1; right = e2 } }
    | e1 = expr; TIMES; e2 = expr { E_Binop { op = Mult; left = e1; right = e2 } }
    | LET; x = IDENTIFIER; EQUALS; e1 = expr; IN; e2 = expr { E_Let_in { param = x; arg = e1; body = e2 } }
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { E_If_Else { guard = e1; if_branch = e2; else_branch = e3} }
    | FUN; x = IDENTIFIER; ARROW; e = expr { E_Abstraction { param = x; body = e } }
    | e1 = expr; e2 = expr { E_Application { func = e1; arg = e2 } }
    ;
