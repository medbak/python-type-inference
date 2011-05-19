/*
 * Soonho Kong (soonhok@cs.cmu.edu)
 *
 * parser.mly : Parser for Python AST
 */

%{ 

%}

%token ADD AND ASSERT ASSIGN ATTRIBUTE AUGASSIGN AUGLOAD AUGSTORE BINOP BITAND BITOR BITXOR BOOLOP BOOLOP BREAK CALL CLASSDEF
%token COMPARE CONTINUE DEL DELETE DICT DICTCOMP DIV ELLIPSIS EQ EXCEPTHANDLER EXEC EXPR EXPRESSION EXTSLICE FLOORDIV FOR
%token FUNCTIONDEF GENERATOREXP GLOBAL GT GTE IF IFEXP IMPORT IMPORTFROM IN INDEX INTERACTIVE INVERT IS ISNOT LSHIFT LAMBDA LIST
%token LISTCOMP LOAD LT LTE MOD MODULE MULT NAME NONE NOT NOTEQ NOTIN NUM OR PARAM PASS POW PRINT RSHIFT RAISE REPR RETURN
%token SET SETCOMP SLICE STORE STR SUB SUBSCRIPT SUITE TRYEXCEPT TRYFINALLY TUPLE UADD USUB UNARYOP WHILE WITH YIELD
%token ALIAS KEYWORD ARGUMENTS COMPREHENSION TRUE FALSE INF NEGINF
%token LINENO COLOFFSET
 
%token LB RB LP RP COMMA EQUAL

%token EOF

%token <int> INUM
%token <int> LNUM
%token <float> FNUM
%token <float> CNUM
%token <string> ID
%token <string> STRING
%token <string> USTRING

%start modu

%type <Ast.modu> modu
%type <Ast.stmt> stmt 
%type <Ast.expr> expr
%type <Ast.expr_context> expr_context
%type <Ast.slice> slice
%type <Ast.boolop> boolop 
%type <Ast.operator> operator
%type <Ast.unaryop> unaryop
%type <Ast.cmpop> cmpop 
%type <Ast.comprehension> comprehension
%type <Ast.excepthandler> excepthandler
%type <Ast.arguments> arguments
%type <Ast.keyword> keyword
%type <Ast.alias> alias
%type <Ast.loc> loc
%type <bool> bool
%%

modu: MODULE LP LB stmt_list RB RP EOF { Ast.Module($4) }
     | INTERACTIVE LP LB stmt_list RB RP EOF { Ast.Interactive ($4) }
     | EXPRESSION LP expr RP EOF { Ast.Expression($3) }
   ;

stmt_list: /* epsilon */ { [] }
   | stmt { [$1] }
   | stmt_list COMMA stmt { $1@[$3] }
;

stmt: FUNCTIONDEF LP STRING COMMA arguments COMMA LB stmt_list RB COMMA LB expr_list RB COMMA loc RP
       { Ast.FunctionDef ($3, $5, $8, $12, $15) }
   | CLASSDEF LP STRING COMMA LB expr_list RB COMMA LB stmt_list RB COMMA LB expr_list RB COMMA loc RP
       { Ast.ClassDef ($3, $6, $10, $14, $17) }
   | RETURN LP expr_option COMMA loc RP
       { Ast.Return($3, $5) }
   | DELETE LP LB expr_list RB COMMA loc RP
       { Ast.Delete($4, $7) }
   | ASSIGN LP LB expr_list RB COMMA expr COMMA loc RP
       { Ast.Assign($4, $7, $9)} 
   | AUGASSIGN LP expr COMMA operator COMMA expr COMMA loc RP
       { Ast.AugAssign($3, $5, $7, $9) }
   | PRINT LP expr_option COMMA LB expr_list RB COMMA bool COMMA loc RP 
       { Ast.Print($3, $6, $9, $11) } 
   | FOR LP expr COMMA expr COMMA LB stmt_list RB COMMA LB stmt_list RB COMMA loc RP 
       { Ast.For($3, $5, $8, $12, $15) }
   | WHILE LP expr COMMA LB stmt_list RB COMMA LB stmt_list RB COMMA loc RP 
       { Ast.While($3, $6, $10, $13) }
   | IF LP expr COMMA LB stmt_list RB COMMA LB stmt_list RB COMMA loc RP 
       { Ast.If($3, $6, $10, $13) }
   | WITH LP expr COMMA expr_option COMMA LB stmt_list RB COMMA loc RP 
       { Ast.With($3, $5, $8, $11) }
   | RAISE LP expr_option COMMA expr_option COMMA expr_option COMMA loc RP 
       { Ast.Raise($3, $5, $7, $9) }
   | TRYEXCEPT LP LB stmt_list RB COMMA LB excepthandler_list RB COMMA LB stmt_list RB COMMA loc RP 
       { Ast.TryExcept($4, $8, $12, $15) }
   | TRYFINALLY LP LB stmt_list RB COMMA LB stmt_list RB COMMA loc RP 
       { Ast.TryFinally($4, $8, $11) }
   | ASSERT LP expr COMMA expr_option COMMA loc RP 
       { Ast.Assert($3, $5, $7) }
   | IMPORT LP LB alias_list RB COMMA loc RP 
       { Ast.Import($4, $7) }
   | IMPORTFROM LP identifier_option COMMA LB alias_list RB COMMA INUM COMMA loc RP 
       { Ast.ImportFrom($3, $6, Some($9), $11) }
   | IMPORTFROM LP identifier_option COMMA LB alias_list RB COMMA NONE COMMA loc RP 
       { Ast.ImportFrom($3, $6, None, $11) }
   | EXEC LP expr COMMA expr_option COMMA expr_option COMMA loc RP 
       { Ast.Exec($3, $5, $7, $9) }
   | GLOBAL LP LB identifier_list RB COMMA loc RP { Ast.Global($4, $7) }
   | EXPR LP expr COMMA loc RP { Ast.Expr($3, $5) }
   | PASS LP loc RP     { Ast.Pass($3) }
   | BREAK LP loc RP    { Ast.Break($3) }
   | CONTINUE LP loc RP { Ast.Continue($3) }
;

bool: TRUE { true }
   | FALSE { false }
;

loc: LINENO EQUAL INUM COMMA COLOFFSET EQUAL INUM { ($3, $7 ) }
;

expr_list: /* epsilon */ { [] }
   | expr { [$1] }
   | expr_list COMMA expr { $1@[$3] }
;

expr: BOOLOP LP boolop COMMA LB expr_list RB COMMA loc RP { Ast.BoolOp ($3, $6, $9) }
   | BINOP LP expr COMMA operator COMMA expr COMMA loc RP { Ast.BinOp($3, $5, $7, $9) }
   | UNARYOP LP unaryop COMMA expr COMMA loc RP { Ast.UnaryOp($3, $5, $7) }
   | LAMBDA LP arguments COMMA expr COMMA loc RP { Ast.Lambda($3, $5, $7) }
   | IFEXP LP expr COMMA expr COMMA expr COMMA loc RP { Ast.IfExp ($3, $5, $7, $9) }
   | DICT LP LB expr_list RB COMMA LB expr_list RB COMMA loc RP { Ast.Dict($4, $8, $11) }
   | SET LP LB expr_list RB COMMA loc RP { Ast.Set($4, $7) }
   | LISTCOMP LP expr COMMA LB comprehension_list RB COMMA loc RP { Ast.ListComp ($3, $6, $9) }
   | SETCOMP LP expr COMMA LB comprehension_list RB COMMA loc RP { Ast.SetComp($3, $6, $9) }
   | DICTCOMP LP expr COMMA expr COMMA LB comprehension_list RB COMMA loc RP { Ast.DictComp($3, $5, $8, $11) }
   | GENERATOREXP LP expr COMMA LB comprehension_list RB COMMA loc RP { Ast.GeneratorExp($3, $6, $9) }
   | YIELD LP expr_option COMMA loc RP { Ast.Yield($3, $5) }
   | COMPARE LP expr COMMA LB cmpop_list RB COMMA LB expr_list RB COMMA loc RP { Ast.Compare($3, $6, $10, $13) }
   | CALL LP expr COMMA LB expr_list RB COMMA LB keyword_list RB COMMA expr_option COMMA expr_option COMMA loc RP
       { Ast.Call($3, $6, $10, $13, $15, $17) }
   | REPR LP expr COMMA loc RP { Ast.Repr($3, $5) }
   | NUM LP LNUM COMMA loc RP { Ast.Long($3, $5) }
   | NUM LP CNUM COMMA loc RP { Ast.Complex(0.0, $3, $5) }       
   | NUM LP INUM COMMA loc RP { Ast.Int($3, $5) }
   | NUM LP FNUM COMMA loc RP { Ast.Float($3, $5) }
   | NUM LP NEGINF COMMA loc RP { Ast.Float(neg_infinity, $5) }
   | NUM LP INF COMMA loc RP { Ast.Float(infinity, $5) }              
   | STR LP STRING COMMA loc RP { Ast.Str($3, $5) }
   | STR LP USTRING COMMA loc RP { Ast.UStr($3, $5) }       
   | ATTRIBUTE LP expr COMMA STRING COMMA expr_context COMMA loc RP { Ast.Attribute($3, $5, $7, $9) }
   | SUBSCRIPT LP expr COMMA slice COMMA expr_context COMMA loc RP { Ast.Subscript($3, $5, $7, $9) }
   | NAME LP STRING COMMA expr_context COMMA loc RP { Ast.Name($3, $5, $7) }       
   | LIST LP LB expr_list RB COMMA expr_context COMMA loc RP { Ast.List($4, $7, $9) }
   | TUPLE LP LB expr_list RB COMMA expr_context COMMA loc RP { Ast.Tuple($4, $7, $9) }
;

expr_option: NONE { None }
   | expr { Some($1) }
;

expr_context: LOAD LP RP { Ast.Load }
   | STORE LP RP         { Ast.Store }
   | DEL LP RP           { Ast.Del }
   | AUGLOAD LP RP       { Ast.AugLoad }
   | AUGSTORE LP RP      { Ast.AugStore }
   | PARAM LP RP         { Ast.Param }
;

slice_list: /* epsilon */ { [] }
   | slice { [$1] }
   | slice_list COMMA slice { $1@[$3] }
;

slice: ELLIPSIS LP RP { Ast.Ellipsis }
   | SLICE LP expr_option COMMA expr_option COMMA expr_option RP { Ast.Slice($3, $5, $7) }
   | EXTSLICE LP LB slice_list RB RP { Ast.ExtSlice $4 }
   | INDEX LP expr RP { Ast.Index $3 }
;


boolop: AND LP RP { Ast.And }
   | OR LP RP     { Ast.Or }
;

operator: ADD LP RP { Ast.Add }
   | SUB LP RP      { Ast.Sub }
   | MULT LP RP     { Ast.Mult }
   | DIV LP RP      { Ast.Div }
   | MOD LP RP      { Ast.Mod }
   | POW LP RP      { Ast.Pow }
   | LSHIFT LP RP   { Ast.LShift }
   | RSHIFT LP RP   { Ast.RShift }
   | BITOR LP RP    { Ast.BitOr }
   | BITXOR LP RP   { Ast.BitXor }
   | BITAND LP RP   { Ast.BitAnd }
   | FLOORDIV LP RP { Ast.FloorDiv }
;

unaryop: INVERT LP RP { Ast.Invert }
   | NOT LP RP        { Ast.Not }
   | UADD LP RP       { Ast.UAdd }
   | USUB LP RP       { Ast.USub }
;

cmpop_list: /* epsilon */ { [] }
   | cmpop { [$1] }
   | cmpop_list COMMA cmpop { $1@[$3] }
;
       
cmpop: EQ LP RP  { Ast.Eq }
   | NOTEQ LP RP { Ast.NotEq }
   | LT LP RP    { Ast.Lt }
   | LTE LP RP   { Ast.LtE }
   | GT LP RP    { Ast.Gt }
   | GTE LP RP   { Ast.GtE }
   | IS LP RP    { Ast.Is }
   | ISNOT LP RP { Ast.IsNot }
   | IN LP RP    { Ast.In }
   | NOTIN LP RP { Ast.NotIn }
;

comprehension_list: /* epsilon */ { [] }
   | comprehension { [$1] }
   | comprehension_list COMMA comprehension { $1@[$3] }
;

comprehension: COMPREHENSION LP expr COMMA expr COMMA LB expr_list RB RP { ($3, $5, $8) }
;

excepthandler_list: /* epsilon */ { [] }
   | excepthandler { [$1] }
   | excepthandler_list COMMA excepthandler { $1@[$3] }
;

excepthandler: EXCEPTHANDLER LP expr_option COMMA expr_option COMMA LB stmt_list RB COMMA loc RP { Ast.ExceptHandler ($3, $5, $8, $11) }
;

arguments: ARGUMENTS LP LB expr_list RB COMMA identifier_option COMMA identifier_option COMMA LB expr_list RB RP { ($4, $7, $9, $12) }
;

identifier_list: /* epsilon */ { [] }
   | STRING { [$1] }
   | identifier_list COMMA STRING { $1@[$3] }
;

identifier_option: NONE { None }
   | STRING { Some($1) }
;

keyword_list: /* epsilon */ { [] }
   | keyword { [$1] }
   | keyword_list COMMA keyword { $1@[$3] }
;

keyword: KEYWORD LP STRING COMMA expr RP { ($3, $5) }
;

alias_list: /* epsilon */ { [] }
   | alias { [$1] }
   | alias_list COMMA alias { $1@[$3] }
;

alias: ALIAS LP STRING COMMA identifier_option RP { ($3, $5) }
;
%%
