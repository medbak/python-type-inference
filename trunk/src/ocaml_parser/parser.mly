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
%token ALIAS KEYWORD ARGUMENTS COMPREHENSION TRUE FALSE 
%token LINENO COLOFFSET
 
%token LB RB LP RP COMMA EQUAL

%token EOF

%token <int> INUM
%token <float> FNUM
%token <string> ID
%token <string> STRING

%start modu

%type <P.modu> modu
%type <P.stmt> stmt 
%type <P.expr> expr
%type <P.expr_context> expr_context
%type <P.slice> slice
%type <P.boolop> boolop 
%type <P.operator> operator
%type <P.unaryop> unaryop
%type <P.cmpop> cmpop 
%type <P.comprehension> comprehension
%type <P.excepthandler> excepthandler
%type <P.arguments> arguments
%type <P.keyword> keyword
%type <P.alias> alias
%type <P.loc> loc
%type <bool> bool
%%

modu: MODULE LP LB stmt_list RB RP EOF { P.Module($4) }
     | INTERACTIVE LP LB stmt_list RB RP EOF { P.Interactive ($4) }
     | EXPRESSION LP expr RP EOF { P.Expression($3) }
   ;

stmt_list: /* epsilon */ { [] }
   | stmt { [$1] }
   | stmt_list COMMA stmt { $1@[$3] }
;

stmt: FUNCTIONDEF LP STRING COMMA arguments COMMA LB stmt_list RB COMMA LB expr_list RB COMMA loc RP
       { P.FunctionDef ($3, $5, $8, $12, $15) }
   | CLASSDEF LP STRING COMMA LB expr_list RB COMMA LB stmt_list RB COMMA LB expr_list RB COMMA loc RP
       { P.ClassDef ($3, $6, $10, $14, $17) }
   | RETURN LP expr_option COMMA loc RP
       { P.Return($3, $5) }
   | DELETE LP LB expr_list RB COMMA loc RP
       { P.Delete($4, $7) }
   | ASSIGN LP LB expr_list RB COMMA expr COMMA loc RP
       { P.Assign($4, $7, $9)} 
   | AUGASSIGN LP expr COMMA operator COMMA expr COMMA loc RP
       { P.AugAssign($3, $5, $7, $9) }
   | PRINT LP expr_option COMMA LB expr_list RB COMMA bool COMMA loc RP 
       { P.Print($3, $6, $9, $11) } 
   | FOR LP expr COMMA expr COMMA LB stmt_list RB COMMA LB stmt_list RB COMMA loc RP 
       { P.For($3, $5, $8, $12, $15) }
   | WHILE LP expr COMMA LB stmt_list RB COMMA LB stmt_list RB COMMA loc RP 
       { P.While($3, $6, $10, $13) }
   | IF LP expr COMMA LB stmt_list RB COMMA LB stmt_list RB COMMA loc RP 
       { P.If($3, $6, $10, $13) }
   | WITH LP expr COMMA expr_option COMMA LB stmt_list RB COMMA loc RP 
       { P.With($3, $5, $8, $11) }
   | RAISE LP expr_option COMMA expr_option COMMA expr_option COMMA loc RP 
       { P.Raise($3, $5, $7, $9) }
   | TRYEXCEPT LP LB stmt_list RB COMMA excepthandler_option COMMA LB stmt_list RB COMMA loc RP 
       { P.TryExcept($4, $7, $10, $13) }
   | TRYFINALLY LP LB stmt_list RB COMMA LB stmt_list RB COMMA loc RP 
       { P.TryFinally($4, $8, $11) }
   | ASSERT LP expr COMMA expr_option COMMA loc RP 
       { P.Assert($3, $5, $7) }
   | IMPORT LP LB alias_list RB COMMA loc RP 
       { P.Import($4, $7) }
   | IMPORTFROM LP identifier_option COMMA LB alias_list RB COMMA INUM COMMA loc RP 
       { P.ImportFrom($3, $6, Some($9), $11) }
   | IMPORTFROM LP identifier_option COMMA LB alias_list RB COMMA NONE COMMA loc RP 
       { P.ImportFrom($3, $6, None, $11) }
   | EXEC LP expr COMMA expr_option COMMA expr_option COMMA loc RP 
       { P.Exec($3, $5, $7, $9) }
   | GLOBAL LP LB identifier_list RB COMMA loc RP { P.Global($4, $7) }
   | EXPR LP expr COMMA loc RP { P.Expr($3, $5) }
   | PASS LP loc RP     { P.Pass($3) }
   | BREAK LP loc RP    { P.Break($3) }
   | CONTINUE LP loc RP { P.Continue($3) }
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

expr: BOOLOP LP boolop COMMA LB expr_list RB COMMA loc RP { P.BoolOp ($3, $6, $9) }
   | BINOP LP expr COMMA operator COMMA expr COMMA loc RP { P.BinOp($3, $5, $7, $9) }
   | UNARYOP LP unaryop COMMA expr COMMA loc RP { P.UnaryOp($3, $5, $7) }
   | LAMBDA LP arguments COMMA expr COMMA loc RP { P.Lambda($3, $5, $7) }
   | IFEXP LP expr COMMA expr COMMA expr COMMA loc RP { P.IfExp ($3, $5, $7, $9) }
   | DICT LP LB expr_list RB COMMA LB expr_list RB COMMA loc { P.Dict($4, $8, $11) }
   | SET LP LB expr_list RB COMMA loc RP { P.Set($4, $7) }
   | LISTCOMP LP expr COMMA LB comprehension_list RB COMMA loc RP { P.ListComp ($3, $6, $9) }
   | SETCOMP LP expr COMMA LB comprehension_list RB COMMA loc RP { P.SetComp($3, $6, $9) }
   | DICTCOMP LP expr COMMA expr COMMA LB comprehension_list RB COMMA loc RP { P.DictComp($3, $5, $8, $11) }
   | GENERATOREXP LP expr COMMA LB comprehension_list RB COMMA loc RP { P.GeneratorExp($3, $6, $9) }
   | YIELD LP expr_option COMMA loc RP { P.Yield($3, $5) }
   | COMPARE LP expr COMMA LB cmpop_list RB COMMA LB expr_list RB COMMA loc RP { P.Compare($3, $6, $10, $13) }
   | CALL LP expr COMMA LB expr_list RB COMMA LB keyword_list RB COMMA expr_option COMMA expr_option COMMA loc RP
       { P.Call($3, $6, $10, $13, $15, $17) }
   | REPR LP expr COMMA loc RP { P.Repr($3, $5) }
   | NUM LP INUM COMMA loc RP { P.Int($3, $5) }
   | NUM LP FNUM COMMA loc RP { P.Float($3, $5) }       
   | STR LP STRING COMMA loc RP { P.Str($3, $5) }
   | ATTRIBUTE LP expr COMMA STRING COMMA expr_context COMMA loc RP { P.Attribute($3, $5, $7, $9) }
   | SUBSCRIPT LP expr COMMA slice COMMA expr_context COMMA loc RP { P.Subscript($3, $5, $7, $9) }
   | NAME LP STRING COMMA expr_context COMMA loc RP { P.Name($3, $5, $7) }       
   | LIST LP LB expr_list RB COMMA expr_context COMMA loc RP { P.List($4, $7, $9) }
   | TUPLE LP LB expr_list RB COMMA expr_context COMMA loc RP { P.Tuple($4, $7, $9) }
;

expr_option: NONE { None }
   | expr { Some($1) }
;

expr_context: LOAD LP RP { P.Load }
   | STORE LP RP         { P.Store }
   | DEL LP RP           { P.Del }
   | AUGLOAD LP RP       { P.AugLoad }
   | AUGSTORE LP RP      { P.AugStore }
   | PARAM LP RP         { P.Param }
;

slice_list: /* epsilon */ { [] }
   | slice { [$1] }
   | slice_list COMMA slice { $1@[$3] }
;

slice: ELLIPSIS LP RP { P.Ellipsis }
   | SLICE LP expr_option COMMA expr_option COMMA expr_option RP { P.Slice($3, $5, $7) }
   | EXTSLICE LP LB slice_list RB RP { P.ExtSlice $4 }
   | INDEX LP expr RP { P.Index $3 }
;


boolop: AND LP RP { P.And }
   | OR LP RP     { P.Or }
;

operator: ADD LP RP { P.Add }
   | SUB LP RP      { P.Sub }
   | MULT LP RP     { P.Mult }
   | DIV LP RP      { P.Div }
   | MOD LP RP      { P.Mod }
   | POW LP RP      { P.Pow }
   | LSHIFT LP RP   { P.LShift }
   | RSHIFT LP RP   { P.RShift }
   | BITOR LP RP    { P.BitOr }
   | BITXOR LP RP   { P.BitXor }
   | BITAND LP RP   { P.BitAnd }
   | FLOORDIV LP RP { P.FloorDiv }
;

unaryop: INVERT LP RP { P.Invert }
   | NOT LP RP        { P.Not }
   | UADD LP RP       { P.UAdd }
   | USUB LP RP       { P.USub }
;

cmpop_list: /* epsilon */ { [] }
   | cmpop { [$1] }
   | cmpop_list COMMA cmpop { $1@[$3] }
;
       
cmpop: EQ LP RP  { P.Eq }
   | NOTEQ LP RP { P.NotEq }
   | LT LP RP    { P.Lt }
   | LTE LP RP   { P.LtE }
   | GT LP RP    { P.Gt }
   | GTE LP RP   { P.GtE }
   | IS LP RP    { P.Is }
   | ISNOT LP RP { P.IsNot }
   | IN LP RP    { P.In }
   | NOTIN LP RP { P.NotIn }
;

comprehension_list: /* epsilon */ { [] }
   | comprehension { [$1] }
   | comprehension_list COMMA comprehension { $1@[$3] }
;

comprehension: COMPREHENSION LP expr COMMA expr COMMA LB expr_list RB RP { ($3, $5, $8) }
;

excepthandler_option: NONE { None }
   | excepthandler { Some($1) }
;

excepthandler: EXCEPTHANDLER LP expr_option COMMA expr_option COMMA LB stmt_list RB COMMA loc RP { P.ExceptHandler ($3, $5, $8, $11) }
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
