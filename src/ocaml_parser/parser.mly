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
%token LISTCOMP LOAD LT LTE MOD MODULE MULT NAME NOT NOTEQ NOTIN NUM OR PARAM PASS POW PRINT RSHIFT RAISE REPR RETURN SET SETCOMP
%token SLICE STORE STR SUB SUBSCRIPT SUITE TRYEXCEPT TRYFINALLY TUPLE UADD USUB UNARYOP WHILE WITH YIELD
%token ALIAS
 
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
//%type <P.slice> slice
//%type <P.boolop> boolop 
//%type <P.operator> operator  
//%type <P.unaryop> unaryop
%type <P.cmpop> cmpop 
//%type <P.comprehension> comprehension
//%type <P.excepthandler> excepthandler
//%type <P.arguments> arguments
//%type <P.keyword> keyword
%type <P.alias> alias
%type <P.loc> loc
 
%%

modu: MODULE LP LB stmt_list RB RP EOF { P.Module($4) }
   ;

stmt_list: /* epsilon */ { [] }
   | stmt { [$1] }
   | stmt COMMA stmt_list { $1::$3 }
;

stmt:
  ASSIGN LP LB expr_list RB COMMA expr COMMA loc RP {P.Assign($4, $7, $9)}
;

loc: LINENO EQUAL INUM COMMA COLOFFSET EQUAL INUM { ($3, $7 ) }
;

expr_list: /* epsilon */ { [] }
   | expr { [$1] }
   | expr COMMA expr_list { $1::$3 }
;

expr:
  COMPARE LP expr COMMA LB cmpop_list RB COMMA LB expr_list RB COMMA loc RP { P.Compare($3, $6, $10, $13) }
   | NUM LP INUM COMMA loc RP { P.Int($3, $5) }
   | NUM LP FNUM COMMA loc RP { P.Float($3, $5) }       
   | NAME LP STRING COMMA expr_context COMMA loc RP { P.Name($3, $5, $7) }
;

expr_context: LOAD LP RP { P.Load }
   | STORE LP RP         { P.Store }
   | DEL LP RP           { P.Del }
   | AUGLOAD LP RP       { P.AugLoad }
   | AUGSTORE LP RP      { P.AugStore }
   | PARAM LP RP         { P.Param }
;

cmpop_list: /* epsilon */ { [] }
   | cmpop { [$1] }
   | cmpop COMMA cmpop_list { $1::$3 }

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

alias: ALIAS LP STRING COMMA NONE RP { P.Alias($3, None) }
   | ALIAS LP STRING COMMA STRING RP { P.Alias($3, Some($5)) }
;
%%
