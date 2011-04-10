(*
 * Soonho Kong (soonhok@cs.cmu.edu)
 *
 * Lexer for Python AST
 *)

{
  open Parser
  open Error
  let debug_tag = false
  let verbose s =  if debug_tag then (print_string s; print_newline())
  let comment_depth = ref 0
  let keyword_tbl = Hashtbl.create 111
  let _ = List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
    [("Add", ADD);
     ("And", AND);
     ("Assert", ASSERT);
     ("Assign", ASSIGN);
     ("Attribute", ATTRIBUTE);
     ("AugAssign", AUGASSIGN);
     ("AugLoad", AUGLOAD);
     ("AugStore", AUGSTORE);
     ("BinOp", BINOP);
     ("BitAnd", BITAND);
     ("BitOr", BITOR);
     ("BitXor", BITXOR);
     ("BoolOp", BOOLOP);
     ("BoolOp", BOOLOP);
     ("Break", BREAK);
     ("Call", CALL);
     ("ClassDef", CLASSDEF);
     ("Compare", COMPARE);
     ("Continue", CONTINUE);
     ("Del", DEL);
     ("Delete", DELETE);
     ("Dict", DICT);
     ("DictComp", DICTCOMP);
     ("Div", DIV);
     ("Ellipsis", ELLIPSIS);
     ("Eq", EQ);
     ("ExceptHandler", EXCEPTHANDLER);
     ("Exec", EXEC);
     ("Expr", EXPR);
     ("Expression", EXPRESSION);
     ("ExtSlice", EXTSLICE);
     ("FloorDiv", FLOORDIV);
     ("For", FOR);
     ("FunctionDef", FUNCTIONDEF);
     ("GeneratorExp", GENERATOREXP);
     ("Global", GLOBAL);
     ("Gt", GT);
     ("GtE", GTE);
     ("If", IF);
     ("IfExp", IFEXP);
     ("Import", IMPORT);
     ("ImportFrom", IMPORTFROM);
     ("In", IN);
     ("Index", INDEX);
     ("Interactive", INTERACTIVE);
     ("Invert", INVERT);
     ("Is", IS);
     ("IsNot", ISNOT);
     ("LShift", LSHIFT);
     ("Lambda", LAMBDA);
     ("List", LIST);
     ("ListComp", LISTCOMP);
     ("Load", LOAD);
     ("Lt", LT);
     ("LtE", LTE);
     ("Mod", MOD);
     ("Module", MODULE);
     ("Mult", MULT);
     ("Name", NAME);
     ("Not", NOT);
     ("NotEq", NOTEQ);
     ("NotIn", NOTIN);
     ("None", NONE);
     ("Num", NUM);
     ("Or", OR);
     ("Param", PARAM);
     ("Pass", PASS);
     ("Pow", POW);
     ("Print", PRINT);
     ("RShift", RSHIFT);
     ("Raise", RAISE);
     ("Repr", REPR);
     ("Return", RETURN);
     ("Set", SET);
     ("SetComp", SETCOMP);
     ("Slice", SLICE);
     ("Store", STORE);
     ("Str", STR);
     ("Sub", SUB);
     ("Subscript", SUBSCRIPT);
     ("Suite", SUITE);
     ("TryExcept", TRYEXCEPT);
     ("TryFinally", TRYFINALLY);
     ("Tuple", TUPLE);
     ("UAdd", UADD);
     ("USub", USUB);
     ("UnaryOp", UNARYOP);
     ("While", WHILE);
     ("With", WITH);
     ("Yield", YIELD);
     ("lineno", LINENO);
     ("col_offset", COLOFFSET);
     ("alias", ALIAS);
     ("arguments", ARGUMENTS);
     ("keyword", KEYWORD);
     ("comprehension", COMPREHENSION);
     ("True", TRUE);
     ("False", FALSE);
    ]
} 

let blank = [' ' '\t']+
let id = ['a'-'z' 'A'-'Z'](['a'-'z' 'A'-'Z' '\'' '0'-'9' '_'])*
let fnumber = ['0'-'9']*'.'(['0'-'9']*)
let inumber = ['0'-'9']+ | '-'['0'-'9']+

rule start =
  parse blank { start lexbuf }
    | "\r\n"     { incr_ln (); start lexbuf}
    | '\n'       { incr_ln (); start lexbuf}
    | fnumber { verbose (Lexing.lexeme lexbuf); FNUM (float_of_string(Lexing.lexeme lexbuf)) }
    | inumber { verbose (Lexing.lexeme lexbuf); INUM (int_of_string(Lexing.lexeme lexbuf)) }
    | id { let id = Lexing.lexeme lexbuf
           in verbose id; try Hashtbl.find keyword_tbl id
             with _ -> ID id
         }
    | eof { verbose "eof"; EOF}
    | "[" { verbose "["; LB}
    | "]" { verbose "]"; RB}
    | "(" { verbose "("; LP}
    | ")" { verbose ")"; RP}
    | "=" { verbose "="; EQUAL}
    | "," { verbose ","; COMMA}
    | "'" { single_quote "" lexbuf }
    | "\"" { double_quote "" lexbuf }
    | _ {raise (Lex_err("illical token "^(Lexing.lexeme lexbuf), get_ln()))} 
and single_quote prefix =
    parse "\\'"   { single_quote (prefix^"\'") lexbuf }
      | "\\n"      { single_quote (prefix^"\n") lexbuf }
      | "\\t"      { single_quote (prefix^"\t") lexbuf }
      | "\\\\"        { single_quote (prefix^"\\") lexbuf }           
      | [^'\\''\'']+   { single_quote (prefix^(Lexing.lexeme lexbuf)) lexbuf }
      | "\\"        { single_quote (prefix^"\\") lexbuf } 
      | "'"        { verbose prefix; STRING(prefix) }
and
  double_quote prefix =
    parse "\\\""   { double_quote (prefix^"\"") lexbuf }
      | "\\n"      { double_quote (prefix^"\n") lexbuf }
      | "\\t"      { double_quote (prefix^"\t") lexbuf }           
      | "\\\\"        { double_quote (prefix^"\\") lexbuf } 
      | [^'\\''\"']+   { double_quote (prefix^(Lexing.lexeme lexbuf)) lexbuf }
      | "\\"        { double_quote (prefix^"\\") lexbuf } 
      | "\""       { verbose prefix; STRING(prefix) }
