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
     ("inf", INF);
     ("True", TRUE);
     ("False", FALSE);
    ]
} 

let blank = [' ' '\t']+
let id = ['a'-'z' 'A'-'Z'](['a'-'z' 'A'-'Z' '0'-'9' '_'])*
let float_number      = '-'? ['0'-'9']*'.'(['0'-'9']*)
let complex_float_num = float_number 'j'
let exp_number        = '-'? ['1'-'9'] ('.' (['0'-'9']+))? 'e' ('+'|'-') ['0'-'9']+
let complex_exp_num   = exp_number 'j'
let int_number        = '-'? ['0'-'9']+
let complex_int_num   = int_number 'j'
let long_number       = '-'? ['0'-'9']+'L'
rule start =
  parse blank { start lexbuf }
    | "\r\n"     { incr_ln (); start lexbuf}
    | '\n'       { incr_ln (); start lexbuf}
    | "-inf"     { NEGINF }
    | exp_number { verbose (Lexing.lexeme lexbuf); FNUM (float_of_string(Lexing.lexeme lexbuf)) } (* float (exp) *)
    | float_number { verbose (Lexing.lexeme lexbuf); FNUM (float_of_string(Lexing.lexeme lexbuf)) } (* float *)
    | float_number { verbose (Lexing.lexeme lexbuf); FNUM (float_of_string(Lexing.lexeme lexbuf)) } (* float *)
    | long_number { let str = Lexing.lexeme lexbuf in                                              (* long *)
                let v = int_of_string(String.sub str 0 ((String.length str) - 1)) in
                (verbose str; LNUM(v))
              }
    | complex_exp_num{ let str = Lexing.lexeme lexbuf in                                              (* complex, exp *)
                let v = float_of_string(String.sub str 0 ((String.length str) - 1)) in
                (verbose str; CNUM(v))
              }        
    | complex_float_num{ let str = Lexing.lexeme lexbuf in                                              (* complex, float *)
                let v = float_of_string(String.sub str 0 ((String.length str) - 1)) in
                (verbose str; CNUM(v))
              }
    | complex_int_num{ let str = Lexing.lexeme lexbuf in
               let v = float_of_string(String.sub str 0 ((String.length str) - 1)) in           (* complex, int *)
               (verbose str; CNUM(v))
              }        
    | int_number { verbose (Lexing.lexeme lexbuf); INUM (int_of_string(Lexing.lexeme lexbuf)) } (* int *)
    | 'u' "\'" { unicode_single_quote "" lexbuf }
    | 'u' "\"" { unicode_double_quote "" lexbuf }
    | id { let id = Lexing.lexeme lexbuf
           in verbose ("ID:"^id); try Hashtbl.find keyword_tbl id
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
and unicode_single_quote prefix =
    parse "\\'"   { unicode_single_quote (prefix^"\'") lexbuf }
      | "\\n"      { unicode_single_quote (prefix^"\n") lexbuf }
      | "\\t"      { unicode_single_quote (prefix^"\t") lexbuf }
      | "\\\\"        { unicode_single_quote (prefix^"\\") lexbuf }           
      | [^'\\''\'']+   { unicode_single_quote (prefix^(Lexing.lexeme lexbuf)) lexbuf }
      | "\\"        { unicode_single_quote (prefix^"\\") lexbuf } 
      | "'"        { verbose ("Unicode:"^prefix); USTRING(prefix) }
and
  unicode_double_quote prefix =
    parse "\\\""   { unicode_double_quote (prefix^"\"") lexbuf }
      | "\\n"      { unicode_double_quote (prefix^"\n") lexbuf }
      | "\\t"      { unicode_double_quote (prefix^"\t") lexbuf }           
      | "\\\\"        { unicode_double_quote (prefix^"\\") lexbuf } 
      | [^'\\''\"']+   { unicode_double_quote (prefix^(Lexing.lexeme lexbuf)) lexbuf }
      | "\\"        { unicode_double_quote (prefix^"\\") lexbuf } 
      | "\""       { verbose ("Unicode:"^prefix); USTRING(prefix) }
