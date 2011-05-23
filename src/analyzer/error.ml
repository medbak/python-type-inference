(*
 * Errors and handlers
 *
 * Soonho Kong (soonhok@cs.cmu.edu)
 *)

exception Lex_err of string * int

let linenum = ref 1
let incr_ln () = linenum := !linenum + 1
let decr_ln () = linenum := !linenum - 1
let get_ln () = !linenum

let init () = linenum := 1
  
let handle_exn v =
  match v with
	  Lex_err (s, i) ->
	    Printf.eprintf ">> lexical error at line %d: %s\n" i s
	| Parsing.Parse_error ->
	  Printf.eprintf ">> syntax error at line %d\n" !linenum
	| Arg.Bad s ->
	  Printf.eprintf ">> file format error: %s\n" s
	| Type.RuntimeError s ->
	  Printf.eprintf ">> runtime error: %s\n" s
	| Type.TypeError (s, (line, col)) ->
	  Printf.eprintf ">> type error: %s at (%d, %d)\n" s line col
	|  _ -> raise v
