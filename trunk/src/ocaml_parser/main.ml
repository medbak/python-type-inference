(*
 * Python Type Inference
 *
 * Main Interface
 *)

open Ast

let print_ast = ref false
let spec = [
  ("-pp", Arg.Set print_ast, "Print Python AST");
]
let usage = "Usage: run [<options>] <Python AST dumpfile> \n<options> are: "

let run () =
  let src = ref "" in
  let _ = Arg.parse spec
    (fun x -> if Sys.file_exists x then src := x
     else raise (Arg.Bad (x^": No such file"))) usage in  
	try 
      Error.init ();
      let lexbuf =
        Lexing.from_channel (if !src = "" then stdin else open_in !src) in
      let modu = Parser.modu Lexer.start lexbuf in
        Analysis.analysis Env.empty_env modu;
        print_string "== Parsing Completed ==\n"
    with v -> Error.handle_exn v 

let _ = Printexc.catch run ()
