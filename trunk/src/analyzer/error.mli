(*
 * Errors and handlers
 *
 * Soonho Kong (soonhok@cs.cmu.edu)
 *)

exception Lex_err of string * int
val linenum : int ref
val incr_ln : unit -> unit
val decr_ln : unit -> unit
val get_ln : unit -> int
val init : unit -> unit
val handle_exn : exn -> unit
