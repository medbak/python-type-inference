(** Errors
    @see <http://docs.python.org/reference/datamodel.html#the-standard-type-hierarchy> Python Standard Type Hierarchy
    
    @author Soonho Kong (soonhok at cs.cmu.edu)
*)
    
(** {6 Exceptions } *)

(** [Lex_err (msg, lno)] means an lexical error at [lno] with message [msg]. *)
exception Lex_err of string * int

(** {6 Values } *)

(** Reference variable for current line number. *)    
val linenum : int ref

(** [incr_ln] increases [linenum]. *)  
val incr_ln : unit -> unit

(** [decr_ln] decreases [linenum]. *)
val decr_ln : unit -> unit

(** [get_ln ()] returns [linenum]. *)
val get_ln : unit -> int

(** [init ()] sets [linenum] to 1. *)
val init : unit -> unit

(** exception handler *)  
val handle_exn : exn -> unit
