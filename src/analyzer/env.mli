(* Soonho Kong (soonhok@cs.cmu.edu) *)

type t = (string, Addrset.t) BatPMap.t

val empty : t

val bind : string -> Addrset.t -> t -> t
val find : string -> t -> Addrset.t 
val get : string -> t -> (t * Addrset.t)
val join : t -> t -> t
val order : t -> t -> bool    
val to_string : t -> string
