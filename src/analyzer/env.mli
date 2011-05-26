(* Soonho Kong (soonhok@cs.cmu.edu) *)
type t = (string, Type.ty) Batteries.PMap.t
val empty_env : t
val bind : string -> Type.ty -> t -> t
val find : string -> t -> Type.ty
val join : t -> t -> t
val joinop : t option -> t option -> t option
val order : t -> t -> bool
val to_string : t -> string
