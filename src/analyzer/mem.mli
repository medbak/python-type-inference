type t = (Addr.t, Tyset.t) BatPMap.t
val empty : t
val bind : Addr.t -> Tyset.t -> t -> t
val find : Addr.t -> t -> Tyset.t
val join : t -> t -> t
val join_list : (t list -> t)
val order : t -> t -> bool
val to_string : t -> string    
