(** Environment
    @author Soonho Kong (soonhok at cs.cmu.edu)
*)

(** {6 Types} *)

(** Environemt is a mapping from variables to types. {{:http://batteries.forge.ocamlcore.org}OCaml Batteries} {{:http://batteries.forge.ocamlcore.org/doc.preview:batteries-beta1/html/api/PMap.html}PMap} is used.  *)
type t = (string, Type.ty) Batteries.PMap.t

(** {6 Values} *)

(** Empty environment. *)
val empty : t
  

(** [bind var ty env] returns a new environment containing the as
    [env], plus a binding of [var] to [ty]. If [var] was already bound in
    [env], its previous binding disappears. *)
val bind : string -> Type.ty -> t -> t

(** [find var ty env] returns the current binding of [var] in [env],
    or @raise Not_found if no such binding exists.
    
*)
val find : string -> t -> Type.ty

(** [join env1 env2] returns the joined environment [env']. This operation is an element-wise joining with {!Type.join}. That is,
    - dom([env']) = dom([env1]) ∪ dom([env2]).
    - [env']([x]) = {!Type.join} [env1]([x]) [env2]([x])
*) 
val join : t -> t -> t

(** Join operation for environment option type. *)
val joinop : t option -> t option -> t option

(** Order operation. [order env1 env2] is true iff
    ∀ v ∈ dom([env1]). v ∈ dom([env2]) & {!Type.order} [env1](v) [env2](v) *)
val order : t -> t -> bool
val to_string : t -> string
