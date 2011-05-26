(** Analysis Module
    @author Soonho Kong (soonhok at cs.cmu.edu)
*)
    
(** {6 Types} *)

type ctl = CtlBreak
           | CtlContinue
           | CtlReturn
           | CtlYield
type size = INT of int | UNKNOWN

(** {6 Values} *)
    
val mul_size : size -> size -> size
val acomp : Env.t -> Ast.comprehension -> Env.t * size
val acomps : Env.t -> Ast.comprehension list -> Env.t * size
val aslice : Env.t -> Ast.slice -> Env.t
val aexp_list : Env.t -> Ast.expr list -> Type.ty list * Env.t
val aexp_op : Env.t -> Ast.expr option -> Type.ty * Env.t
val aexp : Env.t -> Ast.expr -> Type.ty * Env.t
val atarget_list : Env.t -> Ast.expr list -> Type.ty -> Env.t
val atarget : Env.t -> Ast.expr -> Type.ty -> Env.t
val aarguments : Env.t -> Ast.arguments -> string -> Ast.loc -> Env.t * Type.ty list
val astat_list : Env.t -> (Env.t * ctl) list -> Ast.stmt list -> Env.t option * (Env.t * ctl) list
val astat : Env.t -> (Env.t * ctl) list -> Ast.stmt -> Env.t option * (Env.t * ctl) list
val amodule : Env.t -> Ast.modu -> Env.t option
val analysis : Env.t -> Ast.modu -> Env.t option
