(* Soonho Kong (soonhok@cs.cmu.edu) *)
exception NotImplemented of string
exception ShouldNotHappen of string
type env = (Ast.identifier, Type.ty) Batteries.PMap.t
type ctl = CtlBreak | CtlContinue | CtlReturn
val acomp : env -> Ast.comprehension -> env
val aslice : env -> Ast.slice -> env
val aexp_list : env -> Ast.expr list -> Type.ty list * env
val aexp_op : env -> Ast.expr option -> Type.ty * env
val aexp : env -> Ast.expr -> Type.ty * env
val atarget_list : env -> Ast.expr list -> Type.ty -> env
val atarget : env -> Ast.expr -> Type.ty -> env
val astat_list : env -> (env * ctl) list -> Ast.stmt list -> env option * (env * ctl) list
val astat : env -> (env * ctl) list -> Ast.stmt -> env option * (env * ctl) list
val amodule : env -> Ast.modu -> env option
val analysis : env -> Ast.modu -> env option
