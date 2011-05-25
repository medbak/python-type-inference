(*
 * Soonho Kong (soonhok@cs.cmu.edu)
 *
 * type.ml : Python Type Definition
 * Reference : http://docs.python.org/reference/datamodel.html#the-standard-type-hierarchy
 *)
exception NotImplemented
type ty =
    TyBot
  | TyNone
  | TyVar of int
  | TyNotImplemented
  | TyEllipsis
  | TyInt
  | TyLong
  | TyBool
  | TyFloat
  | TyComplex
  | TyString of int
  | TyAString
  | TyUnicode of int
  | TyAUnicode
  | TyTuple of ty list
  | TyATuple of ty
  | TyList of ty list
  | TyAList of ty
  | TyByteArray
  | TySet of ty
  | TyFrozenSet of ty
  | TyDict of (ty * ty) list
  | TyFunction of (ty list * ty)
  | TyGenerator of ty
  | TyObject
  | TyType of ty
  | TyUnion of ty list
  | TyClass of (string * ty) list
val normalize : 'a list -> 'a list
val join : ty list -> ty
val order_set : ty list -> ty list -> bool
val order_list : ty list -> ty list -> bool
val order : ty -> ty -> bool
val to_string : ty -> string
exception RuntimeError of string
exception TypeError of string * Ast.loc
