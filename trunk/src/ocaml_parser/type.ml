(*
 * Soonho Kong (soonhok@cs.cmu.edu)
 *
 * type.ml : Python Type Definition
 * Reference : http://docs.python.org/reference/datamodel.html#the-standard-type-hierarchy
 *)

type ty = TyNone                     (* none type *)
          (* type variable is not a type in python,
           * but we need it for analysis *)
          | TyVar of int             (* type variable *)
          | TyNotImplemented         (* not implemented *)
          | Ellipsis                 (* ellipsis *)
          | TyInt                    (* integer type *)
          (* numerals *)
          | TyLong                   (* long type *)
          | TyBool                   (* boolean type *)
          | TyFloat                  (* float type *)
          | TyComplex                (* complex type *)
          (* sequences *)
          | TyString                 (* string type *)
          | TyUnicode                (* unicode type *)
          | TyTuple of ty list       (* tuple *)
          | TyList of ty list        (* list type *)
          | TyByteArray              (* byte array *)
          (* sets *)              
          | TySet of ty list         (* set *)
          | TyFrozenSet of ty list   (* frozen set *)
          (* mapping *)
          | TyDict of (ty * ty) list       (* dictionary type *)
          (* callable *)
          (* Built-in/User-defined functions *)
          (* Built-in/User-defined methods *)
          | TyFunction of (ty list -> ty)
          (* Generator functions *)
          | TyGenerator of ty list
          | TyObject                       (* object type *)
          | TyType of ty                   (* type type *)
          | TyUnion of ty list             (* union type *)
          | TyClass of (string * ty) list  (* class type *)
                 
(* errors *)
exception RuntimeError of string
exception TypeError of string
