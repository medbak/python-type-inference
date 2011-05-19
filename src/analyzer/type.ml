open Batteries
exception NotImplemented
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
          | TyEllipsis                 (* ellipsis *)
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
          | TyFunction of (ty list * ty)
          (* Generator functions *)
          | TyGenerator of ty list
          | TyObject                       (* object type *)
          | TyType of ty                   (* type type *)
          | TyUnion of ty list             (* union type *)
          | TyClass of (string * ty) list  (* class type *)

let normalize tylist = BatList.sort_unique compare tylist              
              
let join tylist = match normalize tylist with
    ty::[] -> ty
  | tylist' -> TyUnion tylist'


let to_strings ty_list to_string = match ty_list with
    [] -> ""
  | ty::[] -> to_string ty
  | ty::tys -> (to_string ty) ^
    (List.fold_left
       (fun str ty -> str ^ ", " ^ (to_string ty))
       ""
       tys)
    
let rec to_string ty = match ty with
  TyNone -> "TyNone"
  | TyVar x -> "TyVar(" ^ (string_of_int x) ^ ")"
  | TyNotImplemented -> "TyNotImplemented"
  | TyEllipsis -> "TyEllipsis"
  | TyInt -> "TyInt"
  | TyLong -> "TyLong"
  | TyBool -> "TyBool"    
  | TyFloat -> "TyFloat"    
  | TyComplex -> "TyComplex"   
  | TyString -> "TyString"
  | TyUnicode -> "TyUnicode"
  | TyTuple tylist -> "TyTuple(" ^ (to_strings tylist to_string) ^ ")"
  | TyList tylist -> "TyList(" ^ (to_strings tylist to_string) ^ ")"
  | TyByteArray -> "TyByteArray"
  | TySet tylist -> "TySet(" ^ (to_strings tylist to_string) ^ ")"
  | TyFrozenSet tylist -> "TyFrozenSet(" ^ (to_strings tylist to_string) ^ ")"
  | TyDict tyty_list ->
    "TyDict(" ^
      (to_strings tyty_list
         (fun (ty1, ty2) ->
           "(" ^ (to_string ty1) ^ "-> " ^ (to_string ty2) ^ ")"
         )
      )
    ^ ")"
  | TyFunction (tylist, ty) -> "TyFunction{(" ^ (to_strings tylist to_string)  ^") -> " ^ (to_string ty) ^ "}"
  | TyGenerator tylist -> "TyGenerator(" ^ (to_strings tylist to_string) ^ ")"
  | TyObject -> "TyObject"  
  | TyType ty -> "TyType(" ^ to_string ty ^ ")"            
  | TyUnion tylist -> "TyUnion(" ^ (to_strings tylist to_string) ^ ")"
  | TyClass idty_list ->
    "TyClass(" ^
      (to_strings idty_list
         (fun (id, ty2) ->
           "(" ^ id ^ ": " ^ (to_string ty2) ^ ")"
         )
      )
    ^ ")"
      
(* errors *)
exception RuntimeError of string
exception TypeError of string
