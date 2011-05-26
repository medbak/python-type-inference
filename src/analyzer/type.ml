(*
 * Soonho Kong (soonhok@cs.cmu.edu)
 *
 * type.ml : Python Type Definition
 * Reference : http://docs.python.org/reference/datamodel.html#the-standard-type-hierarchy
 *)
open Batteries
exception NotImplemented

type ty =
  (* Bot/Top are not types in python, but we need them for analysis *)
  | TyBot                     (* bottom type *)
  | TyTop                     (* top type *)
  | TyNone                    (* none type *)
  (* type variable is not a type in python, but we need it for analysis *)
  | TyVar of string * Ast.loc * int * ty (* type variable *)
  | TyNotImplemented          (* not implemented *)
  | TyEllipsis                (* ellipsis *)
  | TyInt                     (* integer type *)
  (* numerals *)
  | TyLong                    (* long type *)
  | TyBool                    (* boolean type *)
  | TyFloat                   (* float type *)
  | TyComplex                 (* complex type *)
  (* sequences *)
  | TyString of int           (* string type *)
  | TyAString                 (* abstract string type *)
  | TyUnicode of int          (* unicode type *)
  | TyAUnicode                (* abstract unicode type *)
  | TyTuple of ty list        (* tuple *)
  | TyATuple of ty            (* abstract tuple *)
  | TyList of ty list         (* list type *)
  | TyAList of ty             (* abstract list type *)
  | TyByteArray of int        (* byte array *)
  | TyAByteArray              (* byte array *)
  (* sets *)              
  | TySet of ty * int         (* set *)
  | TyASet of ty              (* abstract set *)
  | TyFrozenSet of ty * int   (* frozen set *)
  | TyAFrozenSet of ty        (* abstract frozen set *)
  (* mapping *)
  | TyDict of (ty * ty) list       (* dictionary type *)
  (* callable *)
  (* Built-in/User-defined functions *)
  (* Built-in/User-defined methods *)
  | TyFunction of (ty list * ty)
  (* Generator functions *)
  | TyGenerator of ty * int
  | TyAGenerator of ty
  | TyObject                       (* object type *)
  | TyType of ty                   (* type type *)
  | TyUnion of ty list             (* union type *)
  | TyClass of (string * ty) list  (* class type *)

let normalize tylist = BatList.sort_unique compare tylist              
  
let rec order_set tylist1 tylist2 =
  List.for_all
    (fun ty1 ->
      List.exists
        (fun ty2 -> order ty1 ty2)
        tylist2)
    tylist1
and order_list tylist1 tylist2 =
  try
    List.for_all2 (fun ty1 ty2 -> order ty1 ty2) tylist1 tylist2
  with Invalid_argument _ -> false
and order ty1 ty2 =
  if ty1 = ty2 then true
  else match (ty1, ty2) with
      (TyBot, _) -> true
    | (_, TyTop) -> true
    | (TyTuple tylist1, TyTuple tylist2) -> order_list tylist1 tylist2
    | (TyList tylist1, TyList tylist2) -> order_list tylist1 tylist2
    | (TySet (ty1, l1), TySet (ty2, l2)) -> l1 = l2 && order ty1 ty2
    | (TyASet ty1, TyASet ty2) -> order ty1 ty2
    | (TyFrozenSet (ty1, l1), TyFrozenSet (ty2, l2)) -> l1 = l2 && order ty1 ty2
    | (TyAFrozenSet ty1, TyAFrozenSet ty2) -> order ty1 ty2
    | (TyGenerator (ty1, l1), TyGenerator (ty2, l2)) -> l1 = l2 && order ty1 ty2
    | (TyAGenerator ty1, TyAGenerator ty2) -> order ty1 ty2
    | (TyType ty1, TyType ty2) -> order ty1 ty2
    | (TyUnion tylist1, TyUnion tylist2) -> order_set tylist1 tylist2
    | (TyDict tytylist1, TyDict tytylist2) -> raise NotImplemented                   (* TODO *)
    | (TyClass idtylist1, TyClass idtylist2) -> raise NotImplemented                 (* TODO *)
    | (TyFunction (tylist1, ty1), TyFunction (tylist2, ty2)) -> raise NotImplemented (* TODO *)
    | (TyVar (name1, loc1, n1, ty1), TyVar (name2, loc2, n2, ty2)) -> name1 = name2 && loc1 = loc2 && n1 == n2 && (order ty1 ty2)
    | _ -> false

let rec join tylist =
  let rec join_typair ty1 ty2 =
    if order ty1 ty2 then ty2
    else if order ty2 ty1 then ty1
    else match (ty1, ty2) with
        (TyUnion tylist1, TyUnion tylist2) -> TyUnion (normalize tylist1@tylist2)
      | (TyUnion tylist1, ty) -> TyUnion (normalize (ty::tylist1))
      | (ty, TyUnion tylist2) -> TyUnion (normalize (ty::tylist2))
      | (ty1, ty2) -> TyUnion [ty1; ty2]
  in
  match normalize tylist with
    | [] -> TyBot
    | ty::[] -> ty
    | ty1::ty2::tylist' -> join (tylist'@[join_typair ty1 ty2])
      
let to_strings ty_list to_string = match ty_list with
    [] -> ""
  | ty::[] -> to_string ty
  | ty::tys -> (to_string ty) ^
    (List.fold_left
       (fun str ty -> str ^ ", " ^ (to_string ty))
       ""
       tys)
    
let rec to_string ty = match ty with
  | TyBot -> "TyBot"
  | TyTop -> "TyTop"
  | TyNone -> "TyNone"
  | TyVar (name, loc, n, ty) -> "TyVar(" ^ name ^ ", " ^ (Ast.string_of_loc loc) ^ ", " ^ (string_of_int n) ^ "," ^ (to_string ty)  ^ ")"
  | TyNotImplemented -> "TyNotImplemented"
  | TyEllipsis -> "TyEllipsis"
  | TyInt -> "TyInt"
  | TyLong -> "TyLong"
  | TyBool -> "TyBool"    
  | TyFloat -> "TyFloat"    
  | TyComplex -> "TyComplex"   
  | TyString i -> "TyString(" ^ (string_of_int i) ^")"
  | TyAString -> "TyAString"
  | TyUnicode i -> "TyUnicode(" ^ (string_of_int i) ^")"
  | TyAUnicode -> "TyAUnicode"
  | TyTuple tylist -> "TyTuple(" ^ (to_strings tylist to_string) ^ ")"
  | TyATuple ty -> "TyATuple(" ^ (to_string ty) ^ ")"
  | TyList tylist -> "TyList(" ^ (to_strings tylist to_string) ^ ")"
  | TyAList ty -> "TyAList(" ^ (to_string ty) ^ ")"
  | TyByteArray l -> "TyByteArray(" ^ (string_of_int l) ^ ")"
  | TyAByteArray -> "TyAByteArray"
  | TySet (ty,l) -> "TySet(" ^ (to_string ty) ^ ", "^ (string_of_int l) ^")"
  | TyFrozenSet (ty,l) -> "TyFrozenSet(" ^ (to_string ty) ^ ", "^ (string_of_int l) ^")"
  | TyASet ty -> "TyASet(" ^ (to_string ty) ^")"
  | TyAFrozenSet ty -> "TyAFrozenSet(" ^ (to_string ty) ^")"
  | TyDict tyty_list ->
    "TyDict(" ^
      (to_strings tyty_list
         (fun (ty1, ty2) ->
           "(" ^ (to_string ty1) ^ "-> " ^ (to_string ty2) ^ ")"
         )
      )
    ^ ")"
  | TyFunction (tylist, ty) -> "TyFunction{(" ^ (to_strings tylist to_string)  ^") -> " ^ (to_string ty) ^ "}"
  | TyGenerator (ty, l) -> "TyGenerator(" ^ (to_string ty) ^ ", "^ (string_of_int l) ^")"
  | TyAGenerator ty -> "TyAGenerator(" ^ (to_string ty) ^ ")"
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
exception TypeError of string * Ast.loc
exception TypeWarning of string * Ast.loc
