(*
 * Soonho Kong (soonhok@cs.cmu.edu)
 *
 * type.ml : Python Type Definition
 * Reference : http://docs.python.org/reference/datamodel.html#the-standard-type-hierarchy
 *)
open Batteries
exception NotImplemented

type ty =
    TyBot                                    (** Bottom *)
  | TyNone
  | TyVar of string * Ast.loc * int * ty
  (** Type variable is introduce at the function definition. TyVar
      (function name, location, position, constraint) *)
  | TyNotImplemented
  | TyEllipsis
  | TyNumber        (** Number *)
  | TyIntegral      (** Integral *)
  | TyInt           (** Int *)
  | TyLong          (** Long *)
  | TyBool          (** Bool *)
  | TyFloat         (** Float *)
  | TyComplex       (** Complex *)
  | TyString of int (** Concrete string type which maintains the length of string *)
  | TyAString       (** Abstract string type *)
  | TyUnicode of int (** Concrete unicode type which maintains the length *)
  | TyAUnicode       (** Abstract unicode type *)
  | TyTuple of ty list (** Concrete tuple type, which maintains the type of each element *)
  | TyATuple of ty     (** Abstract tuple type, every element has ty type *)
  | TyList of ty list  (** Concrete list type, which maintains the type of each element *)
  | TyAList of ty      (** Abstract list type *)
  | TyByteArray of int  (** Concrete bytearray type which maintains the length *)
  | TyAByteArray        (** Abstract bytearray type *)
  | TySet of ty * int   (** Concrete set type which maintains the size *)
  | TyASet of ty              (** Abstract set type *)
  | TyFrozenSet of ty * int   (** Concrete frozenset type which maintains the size *)
  | TyAFrozenSet of ty        (** Abstract frozenset type *)
  | TyDict of (ty * ty) list  (** Dict type, maintains (key_type, value_type) list *)
  | TyFunction of (ty list * ty) (** Function type, (argument_type list, return_type) *)
  | TyGenerator of ty * int  (** Concrete generator type which maintains a length of its contents *)
  | TyAGenerator of ty       (** Abstract generator type *)
  | TyObject                 (** Object type, represents top *)
  | TyType of ty             (** Type type *)
  | TyUnion of ty list       (** Union type *)
  | TySeq                    (** Sequence type *)
  | TyImmSeq                 (** Immutable Sequence type *)
  | TyMuSeq                  (** Mutable Sequence type *)
  | TyFile                   (** File type *)
  | TyCallable               (** Callable type *)
  | TyClass of (string * ty) list (** Class type. Map from attribute/method names -> their types *)

let normalize tylist = BatList.sort_unique compare tylist              
  
let rec order ty1 ty2 =
  if ty1 = ty2 then true
  else match (ty1, ty2) with
      (TyBot, _) -> true
    | (_, TyObject) -> true
    (** Numbers *)
    | (TyInt, _) -> order TyIntegral ty2
    | (TyLong, _) -> order TyIntegral ty2
    | (TyBool, _) -> order TyIntegral ty2
    | (TyIntegral, _) -> order TyNumber ty2
    | (TyFloat, _) -> order TyNumber ty2
    | (TyComplex, _) -> order TyNumber ty2
    (** Seq *)
    | (TyImmSeq, _) -> order TySeq ty2
    | (TyMuSeq, _) -> order TySeq ty2
    | (TyString _, _) -> order TyAString ty2
    | (TyAString, _) -> order TyImmSeq ty2
    | (TyUnicode _, _) -> order TyAUnicode ty2
    | (TyAUnicode, _) -> order TyImmSeq ty2
    | (TyTuple tylist1, TyTuple tylist2) -> order_list tylist1 tylist2
    | (TyTuple tylist1, TyATuple ty2) -> List.for_all (fun ty1 -> order ty1 ty2) tylist1
    | (TyTuple _, _) -> order TyImmSeq ty2
    | (TyATuple ty1, TyATuple ty2) -> order ty1 ty2
    | (TyATuple ty1, _) -> order TyImmSeq ty2
    | (TyList tylist1, TyList tylist2) -> order_list tylist1 tylist2
    | (TyList tylist1, TyAList ty2) -> List.for_all (fun ty1 -> order ty1 ty2) tylist1
    | (TyList _, _) -> order TyMuSeq ty2
    | (TyAList ty1, TyAList ty2) -> order ty1 ty2
    | (TyAList _, _) -> order TyMuSeq ty2
    | (TyByteArray _, _) -> order TyAByteArray ty2
    | (TyAByteArray, _) -> order TyMuSeq ty2
    | (TySet (ty1, l1), TySet (ty2, l2)) -> l1 = l2 && order ty1 ty2
    | (TySet (ty1, l1), TyASet ty2) -> order ty1 ty2  
    | (TyASet ty1, TyASet ty2) -> order ty1 ty2
    | (TyFrozenSet (ty1, l1), TyFrozenSet (ty2, l2)) -> l1 = l2 && order ty1 ty2
    | (TyFrozenSet (ty1, l1), TyAFrozenSet ty2) -> order ty1 ty2
    | (TyAFrozenSet ty1, TyAFrozenSet ty2) -> order ty1 ty2
    | (TyGenerator (ty1, l1), TyGenerator (ty2, l2)) -> l1 = l2 && order ty1 ty2
    | (TyGenerator (ty1, l1), TyAGenerator ty2) -> order ty1 ty2
    | (TyAGenerator ty1, TyAGenerator ty2) -> order ty1 ty2
    | (TyType ty1, TyType ty2) -> order ty1 ty2
    | (TyUnion tylist1, TyUnion tylist2) -> order_set tylist1 tylist2
    | (TyDict tytylist1, TyDict tytylist2) -> raise NotImplemented                   (* TODO *)
    | (TyClass idtylist1, TyClass idtylist2) ->
      List.for_all (fun (id1, ty1) ->
        List.exists (fun (id2, ty2) -> id1 = id2 && order ty1 ty2) idtylist2)
        idtylist1
    | (TyFunction (tylist1, ty1), TyFunction (tylist2, ty2)) -> raise NotImplemented (* TODO *)
    | (TyVar (name1, loc1, n1, ty1), TyVar (name2, loc2, n2, ty2)) -> name1 = name2 && loc1 = loc2 && n1 == n2 && (order ty1 ty2)
    | _ -> false
and order_set tylist1 tylist2 =
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

let rec meet ty1 ty2 =
  if order ty1 ty2 then ty1
  else if order ty2 ty1 then ty2 else
    match (ty1, ty2) with
        _ -> ty1

(** substitue ty1 with ty2 in ty *)      
let rec subst ty1 ty2 ty = match ty with
    (* No Change *)
    TyBot| TyNone| TyNotImplemented| TyEllipsis
  | TyInt| TyLong| TyBool| TyFloat| TyComplex
  | TyNumber | TyIntegral | TySeq | TyImmSeq | TyMuSeq | TyFile | TyCallable
  | TyString _| TyAString| TyUnicode _ 
  | TyAUnicode| TyByteArray _| TyAByteArray
  | TyObject -> ty
  (* map inside *)
  | TyTuple ty_list -> TyTuple (List.map (fun ty -> subst ty1 ty2 ty) ty_list)
  | TyATuple ty -> TyATuple (subst ty1 ty2 ty)
  | TyList ty_list -> TyList (List.map (fun ty -> subst ty1 ty2 ty) ty_list)
  | TyAList ty -> TyAList (subst ty1 ty2 ty)
  | TySet (ty, n) -> TySet ((subst ty1 ty2 ty), n)
  | TyASet ty -> TyASet (subst ty1 ty2 ty)
  | TyFrozenSet (ty, n) -> TyFrozenSet ((subst ty1 ty2 ty), n)
  | TyAFrozenSet ty -> TyAFrozenSet (subst ty1 ty2 ty)
  | TyDict ty_ty_list -> TyDict (List.map (fun (ty_k, ty_v) -> (subst ty1 ty2 ty_k, subst ty1 ty2 ty_v)) ty_ty_list)
  | TyFunction (ty_list, ty) -> TyFunction (List.map (fun ty -> subst ty1 ty2 ty) ty_list, subst ty1 ty2 ty)
  | TyGenerator (ty, n) -> TyGenerator (subst ty1 ty2 ty, n)
  | TyAGenerator ty -> TyAGenerator (subst ty1 ty2 ty)
  | TyType ty -> TyType (subst ty1 ty2 ty)
  | TyUnion ty_list -> TyUnion (List.map (fun ty -> subst ty1 ty2 ty) ty_list)
  | TyClass key_ty_list -> TyClass (List.map (fun (key, ty) -> (key, subst ty1 ty2 ty)) key_ty_list)
  | TyVar _ -> if ty = ty1 then ty2 else ty
      
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
  | TyCallable -> "TyCallable"
  | TyFile -> "TyFile"
  | TyMuSeq -> "TyMuSeq"
  | TyImmSeq -> "TyImmSeq"
  | TySeq -> "TySeq"
  | TyIntegral -> "TyIntegral"
  | TyNumber -> "TyNumber"

      
(* errors *)
exception RuntimeError of string
exception TypeError of string * Ast.loc
exception TypeWarning of string * Ast.loc
