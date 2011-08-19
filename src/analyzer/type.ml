(*
 * Soonho Kong (soonhok@cs.cmu.edu)
 *
 * type.ml : Python Type Definition
 * Reference : http://docs.python.org/reference/datamodel.html#the-standard-type-hierarchy
 *)
open Batteries
exception NotImplemented
(* errors *)
exception RuntimeError of string
exception TypeError of string * Ast.loc
exception TypeWarning of string * Ast.loc

type addrset = Addrset.t
type ty =
    TyBot           (** Bottom *)
  | TyNone
  | TyNotImplemented
  | TyEllipsis
  | TyInt           (** Int *)
  | TyCInt of int
  | TyLong          (** Long *)
  | TyCLong of int
  | TyBool          (** Bool *)
  | TyCBool of bool
  | TyFloat         (** Float *)
  | TyCFloat of float
  | TyComplex       (** Complex *)
  | TyCComplex of (float * float)
  | TyString of int (** Concrete string type which maintains the length of string *)
  | TyCString of string
  | TyAString       (** Abstract string type *)
  | TyUnicode of int (** Concrete unicode type which maintains the length *)
  | TyCUnicode of UTF8.t
  | TyAUnicode       (** Abstract unicode type *)
  | TyTuple of addrset list (** Concrete tuple type, which maintains the set of locations for each element *)
  | TyATuple of addrset     (** Abstract tuple type, forget about position and holds only locations *)
  | TyList of addrset list  (** Concrete list type, which maintains the type of each element *)
  | TyAList of addrset     (** Abstract list type *)
  | TyByteArray of int  (** Concrete bytearray type which maintains the length *)
  | TyAByteArray        (** Abstract bytearray type *)
  | TySet of addrset * int   (** Concrete set type which maintains the size *)
  | TyASet of addrset              (** Abstract set type *)
  | TyFrozenSet of addrset * int   (** Concrete frozenset type which maintains the size *)
  | TyAFrozenSet of addrset        (** Abstract frozenset type *)
  | TyDict of (ty * addrset) list  (** Dict type, maintains (key_type, value_type) list *)
  | TyFunction of Ast.stmt (** Function type, (argument_type list, return_type) *)
  | TyGenerator of addrset * int  (** Concrete generator type which maintains a length of its contents *)
  | TyAGenerator of addrset       (** Abstract generator type *)
  | TyObject                 (** Object type, represents top *)
  | TyType of ty             (** Type type *)
  | TyClass of (string * addrset) list (** Class type. Map from attribute/method names -> their types *)
      
let normalize tylist = BatList.sort_unique compare tylist              

let rec order ty1 ty2 =
  if ty1 = ty2 then true
  else match (ty1, ty2) with
      (TyBot, _) -> true
    | (_, TyObject) -> true
    (** Seq *)
    | (TyCString _, TyString _) -> true
    | (TyCString _, TyAString) -> true
    | (TyString _, TyAString) -> true
    | (TyCUnicode _, TyUnicode _) -> true
    | (TyCUnicode _, TyAUnicode) -> true
    | (TyUnicode _, TyAUnicode) -> true
    (* Tuple *)
    | (TyTuple ls_list1, TyTuple ls_list2) -> Addrset.order_list ls_list1 ls_list2
    | (TyTuple ls_list, TyATuple locset) -> List.for_all (fun ls -> Addrset.order ls locset) ls_list
    | (TyATuple ls1, TyATuple ls2) -> Addrset.order ls1 ls2
    (* List *)
    | (TyList ls_list1, TyList ls_list2) -> Addrset.order_list ls_list1 ls_list2
    | (TyList ls_list, TyAList locset) -> List.for_all (fun ls -> Addrset.order ls locset) ls_list
    | (TyAList ls1, TyAList ls2) -> Addrset.order ls1 ls2
    (* ByteArray *)
    | (TyByteArray _, TyAByteArray) -> true
    (* Set *)  
    | (TySet (locset1, l1), TySet (locset2, l2)) -> l1 = l2 && Addrset.order locset1 locset2
    | (TySet (locset1, l1), TyASet locset2) -> Addrset.order locset1 locset2
    | (TyASet ls1, TyASet ls2) -> Addrset.order ls1 ls2
    (* Frozen Set *)  
    | (TyFrozenSet (locset1, l1), TyFrozenSet (locset2, l2)) -> l1 = l2 && Addrset.order locset1 locset2
    | (TyFrozenSet (locset1, l1), TyAFrozenSet locset2) -> Addrset.order locset1 locset2
    | (TyAFrozenSet locset1, TyAFrozenSet locset2) -> Addrset.order locset1 locset2
    (* Generator *)  
    | (TyGenerator (locset1, l1), TyGenerator (locset2, l2)) -> l1 = l2 && Addrset.order locset1 locset2
    | (TyGenerator (locset1, l1), TyAGenerator locset2) -> Addrset.order locset1 locset2
    | (TyAGenerator locset1, TyAGenerator locset2) -> Addrset.order locset1 locset2
    | (TyType ty1, TyType ty2) -> order ty1 ty2
    | (TyDict tytylist1, TyDict tytylist2) -> raise NotImplemented                   (* TODO *)
    | (TyClass idlocsetlist1, TyClass idlocsetlist2) ->
      List.for_all (fun (id1, locset1) ->
        List.exists (fun (id2, locset2) -> id1 = id2 && Addrset.order locset1 locset2) idlocsetlist2)
        idlocsetlist1
    | _ -> false
and join ty1 ty2 = 
  if order ty1 ty2 then [ty2]
  else if order ty2 ty1 then [ty1]
  else [ty1; ty2]
and join_list tylist =
  match tylist with
    | [] -> []
    | ty::[] -> [ty]
    | ty1::ty2::tylist' -> join_list (tylist' @ (join ty1 ty2))

let rec meet ty1 ty2 =
  if order ty1 ty2 then ty1
  else if order ty2 ty1 then ty2 else
    TyBot
      
let to_strings locset_list to_string = match locset_list with
    [] -> ""
  | locset::[] -> to_string locset
  | locset::locset_list' -> (to_string locset) ^
    (List.fold_left
       (fun str locset -> str ^ ", " ^ (to_string locset))
       ""
       locset_list')
    
let rec to_string ty = match ty with
  | TyBot -> "TyBot"
  | TyNone -> "TyNone"
  | TyNotImplemented -> "TyNotImplemented"
  | TyEllipsis -> "TyEllipsis"
  | TyInt -> "TyInt"
  | TyCInt n -> "TyCInt(" ^ (string_of_int n) ^ ")"
  | TyLong -> "TyLong"
  | TyCLong n -> "TyCLong(" ^ (string_of_int n) ^ ")"    
  | TyBool -> "TyBool"
  | TyCBool b -> "TyCBool(" ^ (string_of_bool b) ^ ")"    
  | TyFloat -> "TyFloat"
  | TyCFloat f -> "TyCFloat(" ^ (string_of_float f) ^ ")"    
  | TyComplex -> "TyComplex"
  | TyCComplex (r, i) -> "TyCComplex(" ^ (string_of_float r) ^ ", " ^ (string_of_float i) ^ ")"   
  | TyString i -> "TyString(" ^ (string_of_int i) ^")"
  | TyCString s -> "TyCString(" ^ s ^")"
  | TyAString -> "TyAString"
  | TyUnicode i -> "TyUnicode(" ^ (string_of_int i) ^")"
  | TyCUnicode s -> "TyUnicode(" ^ (UTF8.to_string s) ^")"
  | TyAUnicode -> "TyAUnicode"
  | TyTuple locset_list -> "TyTuple(" ^ (to_strings locset_list Addrset.to_string) ^ ")"
  | TyATuple locset -> "TyATuple(" ^ (Addrset.to_string locset) ^ ")"
  | TyList locset_list -> "TyList(" ^ (to_strings locset_list Addrset.to_string) ^ ")"
  | TyAList locset -> "TyAList(" ^ (Addrset.to_string locset) ^ ")"
  | TyByteArray l -> "TyByteArray(" ^ (string_of_int l) ^ ")"
  | TyAByteArray -> "TyAByteArray"
  | TySet (locset,l) -> "TySet(" ^ (Addrset.to_string locset) ^ ", "^ (string_of_int l) ^")"
  | TyFrozenSet (locset,l) -> "TyFrozenSet(" ^ (Addrset.to_string locset) ^ ", "^ (string_of_int l) ^")"
  | TyASet locset -> "TyASet(" ^ (Addrset.to_string locset) ^")"
  | TyAFrozenSet locset -> "TyAFrozenSet(" ^ (Addrset.to_string locset) ^")"
  | TyDict tylocset_list ->
    "TyDict(" ^
      (to_strings tylocset_list
         (fun (ty, locset) ->
           "(" ^ (to_string ty) ^ "-> " ^ (Addrset.to_string locset) ^ ")"
         )
      )
    ^ ")"
  | TyFunction (ast) ->
    begin
      match ast with
          Ast.FunctionDef (name, _, _, _, loc) -> (name ^ Ast.string_of_loc loc)
        | Ast.Expr (expr, loc) -> begin
          match expr with
              Ast.Lambda (args, expr', loc') -> ("\\lambda" ^ Ast.string_of_loc loc)
            | _ -> raise (RuntimeError "Function type should be either lambda or function definition")
        end
        | _ -> raise (RuntimeError "Function type should be either lambda or function definition")
    end
  | TyGenerator (locset, l) -> "TyGenerator(" ^ (Addrset.to_string locset) ^ ", "^ (string_of_int l) ^")"
  | TyAGenerator locset -> "TyAGenerator(" ^ (Addrset.to_string locset) ^ ")"
  | TyObject -> "TyObject"  
  | TyType ty -> "TyType(" ^ to_string ty ^ ")"            
  | TyClass idlocset_list ->
    "TyClass(" ^
      (to_strings idlocset_list
         (fun (id, locset) ->
           "(" ^ id ^ ": " ^ (Addrset.to_string locset) ^ ")"
         )
      )
    ^ ")"

let rec add ty1 ty2 = match (ty1, ty2) with
  | (TyCBool true, _)  -> add (TyCInt 1) ty2
  | (TyCBool false, _) -> add (TyCInt 0) ty2
  | (_, TyCBool true)  -> add ty1 (TyCInt 1)
  | (_, TyCBool false) -> add ty1 (TyCInt 0)
  | (TyBool, _) -> add TyInt ty2
  | (_, TyBool) -> add ty1 TyInt
  | (TyCInt n, TyCInt m) -> TyCInt (n + m)
  | (TyCInt n, TyInt) -> TyInt
  | (TyCInt n, _) -> raise NotImplemented
  | (TyInt, TyInt)    -> TyInt
  | (TyInt, _) -> raise NotImplemented
  | (TyCLong n, TyCInt m) -> TyCLong (n + m)
  | (TyCLong n, TyCLong m) -> TyCLong (n + m)
  | (TyCLong n, TyInt) -> TyLong
  | (TyCLong n, TyLong) -> TyLong
  | (TyCLong n, _) -> raise NotImplemented
  | (TyLong, TyCInt _) -> TyLong
  | (TyLong, TyInt) -> TyLong
  | (TyLong, TyCLong _) -> TyLong
  | (TyLong, TyLong)    -> TyLong
  | (TyCFloat n, TyCInt m) -> TyCFloat (n +. (float_of_int m))
  | (TyCFloat n, TyInt) -> TyFloat
  | (TyCFloat n, TyCLong m) -> TyCFloat (n +. (float_of_int m))

