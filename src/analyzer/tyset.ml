open Type
  
type t = Type.ty BatPSet.t

let empty = BatPSet.empty    

let singleton = BatPSet.singleton
  
let order tyset1 tyset2 =
  BatPSet.for_all
    (fun ty1 ->
      BatPSet.exists (fun ty2 -> Type.order ty1 ty2)
        tyset2)
    tyset1

let join tyset1 tyset2 =
  let tyset1' =
    BatPSet.filter (fun ty1 ->
      BatPSet.for_all (fun ty2 -> not (Type.order ty1 ty2)) tyset2)
      tyset1 in
  let tyset2' =
    BatPSet.filter (fun ty2 ->
      BatPSet.for_all (fun ty1 -> not (Type.order ty2 ty1)) tyset1')
      tyset2
  in
  BatPSet.union tyset1' tyset2'
    
let join_list tyset_list =
  List.fold_left join empty tyset_list

let cartesian_product tyset1 tyset2 =
  BatPSet.fold (fun ty1 acc -> 
    BatPSet.fold (fun ty2 acc -> 
      (ty1, ty2) :: acc) 
      tyset2 acc) 
    tyset1 []

let of_list (ty_list : Type.ty list) : t = BatPSet.of_list (Type.join_list ty_list)
    
let to_string tyset =
  let
      str = BatPSet.fold (fun ty str -> str ^ (Type.to_string ty) ^ ", ") tyset ""
  in
  "{" ^ str ^ "}"

let rec add ty1 ty2 mem = match (ty1, ty2) with
  (* Bool *)
  | (TyCBool true, _)  -> add (TyCInt 1) ty2 mem
  | (TyCBool false, _) -> add (TyCInt 0) ty2 mem
  | (_, TyCBool true)  -> add ty1 (TyCInt 1) mem
  | (_, TyCBool false) -> add ty1 (TyCInt 0) mem
  | (TyBool, _) -> add TyInt ty2 mem
  | (_, TyBool) -> add ty1 TyInt mem
  (* CInt *)
  | (TyCInt n, TyCInt m) -> (singleton (TyCInt (n + m)), mem)
  | (TyCInt n, TyInt) -> (singleton (TyInt), mem)
  | (TyCInt n, _) -> (singleton (TyNotImplemented), mem)
  (* Int *)
  | (TyInt, TyCInt _) -> (singleton (TyInt), mem)
  | (TyInt, TyInt)    -> (singleton (TyInt), mem)
  | (TyInt, _) -> (singleton (TyNotImplemented), mem)
  (* CLong *)
  | (TyCLong n, TyCInt m) -> (singleton (TyCLong (n + m)), mem)
  | (TyCLong n, TyCLong m) -> (singleton (TyCLong (n + m)), mem)
  | (TyCLong n, TyInt) -> (singleton (TyLong), mem)
  | (TyCLong n, TyLong) -> (singleton (TyLong), mem)
  | (TyCLong n, _) -> (singleton (TyNotImplemented), mem)
  (* Long *)
  | (TyLong, TyCInt _) -> (singleton (TyLong), mem)
  | (TyLong, TyInt) -> (singleton (TyLong), mem)
  | (TyLong, TyCLong _) -> (singleton (TyLong), mem)
  | (TyLong, TyLong)    -> (singleton (TyLong), mem)
  | (TyLong, _) -> (singleton (TyNotImplemented), mem)
  (* CFloat *)
  | (TyCFloat n, TyCInt m) -> (singleton (TyCFloat (n +. (float_of_int m))), mem)
  | (TyCFloat n, TyInt) -> (singleton (TyFloat), mem)
  | (TyCFloat n, TyCLong m) -> (singleton (TyCFloat (n +. (float_of_int m))), mem)
  | (TyCFloat n, TyLong) -> (singleton (TyFloat), mem)
  | (TyCFloat n, TyCFloat m) -> (singleton (TyCFloat (n +. m)), mem)
  | (TyCFloat n, TyFloat) -> (singleton (TyFloat), mem)
  | (TyCFloat _, _) -> (singleton (TyNotImplemented), mem)
  (* Float *)
  | (TyFloat, TyCInt _) -> (singleton (TyFloat), mem)
  | (TyFloat, TyInt) -> (singleton (TyFloat), mem)
  | (TyFloat, TyCLong _) -> (singleton (TyFloat), mem)
  | (TyFloat, TyLong) -> (singleton (TyFloat), mem)
  | (TyFloat, TyCFloat _) -> (singleton (TyFloat), mem)
  | (TyFloat, TyFloat) -> (singleton (TyFloat), mem)
  | (TyFloat, _) -> (singleton (TyNotImplemented), mem)
  (* CComplex *)
  | (TyCComplex (r, i), TyCInt n) -> (singleton (TyCComplex (r +. (float_of_int n), i)), mem)
  | (TyCComplex (r, i), TyInt) -> (singleton (TyComplex), mem)
  | (TyCComplex (r, i), TyCLong n) -> (singleton (TyCComplex (r +. (float_of_int n), i)), mem)
  | (TyCComplex (r, i), TyLong) -> (singleton (TyComplex), mem)
  | (TyCComplex (r, i), TyCFloat n) -> (singleton (TyCComplex (r +. n, i)), mem)
  | (TyCComplex (r, i), TyFloat) -> (singleton (TyComplex), mem)
  | (TyCComplex (r1, i1), TyCComplex (r2, i2)) -> (singleton (TyCComplex (r1 +. r2, i1 +. i2)), mem)
  | (TyCComplex (r, i), TyComplex) -> (singleton (TyComplex), mem)
  | (TyCComplex _, _) -> (singleton (TyNotImplemented), mem)
  (* Complex *)
  | (TyComplex, TyCInt _) -> (singleton (TyComplex), mem)
  | (TyComplex, TyInt) -> (singleton (TyComplex), mem)
  | (TyComplex, TyCLong _) -> (singleton (TyComplex), mem)
  | (TyComplex, TyLong) -> (singleton (TyComplex), mem)
  | (TyComplex, TyCFloat _) -> (singleton (TyComplex), mem)
  | (TyComplex, TyFloat) -> (singleton (TyComplex), mem)
  | (TyComplex, TyCComplex _) -> (singleton (TyComplex), mem)
  | (TyComplex, TyComplex) -> (singleton (TyComplex), mem)
  | (TyComplex _, _) -> (singleton (TyNotImplemented), mem)
  (* CString *)
  | (TyCString s1, TyCString s2) -> (singleton (TyCString (s1^s2)), mem)
  | (TyCString s, TyString n) -> (singleton (TyString (String.length s + n)), mem)
  | (TyCString s, TyAString) -> (singleton (TyAString), mem)
  | (TyCString _, _) -> (singleton (TyNotImplemented), mem)
  (* String *)
  | (TyString n, TyCString s) -> (singleton (TyString (String.length s + n)), mem)
  | (TyString n1, TyString n2) -> (singleton (TyString (n1 + n2)), mem)
  | (TyString n, TyAString) -> (singleton (TyAString), mem)
  | (TyString _, _) -> (singleton (TyNotImplemented), mem)
  (* AString *)
  | (TyAString, TyCString s) -> (singleton (TyAString), mem)
  | (TyAString, TyString _) -> (singleton (TyAString), mem)
  | (TyAString, TyAString) -> (singleton (TyAString), mem)
  | (TyAString, _) -> (singleton (TyNotImplemented), mem)
  (* CUnicode*)
  | (TyCUnicode s1, TyCUnicode s2) -> (singleton (TyCUnicode (BatUTF8.append s1 s2)), mem)
  | (TyCUnicode s, TyUnicode n) -> (singleton (TyUnicode (BatUTF8.length s + n)), mem)
  | (TyCUnicode s, TyAUnicode) -> (singleton (TyAUnicode), mem)
  | (TyCUnicode s, _) -> (singleton (TyNotImplemented), mem)
  (* Unicode *)
  | (TyUnicode n,  TyCUnicode s) -> (singleton (TyUnicode (BatUTF8.length s + n)), mem)
  | (TyUnicode n1, TyUnicode n2) -> (singleton (TyUnicode (n1 + n2)), mem)
  | (TyUnicode n, TyAUnicode) -> (singleton (TyAUnicode), mem)
  | (TyUnicode n, _) -> (singleton (TyNotImplemented), mem)
  (* AUnicode *)
  | (TyAUnicode, TyCUnicode _) -> (singleton (TyAUnicode), mem)
  | (TyAUnicode, TyUnicode _) -> (singleton (TyAUnicode), mem)
  | (TyAUnicode, TyAUnicode) -> (singleton (TyAUnicode), mem)
  | (TyAUnicode, _) -> (singleton (TyNotImplemented), mem)
  (* Tuple *)
  | (TyTuple addrset_list1, TyTuple addrset_list2) -> (singleton (TyTuple (addrset_list1@addrset_list2)), mem)
  | (TyTuple addrset_list, TyATuple addrset) -> (singleton (TyATuple (List.fold_left Addrset.join addrset addrset_list)), mem)
  | (TyTuple _, _) -> (singleton (TyNotImplemented), mem)
  (* ATuple *)
  | (TyATuple addrset, TyTuple addrset_list) -> (singleton (TyATuple (List.fold_left Addrset.join addrset addrset_list)), mem)
  | (TyATuple addrset1, TyATuple addrset2) -> (singleton (TyATuple (Addrset.join addrset1 addrset2)), mem)
  | (TyATuple _, _) -> (singleton (TyNotImplemented), mem)
  (* List *)
  | (TyList addrset_list1, TyList addrset_list2) -> (singleton (TyList (addrset_list1@addrset_list2)), mem)
  | (TyList addrset_list, TyAList addrset) -> (singleton (TyAList (List.fold_left Addrset.join addrset addrset_list)), mem)
  | (TyList _, _) -> (singleton (TyNotImplemented), mem)
  (* AList *)
  | (TyAList addrset, TyList addrset_list) -> (singleton (TyAList (List.fold_left Addrset.join addrset addrset_list)), mem)
  | (TyAList addrset1, TyAList addrset2) -> (singleton (TyAList (Addrset.join addrset1 addrset2)), mem)
  | (TyAList _, _) -> (singleton (TyNotImplemented), mem)
  (* ByteArray *)
  | (TyByteArray n1, TyByteArray n2) -> (singleton (TyByteArray (n1 + n2)), mem)
  | (TyByteArray _, TyAByteArray) -> (singleton (TyAByteArray), mem)
  | (TyByteArray _, _) -> (singleton (TyNotImplemented), mem)
  (* ByteArray *)
  | (TyAByteArray, TyByteArray n) -> (singleton (TyAByteArray), mem)
  | (TyAByteArray, TyAByteArray) -> (singleton (TyAByteArray), mem)
  | (TyAByteArray, _) -> (singleton (TyNotImplemented), mem)
  (* Set *)
  | (TySet _, _) -> (singleton (TyNotImplemented), mem)
  (* ASet *)
  | (TyASet _, _) -> (singleton (TyNotImplemented), mem)
  (* FrozenSet *)
  | (TyFrozenSet _, _) -> (singleton (TyNotImplemented), mem)
  (* AFrozenSet *)
  | (TyAFrozenSet _, _) -> (singleton (TyNotImplemented), mem)
  (* Dict *)
  | (TyDict _, _) -> (singleton (TyNotImplemented), mem)
  (* Function *)
  | (TyFunction _, _) -> (singleton (TyNotImplemented), mem)
  (* Generator *)
  | (TyGenerator _, _) -> (singleton (TyNotImplemented), mem)
  (* AGenerator *)
  | (TyAGenerator _, _) -> (singleton (TyNotImplemented), mem)
  (* Object *)
  | (TyObject, _) -> (singleton (TyNotImplemented), mem)
  (* Type *)
  | (TyType _, _) -> (singleton (TyNotImplemented), mem)
  (* ETC *)   
  | (TyEllipsis, _) -> (singleton (TyNotImplemented), mem)
  | (TyNotImplemented, _) -> (singleton (TyNotImplemented), mem)
  | (TyNone, _) -> (singleton (TyNotImplemented), mem)
  | (TyBot, _) -> (singleton (TyNotImplemented), mem)
  (* TODO: Handle Class *)
  | (TyClass _, _) -> (singleton (TyNotImplemented), mem)

let rec sub ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec mult ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec div ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec modu ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec pow ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec lshift ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec rshift ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec bor ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec bxor ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec band ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec fdiv ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec radd ty1 ty2 mem = match (ty1, ty2) with
  (* Bool *)
  | (TyCBool true, _)  -> radd (TyCInt 1) ty2 mem
  | (TyCBool false, _) -> radd (TyCInt 0) ty2 mem
  | (_, TyCBool true)  -> radd ty1 (TyCInt 1) mem
  | (_, TyCBool false) -> radd ty1 (TyCInt 0) mem
  | (TyBool, _) -> radd TyInt ty2 mem
  | (_, TyBool) -> radd ty1 TyInt mem
  (* CInt *)
  | (TyCInt n, TyCInt m) -> (singleton (TyCInt (n + m)), mem)
  | (TyCInt n, TyInt) -> (singleton (TyInt), mem)
  | (TyCInt n, _) -> (singleton (TyNotImplemented), mem)
  (* Int *)
  | (TyInt, TyCInt _) -> (singleton (TyInt), mem)
  | (TyInt, TyInt)    -> (singleton (TyInt), mem)
  | (TyInt, _) -> (singleton (TyNotImplemented), mem)
  (* CLong *)
  | (TyCLong n, TyCInt m) -> (singleton (TyCLong (n + m)), mem)
  | (TyCLong n, TyCLong m) -> (singleton (TyCLong (n + m)), mem)
  | (TyCLong n, TyInt) -> (singleton (TyLong), mem)
  | (TyCLong n, TyLong) -> (singleton (TyLong), mem)
  | (TyCLong n, _) -> (singleton (TyNotImplemented), mem)
  (* Long *)
  | (TyLong, TyCInt _) -> (singleton (TyLong), mem)
  | (TyLong, TyInt) -> (singleton (TyLong), mem)
  | (TyLong, TyCLong _) -> (singleton (TyLong), mem)
  | (TyLong, TyLong)    -> (singleton (TyLong), mem)
  | (TyLong, _) -> (singleton (TyNotImplemented), mem)
  (* CFloat *)
  | (TyCFloat n, TyCInt m) -> (singleton (TyCFloat (n +. (float_of_int m))), mem)
  | (TyCFloat n, TyInt) -> (singleton (TyFloat), mem)
  | (TyCFloat n, TyCLong m) -> (singleton (TyCFloat (n +. (float_of_int m))), mem)
  | (TyCFloat n, TyLong) -> (singleton (TyFloat), mem)
  | (TyCFloat n, TyCFloat m) -> (singleton (TyCFloat (n +. m)), mem)
  | (TyCFloat n, TyFloat) -> (singleton (TyFloat), mem)
  | (TyCFloat _, _) -> (singleton (TyNotImplemented), mem)
  (* Float *)
  | (TyFloat, TyCInt _) -> (singleton (TyFloat), mem)
  | (TyFloat, TyInt) -> (singleton (TyFloat), mem)
  | (TyFloat, TyCLong _) -> (singleton (TyFloat), mem)
  | (TyFloat, TyLong) -> (singleton (TyFloat), mem)
  | (TyFloat, TyCFloat _) -> (singleton (TyFloat), mem)
  | (TyFloat, TyFloat) -> (singleton (TyFloat), mem)
  | (TyFloat, _) -> (singleton (TyNotImplemented), mem)
  (* CComplex *)
  | (TyCComplex (r, i), TyCInt n) -> (singleton (TyCComplex (r +. (float_of_int n), i)), mem)
  | (TyCComplex (r, i), TyInt) -> (singleton (TyComplex), mem)
  | (TyCComplex (r, i), TyCLong n) -> (singleton (TyCComplex (r +. (float_of_int n), i)), mem)
  | (TyCComplex (r, i), TyLong) -> (singleton (TyComplex), mem)
  | (TyCComplex (r, i), TyCFloat n) -> (singleton (TyCComplex (r +. n, i)), mem)
  | (TyCComplex (r, i), TyFloat) -> (singleton (TyComplex), mem)
  | (TyCComplex (r1, i1), TyCComplex (r2, i2)) -> (singleton (TyCComplex (r1 +. r2, i1 +. i2)), mem)
  | (TyCComplex (r, i), TyComplex) -> (singleton (TyComplex), mem)
  | (TyCComplex _, _) -> (singleton (TyNotImplemented), mem)
  (* Complex *)
  | (TyComplex, TyCInt _) -> (singleton (TyComplex), mem)
  | (TyComplex, TyInt) -> (singleton (TyComplex), mem)
  | (TyComplex, TyCLong _) -> (singleton (TyComplex), mem)
  | (TyComplex, TyLong) -> (singleton (TyComplex), mem)
  | (TyComplex, TyCFloat _) -> (singleton (TyComplex), mem)
  | (TyComplex, TyFloat) -> (singleton (TyComplex), mem)
  | (TyComplex, TyCComplex _) -> (singleton (TyComplex), mem)
  | (TyComplex, TyComplex) -> (singleton (TyComplex), mem)
  | (TyComplex _, _) -> (singleton (TyNotImplemented), mem)
  (* CString *)
  | (TyCString s1, TyCString s2) -> (singleton (TyCString (s2^s1)), mem) (* Caution!!! *)
  | (TyCString s, TyString n) -> (singleton (TyString (String.length s + n)), mem)
  | (TyCString s, TyAString) -> (singleton (TyAString), mem)
  | (TyCString _, _) -> (singleton (TyNotImplemented), mem)
  (* String *)
  | (TyString n, TyCString s) -> (singleton (TyString (String.length s + n)), mem)
  | (TyString n1, TyString n2) -> (singleton (TyString (n1 + n2)), mem)
  | (TyString n, TyAString) -> (singleton (TyAString), mem)
  | (TyString _, _) -> (singleton (TyNotImplemented), mem)
  (* AString *)
  | (TyAString, TyCString s) -> (singleton (TyAString), mem)
  | (TyAString, TyString _) -> (singleton (TyAString), mem)
  | (TyAString, TyAString) -> (singleton (TyAString), mem)
  | (TyAString, _) -> (singleton (TyNotImplemented), mem)
  (* CUnicode*)
  | (TyCUnicode s1, TyCUnicode s2) -> (singleton (TyCUnicode (BatUTF8.append s2 s1)), mem) (* Caution!!! *)
  | (TyCUnicode s, TyUnicode n) -> (singleton (TyUnicode (BatUTF8.length s + n)), mem)
  | (TyCUnicode s, TyAUnicode) -> (singleton (TyAUnicode), mem)
  | (TyCUnicode s, _) -> (singleton (TyNotImplemented), mem)
  (* Unicode *)
  | (TyUnicode n,  TyCUnicode s) -> (singleton (TyUnicode (BatUTF8.length s + n)), mem)
  | (TyUnicode n1, TyUnicode n2) -> (singleton (TyUnicode (n1 + n2)), mem)
  | (TyUnicode n, TyAUnicode) -> (singleton (TyAUnicode), mem)
  | (TyUnicode n, _) -> (singleton (TyNotImplemented), mem)
  (* AUnicode *)
  | (TyAUnicode, TyCUnicode _) -> (singleton (TyAUnicode), mem)
  | (TyAUnicode, TyUnicode _) -> (singleton (TyAUnicode), mem)
  | (TyAUnicode, TyAUnicode) -> (singleton (TyAUnicode), mem)
  | (TyAUnicode, _) -> (singleton (TyNotImplemented), mem)
  (* Tuple *)
  | (TyTuple addrset_list1, TyTuple addrset_list2) -> (singleton (TyTuple (addrset_list2@addrset_list1)), mem) (* Caution!!! *)
  | (TyTuple addrset_list, TyATuple addrset) -> (singleton (TyATuple (List.fold_left Addrset.join addrset addrset_list)), mem)
  | (TyTuple _, _) -> (singleton (TyNotImplemented), mem)
  (* ATuple *)
  | (TyATuple addrset, TyTuple addrset_list) -> (singleton (TyATuple (List.fold_left Addrset.join addrset addrset_list)), mem)
  | (TyATuple addrset1, TyATuple addrset2) -> (singleton (TyATuple (Addrset.join addrset1 addrset2)), mem)
  | (TyATuple _, _) -> (singleton (TyNotImplemented), mem)
  (* List *)
  | (TyList addrset_list1, TyList addrset_list2) -> (singleton (TyList (addrset_list2@addrset_list1)), mem)  (* Caution!!! *)
  | (TyList addrset_list, TyAList addrset) -> (singleton (TyAList (List.fold_left Addrset.join addrset addrset_list)), mem)
  | (TyList _, _) -> (singleton (TyNotImplemented), mem)
  (* AList *)
  | (TyAList addrset, TyList addrset_list) -> (singleton (TyAList (List.fold_left Addrset.join addrset addrset_list)), mem)
  | (TyAList addrset1, TyAList addrset2) -> (singleton (TyAList (Addrset.join addrset1 addrset2)), mem)
  | (TyAList _, _) -> (singleton (TyNotImplemented), mem)
  (* ByteArray *)
  | (TyByteArray n1, TyByteArray n2) -> (singleton (TyByteArray (n1 + n2)), mem)
  | (TyByteArray _, TyAByteArray) -> (singleton (TyAByteArray), mem)
  | (TyByteArray _, _) -> (singleton (TyNotImplemented), mem)
  (* ByteArray *)
  | (TyAByteArray, TyByteArray n) -> (singleton (TyAByteArray), mem)
  | (TyAByteArray, TyAByteArray) -> (singleton (TyAByteArray), mem)
  | (TyAByteArray, _) -> (singleton (TyNotImplemented), mem)
  (* Set *)
  | (TySet _, _) -> (singleton (TyNotImplemented), mem)
  (* ASet *)
  | (TyASet _, _) -> (singleton (TyNotImplemented), mem)
  (* FrozenSet *)
  | (TyFrozenSet _, _) -> (singleton (TyNotImplemented), mem)
  (* AFrozenSet *)
  | (TyAFrozenSet _, _) -> (singleton (TyNotImplemented), mem)
  (* Dict *)
  | (TyDict _, _) -> (singleton (TyNotImplemented), mem)
  (* Function *)
  | (TyFunction _, _) -> (singleton (TyNotImplemented), mem)
  (* Generator *)
  | (TyGenerator _, _) -> (singleton (TyNotImplemented), mem)
  (* AGenerator *)
  | (TyAGenerator _, _) -> (singleton (TyNotImplemented), mem)
  (* Object *)
  | (TyObject, _) -> (singleton (TyNotImplemented), mem)
  (* Type *)
  | (TyType _, _) -> (singleton (TyNotImplemented), mem)
  (* ETC *)   
  | (TyEllipsis, _) -> (singleton (TyNotImplemented), mem)
  | (TyNotImplemented, _) -> (singleton (TyNotImplemented), mem)
  | (TyNone, _) -> (singleton (TyNotImplemented), mem)
  | (TyBot, _) -> (singleton (TyNotImplemented), mem)
  (* TODO: Handle Class *)
  | (TyClass _, _) -> (singleton (TyNotImplemented), mem)
    

let rec rsub ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented
      
let rec rmult ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec rdiv ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec rmodu ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec rpow ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec rlshift ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec rrshift ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec rbor ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec rbxor ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec rband ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented

let rec rfdiv ty1 ty2 mem = match (ty1, ty2) with
    _ -> raise NotImplemented
