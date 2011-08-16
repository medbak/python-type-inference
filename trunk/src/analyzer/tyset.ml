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


