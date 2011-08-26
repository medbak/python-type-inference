type t = Addr.t BatPSet.t    

let empty = BatPSet.empty    
    
let singleton = BatPSet.singleton

let order = BatPSet.subset

let order_list locset_list1 locset_list2 =
  List.for_all2 order locset_list1 locset_list2

let join = BatPSet.union

let join_list : (t list -> t) = List.fold_left join empty

let meet = BatPSet.intersect

let diff = BatPSet.diff

let of_list = BatPSet.of_list  

let to_string locset =
  let str = BatPSet.fold (fun loc str -> str ^ (string_of_int loc) ^ " ") locset "" in
  "{" ^ str ^ "}"
