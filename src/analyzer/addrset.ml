type t = Addr.t BatPSet.t    

let empty = BatPSet.empty

let singleton addr = BatPSet.singleton addr

let get () =
  let new_addr = Addr.get () in
  (new_addr, singleton new_addr)

let order = BatPSet.subset

let order_list addrset_list1 addrset_list2 =
  List.for_all2 order addrset_list1 addrset_list2

let join = BatPSet.union

let join_list : (t list -> t) = List.fold_left join empty

let meet = BatPSet.intersect

let diff = BatPSet.diff

let of_list = BatPSet.of_list  

let to_string addrset =
  let str = BatPSet.fold (fun loc str -> str ^ (string_of_int loc) ^ " ") addrset "" in
  "{" ^ str ^ "}"
