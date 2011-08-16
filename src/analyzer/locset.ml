type elt = int
type t = elt BatPSet.t    
let value = ref 0
let get_loc () = incr value; !value
let get_locset () = BatPSet.singleton (get_loc())
let singleton = BatPSet.singleton
let order = BatPSet.subset
let order_list locset_list1 locset_list2 =
  List.for_all2 order locset_list1 locset_list2
let join = BatPSet.union
let meet = BatPSet.intersect  

let to_string locset =
  let str = BatPSet.fold (fun loc str -> str ^ (string_of_int loc) ^ " ") locset "" in
  "{" ^ str ^ "}"

let string_of_loc loc = string_of_int loc
