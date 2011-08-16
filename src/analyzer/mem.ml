(* Soonho Kong (soonhok@cs.cmu.edu) *)

type t = (Addr.t, Tyset.t) BatPMap.t

let empty = BatPMap.empty

let bind addr tyset mem = BatPMap.add addr tyset mem
let find addr mem = BatPMap.find addr mem
  
let join mem1 mem2 =
  BatPMap.foldi
    (fun addr tyset1 mem ->
      try
        let tyset2 = BatPMap.find addr mem in
        let join_result = Tyset.join tyset1 tyset2 in
        BatPMap.add addr join_result mem
      with
          Not_found ->
            BatPMap.add addr tyset1 mem
    )
    mem1
    mem2

let join_list : (t list -> t) = List.fold_left join empty
    
let joinop memop1 memop2 = match (memop1, memop2) with
    (None, None) -> None
  | (Some mem, None) -> Some mem
  | (None, Some mem) -> Some mem
  | (Some mem1, Some mem2) -> Some (join mem1 mem2)

let order mem1 mem2 =
  BatPMap.foldi
    (fun addr tyset1 result ->
      try 
        let tyset2 = BatPMap.find addr mem2 in
        Tyset.order tyset1 tyset2
      with Not_found -> false)
    mem1
    true
    
let to_string mem =
  BatPMap.foldi
    (fun addr tyset str ->
      str ^ (Addr.string_of_addr addr) ^ " ==> " ^ (Tyset.to_string tyset) ^ "\n")
    mem
    ""
