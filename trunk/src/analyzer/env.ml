(* Soonho Kong (soonhok@cs.cmu.edu) *)

type t = (string, Addrset.t) BatPMap.t

let empty = BatPMap.empty

let bind var locset env = BatPMap.add var locset env    
let find var env = BatPMap.find var env
  
let join env1 env2 =
  BatPMap.foldi
    (fun key value1 env ->
      try
        let value2 = BatPMap.find key env in
        let join_result = Addrset.join value1 value2 in
        BatPMap.add key join_result env
      with
          Not_found ->
            BatPMap.add key value1 env
    )
    env1
    env2
  
let joinop envop1 envop2 = match (envop1, envop2) with
    (None, None) -> None
  | (Some env, None) -> Some env
  | (None, Some env) -> Some env
  | (Some env1, Some env2) -> Some (join env1 env2)

let order env1 env2 =
  BatPMap.foldi
    (fun key value1 result ->
      try 
        let value2 = BatPMap.find key env2 in
        Addrset.order value1 value2
      with Not_found -> false)
    env1
    true
    
let to_string env =
  BatPMap.foldi
    (fun key value str ->
      str ^ key ^ " ==> " ^ (Addrset.to_string value) ^ "\n")
    env
    ""

    
