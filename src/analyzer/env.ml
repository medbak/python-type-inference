(* Soonho Kong (soonhok@cs.cmu.edu) *)

type t = (string, Addrset.t) BatPMap.t

let empty = BatPMap.empty

let bind var addrset env = BatPMap.add var addrset env    
let find var env = BatPMap.find var env
let get var env =
  try
    (env, BatPMap.find var env)
  with
      Not_found ->
        let new_addrset = Addrset.singleton (Addr.get ()) in
        let env' = bind var new_addrset env in
        (env', new_addrset)
          
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

    
