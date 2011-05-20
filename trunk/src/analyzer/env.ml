open Batteries
open Ast
open Type

let empty_env = PMap.empty

let join env1 env2 =
  PMap.foldi
    (fun key value1 env ->
      try
        let value2 = PMap.find key env in
        let join_result = Type.join [value1; value2] in
        PMap.add key join_result env
      with
          Not_found ->
            PMap.add key value1 env
    )
    env1
    env2

let joinop envop1 envop2 = match (envop1, envop2) with
    (None, None) -> None
  | (Some env, None) -> Some env
  | (None, Some env) -> Some env
  | (Some env1, Some env2) -> Some (join env1 env2)

let order env1 env2 =
  PMap.foldi
    (fun key value1 result ->
      try 
        let value2 = PMap.find key env2 in
        order value1 value2
      with Not_found -> false)
    env1
    true
    
let to_string env =
  PMap.foldi
    (fun key value str ->
      str ^ key ^ " ==> " ^ (Type.to_string value) ^ "\n")
    env
    ""
