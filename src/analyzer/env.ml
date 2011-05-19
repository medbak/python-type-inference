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

let to_string env =
  PMap.foldi
    (fun key value str ->
      str ^ key ^ " ==> " ^ (Type.to_string value) ^ "\n")
    env
    ""
(* PMap.foldi    ('a -> 'b -> 'c -> 'c) -> ('a, 'b) Batteries.PMap.t -> 'c -> 'c = <fun> *)
