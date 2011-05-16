open Batteries
open Ast
exception NotImplemented
let empty_env = PMap.empty
  
let astat env stat = match stat with
  | Assign(exprs, expr, loc) -> raise NotImplemented
  | _ -> raise NotImplemented
    
let amodule env modu = match modu with
    Module stmts ->
      List.fold_left astat env stmts
  | Interactive stmts ->
    List.fold_left astat env stmts
  | Expression e -> env

let analysis = amodule
     
