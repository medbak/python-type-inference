open Batteries
open Ast
open Type

exception NotImplemented of string
exception ShouldNotHappen of string
(* TODO *)    
let acomp env comp =
  raise (NotImplemented "acomp")

let rec aslice env slice =
  let aindex env exp =
    let (ty, env') = aexp env exp in
    if ty = TyInt then
      env'
    else raise (TypeError "Slice should have int type.")
  in match slice with
      Ellipsis -> env
    (* TODO: Need to extract the constraint *)
    | Slice (lb, ub, stride) ->
      List.fold_left
        (fun env exp_option -> match exp_option with
            Some exp -> aindex env exp
          | None -> env)
        env
        [lb; ub; stride]
    | Index index_exp -> aindex env index_exp
    | ExtSlice slice_list -> List.fold_left aslice env slice_list
and aexp_list env exp_list =
  List.fold_left
    (fun (ty_list, env) exp ->
      let (ty, env') = aexp env exp in
      (ty::ty_list, env'))
    ([], env)
    exp_list
and aexp_op env exp_op =
  match exp_op with
      Some exp -> aexp env exp
    | None -> (TyNone, env)
and aexp env exp = match exp with
  (* The Boolean operations "or" and "and" always return one of their operands 
   * For example: [1,2,3] and 7 and [4,5,6] = [4,5,6]
   * These operations are "short-circuit" operations. However, we assume that it's not short-circuit.
   * TODO: Change this to short-circuit.
   *) 
  | BoolOp (op, values, loc) ->
    let (ty_list, env') =
      aexp_list env values
    in
    (TyUnion (Type.normalize ty_list), env')
    (* TODO : Not implemented *)
  | BinOp (left, op, right, loc) -> 
    begin match op with
      | Add
      | Sub
      | Mult
      | Div
      | Mod
      | Pow
      | LShift
      | RShift
      | BitOr
      | BitXor
      | BitAnd
      | FloorDiv -> raise (NotImplemented "BinOP")
    end
    (* TODO : Not implemented *)
  | UnaryOp (op, exp, loc) ->
    begin match op with
      | Invert
      | Not
      | UAdd
      | USub -> raise (NotImplemented "UnaryOp")
    end
    (* TODO : Not implemented *)
  | Lambda (args, body, loc) -> raise (NotImplemented "Lambda")
  | IfExp (bexp, true_exp, false_exp, loc) ->
    let (_, env') = aexp env bexp in
    let (true_ty, true_env) = aexp env' true_exp in
    let (false_ty, false_env) = aexp env' false_exp in
    (Type.join [true_ty; false_ty], Env.join true_env false_env)
  | Dict (keys, values, loc) ->
    let (env', key_value_ty_list) = 
      List.fold_left2
        (fun (env, result)  key_exp value_exp ->
          let (key_ty, env') = aexp env key_exp in
          let (value_ty, env'') = aexp env' value_exp in
          (env'', (key_ty, value_ty)::result)
        )
        (env, [])
        keys
        values
    in
    (TyDict (Type.normalize key_value_ty_list), env')
  | Set (elts, loc) ->
    let (ty_list, env') =
      aexp_list env elts
    in
    (TySet (Type.normalize ty_list), env')
    (* List Comprehensions: PEP 202 http://www.python.org/dev/peps/pep-0202/ *)
  | ListComp (exp, comprehensions, loc) ->
    let env' = List.fold_left acomp env comprehensions in
    aexp env' exp
  | SetComp (exp, comprehensions, loc) ->
    let env' = List.fold_left acomp env comprehensions in
    aexp env' exp
    (* Dictionary Comprehensions: PEP 274 http://www.python.org/dev/peps/pep-0274/ *)
  | DictComp (exp1, exp2, comprehensions, loc) ->
    let env' = List.fold_left acomp env comprehensions in
    let (ty1, env'') = aexp env' exp1 in
    let (ty2, env''') = aexp env'' exp2 in
    (TyDict [(ty1, ty2)], env''')
    (* TODO : Not implemented *)
  | GeneratorExp (exp, comprehensions, loc) -> raise (NotImplemented "Generator")
    (* TODO : Not implemented *)
  | Yield (exp_option, loc) -> raise (NotImplemented "Yield")
    (* TODO : Not implemented *)
  | Compare (exp, cmpops, exps, loc) -> raise (NotImplemented "Compare")
    (* TODO : Not implemented *)
  | Call (exp, exps, keywords, exp1_option, exp2_option, loc) -> raise (NotImplemented "Call")
    (* "repr" expression `x` is not equivalent to the function call repr(x)
     * function call repr(x) is overriden by the function definition of repr,
     * but repr expression is not overriden by that. *)
  | Repr (exp, loc) -> let (ty, env') = aexp env exp in (TyString, env')
  | Int (_, loc) -> (TyInt, env)
  | Long (_, loc) -> (TyLong, env)
  | Float (_, loc) -> (TyFloat, env)
  | Complex (_, _, loc) -> (TyComplex, env)
  | Str (_, loc) -> (TyString, env)
  | UStr (string, loc) -> (TyUnicode, env)
    (* TODO: Need to extend to support non-class type.
     * If exp_context is "Store()", and the object is allowed to have new field,
     * then we need to add that field, instead of rasing type error. *)
  | Attribute (exp, id, exp_context, loc) ->
    let (ty, env') = aexp env exp in
    begin
      match ty with
          TyClass attr_list ->
            begin
              try
                let (attr_id, attr_ty) = List.find (fun (id', _) -> id = id') attr_list in
                (attr_ty, env')
              with Not_found -> raise (TypeError ("The object has no " ^ id ^" field."))
            end
        | _ -> raise (TypeError "Right hand side of attribute access should be object type.")
    end
  | Subscript (exp, slice, exp_context, loc) ->
    let (ty, env') = aexp env exp in
    let env'' = aslice env' slice in
    (ty, env'')
  | Name (id, ctx, loc) ->
    begin
      try
        (PMap.find id env, env)
      with Not_found -> raise (TypeError ("Name " ^ id ^ " is not in the environment"))
    end
  | List (exps, exp_context, loc) ->
    let (ty_list, env') =
      aexp_list env exps
    in
    (TyList ty_list, env')
  | Tuple (exps, exp_context, loc) ->
    let (ty_list, env') =
      aexp_list env exps
    in
    (TyTuple ty_list, env')

let rec atarget_list env target_list ty =
  List.fold_left (fun env target -> atarget env target ty)
    env
    target_list
and atarget env target ty =
  match target with
      Name (id, exp_ctx, loc) -> PMap.add id ty env
    | List (exp_list, exp_ctx, loc) 
    | Tuple (exp_list, exp_ctx, loc) ->
      begin
      (* TODO: Extend to support any arbitrary iterable type *)
        match ty with
            TyString -> atarget_list env exp_list TyString
          | TyUnicode -> atarget_list env exp_list TyUnicode
          | TyByteArray -> atarget_list env exp_list TyInt
          | TyTuple ty_list
          | TyList ty_list ->
            begin
              try
                List.fold_left2 atarget env exp_list ty_list
              with Invalid_argument _ -> raise (TypeError "Invalid numbers")
            end
          | _ -> raise (TypeError ("Should be an iterable type but " ^ (Type.to_string ty)))
      end      
    | Attribute (exp, id, exp_ctx, loc) -> raise (NotImplemented "atarget/Attribute")
    | Subscript (exp, slice, exp_ctx, loc) -> raise (NotImplemented "atarget/Subscript")
    | _ -> raise (ShouldNotHappen "Target of assignment should be one of (name, list, tuple, attribute, and subscript).")
let rec astat env stat =
  let astat_list env stats =
    List.fold_left (fun env stat -> astat env stat) env stats
  in
  match stat with
    (* TODO *)    
    | FunctionDef (name, args, body, decorator_list, loc) -> raise (NotImplemented "FunctionDef")
    (* TODO *)    
    | ClassDef (name, bases, body, decorator_list, loc) -> raise (NotImplemented "ClassDef")
    (* TODO *)    
    | Return (value_op, loc) -> raise (NotImplemented "Return")
    (* TODO *)    
    | Delete (targets, loc) -> raise (NotImplemented "Delete")
    (* TODO *)    
    | Assign (targets, value, loc) ->
      let (ty, env') = aexp env value in
      atarget_list env' targets ty
    (* TODO *)    
        | AugAssign (target, op, value, loc) -> raise (NotImplemented "AugAssing")
        | Print (dest_op, values, nl, loc) ->
          let (ty, env') = match dest_op with
              (* In extended print form, the first expression after
                 the >> must evaluate to a “file-like” object,
                 specifically an object that has a write() method *)
              Some exp -> aexp env exp (* TODO: Restrict to have "write" method *)
            | None -> (TyNone, env) in
          let (ty_list, env'') = aexp_list env' values in
          env''
        (* TODO *)    
        | For (target, iter, body, orelse, loc) -> raise (NotImplemented "For")
    (* TODO *)    
        | While (test, body, orelse, loc) -> raise (NotImplemented "While")
    (* TODO *)    
        | If (test, body, orelse, loc) ->
          let (ty, env') = aexp env test in
          let env_true = astat_list env' body in
          let env_false = astat_list env' orelse in
          Env.join env_true env_false
    (* TODO *)    
        | With (context_exp, op_vars, body, loc) -> raise (NotImplemented "With")
    (* TODO *)    
        | Raise (type_op, inst_op, tback_op, loc) -> raise (NotImplemented "Raise")
    (* TODO *)    
        | TryExcept (body, handlers, orelse, loc) -> raise (NotImplemented "TryExcept")
    (* TODO *)    
        | TryFinally (body, finalbody, loc) -> raise (NotImplemented "TryFinally")
    (* TODO *)    
        | Assert (test, msg_op, loc) -> raise (NotImplemented "Assert")
    (* TODO *)    
        | Import (names, loc) -> raise (NotImplemented "Import")
    (* TODO *)    
        | ImportFrom (module_op, names, level_op, loc) -> raise (NotImplemented "ImportFrom")
    (* TODO *)    
        | Exec (body, globals_op, locals_op, loc) -> raise (NotImplemented "Exec")
    (* TODO *)    
        | Global (names, loc) -> raise (NotImplemented "Global")
    (* TODO *)    
        | Expr (exp, loc) -> let (ty, env') = aexp env exp in env'
        | Pass loc -> env
    (* TODO *)    
        | Break loc -> raise (NotImplemented "Break")
    (* TODO *)    
        | Continue loc -> raise (NotImplemented "Continue")
          
let amodule env modu = match modu with
    Module stmts ->
      List.fold_left astat env stmts
  | Interactive stmts ->
    List.fold_left astat env stmts
  | Expression e -> env

let analysis = amodule
