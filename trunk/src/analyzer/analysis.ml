(* Soonho Kong (soonhok@cs.cmu4.edu) *)
open Batteries
open Ast
open Type

exception NotImplemented of string
exception ShouldNotHappen of string
type env = (Ast.identifier, Type.ty) Batteries.PMap.t
type ctl =
  | CtlBreak
  | CtlContinue
  | CtlReturn
  | CtlYield
type size = INT of int | UNKNOWN
let mul_size s1 s2 = match (s1, s2) with
    (INT x, INT y) -> INT (x * y)
  | _ -> UNKNOWN

let rec repeat elt n = match n with
  | 0 -> []
  | n -> elt::(repeat elt (n-1))

(* TODO *)
let rec acomp env (target, iter, ifs) =
  let (iter_ty, env') = aexp env iter in
  let (target_ty, size) = match iter_ty with
      TyList tylist -> (Type.join (Type.normalize tylist), INT (List.length tylist))
    | TyAList ty -> (ty, UNKNOWN)
    | TyTuple tylist -> (Type.join (Type.normalize tylist), INT (List.length tylist))
    | TyATuple ty -> (ty, UNKNOWN)
    | TyByteArray l -> (TyInt, INT l)
    | TyAByteArray -> (TyInt, UNKNOWN)
    | TyDict tyty_list ->
      (Type.join (Type.normalize (List.fold_left (fun tylist (ty_key, ty_value) -> ty_key::tylist) [] tyty_list)), UNKNOWN)
    | TyString l -> (TyString 1, INT l)
    | TyAString -> (TyString 1, UNKNOWN)
    | TyUnicode l -> (TyUnicode 1, INT l)
    | TyAUnicode -> (TyUnicode 1, UNKNOWN)
    | TySet (ty, l) -> (ty, INT l)
    | TyASet ty -> (ty, UNKNOWN)
    | TyFrozenSet (ty, l) -> (ty, INT l)
    | TyAFrozenSet ty -> (ty, UNKNOWN)
    | TyGenerator (ty, l) -> (ty, INT l)
    | TyAGenerator ty -> (ty, UNKNOWN)
    (* ---- TODO: Currently, the following types are not iterable.-----------------*)
    | TyClass _|TyUnion _|TyType _|TyFunction _|TyVar _|TyObject
      -> raise (ShouldNotHappen "acomp: Not iterable")
    (* ---------------------------------------------------.-----------------*)
    | TyComplex|TyFloat|TyBool|TyLong|TyInt|TyEllipsis|TyNotImplemented|TyNone
    | TyTop|TyBot -> raise (ShouldNotHappen "acomp: Not iterable")
  in
  match ifs with
      [] -> (atarget env' target target_ty, size)
    (* TODO: ifs are not evaluated. *)
    | _ -> (atarget env' target target_ty, UNKNOWN)
and acomps env comprehensions =
  List.fold_left
    (fun (env, size) comp ->
      let (env', size') = acomp env comp in
      (env', mul_size size size'))
    (env, INT 1)
    comprehensions
and aslice env slice =
  let aindex env exp =
    let (ty, env') = aexp env exp in
    if ty = TyInt then
      env'
    else raise (TypeError ("Slice should have int type.", Ast.exp2loc exp))
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
      (ty_list@[ty], env'))
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
          (env'', result@[(key_ty, value_ty)])
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
    (TySet ((Type.join ty_list), List.length elts), env')
  (* List Comprehensions: PEP 202 http://www.python.org/dev/peps/pep-0202/ *)
  | ListComp (exp, comprehensions, loc) ->
    begin
      let (env', size) = acomps env comprehensions in
      let (ty, env'') = aexp env' exp in
      match size with
          INT l -> (TyList (repeat ty l), env'')
        | UNKNOWN -> (TyAList ty, env'')
    end
  (*[(x,y) for x in [1,2,3] for y in [4,5,6] if y > 4]
    ListComp(Tuple([Name('x', Load()), Name('y', Load())], Load()),
    [comprehension(Name('x', Store()), List([Num(1), Num(2), Num(3)], Load()), []),
    comprehension(Name('y', Store()), List([Num(4), Num(5), Num(6)], Load()), [Compare(Name('y', Load()), [Gt()], [Num(4)])])])
  *)   
  | SetComp (exp, comprehensions, loc) ->
    begin
      let (env', size) = acomps env comprehensions in
      let (ty, env'') = aexp env' exp in
      match size with
          INT l -> (TySet (ty, l), env'')
        | UNKNOWN -> (TyASet ty, env'')
    end
  (* Dictionary Comprehensions: PEP 274 http://www.python.org/dev/peps/pep-0274/ *)
  | DictComp (exp1, exp2, comprehensions, loc) ->
    let (env', size) = acomps env comprehensions in
    let (ty1, env'') = aexp env' exp1 in
    let (ty2, env''') = aexp env'' exp2 in
    (TyDict [(ty1, ty2)], env''')
  | GeneratorExp (exp, comprehensions, loc) ->
    begin
      let (env', size) = acomps env comprehensions in
      let (ty, env'') = aexp env' exp in
      match size with
          INT l -> (TyGenerator (ty, l), env'')
        | UNKNOWN -> (TyAGenerator ty, env'')
    end
  | Yield (exp_option, loc) ->
    let (ty, env') = aexp_op env exp_option in
    (TyAGenerator ty, env')
  (* TODO : Not implemented *)
  | Compare (exp, cmpops, exps, loc) -> raise (NotImplemented "Compare")
  (* TODO : Not implemented *)
  | Call (exp, exps, keywords, exp1_option, exp2_option, loc) -> raise (NotImplemented "Call")
  (* "repr" expression `x` is not equivalent to the function call repr(x)
   * function call repr(x) is overriden by the function definition of repr,
   * but repr expression is not overriden by that.
   *
   * http://docs.python.org/reference/expressions.html#string-conversions
   *)
  | Repr (exp, loc) -> let (ty, env') = aexp env exp in (TyAString, env')
  | Int (_, loc) -> (TyInt, env)
  | Long (_, loc) -> (TyLong, env)
  | Float (_, loc) -> (TyFloat, env)
  | Complex (_, _, loc) -> (TyComplex, env)
  | Str (s, loc) -> (TyString (String.length s), env)
  | UStr (s, loc) -> (TyUnicode (UTF8.length s), env)
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
              with Not_found -> raise (TypeError ("The object has no " ^ id ^" field.", loc))
            end
        | TyUnion _|TyType _|TyGenerator _|TyFunction _|TyDict _|TyFrozenSet _
        | TySet _|TyAList _|TyList _|TyATuple _|TyTuple _|TyUnicode _|TyString _
        | TyVar _|TyObject|TyByteArray _|TyAByteArray _|TyAUnicode|TyAString|TyComplex|TyFloat|TyBool
        | TyLong|TyInt|TyEllipsis|TyNotImplemented|TyNone|TyBot|TyTop
        | TyAGenerator _|TyAFrozenSet _|TyASet _
          -> raise (TypeError ("Right hand side of attribute access should be object type.", loc))
    end
  | Subscript (exp, slice, exp_context, loc) ->
    let (ty, env') = aexp env exp in
    let env'' = aslice env' slice in
    (ty, env'')
  | Name (id, ctx, loc) ->
    begin
      try
        (Env.find id env, env)
      with Not_found -> raise (TypeError ("Name " ^ id ^ " is not in the environment", loc))
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
and atarget_list env target_list ty =
  List.fold_left (fun env target -> atarget env target ty)
    env
    target_list
(* atarget env target ty :
 * Add (target : ty) into the environment env *)
and atarget env target ty =
  match target with
      Name (id, exp_ctx, loc) -> Env.bind id ty env
    | List (exp_list, exp_ctx, loc) 
    | Tuple (exp_list, exp_ctx, loc) ->
      begin
        (* TODO: Extend to support any arbitrary iterable type *)
        let exp_len = List.length exp_list in
        match ty with
            TyString l ->
              if exp_len = l then atarget_list env exp_list (TyString 1)
              else raise (TypeError ("Invalid numbers.", loc))
          | TyAString -> raise (TypeError ("It should have string type, not abstract string type.", loc))
          | TyUnicode l ->
            if exp_len = l then atarget_list env exp_list (TyUnicode 1)
            else raise (TypeError ("Invalid numbers.", loc))
          | TyAUnicode -> raise (TypeError ("It should have unicode type, not abstract unicode type.", loc))
          | TyByteArray l ->
            if exp_len = l then atarget_list env exp_list TyInt
            else raise (TypeError ("Invalid numbers", loc))
          | TyAByteArray -> raise (TypeError ("Invalid numbers", loc))
          | TyTuple ty_list
          | TyList ty_list ->
            begin
              try
                List.fold_left2 atarget env exp_list ty_list
              with Invalid_argument _ -> raise (TypeError ("Invalid numbers", loc))
            end
          | TyAList _ -> raise (TypeError ("Invalid numbers", loc))
          | TyATuple _ -> raise (TypeError ("Invalid numbers", loc))
          | TyDict tyty_list -> raise (TypeError ("Invalid numbers", loc))
          | TySet (ty, l) -> if exp_len = l then atarget_list env exp_list ty
            else raise (TypeError ("Invalid numbers", loc))
          | TyASet _ -> raise (TypeError ("Invalid numbers", loc))
          | TyFrozenSet (ty, l) -> if exp_len = l then atarget_list env exp_list ty
            else raise (TypeError ("Invalid numbers", loc))
          | TyAFrozenSet _ -> raise (TypeError ("Invalid numbers", loc))
          | TyGenerator (ty, l) -> if exp_len = l then atarget_list env exp_list ty
            else raise (TypeError ("Invalid numbers", loc))
          | TyAGenerator _ -> raise (TypeError ("Invalid numbers", loc))
          |TyClass _|TyUnion _|TyType _|TyFunction _
          |TyVar _|TyObject|TyComplex|TyFloat|TyBool|TyLong|TyInt|TyEllipsis|TyNotImplemented
          |TyNone|TyBot|TyTop
            -> raise (TypeError ("Should be an iterable type but " ^ (Type.to_string ty), loc))
      end      
    | Attribute (exp, id, exp_ctx, loc) -> raise (NotImplemented "atarget/Attribute")
    | Subscript (exp, slice, exp_ctx, loc) -> raise (NotImplemented "atarget/Subscript")
    | _ -> raise (ShouldNotHappen "Target of assignment should be one of (name, list, tuple, attribute, and subscript).")
(* TODO: Currently, it only support positional arguments (no vararg, keyword, and default) *)
and aarguments env (args, vararg_op, kwarg_op, defaults) func_name loc = 
  let (env', _, arg_ty_list) = List.fold_left
    (fun (env, i, arg_ty_list) arg ->
      let arg_ty = TyVar (func_name, loc, i, TyTop) in
      (atarget env arg arg_ty , i+1, arg_ty::arg_ty_list))
    (env, 1, [])
    args
  in (env', List.rev arg_ty_list)
and astat env envlist stat =
  match stat with 
    (* TODO *)    
    | FunctionDef (name, args, body, decorator_list, loc) ->
      let (env', arg_ty_list) = aarguments env args name loc in
      let (envop, env_ctl_list) = astat_list env' envlist body in
      let return_env_ctl_list = List.filter (fun (env, ctl) -> ctl = CtlReturn) env_ctl_list in
      let ret_ty_list = List.map (fun (env, ctl) -> Env.find "!return!" env) return_env_ctl_list in
      let ret_ty_list' = match envop with
          None -> ret_ty_list
        | Some _ -> TyNone::ret_ty_list in
      let ret_ty = Type.join ret_ty_list' in
      let env'' = Env.bind name (TyFunction (arg_ty_list, ret_ty)) env in
      (Some env'', envlist)
    (* TODO *)    
    | ClassDef (name, bases, body, decorator_list, loc) -> raise (NotImplemented "ClassDef")
    (* TODO *)    
    | Return (value_op, loc) ->
      let (ty, env') = aexp_op env value_op in
      let env'' = Env.bind "!return!" ty env' in
      (None, (env'', CtlReturn)::envlist)
    (* TODO *)    
    | Delete (targets, loc) -> raise (NotImplemented "Delete")
    (* TODO *)    
    | Assign (targets, value, loc) ->
      let (ty, env') = aexp env value in
      (Some (atarget_list env' targets ty), envlist)
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
      (Some env'', envlist)
    (* TODO *)    
    | For (target, iter, body, orelse, loc) -> raise (NotImplemented "For")
    (* TODO *)    
    | While (test, body, orelse, loc) ->
      let (ty, env') = aexp env test in
      let (envop, envlist') = astat_list env' [] body in
      raise (NotImplemented "For Statement")
    | If (test, body, orelse, loc) ->
      let (ty, env') = aexp env test in
      let (envop_t, envlist_t) = astat_list env' [] body in
      let (envop_f, envlist_f) = astat_list env' [] orelse in
      (Env.joinop envop_t envop_f, envlist @ envlist_t @ envlist_f)
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
    | Expr (exp, loc) ->
      let (ty, env') = aexp env exp in
      (Some env', [])
    | Pass loc -> (Some env, [])
    | Break loc -> (None, [(env, CtlBreak)])
    | Continue loc -> (None, [(env, CtlContinue)])
and astat_list env envlist stat_list =
  match stat_list with
      [] -> (Some env, envlist)
    | stat::stat_list ->
      let (envop', envlist') = astat env envlist stat in
      match envop' with
          None -> (None, envlist')
        | Some env' ->  astat_list env' envlist' stat_list
and amodule env modu = match modu with
    Module stmts ->
      fst (astat_list env [] stmts)
  | Interactive stmts ->
    fst (astat_list env [] stmts)
  | Expression exp ->
    let (ty, env') = aexp env exp
    in Some (Env.bind "!it!" ty env')

let analysis = amodule
