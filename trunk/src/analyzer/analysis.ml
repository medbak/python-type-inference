(* Soonho Kong (soonhok@cs.cmu.edu) *)
open Batteries
open Ast
open Type
open Lib

exception NotImplemented of string
exception ShouldNotHappen of string
type env = (Ast.identifier, Type.ty) Batteries.PMap.t
type ctl =
  | CtlBreak
  | CtlContinue
  | CtlReturn
type size = INT of int | UNKNOWN
let mul_size s1 s2 = match (s1, s2) with
    (INT x, INT y) -> INT (x * y)
  | _ -> UNKNOWN

let get_addr exp = Addr.get ()    
    
(* TODO *)
(*
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
    | TyClass _ | TyType _ | TyFunction _ | TyObject
      -> raise (ShouldNotHappen "acomp: Not iterable")
    (* ---------------------------------------------------.-----------------*)
    | TyComplex|TyFloat|TyBool|TyLong|TyInt|TyEllipsis|TyNotImplemented|TyNone
    | TyBot -> raise (ShouldNotHappen "acomp: Not iterable")
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
    | ExtSlice slice_list -> List.fold_left aslice env slice_list *)
let rec aexp_list (env : Env.t) (mem : Mem.t) (exp_list : Ast.expr list) : (Tyset.t list * Mem.t) =
  List.fold_left
    (fun (tyset_list, mem) exp ->
      let (tyset, mem') = aexp env mem exp in
      (tyset_list@[tyset], mem'))
    ([], mem)
    exp_list
(*
and aexp_op env mem exp_op =
  match exp_op with
      Some exp -> aexp env mem exp
    | None -> (Tyset.empty, mem)
*)
and aexp (env : Env.t) (mem : Mem.t) (exp : Ast.expr) : (Tyset.t * Mem.t) = match exp with
  (* The Boolean operations "or" and "and" always return one of their operands 
   * For example: [1,2,3] and 7 and [4,5,6] = [4,5,6]
   * These operations are "short-circuit" operations. However, we assume that it's not short-circuit.
   * TODO: Change this to short-circuit.
   *) 
  | BoolOp (op, values, loc) ->
    let (tyset_list, mem') =
      aexp_list env mem values
    in
    (Tyset.join_list tyset_list, mem')
  (* TODO : Not implemented *)
  | BinOp (left, op, right, loc) ->
    (* TODO: add more binary operation *)      
    let binop ty1 ty2 (op : Ast.operator) (mem : Mem.t) : (Tyset.t * Mem.t) = (Tyset.of_list [ty1;ty2], mem) in
    let (tyset_left, mem') = aexp env mem left in
    let (tyset_right, mem'') = aexp env mem' right in
    let tyset_product : (Type.ty * Type.ty) list = Tyset.cartesian_product tyset_left tyset_right in
    let (result_types, result_mems) = List.split (List.map (fun (ty1, ty2) -> binop ty1 ty2 op mem'') tyset_product)
    in
    (Tyset.join_list result_types, Mem.join_list result_mems)
(*      
    
    begin match op with
      | Add
      | Sub
      | Mult
      | Div ->
        begin
          match (ty_left, ty_right) with
              (TyInt, TyInt) -> (TyInt, env'')
            | (TyInt, TyLong) -> (TyLong, env'')
            | (TyInt, TyFloat) -> (TyFloat, env'')
            | (TyInt, TyComplex) -> (TyComplex, env'')
            | (TyLong, TyInt) -> (TyLong, env'')
            | (TyLong, TyLong) -> (TyLong, env'')
            | (TyLong, TyFloat) -> (TyFloat, env'')
            | (TyLong, TyComplex) -> (TyComplex, env'')
            | (TyFloat, TyInt) -> (TyFloat, env'')
            | (TyFloat, TyLong) -> (TyFloat, env'')
            | (TyFloat, TyFloat) -> (TyFloat, env'')
            | (TyFloat, TyComplex) -> (TyComplex, env'')
            | (TyComplex, TyInt) -> (TyComplex, env'')
            | (TyComplex, TyLong) -> (TyComplex, env'')
            | (TyComplex, TyFloat) -> (TyComplex, env'')
            | (TyComplex, TyComplex) -> (TyComplex, env'')
            | _ -> raise (NotImplemented ("BinOP: left_exp = " ^ (to_string ty_left) ^ " , right_exp = " ^ (to_string ty_right)))
        end
      | Mod
      | Pow
      | LShift
      | RShift
      | BitOr
      | BitXor
      | BitAnd
      | FloorDiv -> raise (NotImplemented "BinOP")
    end
*)

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
    let (_, mem') = aexp env mem bexp in
    let (true_tyset, true_mem) = aexp env mem' true_exp in
    let (false_tyset, false_mem) = aexp env mem' false_exp in
    (Tyset.join true_tyset false_tyset, Mem.join true_mem false_mem)
  | Dict (keys, values, loc) -> raise (NotImplemented "Dict")
(*    let (mem', key_value_ty_list) = 
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
    (TyDict (Type.normalize key_value_ty_list), env') *)
  | Set (elts, loc) ->
    let (tyset_list, mem') = aexp_list env mem elts in
    let addr_list = List.map get_addr elts in
    let mem'' = List.fold_left2 (fun mem addr tyset -> Mem.bind addr tyset mem) mem' addr_list tyset_list in
    (Tyset.singleton (TySet(Addrset.of_list addr_list, List.length elts)), mem'')
    
(*    (TySet (Tyset.join_list tyset_list, List.length elts), mem') *)
  (* List Comprehensions: PEP 202 http://www.python.org/dev/peps/pep-0202/ *)
  | ListComp (exp, comprehensions, loc) -> raise (NotImplemented "Dict")
(*    begin
      let (env', size) = acomps env comprehensions in
      let (ty, env'') = aexp env' exp in
      match size with
          INT l -> (TyList (repeat ty l), env'')
        | UNKNOWN -> (TyAList ty, env'')
    end *)
  (*[(x,y) for x in [1,2,3] for y in [4,5,6] if y > 4]
    ListComp(Tuple([Name('x', Load()), Name('y', Load())], Load()),
    [comprehension(Name('x', Store()), List([Num(1), Num(2), Num(3)], Load()), []),
    comprehension(Name('y', Store()), List([Num(4), Num(5), Num(6)], Load()), [Compare(Name('y', Load()), [Gt()], [Num(4)])])])
  *)   
  | SetComp (exp, comprehensions, loc) -> raise (NotImplemented "Dict")
(*    begin
      let (env', size) = acomps env comprehensions in
      let (ty, env'') = aexp env' exp in
      match size with
          INT l -> (TySet (ty, l), env'')
        | UNKNOWN -> (TyASet ty, env'')
    end *)
  (* Dictionary Comprehensions: PEP 274 http://www.python.org/dev/peps/pep-0274/ *)
  | DictComp (exp1, exp2, comprehensions, loc) -> raise (NotImplemented "Dict")
(*    let (env', size) = acomps env comprehensions in
    let (ty1, env'') = aexp env' exp1 in
    let (ty2, env''') = aexp env'' exp2 in
    (TyDict [(ty1, ty2)], env''') *)
  | GeneratorExp (exp, comprehensions, loc) -> raise (NotImplemented "Dict")
(*    begin
      let (env', size) = acomps env comprehensions in
      let (ty, env'') = aexp env' exp in
      match size with
          INT l -> (TyGenerator (ty, l), env'')
        | UNKNOWN -> (TyAGenerator ty, env'')
    end *)
  | Yield (exp_option, loc) -> raise (NotImplemented "Dict")
(*    let (ty, env') = aexp_op env exp_option in
    (TyAGenerator ty, env') *)
  (* TODO : Not implemented *)
  | Compare (exp, cmpops, exps, loc) -> raise (NotImplemented "Compare")
  (* TODO: Current limitations
     1. We do not support keywords, starargs, and kwargs.
     2. We do not consider the side effect of function call
        - change of passed arguments
        - change of global variables
     3. The only callable type is TyFunction. We do not support
        - class method, class initialization, etc.
  *)
  | Call (func, args, keywords, starargs, kwargs, loc) -> raise (NotImplemented "Call")
(*    begin
      let (func_ty, env') = aexp env func in
      let (arg_ty_list, env'') = aexp_list env args in
      match func_ty with
          TyFunction (param_ty_list, ret_ty) ->
            let ret_ty' =
              try
                List.fold_left2
                  (fun ret_ty ty1 ty2 ->
                    match ty1 with
                        TyVar(_, _, _, ty_con) ->
                          if order ty2 ty_con then 
                            subst ty1 ty2 ret_ty
                          else
                            raise (TypeError ((to_string ty_con) ^ " argument expected, got " ^ (to_string ty2), loc))
                      | _ -> raise (TypeError ("Should be TyVar" ^ (to_string ty1), loc)) 
                  )
                  ret_ty
                  param_ty_list
                  arg_ty_list
              with
                  Invalid_argument _ -> raise (TypeError ("# of formal parameters and # of actual arguments do not match.", loc))
            in
            (ret_ty', env'')
        | _ -> raise (TypeError ("Function call", loc))
    end *)
  (* "repr" expression `x` is not equivalent to the function call repr(x)
   * function call repr(x) is overriden by the function definition of repr,
   * but repr expression is not overriden by that.
   *
   * http://docs.python.org/reference/expressions.html#string-conversions
   *)
  | Repr (exp, loc) ->
    let (ty, mem') = aexp env mem exp in
    (Tyset.singleton TyAString, mem')
  | Int (n, loc) -> (Tyset.singleton (TyCInt n), mem)
  | Long (n, loc) -> (Tyset.singleton (TyCLong n), mem)
  | Float (f, loc) -> (Tyset.singleton (TyCFloat f), mem)
  | Complex (r, i, loc) -> (Tyset.singleton (TyCComplex (r, i)), mem)
  | Str (s, loc) -> (Tyset.singleton (TyCString s), mem)
  | UStr (s, loc) -> (Tyset.singleton (TyCUnicode s), mem)
  (* TODO: Need to extend to support non-class type.
   * If exp_context is "Store()", and the object is allowed to have new field,
   * then we need to add that field, instead of rasing type error. *)
  | Attribute (exp, id, exp_context, loc) -> raise (NotImplemented "Call")
(*    let (ty, env') = aexp env exp in
    begin
      match ty with
          TyClass attr_list ->
            begin
              try
                let (attr_id, attr_ty) = List.find (fun (id', _) -> id = id') attr_list in
                (attr_ty, env')
              with Not_found -> raise (TypeError ("The object has no " ^ id ^" field.", loc))
            end
        | TyString l ->
          begin
            match id with
                (* Ref: http://docs.python.org/library/string.html#string-functions *)
                (* TODO: Change TyInt -> TyVar(..., TyInt) *)
                "capitalize" -> (Type.make_prefn([], TyString l), env')
              | "expandtabs" -> (Type.make_prefn([TyInt], TyAString), env')
              | "find" -> (Type.make_prefn([TyAString; TyInt; TyInt], TyInt), env')
              | "rfind" -> (Type.make_prefn([TyAString; TyInt; TyInt], TyInt), env')
              | "index" -> (Type.make_prefn([TyAString; TyInt; TyInt], TyInt), env')
              | "rindex" -> (Type.make_prefn([TyAString; TyInt; TyInt], TyInt), env')
              | "count" -> (Type.make_prefn([TyAString; TyInt; TyInt], TyInt), env')
              | "lower" -> (Type.make_prefn([], TyString l), env')
              | "split" -> (Type.make_prefn([TyAString; TyInt], TyAList(TyAString)), env')
              | "rsplit" -> (Type.make_prefn([TyAString; TyInt], TyAList(TyAString)), env')
              | "splitfields" -> (Type.make_prefn([TyAString; TyInt], TyAList(TyAString)), env')
              | "join" -> (Type.make_prefn([TyAList(TyAString); TyAString], TyAList(TyAString)), env')
              | "joinfields" -> (Type.make_prefn([TyAList(TyAString); TyAString], TyAList(TyAString)), env')
              | "lstrip" -> (Type.make_prefn([TyAString], TyAString), env')
              | "rstrip" -> (Type.make_prefn([TyAString], TyAString), env')
              | "strip" -> (Type.make_prefn([TyAString], TyAString), env')
              | "swapcase" -> (Type.make_prefn([], TyString l), env')
              | "translate" -> (Type.make_prefn([TyString 256; TyAString], TyAString), env')
              | "upper" -> (Type.make_prefn([], TyString l), env')
              | "ljust" -> (Type.make_prefn([TyInt; TyString 1], TyAString), env')
              | "rjust" -> (Type.make_prefn([TyInt; TyString 1], TyAString), env')
              | "center" -> (Type.make_prefn([TyInt; TyString 1], TyAString), env')
              | "zfill" -> (Type.make_prefn([TyInt], TyAString), env')
              | "replace" -> (Type.make_prefn([TyAString; TyAString; TyInt], TyAString), env')
              | _ -> raise (TypeError ("'str' object has no attribute '" ^ id ^ "'", loc))
          end
        | TyUnion _|TyType _|TyGenerator _|TyFunction _|TyDict _|TyFrozenSet _
        | TySet _|TyAList _|TyList _|TyATuple _|TyTuple _|TyUnicode _
        | TyVar _|TyObject|TyByteArray _|TyAByteArray _|TyAUnicode|TyAString|TyComplex|TyFloat|TyBool
        | TyLong|TyInt|TyEllipsis|TyNotImplemented|TyNone|TyBot
        | TyAGenerator _|TyAFrozenSet _|TyASet _
        | TyMuSeq _|TyImmSeq _|TySeq _|TyCallable|TyFile|TyIntegral|TyNumber
          -> raise (TypeError ("Right hand side of attribute access should be object type.", loc))
    end *)
  | Subscript (exp, slice, exp_context, loc) -> raise (NotImplemented "Subscript")
(*    let (ty, env') = aexp env exp in
    let env'' = aslice env' slice in
    (ty, env'') *)
    
    (* TODO: handle cases where A) name is not in the env or B) addr is not in the mem *)
  | Name (id, ctx, loc) ->
    let addrset =
      try Env.find id env
      with Not_found -> raise (RuntimeError ("Variable "^ id ^ " is not defined at " ^ (Ast.string_of_loc loc)))
    in
    let tyset_set =
      BatPSet.map
        (fun addr ->
          try Mem.find addr mem
          with Not_found -> raise (RuntimeError ("Address "^ (Addr.string_of_addr addr) ^ " has no entry.")))
        addrset
    in
    let tyset = BatPSet.fold Tyset.join tyset_set Tyset.empty in
    (tyset, mem)
(*    begin
      try
        (Env.find id env, env)
      with Not_found ->
        begin
          match id with
              "abs" -> (Type.make_prefn([TyNumber], TyFloat), env)
            | "bin" -> (Type.make_prefn([TyUnion([TyInt;TyLong])], TyAString), env)
            | "chr" -> (Type.make_prefn([TyInt], TyString 1), env)
            | "cmp" -> (Type.make_prefn([TyObject; TyObject], TyBool), env)
            | "id" -> (Type.make_prefn([TyObject], TyInt), env)
            | "ord" -> (Type.make_prefn([TyUnion([TyString 1; TyUnicode 1])], TyInt), env)
            | _ -> raise (TypeError ("Name " ^ id ^ " is not in the environment", loc))
        end
    end *)
  | List (exps, exp_context, loc) ->
    let (tyset_list, mem') = aexp_list env mem exps in
    let addr_list = List.map get_addr exps in
    let mem'' = List.fold_left2 (fun mem addr tyset -> Mem.bind addr tyset mem) mem' addr_list tyset_list in
    let addrset_list = List.map Addrset.singleton addr_list in
    (Tyset.singleton (TyList addrset_list), mem'')
  | Tuple (exps, exp_context, loc) ->
    let (tyset_list, mem') = aexp_list env mem exps in
    let addr_list = List.map get_addr exps in
    let mem'' = List.fold_left2 (fun mem addr tyset -> Mem.bind addr tyset mem) mem' addr_list tyset_list in
    let addrset_list = List.map Addrset.singleton addr_list in
    (Tyset.singleton (TyTuple addrset_list), mem'')
and atarget_list env mem target_list tyset =
  List.fold_left (fun (env, mem) target -> atarget env mem target tyset)
    (env, mem)
    target_list
(* atarget env target ty :
 * Add (target : ty) into the environment env *)
and atarget env mem target tyset =
  match target with
      Name (id, exp_ctx, loc) ->
        let (env', addrset) = Env.get id env in
        let mem' = BatPSet.fold (fun addr mem -> Mem.bind addr tyset mem) addrset mem in
        (env', mem')
    | _ -> raise (NotImplemented "atarget")
(*    | List (exp_list, exp_ctx, loc) 
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
          | TyMuSeq _|TyImmSeq _|TySeq _|TyFile -> raise (TypeError ("Invalid numbers", loc))
          | TyClass _|TyUnion _|TyType _|TyFunction _
          | TyVar _|TyObject|TyComplex|TyFloat|TyBool|TyLong|TyInt|TyEllipsis|TyNotImplemented              
          | TyNone|TyBot|TyCallable|TyIntegral|TyNumber
            -> raise (TypeError ("Should be an iterable type but " ^ (Type.to_string ty), loc))
      end      
    | Attribute (exp, id, exp_ctx, loc) -> raise (NotImplemented "atarget/Attribute")
    | Subscript (exp, slice, exp_ctx, loc) -> raise (NotImplemented "atarget/Subscript")
    | _ -> raise (ShouldNotHappen "Target of assignment should be one of (name, list, tuple, attribute, and subscript).") *)
(* TODO: Currently, it only support positional arguments (no vararg, keyword, and default) *)
(* and aarguments env (args, vararg_op, kwarg_op, defaults) func_name loc = 
  let (env', _, arg_ty_list) = List.fold_left
    (fun (env, i, arg_ty_list) arg ->
      let arg_ty = TyVar (func_name, loc, i, TyObject) in
      (atarget env arg arg_ty , i+1, arg_ty::arg_ty_list))
    (env, 1, [])
    args
  in (env', List.rev arg_ty_list) *)
and astat env mem stat =
  match stat with 
    (* TODO *)    
    | FunctionDef (name, args, body, decorator_list, loc) -> raise (NotImplemented "FuncDef")
    (* TODO *)    
    | ClassDef (name, bases, body, decorator_list, loc) -> raise (NotImplemented "ClassDef")
    (* TODO *)    
    | Return (value_op, loc) -> raise (NotImplemented "Return")
    (* TODO *)    
    | Delete (targets, loc) -> raise (NotImplemented "Delete")
    | Assign (targets, value, loc) ->
      let (tyset, mem') = aexp env mem value in
      atarget_list env mem' targets tyset
    (* TODO *)    
    | AugAssign (target, op, value, loc) -> raise (NotImplemented "AugAssing")
    (* TODO:
       1. Support string conversion (See http://docs.python.org/reference/expressions.html 5.2.9)
       2. Extended form (ex: print to file) (See http://docs.python.org/reference/simple_stmts.html#grammar-token-print_stmt 6.6)
    *)    
    | Print (dest_op, values, nl, loc) ->
      let (tyset_list, mem') = aexp_list env mem values in
      if List.for_all (fun tyset -> BatPSet.for_all (fun ty -> Type.order ty TyAString) tyset) tyset_list then
        (env, mem')
      else
        raise (TypeError ("Non string type is printed", loc))
    (* TODO *)    
    | For (target, iter, body, orelse, loc) -> raise (NotImplemented "For")
    (* TODO *)    
    | While (test, body, orelse, loc) -> raise (NotImplemented "While")
    | If (test, body, orelse, loc) ->
      let (tyset, mem') = aexp env mem test in
      let (env_t, mem_t) = astat_list env mem' body in
      let (env_f, mem_f) = astat_list env mem' orelse in
      (Env.join env_t env_f, Mem.join mem_t mem_f)
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
      let (tyset, mem') = aexp env mem exp in
      (env, mem')
    | Pass loc -> raise (NotImplemented "Global")
    | Break loc -> raise (NotImplemented "Break")
    | Continue loc -> raise (NotImplemented "Continue")
and astat_list env mem stat_list =
  List.fold_left (fun (env, mem) stat -> astat env mem stat) (env, mem) stat_list
(*  match stat_list with
      [] -> (env, mem)
    | stat::stat_list ->
      let (env', mem') = astat env mem stat in
      match envop' with
          None -> (None, envlist')
        | Some env' ->  astat_list env' envlist' stat_list *)
and amodule env mem modu = match modu with
    Module stmts ->
      astat_list env mem stmts
  | Interactive stmts ->
    astat_list env mem stmts
  | Expression exp ->
    let (tyset, mem') = aexp env mem exp in
    let new_addr = Addr.get () in
    let mem'' = Mem.bind new_addr tyset mem' in
    let env' = Env.bind "!it" (Addrset.singleton new_addr) env in
    (env', mem'')

let analysis = amodule
