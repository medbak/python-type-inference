open Batteries
open Ast
open Type

exception NotImplemented
  
let astat env stat = match stat with
  | FunctionDef (name, args, body, decorator_list, loc) -> raise NotImplemented
  | ClassDef (name, bases, body, decorator_list, loc) -> raise NotImplemented
  | Return (value_op, loc) -> raise NotImplemented
  | Delete (targets, loc) -> raise NotImplemented
  | Assign (targets, value, loc) -> raise NotImplemented
  | AugAssign (target, op, value, loc) -> raise NotImplemented
  | Print (dest_op, values, nl, loc) -> raise NotImplemented
  | For (target, iter, body, orelse, loc) -> raise NotImplemented
  | While (test, body, orelse, loc) -> raise NotImplemented
  | If (test, body, orelse, loc) -> raise NotImplemented
  | With (context_expr, op_vars, body, loc) -> raise NotImplemented
  | Raise (type_op, inst_op, tback_op, loc) -> raise NotImplemented
  | TryExcept (body, handlers, orelse, loc) -> raise NotImplemented
  | TryFinally (body, finalbody, loc) -> raise NotImplemented
  | Assert (test, msg_op, loc) -> raise NotImplemented
  | Import (names, loc) -> raise NotImplemented
  | ImportFrom (module_op, names, level_op, loc) -> raise NotImplemented
  | Exec (body, globals_op, locals_op, loc) -> raise NotImplemented
  | Global (names, loc) -> raise NotImplemented
  | Expr (values, loc) -> raise NotImplemented
  | Pass loc -> raise NotImplemented
  | Break loc -> raise NotImplemented
  | Continue loc -> raise NotImplemented
    
let amodule env modu = match modu with
    Module stmts ->
      List.fold_left astat env stmts
  | Interactive stmts ->
    List.fold_left astat env stmts
  | Expression e -> env

let aexp env exp =
  let rec aexp_aux env exp = match exp with
      (* The Boolean operations "or" and "and" always return one of their operands 
       * For example: [1,2,3] and 7 and [4,5,6] = [4,5,6]
       * These operations are "short-circuit" operations. However, we assume that it's not short-circuit.
       *
       * TODO: Change this to short-circuit.
       *) 
    | BoolOp (op, values, loc) ->
      let (ty_list, env') =
        List.fold_left
          (fun (ty_list, env) exp ->
            let (ty, env') = aexp_aux env exp in
            (ty::ty_list, env'))
          ([], env)
          values
      in
      (TyUnion ty_list, env')
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
        | FloorDiv -> raise NotImplemented
      end
    | UnaryOp (op, expr, loc) ->
      begin match op with
        | Invert
        | Not
        | UAdd
        | USub -> raise NotImplemented
      end
    | Lambda (args, body, loc) -> raise NotImplemented
    | IfExp (bexpr, true_expr, false_expr, loc) ->
      let (_, env') = aexp_aux env bexpr in
      let (true_ty, true_env) = aexp_aux env' true_expr in
      let (false_ty, false_env) = aexp_aux env' false_expr in
      (Type.join [true_ty; false_ty], Env.join true_env false_env)

    | Dict (keys, values, loc) -> raise NotImplemented
    | Set (elts, loc) -> raise NotImplemented
    | ListComp (expr, comprehensions, loc) -> raise NotImplemented
    | SetComp (expr, comprehensions, loc) -> raise NotImplemented
    | DictComp (expr1, expr2, comprehensions, loc) -> raise NotImplemented
    | GeneratorExp (expr, comprehensions, loc) -> raise NotImplemented
    | Yield (expr_option, loc) -> raise NotImplemented 
    | Compare (expr, cmpops, exprs, loc) -> raise NotImplemented
    | Call (expr, exprs, keywords, expr1_option, expr2_option, loc) -> raise NotImplemented
    | Repr (expr, loc) -> raise NotImplemented
    | Int (_, loc) -> (TyInt, env)
    | Long (_, loc) -> (TyLong, env)
    | Float (_, loc) -> (TyFloat, env)
    | Complex (_, _, loc) -> (TyComplex, env)
    | Str (_, loc) -> (TyString, env)
    | UStr (string, loc) -> (TyUnicode, env)
    | Attribute (expr, identifier, expr_context, loc) -> raise NotImplemented
    | Subscript (expr, slice, expr_context, loc) -> raise NotImplemented
    | Name (id, ctx, loc) ->
      begin
        try
          (PMap.find id env, env)
        with Not_found -> raise (TypeError ("Name " ^ id ^ " is not in the environment"))
      end
    | List (exprs, expr_context, loc) -> raise NotImplemented
    | Tuple (exprs, expr_context, loc) -> raise NotImplemented
  in
  aexp_aux env exp

let analysis = amodule
