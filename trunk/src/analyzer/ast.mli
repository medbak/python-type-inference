(** Python AST
    @see <http://docs.python.org/library/ast.html> Python AST
    @author Soonho Kong (soonhok at cs.cmu.edu)
*)

(** {6 Types} *)

(** location : line number * column number *)
type loc = int * int
    
type identifier = string

(** Module Type. See also
    {{:http://docs.python.org/reference/toplevel_components.html}Top Level
    Components} *)
type modu =
    Module of stmt list
  | Interactive of stmt list
  | Expression of expr

(** Statement Type. See also
    {{:http://docs.python.org/reference/simple_stmts.html}Simple
    Statement} and
    {{:http://docs.python.org/reference/compound_stmts.html}Compound
    Statement}. *)
and stmt =
    FunctionDef of string * arguments * stmt list * expr list * loc (** (function name, arguments list, body, decorator list) *)
  | ClassDef of string * expr list * stmt list * expr list * loc (** (class name, base classes, body, docorator list) *)
  | Return of expr option * loc      (** (expr? value) *)
  | Delete of expr list * loc        (** (expr* targets) *)
  | Assign of expr list * expr * loc (** (expr* targets, expr value) *)
  | AugAssign of expr * operator * expr * loc (** (expr target, operator op, expr value) *)
  | Print of expr option * expr list * bool * loc (** (expr? dest, expr* values, bool nl) *)
  | For of expr * expr * stmt list * stmt list * loc (** (expr target, expr iter, stmt* body, stmt* orelse) *)
  | While of expr * stmt list * stmt list * loc (** (expr test, stmt* body, stmt* orelse)  *)
  | If of expr * stmt list * stmt list * loc (** (expr test, stmt* body, stmt* orelse) *)
  | With of expr * expr option * stmt list * loc (** (expr context_expr, expr? optional_vars, stmt* body)  *)
  | Raise of expr option * expr option * expr option * loc (** (expr? type, expr? inst, expr? tback)  *)
  | TryExcept of stmt list * excepthandler list * stmt list * loc (** (stmt* body, excepthandler* handlers, stmt* orelse) *) 
  | TryFinally of stmt list * stmt list * loc (** (stmt* body, stmt* finalbody) *)
  | Assert of expr * expr option * loc (** (expr test, expr? msg) *)
  | Import of alias list * loc 
  | ImportFrom of identifier option * alias list * int option * loc (** (identifier? module, alias* names, int? level) *)
  | Exec of expr * expr option * expr option * loc (** (expr body, expr? globals, expr? locals) *)
  | Global of identifier list * loc 
  | Expr of expr * loc 
  | Pass of loc 
  | Break of loc 
  | Continue of loc

(** Expression Type. See also
    {{:http://docs.python.org/reference/expressions.html}Expressions}.*)
and expr =
    BoolOp of boolop * expr list * loc 
  | BinOp of expr * operator * expr * loc 
  | UnaryOp of unaryop * expr * loc 
  | Lambda of arguments * expr * loc 
  | IfExp of expr * expr * expr * loc (** (expr test, expr body, expr orelse) *)
  | Dict of expr list * expr list * loc 
  | Set of expr list * loc
  | ListComp of expr * comprehension list * loc (** (expr elt, comprehension* generators) *)
  | SetComp of expr * comprehension list * loc (** (expr elt, comprehension* generators) *)
  | DictComp of expr * expr * comprehension list * loc (** (expr key, expr value, comprehension* generators) *)
  | GeneratorExp of expr * comprehension list * loc (** (expr elt, comprehension* generators) *)
  | Yield of expr option * loc 
  | Compare of expr * cmpop list * expr list * loc (** (expr left, cmpop* ops, expr* comparators) *)
  | Call of expr * expr list * keyword list * expr option * expr option * loc (** (expr func, expr* args, keyword* keywords,
			 expr? starargs, expr? kwargs) *)
  | Repr of expr * loc 
  | Int of int * loc 
  | Long of int * loc 
  | Float of float * loc 
  | Complex of float * float * loc 
  | Str of string * loc 
  | UStr of Batteries.UTF8.t * loc 
  | Attribute of expr * identifier * expr_context * loc  (**  (expr value, identifier attr, expr_context ctx) *)
  | Subscript of expr * slice * expr_context * loc (** (expr value, slice slice, expr_context ctx) *)
  | Name of identifier * expr_context * loc
  | List of expr list * expr_context * loc 
  | Tuple of expr list * expr_context * loc 
and expr_context = Load | Store | Del | AugLoad | AugStore | Param
and slice =
    Ellipsis
  | Slice of expr option * expr option * expr option (** (expr? lower, expr? upper, expr? step) *)
  | ExtSlice of slice list
  | Index of expr
and boolop = And | Or
and operator =
    Add
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
  | FloorDiv
and unaryop = Invert | Not | UAdd | USub
and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn

(** (expr target, expr iter, expr* ifs) *)
and comprehension = expr * expr * expr list 
    
(** (expr? type, expr? name, stmt* body) *)
and excepthandler =
    ExceptHandler of expr option * expr option * stmt list * loc  
        
(** (expr* args, identifier? vararg, identifier? kwarg, expr*
    defaults) *)
and arguments = expr list * identifier option * identifier option * expr list  

and keyword = identifier * expr
    
(** import name with optional 'as' alias *)
and alias = identifier * identifier option 

(** {6 Values} *)
    
(** [string_of_loc (l, c)] returns "(l, c)" *)
val string_of_loc : loc -> string

(** [stmt2loc stmt] returns the associated location information from
    the statement [stmt]. *)
val stmt2loc : stmt -> loc

(** [exp2loc stmt] returns the associated location information from
    the expression [exp]. *)
val exp2loc : expr -> loc

