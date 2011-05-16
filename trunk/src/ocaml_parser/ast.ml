(*
 * Soonho Kong (soonhok@cs.cmu.edu)
 *
 * ast.ml : Python AST Type definition
 * Reference: http://docs.python.org/library/ast.html
 *)

type loc = int * int                 (* line number * column offset *)
type identifier = string
type modu = Module of stmt list
            | Interactive of stmt list
            | Expression of expr
and stmt = FunctionDef of string * arguments * stmt list * expr list * loc
           | ClassDef of string * expr list * stmt list * expr list * loc
           | Return of expr option * loc
           | Delete of expr list * loc
           | Assign of expr list * expr * loc
           | AugAssign of expr * operator * expr * loc
           | Print of expr option * expr list * bool * loc
           | For of expr * expr * stmt list * stmt list * loc
           | While of expr * stmt list * stmt list * loc
           | If of expr * stmt list * stmt list * loc
           | With of expr * expr option * stmt list * loc
           | Raise of expr option * expr option * expr option * loc
           | TryExcept of stmt list * excepthandler list * stmt list * loc
           | TryFinally of stmt list * stmt list * loc
           | Assert of expr * expr option * loc
           | Import of alias list * loc
           | ImportFrom of identifier option * alias list * int option * loc
           | Exec of expr * expr option * expr option * loc
           | Global of identifier list * loc
           | Expr of expr * loc
           | Pass of loc
           | Break of loc
           | Continue of loc
and expr = BoolOp of boolop * expr list * loc
           | BinOp of expr * operator * expr * loc
           | UnaryOp of unaryop * expr * loc
           | Lambda of arguments * expr * loc
           | IfExp of expr * expr * expr * loc
           | Dict of expr list * expr list * loc
           | Set of expr list * loc
           | ListComp of expr * comprehension list * loc
           | SetComp of expr * comprehension list * loc
           | DictComp of expr * expr * comprehension list * loc
           | GeneratorExp of expr * comprehension list * loc
           | Yield of expr option * loc
           | Compare of expr * cmpop list * expr list * loc
           | Call of expr * expr list * keyword list * expr option * expr option * loc
           | Repr of expr * loc
           | Int of int * loc          (* in Python side AST, there is only Num for int, long, float, and complex type *)
           | Long of int * loc 
           | Float of float * loc
           | Complex of float * float * loc                
(*         | Bool of bool * loc        (* in Python side AST, there is no bool, it's just Name('True') and Name('False') *) *)
           | Str of string * loc
           | UStr of string * loc      (* Unicode String, start with Str(u'....') *)
           | Attribute of expr * identifier * expr_context * loc
           | Subscript of expr * slice * expr_context * loc
           | Name of identifier * expr_context * loc
           | List of expr list * expr_context * loc
           | Tuple of expr list * expr_context * loc
and expr_context = Load | Store | Del | AugLoad | AugStore | Param
and slice = Ellipsis | Slice of expr option * expr option * expr option
            | ExtSlice of slice list
            | Index of expr
and boolop = And | Or 
and operator = Add | Sub | Mult | Div | Mod | Pow | LShift 
               | RShift | BitOr | BitXor | BitAnd | FloorDiv
and unaryop = Invert | Not | UAdd | USub
and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
and comprehension = expr * expr * expr list
and excepthandler = ExceptHandler of expr option * expr option * stmt list * loc
and arguments = expr list * identifier option * identifier option * expr list
and keyword = identifier * expr
and alias = identifier * identifier option
