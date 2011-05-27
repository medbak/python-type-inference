(** Python Types
    @see <http://docs.python.org/reference/datamodel.html#the-standard-type-hierarchy> Python Standard Type Hierarchy
    
    @author Soonho Kong (soonhok at cs.cmu.edu)
*)
    
(** {6 Types} *)
    
type ty =
    TyBot                                    (** Bottom *)
  | TyNone
  | TyVar of string * Ast.loc * int * ty
  (** Type variable is introduce at the function definition. TyVar
      (function name, location, position, constraint) *)
  | TyNotImplemented
  | TyEllipsis
  | TyNumber        (** Number *)
  | TyIntegral      (** Integral *)
  | TyInt           (** Int *)
  | TyLong          (** Long *)
  | TyBool          (** Bool *)
  | TyFloat         (** Float *)
  | TyComplex       (** Complex *)
  | TyString of int (** Concrete string type which maintains the length of string *)
  | TyAString       (** Abstract string type *)
  | TyUnicode of int (** Concrete unicode type which maintains the length *)
  | TyAUnicode       (** Abstract unicode type *)
  | TyTuple of ty list (** Concrete tuple type, which maintains the type of each element *)
  | TyATuple of ty     (** Abstract tuple type, every element has ty type *)
  | TyList of ty list  (** Concrete list type, which maintains the type of each element *)
  | TyAList of ty      (** Abstract list type *)
  | TyByteArray of int  (** Concrete bytearray type which maintains the length *)
  | TyAByteArray        (** Abstract bytearray type *)
  | TySet of ty * int   (** Concrete set type which maintains the size *)
  | TyASet of ty              (** Abstract set type *)
  | TyFrozenSet of ty * int   (** Concrete frozenset type which maintains the size *)
  | TyAFrozenSet of ty        (** Abstract frozenset type *)
  | TyDict of (ty * ty) list  (** Dict type, maintains (key_type, value_type) list *)
  | TyFunction of (ty list * ty) (** Function type, (argument_type list, return_type) *)
  | TyGenerator of ty * int  (** Concrete generator type which maintains a length of its contents *)
  | TyAGenerator of ty       (** Abstract generator type *)
  | TyObject                 (** Object type, represents top *)
  | TyType of ty             (** Type type *)
  | TyUnion of ty list       (** Union type *)
  | TySeq                    (** Sequence type *)
  | TyImmSeq                 (** Immutable Sequence type *)
  | TyMuSeq                  (** Mutable Sequence type *)
  | TyFile                   (** File type *)
  | TyCallable               (** Callable type *)
  | TyClass of (string * ty) list (** Class type. Map from attribute/method names -> their types *)

(** {6 Exceptions} *)
  
exception RuntimeError of string
exception TypeError of string * Ast.loc
exception TypeWarning of string * Ast.loc
      
(** {6 Values} *)

(** [normalize tylist] returns tylist where duplicates are removed. *)
val normalize : 'a list -> 'a list

(** Order operation. *)  
val order : ty -> ty -> bool

(** [order_set tylist1 tylist2] is true iff
    ∀ ty1 ∈ tylist1. ∃ ty2 ∈ tylist2. [order ty1 ty2]. *)
val order_set : ty list -> ty list -> bool

(** [order_list \[ty_11, ty_12, ..., ty_1n\] \[ty_21, ty_22, ...,
    ty_2n\] ] is true iff ([order ty_11 ty_21]) & ([order ty_12
    ty_22]) & ... & ([order ty_1n ty_2n]). *)
val order_list : ty list -> ty list -> bool

(** [subst ty1 ty2] substitutes [ty1] in [ty] with [ty2]. *)
val subst : ty -> ty -> ty -> ty
  
(** Join operation. *)
val join : ty list -> ty

val to_string : ty -> string

  
