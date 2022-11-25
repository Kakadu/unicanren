(** An implementation of miniKanren as a library *)

(** Syntactic terms. Non-typed *)
module Term : sig
  type t =
    | Var of string (** Logic variables with name *)
    | Symbol of string (** Scheme's symbol *)
    | Cons of t * t (* List's cons cell *)
    | Nil (** Empty list *)

  (** {3} Smart constructors *)

  val cons : t -> t -> t
  val symbol : string -> t
  val var : string -> t

  (** {3} Printing *)
  val pp : Format.formatter -> t -> unit
end

(** A goal -- something that can be calculated relationally *)
type goal =
  | Unify of Term.t * Term.t (** Unification *)
  | Conj of goal list (** Conjunction *)
  | Conde of goal list (** (interleaved) Disjunction *)
  | CondeOf2 of goal * goal
  | Fresh of string * goal (** Creating of fresh (existential) variables *)
  | Call of string * Term.t list (** Call to another relation with arguments *)
  | TraceSVars of string list (** Tracing of some variables. Only for debugging *)

(** A combinator to create many fresh variables instantly *)
val fresh : string list -> goal -> goal

val pp_goal : Format.formatter -> goal -> unit

(** Substitution stores binding from logic variables to logic values *)
module Subst : sig
  type key = int
  type 'a t = 'a Map.Make(Int).t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val find : key -> 'a t -> 'a
  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val find_last : (key -> bool) -> 'a t -> key * 'a
  val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_rev_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
  val of_seq : (key * 'a) Seq.t -> 'a t
  val find : key -> 'a t -> 'a
  val bindings : 'a t -> (key * 'a) list
  val min_binding : 'a t -> key * 'a
  val min_binding_opt : 'a t -> (key * 'a) option
  val max_binding : 'a t -> key * 'a
  val max_binding_opt : 'a t -> (key * 'a) option
  val choose : 'a t -> key * 'a
  val choose_opt : 'a t -> (key * 'a) option
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val find_opt : key -> 'a t -> 'a option
  val find_first : (key -> bool) -> 'a t -> key * 'a
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

(** Runtime logic values *)
module Value : sig
  type t =
    | Var of int
    | Symbol of string
    | Cons of t * t
    | Nil

  (** {3} Smart constructors *)

  val var : int -> t
  val symbol : string -> t
  val cons : t -> t -> t
  val nil : t
  val _0 : t
  val _1 : t

  (** Walking via substitution *)

  (** Call [walk subst v] replace all occurences of variables from [dom subst] by [subst(v)].
      The corner case is when variable [v] is going to be replaced by something that contains this [v].
      In this situation an ``Occurs check'' should happen and give an error, but it's currently not implemented
      *)
  val walk : t Subst.t -> t -> t

  (** {3} Printing *)

  val pp : Format.formatter -> t -> unit
  val ppw : t Subst.t -> Format.formatter -> t -> unit
end

(** A substitution. We will have only substitutions of logic variables (i.e. it's indexes) to Value.t *)
type subst = Value.t Subst.t

(** Unification [unify subst a b] unifies [a] and [b] or gives an error. Commutative. *)
val unify : subst -> Value.t -> Value.t -> subst option

module State : sig
  type t

  val empty : t
  val add_var : string -> Value.t -> t -> t
  val ( --> ) : string -> Value.t -> t -> t
  val add_var_logic : int -> Value.t -> t -> t
  val ( ->> ) : int -> Value.t -> t -> t
  val add_rel : string -> string list -> goal -> t -> t
  val add_rel1 : string * string list * goal -> t -> t
end

type st = State.t

val makerev : ('a -> 'a) -> int -> 'a -> 'a

val funct : Term.t -> Term.t

type error =
  [ `BadArity
  | `UnboundRelation of string
  | `UnboundSyntaxVariable of string
  ]

val pp_error
  :  Format.formatter
  -> [< `BadArity | `UnboundRelation of string | `UnboundSyntaxVariable of string ]
  -> unit

val failwiths : ('a, Format.formatter, unit, 'b) format4 -> 'a

(** A monad module, to implement a miniKanren interpreter *)
module StateMonad : sig
  type ('a, 'b) t

  val fail : error -> ('a, 'b) t
  val return : 'b -> ('a, 'b) t
  val ( >>= ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  val ( <*> ) : ('st, 'a -> 'b) t -> ('st, 'a) t -> ('st, 'b) t
  val ( >>| ) : ('st, 'a) t -> ('a -> 'b) -> ('st, 'b) t

  module Syntax : sig
    val ( let* ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  end

  val run : ('st, 'r) t -> 'st -> ('r, error) result
  val read : ('a, 'a) t

  (* val lookup_var_syntax : tag -> (st, Value.t option) t *)
  val lookup_var_logic : int -> (st, Value.t option) t
  val put : st -> (st, unit) t

  (* val put_svars : Value.t VarsMap.t -> (st, unit) t *)
  val put_lvars : subst -> (st, unit) t

  module List : sig
    val mapm : ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t

    val foldlm
      :  ('acc -> 'a -> ('st, 'acc) t)
      -> ('st, 'acc) t
      -> 'a list
      -> ('st, 'acc) t

    val foldl2m
      :  on_fail:('st, 'acc) t
      -> ('acc -> 'a -> 'b -> ('st, 'acc) t)
      -> ('st, 'acc) t
      -> 'a list
      -> 'b list
      -> ('st, 'acc) t
  end
end

type 'a state = (st, 'a) StateMonad.t

(** A stream of answers of calculation *)
module Stream : sig
  type 'a t

  (* type 'a t = Nil | Cons of 'a * 'a t lazy_t *)
  (* val pp : formatter -> 'a t -> unit *)
  val nil : 'a t
  val return : 'a -> 'a t
  val mplus : 'a t -> 'a t -> 'a t
  val from_funm : (unit -> 'a t state) -> 'a t state
  val bindm : 'a t state -> ('a -> 'b t state) -> 'b t state
  val take : ?n:int -> 'a t -> 'a list
  val take : ?n:int -> 'a t -> 'a list
end

val eval
  :  ?trace_svars:bool
  -> ?trace_uni:bool
  -> ?trace_calls:bool
  -> goal
  -> subst Stream.t state
