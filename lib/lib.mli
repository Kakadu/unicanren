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
  | CondePar of goal list (** parallel disjunction *)
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

  (** Main function to run something *)
  val run : ('st, 'r) t -> 'st -> ('r, error) result
end

type 'a state = (st, 'a) StateMonad.t

(** A stream of answers of calculation *)
module Stream : sig
  type 'a t
  val mplus : 'a t -> 'a t -> 'a t
  val take : ?n:int -> 'a t -> 'a list
end

val eval
  :  ?trace_svars:bool
  -> ?trace_uni:bool
  -> ?trace_calls:bool
  -> ?domain_mgr:(Eio.Domain_manager.t)
  -> goal
  -> subst Stream.t state
