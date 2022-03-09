type term =
  | Var of string
  | Symbol of string
  | Cons of term * term
  | Nil

type goal =
  | Unify of term * term
  | Conj of goal list
  | Conde of goal list
  | Fresh of string * goal
  | Call of string * term list

module Value = struct
  (* open GT *)

  type t =
    | Var of int
    | Symbol of string
    | Cons of t * t
    | Nil

  let var x = Var x
  let symbol s = Symbol s
  let cons x y = Cons (x, y)
end

module Subst = struct
  include Map.Make (Int)
end

let unify acc _ _ = None

module IntMap = Map.Make (Int)

module VarsMap = struct
  include Map.Make (String)
end

(* State is syntax variables + subject variables
  TODO: map for relations
*)
type st = Value.t VarsMap.t * Value.t IntMap.t
type error = [ `UnboundVariable of string ]

module State : sig
  type ('a, 'b) t

  val return : 'b -> ('a, 'b) t
  val ( >>= ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  val ( <*> ) : ('st, 'a -> 'b) t -> ('st, 'a) t -> ('st, 'b) t

  module Syntax : sig
    val ( let* ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  end

  val read : ('a, 'a) t
  val lookup_var_syntax : string -> (st, Value.t option) t
  val lookup_var_logic : int -> (st, Value.t option) t
end = struct
  type ('st, 'b) t = 'st -> ('st * 'b, error) Result.t

  let return x st = Result.ok (st, x)

  let bind x f st =
    match x st with
    | Result.Ok (st, r) -> f r st
    | Error e -> Error e
  ;;

  let ( <*> ) f x st =
    Result.bind (f st) (fun (st, f) ->
        Result.bind (x st) (fun (st, x) -> Result.Ok (st, f x)))
  ;;

  let ( >>= ) = bind
  let run st m = snd (m st)

  module Syntax = struct
    let ( let* ) = bind
  end

  let read : 'st. ('st, 'st) t = fun st -> Result.Ok (st, st)

  let lookup_var_syntax : string -> (st, Value.t option) t =
   fun name ->
    let open Syntax in
    let* map, _ = read in
    return (VarsMap.find_opt name map)
 ;;

  let lookup_var_logic : int -> (st, Value.t option) t =
   fun name ->
    let open Syntax in
    let* _, map = read in
    return (IntMap.find_opt name map)
 ;;
end

let rec eval =
  let open State in
  let open State.Syntax in
  let rec walk : Value.t -> (st, Value.t) t = function
    | Value.Var v ->
      let* next = lookup_var_logic v in
      (match next with
      | None -> return (Value.var v)
      | Some t2 -> walk t2)
    | Symbol s -> return (Value.symbol s)
    | Cons (l, r) -> return Value.cons <*> walk l <*> walk r
    | Nil -> return Value.Nil
    (* | _ -> term *)
  in
  let eval_goal root =
    let* root = root in
    match root with
    | Unify (l, r) ->
      (match unify Subst.empty l r with
      | _ -> assert false)
    | _ -> assert false
  and eval_term t =
    let* t = t in
    t
  in
  eval_goal
;;
