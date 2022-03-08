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
  type t =
    | Var of int
    | Symbol of string
    | Cons of t * t
    | Nil
end

module Subst = struct
  include Map.Make (Int)
end

let unify acc _ _ = None

module State : sig
  type ('a, 'b) t

  val return : 'b -> ('a, 'b) t
  val bind : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
end = struct
  type ('a, 'b) t = 'a -> 'a * 'b

  let return x st = st, x

  let bind x f st =
    let st, r = x st in
    f r st
  ;;

  let run st m = snd (m st)
end

let rec eval env env_rel = function
  | Unify (l, r) ->
    (match unify Subst.empty l r with
    | _ -> assert false)
  | _ -> assert false
;;
