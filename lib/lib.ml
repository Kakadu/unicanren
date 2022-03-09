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
type subst = Value.t IntMap.t
type st = Value.t VarsMap.t * subst
type error = [ `UnboundSyntaxVariable of string ]

module State : sig
  type ('a, 'b) t

  val fail : error -> ('a, 'b) t
  val return : 'b -> ('a, 'b) t
  val ( >>= ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  val ( <*> ) : ('st, 'a -> 'b) t -> ('st, 'a) t -> ('st, 'b) t

  module Syntax : sig
    val ( let* ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
  end

  val read : ('a, 'a) t
  val lookup_var_syntax : string -> (st, Value.t option) t
  val lookup_var_logic : int -> (st, Value.t option) t
  val put : st -> (st, unit) t

  module List : sig
    val foldlm
      :  ('acc -> 'a -> ('st, 'acc) t)
      -> ('st, 'acc) t
      -> 'a list
      -> ('st, 'acc) t
  end
end = struct
  type ('st, 'b) t = 'st -> ('st * 'b, error) Result.t

  let fail e _st = Result.error e
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

  let put st0 _st = return () st0

  module List = struct
    let rec foldlm f acc = function
      | [] -> acc
      | x :: xs -> foldlm f (acc >>= fun acc -> f acc x) xs
    ;;
  end
end

type 'a state = (st, 'a) State.t

module Stream = struct
  type 'a t =
    | Nil
    | Cons of 'a * 'a t Lazy.t
    | Thunk of 'a t Lazy.t

  let nil = Nil
  let return x = Cons (x, lazy Nil)
  let cons x xs = Cons (x, xs)
  let from_fun zz = Thunk (lazy (zz ()))

  let force = function
    | Thunk (lazy zz) -> zz
    | xs -> xs
  ;;

  let rec mplus : 'a. 'a t -> 'a t -> 'a t =
   fun x y ->
    match x, y with
    | Nil, _ -> y
    | Thunk l, r -> mplus r (Lazy.force l)
    | Cons (x, l), r -> Cons (x, lazy (mplus r (Lazy.force l)))
 ;;

  let rec bind s f =
    match s with
    | Nil -> Nil
    | Cons (x, s) -> mplus (f x) (from_fun (fun () -> bind (Lazy.force s) f))
    | Thunk zz -> from_fun (fun () -> bind (Lazy.force zz) f)
  ;;

  (* TODO: I think wqe need monad transformer *)
  let bindm : 'a 'b. 'a t state -> ('a -> 'b t) -> 'b t = fun s f -> assert false
end

let eval =
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
  in
  let rec eval root : (st, subst Stream.t) State.t =
    match root with
    | Unify (l, r) ->
      let* l = eval_term l in
      let* r = eval_term r in
      let* svars, lvars = read in
      (match unify lvars l r with
      | None -> assert false
      | Some subst2 ->
        let* () = put (svars, subst2) in
        return (Stream.return subst2))
    | Conde [] -> assert false
    | Conde (x :: xs) ->
      List.foldlm (fun acc x -> return (Stream.mplus acc) <*> eval x) (eval x) xs
    | Conj [] -> assert false
    | Conj [ x ] -> eval x
    | Conj (x :: xs) ->
      let* str1 = eval x in
      (* return (Stream.bind) *)
      let* svars, lvars = read in
      (* let _ = Stream.bind str1 (fun s -> ) in *)
      assert false
    | Fresh _ | Call (_, _) -> failwith "Not implemented"
  and eval_term = function
    | Nil -> return Value.Nil
    | Symbol s -> return (Value.symbol s)
    | Cons (l, r) -> return Value.cons <*> eval_term l <*> eval_term r
    | Var s ->
      let* next = lookup_var_syntax s in
      (match next with
      | None -> fail (`UnboundSyntaxVariable s)
      | Some t2 -> return t2)
  in
  eval
;;
