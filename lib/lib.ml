open Format

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

module Subst = struct
  include Map.Make (Int)
end

module Value = struct
  type t =
    | Var of int
    | Symbol of string
    | Cons of t * t
    | Nil

  let var x = Var x
  let symbol s = Symbol s
  let cons x y = Cons (x, y)

  let rec pp ppf = function
    | Var n -> Format.fprintf ppf "_.%d" n
    | Symbol s -> Format.fprintf ppf "'%s" s
    | Cons (l, r) -> Format.fprintf ppf "(cons (%a) (%a))" pp l pp r
    | Nil -> Format.fprintf ppf "nil"
  ;;

  let rec walk subst : t -> t = function
    | Var v ->
      (match Subst.find v subst with
      | exception Not_found -> Var v
      | t2 -> walk subst t2)
    | Symbol s -> Symbol s
    | Cons (l, r) -> cons (walk subst l) (walk subst r)
    | Nil -> Nil
  ;;
end

let pp_subst ppf s = Subst.iter (fun n -> Format.fprintf ppf "%d -> %a\n%!" n Value.pp) s

let rec unify acc x y =
  (* printf "Calling unify of `%a` and `%a`\n%!" Value.pp x Value.pp y; *)
  match Value.walk acc x, Value.walk acc y with
  | Value.Var n, Value.Var m when n = m -> Some acc
  | Var _, Var _ -> None
  | Symbol m, Symbol n when n = m -> Some acc
  | Symbol _, Symbol _ -> None
  | Nil, Nil -> Some acc
  | rhs, Var n | Var n, rhs -> Some (Subst.add n rhs acc)
  | Cons (l1, r1), Cons (l2, r2) ->
    let open Base.Option in
    unify acc l1 l2 >>= fun acc -> unify acc r1 r2
  | Symbol _, Cons (_, _)
  | Cons (_, _), Symbol _
  | Nil, Cons (_, _)
  | Cons (_, _), Nil
  | Symbol _, Nil
  | Nil, Symbol _ -> None
;;

module VarsMap = struct
  include Map.Make (String)
end

(* State is syntax variables + subject variables
  TODO: map for relations
*)
type subst = Value.t Subst.t

module State = struct
  type t =
    { svars : Value.t VarsMap.t
    ; lvars : subst
    ; rels : (string * string list * goal) VarsMap.t
    }

  let empty = { svars = VarsMap.empty; lvars = Subst.empty; rels = VarsMap.empty }
  let add_var name t st = { st with svars = VarsMap.add name t st.svars }
  let add_var_logic idx t st = { st with lvars = Subst.add idx t st.lvars }
end

type st = State.t

type error =
  [ `UnboundSyntaxVariable of string
  | `BadArity
  ]

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

  val run : ('st, 'r) t -> 'st -> ('r, error) Result.t
  val read : ('a, 'a) t
  val lookup_var_syntax : string -> (st, Value.t option) t
  val lookup_var_logic : int -> (st, Value.t option) t
  val put : st -> (st, unit) t
  val put_svars : Value.t VarsMap.t -> (st, unit) t
  val put_lvars : subst -> (st, unit) t

  module List : sig
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
end = struct
  type ('st, 'b) t = 'st -> ('st * 'b, error) Result.t

  let fail e _st = Result.error e
  let return x st = Result.ok (st, x)
  let ( >>| ) x f st = Result.map (fun (st, x) -> st, f x) (x st)

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
  let run : (_, _) t -> _ -> _ = fun m st -> Result.map snd (m st)

  module Syntax = struct
    let ( let* ) = bind
  end

  let read : 'st. ('st, 'st) t = fun st -> Result.Ok (st, st)

  let lookup_var_syntax : string -> (st, Value.t option) t =
   fun name ->
    let open Syntax in
    let* { State.svars; _ } = read in
    return (VarsMap.find_opt name svars)
 ;;

  let lookup_var_logic : int -> (st, Value.t option) t =
   fun name ->
    let open Syntax in
    let* { State.lvars; _ } = read in
    return (Subst.find_opt name lvars)
 ;;

  let put st0 _st = return () st0

  let put_svars svars =
    let open Syntax in
    let* st = read in
    put { st with State.svars }
  ;;

  let put_lvars map =
    let open Syntax in
    let* st = read in
    put { st with State.lvars = map }
  ;;

  module List = struct
    let rec foldlm f acc = function
      | [] -> acc
      | x :: xs -> foldlm f (acc >>= fun acc -> f acc x) xs
    ;;

    let rec foldl2m :
              'st 'b 'acc.
              on_fail:('st, 'acc) t
              -> ('acc -> 'a -> 'b -> ('st, 'acc) t)
              -> ('st, 'acc) t
              -> 'a list
              -> 'b list
              -> ('st, 'acc) t
      =
     fun ~on_fail f acc xs ys ->
      let rec helper acc = function
        | [], [] -> acc
        | x :: xs, y :: ys -> helper (acc >>= fun acc -> f acc x y) (xs, ys)
        | _ -> on_fail
      in
      helper acc (xs, ys)
   ;;
  end
end

type 'a state = (st, 'a) StateMonad.t

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

  let from_funm : (unit -> 'a t state) -> 'a t state =
   fun f ->
    let open StateMonad in
    (* Bullshit ? *)
    return () >>= fun () -> f ()
 ;;

  (* TODO: I think we need monad transformer *)
  let rec bindm : 'a 'b. 'a t state -> ('a -> 'b t state) -> 'b t state =
   fun s f ->
    let open StateMonad in
    let open StateMonad.Syntax in
    let* init = s in
    match init with
    | Nil -> return Nil
    | Cons (x, s) ->
      let* l = f x in
      (* Bullshit ? *)
      let* r = from_funm (fun () -> bindm (return @@ Lazy.force s) f) in
      return @@ mplus l r
    | Thunk zz ->
      (* Bullshit ? *)
      from_funm (fun () -> bindm (return (Lazy.force zz)) f)
 ;;

  let take ?(n = -1) =
    let rec helper n = function
      | Nil -> []
      | _ when n = 0 -> []
      | Cons (s, (lazy tl)) -> s :: helper (n - 1) tl
      | Thunk (lazy zz) -> helper n zz
    in
    helper n
  ;;
end

let next_logic_var =
  let last = ref 0 in
  fun () ->
    incr last;
    !last
;;

let eval =
  let open State in
  let open StateMonad in
  let open StateMonad.Syntax in
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
  let rec eval root : (st, subst Stream.t) StateMonad.t =
    match root with
    | Unify (l, r) ->
      let* l = eval_term l in
      let* r = eval_term r in
      let* ({ State.lvars } as st) = read in
      (match unify lvars l r with
      | None -> return Stream.nil
      | Some subst2 ->
        let* () = put { st with lvars = subst2 } in
        return (Stream.return subst2))
    | Conde [] -> assert false
    | Conde (x :: xs) ->
      List.foldlm (fun acc x -> return (Stream.mplus acc) <*> eval x) (eval x) xs
    | Conj [] -> assert false
    | Conj [ x ] -> eval x
    | Conj (x :: xs) ->
      let* st = read in
      Stream.bindm (eval x) (fun subst ->
          put { st with lvars = subst } >>= fun () -> eval (Conj xs))
    | Fresh (name, rhs) ->
      let* st = read in
      let term = Value.var (next_logic_var ()) in
      let svars = VarsMap.add name term st.State.svars in
      let* () = put { st with svars } in
      eval rhs
    | Call (fname, args) ->
      (* failwith "Not implemented" *)
      let* st = read in
      (match VarsMap.find fname st.rels with
      | exception Not_found -> assert false
      | _, formal_args, body ->
        assert (Stdlib.List.length formal_args = Stdlib.List.length args);
        let* svars =
          List.foldl2m
            (fun acc name t -> eval_term t >>= fun t -> return (VarsMap.add name t acc))
            (return st.svars)
            formal_args
            args
            ~on_fail:(fail `BadArity)
        in
        let* () = put { st with svars } in
        eval body)
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

let%test _ =
  StateMonad.run (eval (Unify (Var "x", Var "y"))) State.empty
  = Result.error (`UnboundSyntaxVariable "x")
;;

let%expect_test _ =
  let goal = Unify (Symbol "x", Symbol "y") in
  StateMonad.run (eval goal) State.empty
  |> Result.get_ok
  |> Stream.take ~n:(-1)
  |> List.iter (fun _st -> Format.printf "AAA\n%!");
  [%expect {|  |}]
;;

let%expect_test _ =
  let goal = Unify (Var "x", Symbol "y") in
  StateMonad.run
    (eval goal)
    State.(add_var_logic 10 (Symbol "y") @@ add_var "x" (Symbol "y") empty)
  |> Result.get_ok
  |> Stream.take ~n:(-1)
  |> List.iter (fun st -> Format.printf "%a\n%!" pp_subst st);
  [%expect {| 10 -> 'y |}]
;;
(* let%test_unit "rev" =
  let open Base in
  [%test_eq: int list] (List.rev [ 3; 2; 1 ]) [ 3; 2; 1 ]
;; *)
