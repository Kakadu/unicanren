open Unicanren.Lib
open Stdlib

(* open Printf *)
open Domainslib
open Format

(* let res =
  StateMonad.run
    (eval
       (fresh
          [ "x" ]
          (Conde [ Unify (Var "x", Symbol "u"); Unify (Var "x", Symbol "v") ])))
    State.empty
;; *)

let caro a l = Fresh ("d", Unify (Cons (a, Var "d"), l))

(* let res1 =
  StateMonad.run
    (eval
       (fresh
          [ "a" ]
          (caro
             (Var "a")
             (Cons (Symbol "gu", Cons (Symbol "hu", Cons (Symbol "su", Symbol "du")))))))
    State.empty
;; *)

(* let prin1 =
  res1
  |> Result.get_ok
  |> Stream.take ~n:1
  |> List.iter (fun _st -> Format.printf "%a" (Value.ppw _st) (Subst.find 12 _st))
;; *)

(* let pool = Task.setup_pool ~num_domains:3 () *)

(* let paraltest =
  Task.run pool (fun () ->
    StateMonad.run
      (eval
         (fresh
            [ "x" ]
            (let a = Task.async pool (fun _ -> Unify (Var "x", Symbol "u")) in
             let b = Task.async pool (fun _ -> Unify (Var "x", Symbol "v")) in
             Conj [ Task.await pool a; Task.await pool b ])))
      State.empty)
;; *)

let caro a l = Fresh ("d", Unify (Cons (a, Var "d"), l))
let cdro p d = Fresh ("a", Unify (Cons (Var "a", d), p))
let conso a d p = Conde [ caro p a; cdro p d ]
(* let res1 =
  StateMonad.run
    (eval
       (fresh
          [ "a" ]
          (caro
             (Var "a")
             (Cons (Symbol "gu", Cons (Symbol "hu", Cons (Symbol "su", Symbol "du")))))))
    State.empty
;; *)

(* let prin1 =
  res1
  |> Result.get_ok
  |> Stream.take ~n:1
  |> List.iter (fun _st -> Format.printf "%a" (Value.ppw _st) (Subst.find 12 _st))
;; *)

(* let pool = Task.setup_pool ~num_domains:3 () *)

let summ l r =
  let open StateMonad in
  let open StateMonad.Syntax in
  let* st = read in
  l
  >>= fun acc ->
  (fun acc y ->
    let* () = put st in
    return (Stream.mplus acc) <*> y)
    acc
    r
;;

(* let paraltest =
  Task.run pool (fun () ->
    let a : goal Task.promise =
      Task.async pool (fun _ -> fresh [ "x" ] (Unify (Var "x", Symbol "u")))
    in
    let b : goal Task.promise =
      Task.async pool (fun _ -> fresh [ "x" ] (Unify (Var "x", Symbol "v")))
    in
    StateMonad.run (eval (Conj[Task.await pool a; Task.await pool b])) State.empty)  (*summ (Task.await pool a*)
;; *)

(* let res2 =
  Task.run pool (fun () ->
    let a =
      Task.async pool (fun _ -> eval (fresh [ "x" ] (Unify (Var "x", Symbol "u"))))
    in
    let b =
      Task.async pool (fun _ -> eval (fresh [ "x" ] (Unify (Var "x", Symbol "v"))))
    in
    StateMonad.run (summ (Task.await pool a) (Task.await pool b)) State.empty)
;; *)

(* let prin1 =
  res2
  |> Result.get_ok
  |> Stream.take ~n:(-1)
  |> List.iter (fun _st -> Format.printf "%a" (Subst.pp Value.pp) _st)
;; *)

let g = makerev funct 700 Nil "y"
let h = makerev funct 700 Nil "x"
let failwithf fmt = Format.kasprintf failwith fmt

let appendo_body =
  Conde
    [ Conj [ Unify (Var "xs", Nil); Unify (Var "ys", Var "xys") ]
    ; Fresh
        ( "h"
        , Fresh
            ( "tmp"
            , Fresh
                ( "tl"
                , Conj
                    [ Unify (Cons (Var "h", Var "tl"), Var "xs")
                    ; Unify (Cons (Var "h", Var "tmp"), Var "xys")
                    ; Call ("appendo", [ Var "tl"; Var "ys"; Var "tmp" ])
                    ] ) ) )
    ]
;;

let reverso_body =
  Conde
    [ Conj [ Unify (Var "xy", Nil); Unify (Var "yx", Nil) ]
    ; Fresh
        ( "h"
        , Fresh
            ( "tmp"
            , Fresh
                ( "tl"
                , Conj
                    [ Unify (Cons (Var "h", Var "tl"), Var "xy")
                    ; Call ("reverso", [ Var "tl"; Var "tmp" ])
                    ; Call ("appendo", [ Var "tmp"; Cons (Var "h", Nil); Var "yx" ])
                    ] ) ) )
    ]
;;

let first_logic =
  let last = ref 10 in
  fun () ->
    incr last;
    !last
;;

let second_logic =
  let last = ref 10 in
  fun () ->
    incr last;
    !last
;;

(* let () =
  let goal = Call ("reverso", [ g; Var "xs" ]) in
  let goal2 = Call ("reverso", [ h; Var "xz" ]) in
  let state0 =
    State.(
      empty
      |> "xs" --> Var 10
      |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
      |> add_rel "reverso" [ "xy"; "yx" ] reverso_body)
  in
  let state1 =
    State.(
      empty
      |> "xz" --> Var 10
      |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
      |> add_rel "reverso" [ "xy"; "yx" ] reverso_body)
  in
  let wrap g =
    let s = StateMonad.run (eval first_logic g) state0 in
    (* let _ = Result.map (Stream.take ~n:1) s in ~trace_uni:true*)
    s
  in
  let wrap1 g =
    let s = StateMonad.run (eval second_logic g) state1 in
    s
  in
  let pool = Task.setup_pool ~num_domains:2 () in
  let d = Task.async pool (fun _ -> wrap goal) in
  let d1 = Task.async pool (fun _ -> wrap1 goal2) in
  match Task.run pool (fun () -> Task.await pool d, Task.await pool d1) with
  | Result.Ok a, Result.Ok b ->
    (a
    |> Stream.take ~n:(-1)
    |> fun xs -> Format.printf "Got %d answers\n%!" (List.length xs));
    b
    |> Stream.take ~n:(-1)
    |> fun xs -> Format.printf "Got %d answers\n%!" (List.length xs)
  | Ok _, Error _ (* -> failwiths "%s %d" __FILE__ __LINE__ *)
  | Error _, Ok _ (* -> failwiths "%s %d" __FILE__ __LINE__ *)
  | Error _, Error _ ->
    (* Format.printf "%a\n%a\n%!" pp_error s1 pp_error s2; *)
    failwithf "%s %d" __FILE__ __LINE__
;; *)

(* let _ =
  let goal = Call ("appendo", [ Var "xs"; Var "ys"; g ]) in
  let goal1 = Call ("appendo", [ Var "xs"; Var "ys"; h ]) in
  let env =
    State.(
      empty
      |> "xs" --> Var 9
      |> "ys" --> Var 10
      |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body)
  in
  let env1 =
    State.(
      empty
      |> "xs" --> Var 9
      |> "ys" --> Var 10
      |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body)
  in
  let pool = Task.setup_pool ~num_domains:2 () in
  let a =
    Task.async pool (fun _ -> StateMonad.run (eval ~next_logic_var1:first_logic goal) env)
  in
  let b =
    Task.async pool (fun _ ->
      StateMonad.run (eval ~next_logic_var1:second_logic goal1) env1)
  in
  (match Task.run pool (fun () -> Task.await pool a, Task.await pool b) with
   | Result.Ok a, Result.Ok b -> Stream.mplus a b
   | Ok _, Error _ | Error _, Ok _ | Error _, Error _ ->
     failwithf "%s %d" __FILE__ __LINE__)
  |> Stream.take ~n:(-1)
  |> (fun xs ->
       Format.printf "Got %d answers\n%!" (List.length xs);
       xs) *)
(* |> List.iter (fun st -> printf "%a\n" Value.pp (Value.walk st (Value.var 10))) *)

(* let _ =
  let goal = (Fresh ("x", Unify (Cons (Var "x", Nil), Nil))) in
  let state = State.empty in
  (StateMonad.run (eval ~trace_uni:true goal) state)
  |> Result.get_ok
  |> Stream.take ~n:(-1) 
  |> List.iter (fun st -> printf "%a" (Subst.pp Value.pp) st)
;; *)
let c = Chan.make_unbounded ()

(* let recv_poll = 
  match Chan.recv_poll c with
  | Some _ -> true
  | None -> false *)

let rec merge_Stream n =
  let open StateMonad.Syntax in
  let open StateMonad in
  let* st = read in
  match Chan.recv_poll c with
  | Some x ->
    let* () = put st in
    return (Stream.mplus (Stream.return x)) <*> merge_Stream n
  | None -> return Stream.Nil
;;

let rec force_Stream x =
  match x with
  | Stream.Cons (x, y) ->
    Chan.send c x;
    force_Stream (Lazy.force y)
  | Stream.Nil -> ()
  | _ -> assert false
;;

(* let _ =
  let goal = Call ("appendo", [ Var "xs"; Var "ys"; g ]) in
  let goal1 = Call ("appendo", [ Var "xs"; Var "ys"; h ]) in
  let env =
    State.(
      empty
      |> "xs" --> Var 9
      |> "ys" --> Var 10
      |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body)
  in
  let env1 =
    State.(
      empty
      |> "xs" --> Var 9
      |> "ys" --> Var 10
      |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body)
  in
  let pool = Task.setup_pool ~num_domains:2 () in
  let a =
    Task.async pool (fun _ ->
      force_Stream (StateMonad.run (eval goal) env |> Result.get_ok))
  in
  let b =
    Task.async pool (fun _ ->
      force_Stream (StateMonad.run (eval goal1) env1 |> Result.get_ok))
  in
  let _ = Task.run pool (fun _ -> Task.await pool a, Task.await pool b) in
  merge_Stream c
  |> Stream.take ~n:(-1)
  |> fun xs ->
  Format.printf "Got %d answers\n%!" (List.length xs);
  xs
;; *)

let rec fib n = if n <= 2 then 1 else fib (n - 1) + fib (n - 2)

(* let c = Chan.make_bounded 1 *)
(* let pool = Task.setup_pool ~num_domains:2 () *)

(* let dev a =
  let b = a * a in
  Chan.send c (a * 82708270987);
  let d = Chan.recv c in
  print_int d;
  b * 970987098790
;; *)

(* module C = Domainslib.Chan

let num_domains =
  try int_of_string Sys.argv.(1) with
  | _ -> 4
;;

let n =
  try int_of_string Sys.argv.(2) with
  | _ -> 22
;;

type 'a message =
  | Task of 'a
  | Quit *)

(* let c = C.make_unbounded () *)

(* let create_work tasks =
  Array.iter (fun t -> C.send c (Task t)) tasks;
  for _ = 1 to num_domains do
    C.send c Quit
  done
;;

let rec worker f () =
  match C.recv c with
  | Task a ->
    f a;
    worker f ()
  | Quit -> ()
;; *)

(* let _ =
  let tasks = Array.init n (fun i -> i) in
  create_work tasks;
  let factorial n =
    if n <= 2 then 1 else fib (n - 1) + fib (n - 2)
  in
  let results = Array.make n 0 in
  let update r i = r.(i) <- factorial i in
  let domains =
    Array.init (num_domains -1) (fun _ -> Domain.spawn (worker (update results)))
  in
  (* worker (update results) (); *)
  Array.iter Domain.join domains;
  Array.iter (Printf.printf "%d ") results
;; *)

let makeEven i = i * 2
let makeOdd i = (i * 2) + 1
let rec repeat f i acc eo = if i = 0 then acc else repeat f (i - 1) (f acc (eo i)) eo
let fun_for_stream x i = lazy (Stream.Cons (i, x))
let streamList eo = Stream.Cons (eo 0, repeat fun_for_stream 50 (lazy Stream.Nil) eo)
let c = Chan.make_unbounded ()
let pool = Task.setup_pool ~num_domains:2 ()
(* 
let _ =
  let t1 = Task.async pool (fun _ -> forceStream (streamList makeOdd)) in
  let t2 = Task.async pool (fun _ -> forceStream (streamList makeEven)) in
  let a = Task.run pool (fun _ -> Task.await pool t1) in
  let b = Task.run pool (fun _ -> Task.await pool t2) in
  (* Task.await pool t2); *)
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d  " (Chan.recv c);
  printf "%d" (Chan.recv c)
;; *)

(* Stream.mplus (Chan.recv c) (Chan.recv c) |> Stream.take |> List.iter (printf "%d") *)
(* Stream.take (Chan.recv c) |> List.iter (printf "%d") *)
(* дописать так чтобы chan превратился в stream и обьеденить с mplus *)

(* let new_conde lst =
  let x = List.hd lst in
  let xs = List.tl lst in
  let open State in
  let open StateMonad in
  let open StateMonad.Syntax in
  let* st = read in
  List.foldlm
    (fun acc y ->
      let* () = put st in
      return (Stream.mplus acc) <*> eval y)
    (eval x)
    xs
;; *)

let make_task acc =
  Task.async pool (fun _ ->
    force_Stream (StateMonad.run (eval acc) State.empty |> Result.get_ok))
;;

let make_task_list lst =
  let open StateMonad.Syntax in
  List.map make_task lst
;;

let new_conde lst =
  Task.run pool (fun () -> List.iter (fun x -> Task.await pool x) (make_task_list lst));
  merge_Stream c
;;

(* let _ =
  StateMonad.run
  (eval (fresh [ "x" ]
    (Conde
          [ Unify (Var "x", Symbol "u")
          ; Unify (Var "x", Symbol "v")
          ; Unify (Var "x", Symbol "w")
          ])))
    State.empty
  |> Result.get_ok
  |> Stream.take
  |> List.iter (fun st -> printf "%a" (Subst.pp Value.pp) st)
;; *)

let even_body =
  Conde
    [ Unify (Var "q", Symbol "z")
    ; Fresh
        ( "tmp"
        , Conj
            [ Unify (Cons (Symbol "b", Cons (Symbol "b", Var "tmp")), Var "q")
            ; Call ("even", [ Var "tmp" ])
            ] )
    ]
;;

let _ =
  StateMonad.run
    (eval (Call ("even", [ Var "q" ])))
    State.(empty |> "q" --> Var 10 |> add_rel "even" [ "q" ] even_body)
  |> Result.get_ok
  |> Stream.take ~n:1
  |> List.iter (fun st -> printf "%a" (Value.ppw st) (Value.var 10))
;;
