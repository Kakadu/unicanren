open Unicanren.Lib

(* open Printf *)
open Domainslib
open Format
(*
let res =
  StateMonad.run
    (eval
       (fresh
          [ "x" ]
          (Conde [ Unify (Var "x", Symbol "u"); Unify (Var "x", Symbol "v") ])))
    State.empty
;;

let caro a l = Fresh ("d", Unify (Cons (a, Var "d"), l))

let res1 =
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

let pool = Task.setup_pool ~num_domains:3 ()

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
let __ () =
let res2 =
  Task.run pool (fun () ->
    let a =
      Task.async pool (fun _ -> eval (fresh [ "x" ] (Unify (Var "x", Symbol "u"))))
    in
    let b =
      Task.async pool (fun _ -> eval (fresh [ "x" ] (Unify (Var "x", Symbol "v"))))
    in
    StateMonad.run (summ (Task.await pool a) (Task.await pool b)) State.empty)
  in


  res2
  |> Result.get_ok
  |> Stream.take ~n:(-1)
  |> List.iter (fun _st -> Format.printf "%a" (Subst.pp Value.pp) _st)
;;

let run_optimistically g st =
  match StateMonad.run (eval g) st with
  | Result.Ok r -> Stream.take ~n:(-1) r
  | Result.Error e -> failwiths "Error: %a" pp_error e
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

(* let reverso_body = Task.run pool(fun _->
  let a = Task.async pool (fun _ -> eval(Conj [ Unify (Var "xy", Nil); Unify (Var "yx", Nil) ])) in
  let b = Task.async pool (fun _ -> eval (
    Fresh
      ( "h"
      , Fresh
          ( "tmp"
          , Fresh
              ( "tl"
              , Conj
                  [ Unify (Cons (Var "h", Var "tl"), Var "xy")
                  ; Call ("reverso", [ Var "tl"; Var "tmp" ])
                  ; Call ("appendo", [ Var "tmp"; Cons (Var "h", Nil); Var "yx" ])
                  ] ) ) )))
  in
  StateMonad.run(summ(Task.await pool a) (Task.await pool b)) State.empty)
;; *)

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

let g = makerev funct 700  Nil
let failwithf fmt = Format.kasprintf failwith fmt

let () =
  Task.run pool (fun () ->
    let goal = Call ("reverso", [ g; Var "xs" ]) in
    let state0 =
      State.(
        empty
        |> "xs" --> Var 10
        |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
        |> add_rel "reverso" [ "xy"; "yx" ] reverso_body)
    in
    let wrap g =
      let s = StateMonad.run (eval g) state0 in
      let _ = Result.map (Stream.take ~n:1) s in
      s
    in
    let pool = Task.setup_pool ~num_domains:2 () in
    let a = Task.async pool (fun _ -> wrap goal) in
    let b = Task.async pool (fun _ -> wrap goal) in
    (match Task.await pool a, Task.await pool b with
    | Result.Ok a, Result.Ok b -> Stream.mplus a b
    | Ok _, Error _  (* -> failwiths "%s %d" __FILE__ __LINE__ *)
    | Error _, Ok _ (* -> failwiths "%s %d" __FILE__ __LINE__ *)
    | Error _, Error _ ->
      (* Format.printf "%a\n%a\n%!" pp_error s1 pp_error s2; *)
      failwithf "%s %d" __FILE__ __LINE__
    )
    |> Stream.take ~n:(-1)
    |> (fun xs -> Format.printf "Got %d answers\n%!" (List.length xs); xs)
    |> List.iter (fun st ->
      printf "%a\n" Value.pp (Value.walk st (Value.var 10))))
;;
