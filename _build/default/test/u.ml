open Unicanren.Lib

(* open Printf *)
open Domainslib
open Format

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
;;

(* let prin1 =
  res1
  |> Result.get_ok
  |> Stream.take ~n:1
  |> List.iter (fun _st -> Format.printf "%a" (Value.ppw _st) (Subst.find 12 _st))
;; *)

let pool = Task.setup_pool ~num_domains:3 ()

let summ acc y =
  let open StateMonad in
  (* let open StateMonad.Syntax in *)
  return (Stream.mplus acc) <*>  eval y
;;

let paraltest =
  Task.run pool (fun () ->
    StateMonad.run
      (eval
         (fresh
            [ "x" ]
            (let a = Task.async pool (fun _ -> Unify (Var "x", Symbol "u")) in
             let b = Task.async pool (fun _ -> Unify (Var "x", Symbol "v"))in
             Conj[Task.await pool a;Task.await pool b]) ))
      State.empty)
;;

let prin1 =
  paraltest
  |> Result.get_ok
  |> Stream.take ~n:1
  |> List.iter (fun _st -> Format.printf "%a" (Subst.pp Value.pp) _st)
;;