open Lib
open Format

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

let run_optimistically g st =
  match StateMonad.run (eval g) st with
  | Result.Ok r -> Stream.take ~n:(-1) r
  | Result.Error e -> failwiths "Error: %a" pp_error e
;;

let%expect_test _ =
  let goal = Unify (Var "x", Symbol "y") in
  run_optimistically
    goal
    State.(add_var_logic 10 (Symbol "y") @@ add_var "x" (Symbol "y") empty)
  |> List.iter (fun st -> Format.printf "%a\n%!" (Subst.pp Value.pp) st);
  [%expect {| _.10 -> 'y |}]
;;

let%expect_test _ =
  let goal = Unify (Var "x", Cons (Symbol "y", Nil)) in
  run_optimistically goal State.(add_var "x" (Var 10) empty)
  |> List.iter (fun st -> Format.printf "%a\n%!" (Subst.pp Value.pp) st);
  [%expect {| _.10 -> (cons 'y nil) |}]
;;

let%expect_test _ =
  let goal = Conj [ Unify (Var "x", Cons (Symbol "y", Nil)); Unify (Var "x", Var "z") ] in
  run_optimistically goal State.(empty |> "x" --> Var 10 |> "z" --> Var 11)
  |> List.iter (fun st -> Format.printf "%a\n%!" (Subst.pp Value.pp) st);
  [%expect {|
    _.10 -> (cons 'y nil)
    _.11 -> (cons 'y nil) |}]
;;

let%expect_test _ =
  let goal = Conde [ Unify (Var "x", Symbol "u"); Unify (Var "x", Symbol "v") ] in
  run_optimistically goal State.(empty |> "x" --> Var 10)
  |> List.iteri (fun n st -> Format.printf "@[<h>%d: %a@]%!" n (Subst.pp Value.pp) st);
  [%expect {|
    0: _.10 -> 'u
       1: _.10 -> 'v |}]
;;

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

let%expect_test _ =
  let goal =
    Call
      ( "appendo"
      , [ Cons (Symbol "a", Nil); Cons (Symbol "b", Cons (Symbol "c", Nil)); Var "xys" ]
      )
  in
  let env =
    State.(
      empty |> "xys" --> Var 10 |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body)
  in
  (run_optimistically goal env
  |> fun xs ->
  printf "@[<v>";
  List.iter (fun st -> printf "@[Answer: %a@]\n" (Value.ppw st) (Value.var 10)) xs;
  printf "@]%!");
  [%expect {|
    Answer: (cons 'a (cons 'b (cons 'c nil))) |}]
;;

let%expect_test _ =
  let goal =
    Call
      ( "appendo"
      , [ Var "xs"
        ; Var "ys"
        ; Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil)))
        ] )
  in
  (run_optimistically
     goal
     State.(
       empty
       |> "xs" --> Var 10
       |> "ys" --> Var 11
       |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body)
  |> fun xs ->
  printf "@[<v>";
  List.iter
    (fun st -> printf "@[Answer: %a@]\n" Value.pp (Value.walk st (Value.var 10)))
    xs;
  printf "@]%!");
  [%expect
    {|
    Answer: nil
    Answer: (cons 'a nil)
    Answer: (cons 'a (cons 'b nil))
    Answer: (cons 'a (cons 'b (cons 'c nil))) |}]
;;

(* let%test_unit "rev" =
  let open Base in
  [%test_eq: int list] (List.rev [ 3; 2; 1 ]) [ 3; 2; 1 ]
;; *)

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

let%expect_test _ =
  let goal =
    Call
      ( "reverso"
      , [ Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil))); Var "xs" ] )
  in
  (run_optimistically
     goal
     State.(
       empty
       |> "xs" --> Var 10
       |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
       |> add_rel "reverso" [ "xy"; "yx" ] reverso_body)
  |> fun xs ->
  printf "@[<v>";
  List.iter
    (fun st -> printf "@[Answer: %a@]\n" Value.pp (Value.walk st (Value.var 10)))
    xs;
  printf "@]%!");
  [%expect {| Answer: (cons 'c (cons 'b (cons 'a nil))) |}]
;;
