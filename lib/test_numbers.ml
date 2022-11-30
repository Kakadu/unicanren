(* * A relational implementation of Oleg-style arithmetic *)

open Lib
open Format


let next_logic_var =
  let last = ref 10 in
  fun () ->
    incr last;
    !last
;;


let run_optimistically
  ?(trace_svars = false)
  ?(trace_uni = false)
  ?(trace_calls = false)
  g
  st
  =
  printf "\nRunning: %a\n" pp_goal g;
  match StateMonad.run (eval ~trace_svars ~trace_uni next_logic_var  g) st with
  | Result.Ok r -> Stream.take ~n:(-1) r
  | Result.Error e -> failwiths "Error: %a" pp_error e
;;

let rec build_num n =
  let open Term in
  if n mod 2 = 1
  then Cons (Symbol "1", build_num ((n - 1) / 2))
  else if n = 0
  then Nil
  else (
    let () = assert (n mod 2 = 0) in
    Cons (Symbol "0", build_num (n / 2)))
;;

let poso = "poso", [ "n" ], fresh [ "a"; "d" ] (Unify (Var "n", Cons (Var "a", Var "d")))

let gt1o =
  let open Term in
  ( "gt1o"
  , [ "n" ]
  , fresh
      [ "a"; "ad"; "dd" ]
      (Unify (var "n", cons (var "a") (cons (var "ad") (var "dd")))) )
;;

let full_addero =
  let open Term in
  let ( == ) a b = Unify (a, b) in
  let _0 = Symbol "0" in
  let _1 = Symbol "1" in
  ( "full-addero"
  , [ "b"; "x"; "y"; "r"; "c" ]
  , let b, x, y, r, c = Var "b", Var "x", Var "y", Var "r", Var "c" in
    Conde
      [ Conj [ _0 == b; _0 == x; _0 == y; _0 == r; _0 == c ]
      ; Conj [ _1 == b; _0 == x; _0 == y; _1 == r; _0 == c ]
      ; Conj [ _0 == b; _1 == x; _0 == y; _1 == r; _0 == c ]
      ; Conj [ _1 == b; _1 == x; _0 == y; _0 == r; _1 == c ]
      ; Conj [ _0 == b; _0 == x; _1 == y; _1 == r; _0 == c ]
      ; Conj [ _1 == b; _0 == x; _1 == y; _0 == r; _1 == c ]
      ; Conj [ _0 == b; _1 == x; _1 == y; _0 == r; _1 == c ]
      ; Conj [ _1 == b; _1 == x; _1 == y; _1 == r; _1 == c ]
      ] )
;;

let addero =
  let open Term in
  let ( == ) a b = Unify (a, b) in
  let _0 = Symbol "0" in
  let _1 = Symbol "1" in
  ( "addero"
  , [ "d"; "n"; "m"; "r" ]
  , let d, n, m, r, a, c = Var "d", Var "n", Var "m", Var "r", Var "a", Var "c" in
    Conde
      [ Conj [ _0 == d; Nil == m; n == r ]
      ; Conj [ _0 == d; Nil == n; m == r; Call ("poso", [ m ]) ]
      ; Conj [ _1 == d; Nil == m; Call ("addero", [ _0; n; cons _1 Nil; r ]) ]
      ; Conj
          [ _1 == d
          ; Nil == n
          ; Call ("poso", [ m ])
          ; Call ("addero", [ _0; m; cons _1 Nil; r ])
          ]
      ; Conj
          [ cons _1 Nil == n
          ; cons _1 Nil == m (* ; TraceSVars [ "d"; "n"; "m"; "r" ] *)
          ; fresh
              [ "a"; "c" ]
              (Conj [ r == cons a c; Call ("full-addero", [ d; _1; _1; a; c ]) ])
          ]
      ; Conj
          [ cons _1 Nil == n (* ; TraceSVars [ "d"; "m"; "r" ] *)
          ; Call ("gen-addero", [ d; n; m; r ])
          ]
      ; Conj
          [ cons _1 Nil == m
          ; Call ("gt1o", [ n ])
          ; Call ("gt1o", [ r ])
          ; Call ("addero", [ d; cons _1 Nil; n; r ])
          ]
      ; Conj [ Call ("gt1o", [ n ]); Call ("gen-addero", [ d; n; m; r ]) ]
      ] )
;;

let gen_addero =
  let open Term in
  let ( == ) a b = Unify (a, b) in
  let _0 = Symbol "0" in
  let _1 = Symbol "1" in
  ( "gen-addero"
  , [ "d"; "n"; "m"; "r" ]
  , fresh
      [ "u"; "b"; "c"; "e"; "x"; "y"; "z" ]
      (* TODO: renaming b -> v kind of helps but maybe there is an issue with clashing names somewhere else *)
      (Conj
         [ cons (var "u") (var "x") == var "n"
         ; cons (var "b") (var "y") == var "m"
         ; Call ("poso", [ Var "y" ])
         ; cons (var "c") (var "z") == var "r"
         ; Call ("poso", [ Var "z" ])
         ; TraceSVars [ "b" ]
         ; Call ("full-addero", [ Var "d"; Var "u"; Var "b"; Var "c"; Var "e" ])
         ; TraceSVars [ "c"; "e" ]
         ; Call ("addero", [ Var "e"; Var "x"; Var "y"; Var "z" ])
         ]) )
;;

let pluso =
  let open Term in
  let _0 = Symbol "0" in
  let _1 = Symbol "1" in
  "pluso", [ "n"; "m"; "k" ], Call ("addero", [ _0; Var "n"; Var "m"; Var "k" ])
;;

let default_env =
  State.empty
  |> State.add_rel1 poso
  |> State.add_rel1 gt1o
  |> State.add_rel1 full_addero
  |> State.add_rel1 addero
  |> State.add_rel1 gen_addero
  |> State.add_rel1 pluso
;;

let%expect_test _ =
  let goal = Call ("pluso", [ build_num 1; build_num 2; Var "q" ]) in
  run_optimistically goal State.(default_env |> "q" --> Var 10)
  |> List.iter (fun st ->
       Format.printf
         "Result: @[%a = %a@]\n%!"
         pp_goal
         goal
         Value.pp
         (Value.walk st (Var 10)));
  [%expect
    {|
    Running: (pluso (cons '1 '()) (cons '0 (cons '1 '()))
    q)
    Result: (pluso (cons '1 '()) (cons '0 (cons '1 '()))
               q) = (cons '1 (cons '1 nil))
     |}]
;;

let%expect_test _ =
  let goal =
    Call
      ( "pluso"
      , [ Cons (Symbol "1", Nil); Cons (Symbol "0", Cons (Symbol "1", Nil)); Var "q" ] )
  in
  run_optimistically goal State.(default_env |> "q" --> Var 10)
  |> List.iter (fun st ->
       Format.printf
         "Result: @[%a = %a@]\n%!"
         pp_goal
         goal
         Value.pp
         (Value.walk st (Var 10)));
  [%expect
    {|
    Running: (pluso (cons '1 '()) (cons '0 (cons '1 '()))
    q)
    Result: (pluso (cons '1 '()) (cons '0 (cons '1 '()))
               q) = (cons '1 (cons '1 nil))

     |}]
;;
