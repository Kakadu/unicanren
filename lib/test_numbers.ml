open Lib
open Format

let run_optimistically g st =
  match StateMonad.run (eval g) st with
  | Result.Ok r -> Stream.take ~n:(-1) r
  | Result.Error e -> failwiths "Error: %a" pp_error e
;;

let default_env = State.empty

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

let default_env =
  let open Term in
  let ( == ) a b = Unify (a, b) in
  let _0 = Symbol "0" in
  let _1 = Symbol "1" in
  default_env
  |> State.add_rel
       "poso"
       [ "n" ]
       (fresh [ "a"; "d" ] (Unify (Var "n", Cons (Var "a", Var "d"))))
  |> State.add_rel
       "gt1o"
       [ "n" ]
       (fresh
          [ "a"; "ad"; "dd" ]
          (Unify (var "n", cons (var "a") (cons (var "ad") (var "dd")))))
  |> State.add_rel
       "full-addero"
       [ "b"; "x"; "y"; "r"; "c" ]
       (let b, x, y, r, c = Var "b", Var "x", Var "y", Var "r", Var "c" in
        Conde
          [ Conj [ _0 == b; _0 == x; _0 == y; _0 == r; _0 == c ]
          ; Conj [ _1 == b; _0 == x; _0 == y; _1 == r; _0 == c ]
          ; Conj [ _0 == b; _1 == x; _0 == y; _1 == r; _0 == c ]
          ; Conj [ _1 == b; _1 == x; _0 == y; _0 == r; _1 == c ]
          ; Conj [ _0 == b; _0 == x; _1 == y; _1 == r; _0 == c ]
          ; Conj [ _1 == b; _0 == x; _1 == y; _0 == r; _1 == c ]
          ; Conj [ _0 == b; _1 == x; _1 == y; _0 == r; _1 == c ]
          ; Conj [ _1 == b; _1 == x; _1 == y; _1 == r; _1 == c ]
          ])
  |> State.add_rel
       "addero"
       [ "d"; "n"; "m"; "r" ]
       (let d, n, m, r, a, c = Var "d", Var "n", Var "m", Var "r", Var "a", Var "c" in
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
              ; cons _1 Nil == m
              ; fresh
                  [ "a"; "c" ]
                  (Conj [ r == cons a c; Call ("full-addero", [ d; _1; _1; a; c ]) ])
              ]
          ; Conj [ cons _1 Nil == n; Call ("gen-addero", [ d; n; m; r ]) ]
          ; Conj
              [ cons _1 Nil == m
              ; Call ("gt1o", [ n ])
              ; Call ("gt1o", [ r ])
              ; Call ("addero", [ d; cons _1 Nil; n; r ])
              ]
          ; Conj [ Call ("gt1o", [ n ]); Call ("gen-addero", [ d; n; m; r ]) ]
          ])
  |> State.add_rel
       "gen-addero"
       [ "d"; "n"; "m"; "r" ]
       (fresh
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
             ; Call ("addero", [ Var "e"; Var "x"; Var "y"; Var "z" ])
             ]))
  |> State.add_rel
       "pluso"
       [ "n"; "m"; "k" ]
       (Call ("addero", [ _0; Var "n"; Var "m"; Var "k" ]))
;;
(*
let%expect_test _ =
  let goal = Call ("pluso", [ build_num 1; build_num 2; Var "q" ]) in
  run_optimistically goal State.(empty |> "q" --> Var 10)
  |> List.iteri (fun n st -> Format.printf "@[<h>%d: %a@]%!" n pp_subst st);
  [%expect {|
     |}]
;; *)
