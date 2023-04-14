open Lib


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
  CondePar
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


let rec makerev f i acc sym = if i = 0 then acc else makerev f (i - 1) (f acc sym) sym
let funct x symbol = Term.Cons (Symbol symbol, x)

(* lists *)
let lst_y len = makerev funct len Nil "y"
let lst_x len = makerev funct len Nil "c"


let run goal env = 
  let time = Sys.time() in
  match (StateMonad.run (eval  goal) env) with
  | Result.Ok r -> 
    Stream.take ~n:2 r
    |> (fun xs ->
      Format.printf "Answers: %d\nTime: %f\n" (List.length xs) (Sys.time() -. time);
      List.iter (fun st -> Format.printf "%a\n" Value.pp (Value.walk st (Value.var 10))
      ) xs
    )
  | _ -> Format.printf "el problema\n"


let test1 () = 
  let goal = CondePar [
    Unify (Symbol "x", Symbol "y"); 
    Unify (Symbol "x", Symbol "x"); 
  ] in 
  let env = State.empty in 
  run goal env


let test2 () = 
  let goal = Call ("reverso", [lst_x 100; Var "xs"]) in 
  let state = State.(
    empty
    |> "xs" --> Var 10
    |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
    |> add_rel "reverso" [ "xy"; "yx" ] reverso_body
  ) in
  run goal state

let _ = test2()


(* let _ = 
  Format.printf "Non parallel version\n";
  let time = Sys.time() in
  let goal = Call ("reverso", [lst_x 10; Var "xs"]) in 
  let state = State.(
    empty
    |> "xs" --> Var 10
    |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
    |> add_rel "reverso" [ "xy"; "yx" ] reverso_body
  ) in
  match (StateMonad.run (eval goal) state) with
  | Result.Ok r -> 
    Stream.take ~n:(-1) r
    |> (fun xs ->
      Format.printf "NP: Answers: %d and time: %f\n" (List.length xs) (Sys.time() -. time);
      List.iter (fun st -> Format.printf "%a\n" Value.pp (Value.walk st (Value.var 10))
      ) xs
    )
  | _ -> Format.printf "el problema\n" *)
