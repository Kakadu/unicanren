open Unicanren.Lib
open Effect
open Effect.Deep
open Format


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

let rec makerev f i acc sym = if i = 0 then acc else makerev f (i - 1) (f acc sym) sym
let funct x symbol = Term.Cons (Symbol symbol, x)
let len = makerev funct 50 Nil "y"

let failwithf fmt = Format.kasprintf failwith fmt

open Unicanren.Promises.Promise
open Printf

let testRev2 () =
  let goal = Call ("reverso", [len; Var "xs"]) in
  let _ = printf "reverso of list ['y'] * 50\n" in
  let state = State.(
    empty
    |> "xs" --> Var 10
    |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
    |> add_rel "reverso" [ "xy"; "yx" ] reverso_body
  ) in
  let wrap g = 
    let s = StateMonad.run (eval g) state in
    s
  in 
  let a = fork (fun () -> wrap goal) in
  let b = fork (fun () -> wrap goal) in 
  let mergeStreams x y = (
    match (x, y) with
      | (Result.Ok a, Result.Ok b) -> 
        let _ = Format.printf "inside mplus:\n" in
        Stream.mplus a b
      | _ -> failwithf "%s %d" __FILE__ __LINE__)
    |> Stream.take ~n:(-1)
    |> (fun xs ->
        Format.printf "Got %d answers\n%!" (List.length xs);
        xs)
    (*|> List.iter (fun st -> Format.printf "%a\n" Value.pp (Value.walk st (Value.var 10)))*)
  in 
  let res = pure mergeStreams <*> a <*> b in
  get_val res

let _ = 
  let time = Sys.time() in 
  match run testRev2 with
  | Ok v -> Format.printf "Execution time: %f" (Sys.time() -. time)
  | Error e -> Printf.printf "test2: error: %s\n" @@ Printexc.to_string e

