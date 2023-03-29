open Lib
open Async.Scheduler

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

let failwithf fmt = Format.kasprintf failwith fmt
(*
let reverso2lst lst1 lst2 = 
    let goal1 = Call ("reverso", [lst1; Var "xs"]) in
    let goal2 = Call ("reverso", [lst2; Var "xs"]) in
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
    let task name () =
        Printf.printf "starting %s\n%!" name;
        let v = Random.int 100 in
        Printf.printf "ending %s with %d\n%!" name v;
        v
      in
    let pa = async (task "a") in
    let pb = async (task "b") in
    let a = (fun () -> let time = Sys.time() in 
        match (wrap goal1) with
        | Result.Ok a -> a
        |> Stream.take ~n:(-1)
        | _ -> failwithf "%s %d" __FILE__ __LINE__
        |> (fun xs ->
            Format.printf "ET: %f\n" (Sys.time() -. time);
            xs);
    ) in
    Format.printf " %d and %d\n" (await pa) (await pb);
    a()
  ;;
*)

let output = open_out "./answer.txt"  (* unicanren/_build/default/lib *)
(*   _ _ _ _ _    *)

let rec makerev f i acc sym = if i = 0 then acc else makerev f (i - 1) (f acc sym) sym
let funct x symbol = Term.Cons (Symbol symbol, x)

(* lists *)
let lst_y len = makerev funct len Nil "y"
let lst_x len = makerev funct len Nil "x"

(* sizes of lists *)
let lens = [512]
(*
let _ = List.iter (fun len -> 
    Format.printf "run 2 boolean reverso  len: %d\n" len; 
    let lst = lst_y len in
    let time = Sys.time() in 
    reverso2lst (lst) (lst)
    |> (fun xs ->
        Format.printf "ET: %f\n" (Sys.time() -. time);
        Format.printf "ANSWERS: Got %d answers:\n%!" (List.length xs);
        xs)
    |> List.iter (fun st ->
        let str = Format.asprintf "%a\n" Value.pp (Value.walk st (Value.var 10)) in 
        Printf.fprintf output "%s" str);
    Format.printf "\n"
    ) lens
  ;;
*)
let len = 512
let main () =
    let goal1 = Call ("reverso", [lst_y len; Var "xs"]) in
    let goal2 = Call ("reverso", [lst_x len; Var "xs"]) in
    let state = State.(
      empty
      |> "xs" --> Var 10
      |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
      |> add_rel "reverso" [ "xy"; "yx" ] reverso_body
    ) in
    let wrap g = 
        yield ();
      let s = StateMonad.run (eval g) state in
      s
    in 
    let pf = async (fun () -> 
        Format.printf "pf start: %f\n" (Sys.time());
        match (wrap goal1) with
        | Result.Ok a -> 
            Format.printf "pf end: %f\n" (Sys.time());
            a
        | _ -> failwithf "%s %d" __FILE__ __LINE__
    ) in 
    let pd = async (fun () -> 
        Format.printf "pd start: %f\n" (Sys.time());
        match (wrap goal2) with
        | Result.Ok a -> 
            Format.printf "pf end: %f\n" (Sys.time());
            a
        | _ -> failwithf "%s %d" __FILE__ __LINE__
    )
    in
    let time = Sys.time() in 
    let f v u = Stream.mplus v u
    |> Stream.take ~n:(-1)
    |> (fun xs ->
        Format.printf "ET: %f\n" (Sys.time() -. time);
        List.iter (fun st ->
            let str = Format.asprintf "%a\n" Value.pp (Value.walk st (Value.var 10)) in 
            Printf.fprintf output "%s" str;
        ) xs;
        xs) in 
    Format.printf "Before waiting on anything\n%!";
    Format.printf "\npf is length: %d\n" (List.length (f (await pf) (await pd)));
    Format.printf "ET: %f\n" (Sys.time() -. time)
  
  let _ = run main

