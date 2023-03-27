open Lib
open Promises.Promise

(* basic relations *)

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

(* --- tasks --- *)

(* external parallelization of reverso lst1 and reverso lst2 *)
let reverso2lst lst1 lst2 () = 
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
  let a = fork (fun () -> wrap goal1) in
  let b = fork (fun () -> wrap goal2) in 
  let mergeStreams x y = match (x, y) with
      | (Result.Ok a, Result.Ok b) -> Stream.mplus a b
        |> Stream.take ~n:(-1)
      | _ -> failwithf "%s %d" __FILE__ __LINE__
  in 
  let res = pure mergeStreams <*> a <*> b in
  get_val res
;;


(* external parallelization of reverso with true/false answer *)
let reverso_boolean lst1 lst2 () = 
  let goal1 = Call ("reverso", [lst1; lst1]) in
  let goal2 = Call ("reverso", [lst2; lst2]) in
  let state = State.(
    empty (* works without 
    |> "xs" --> Var 10 ?? *)
    |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
    |> add_rel "reverso" [ "xy"; "yx" ] reverso_body
  ) in
  let wrap g = 
    let s = StateMonad.run (eval g) state in
    s
  in 
  let a = fork (fun () -> wrap goal1) in
  let b = fork (fun () -> wrap goal2) in 
  let mergeStreams x y = match (x, y) with
      | (Result.Ok a, Result.Ok b) -> Stream.mplus a b
        |> Stream.take ~n:(-1)
      | _ -> failwithf "%s %d" __FILE__ __LINE__
  in 
  let res = pure mergeStreams <*> a <*> b in
  get_val res
;;


(* external parallelization of appendo lst1 lst 2 *)

let appendo_direct lst1 lst2 () = 
  let goal = Call ("appendo", [lst1; lst2; Var "xs"]) in
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
  let mergeStreams x y = match (x, y) with
      | (Result.Ok a, Result.Ok b) -> Stream.mplus a b
        |> Stream.take ~n:(-1)
      | _ -> failwithf "%s %d" __FILE__ __LINE__
  in 
  let res = pure mergeStreams <*> a <*> b in
  get_val res
;;

(* 10 appendo *)

let appendo10 lst1 lst2 () = 
  let goal = Call ("appendo", [lst1; lst2; Var "xs"]) in
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
  let b1 = fork (fun () -> Printf.printf "hi1 -- %f\n" (Sys.time()); let res = wrap goal in Printf.printf "by1 -- %f\n" (Sys.time()); res) in
  let b2 = fork (fun () -> Printf.printf "hi2 -- %f\n" (Sys.time()); let res = wrap goal in Printf.printf "by2 -- %f\n" (Sys.time()); res) in 
  let b3 = fork (fun () -> Printf.printf "hi3 -- %f\n" (Sys.time()); let res = wrap goal in Printf.printf "by3 -- %f\n" (Sys.time()); res) in 
  let b4 = fork (fun () -> Printf.printf "hi4 -- %f\n" (Sys.time()); let res = wrap goal in Printf.printf "by4 -- %f\n" (Sys.time()); res) in 
  let b5 = fork (fun () -> Printf.printf "hi5 -- %f\n" (Sys.time()); let res = wrap goal in Printf.printf "by5 -- %f\n" (Sys.time()); res) in 
  let b6 = fork (fun () -> Printf.printf "hi6 -- %f\n" (Sys.time()); let res = wrap goal in Printf.printf "by6 -- %f\n" (Sys.time()); res) in 
  let b7 = fork (fun () -> Printf.printf "hi7 -- %f\n" (Sys.time()); let res = wrap goal in Printf.printf "by7 -- %f\n" (Sys.time()); res) in 
  let b8 = fork (fun () -> Printf.printf "hi8 -- %f\n" (Sys.time()); let res = wrap goal in Printf.printf "by8 -- %f\n" (Sys.time()); res) in 
  let b9 = fork (fun () -> Printf.printf "hi9 -- %f\n" (Sys.time()); let res = wrap goal in Printf.printf "by9 -- %f\n" (Sys.time()); res) in 
  let b10 = fork (fun () -> Printf.printf "hi10 -- %f\n"( Sys.time()); let res = wrap goal in Printf.printf "by10 -- %f\n"( Sys.time()); res) in 
  let mergeStreams x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 = match (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) with
  (* change to List.foldlm *)
      | (Result.Ok a1, Result.Ok a2, 
      Result.Ok a3, Result.Ok a4, 
      Result.Ok a5, Result.Ok a6, 
      Result.Ok a7, Result.Ok a8, 
      Result.Ok a9, Result.Ok a10) ->
        Stream.mplus
          (Stream.mplus
            (Stream.mplus
              (Stream.mplus a1 a2) (Stream.mplus a3 a4)
            )
            (Stream.mplus
              (Stream.mplus a5 a6) (Stream.mplus a7 a8)
            )
          )
          (Stream.mplus a9 a10)
      |> Stream.take ~n:(-1)
      | _ -> failwithf "%s %d" __FILE__ __LINE__
  in 
  let res = pure mergeStreams <*> b1 <*> b2 <*> b3 <*> b4 <*> b5 <*> b6 <*> b7 <*> b8 <*> b9 <*> b10 in
  get_val res
;;


(* running parallel task *)

let output = open_out "./answer.txt"  (* unicanren/_build/default/lib *)

let runTask task = 
  let time = Sys.time() in 
  match Promises.Promise.run task with
| Ok v -> 
    Format.printf "ET: %f\n" (Sys.time() -. time); (* execution time *)
    v
    |> (fun xs ->
        Format.printf "ANSWERS: Got %d answers:\n%!" (List.length xs);
        xs)
    |> List.iter (fun st ->
      let str = Format.asprintf "%a\n" Value.pp (Value.walk st (Value.var 10)) in 
      Printf.fprintf output "%s" str);
| Error e -> Printf.printf "test2: error: %s\n" @@ Printexc.to_string e

(*   _ _ _ _ _    *)

let rec makerev f i acc sym = if i = 0 then acc else makerev f (i - 1) (f acc sym) sym
let funct x symbol = Term.Cons (Symbol symbol, x)

(* lists *)
let lst_y len = makerev funct len Nil "y"
let lst_x len = makerev funct len Nil "x"

(* sizes of lists *)
let lens = [128; 256; 512]

(*
let _ = List.iter (fun len -> 
  Format.printf "run 2 boolean reverso  len: %d\n" len; 
  let lst = lst_y len in
  runTask (reverso_boolean (lst) (lst)); 
  Format.printf "\n"
  ) lens
;;
*)

(*
let _ = List.iter (fun len -> 
  Format.printf "run 2 reverso 'y' * %d\n" len; 
  let lst = lst_y len in
  runTask (reverso2lst (lst) (lst)); 
  Format.printf "\n\n"
  ) lens
;;
*)

(*
let _ = List.iter (fun len -> 
  Format.printf "run reverso 'y'*%d and 'x'*%d\n" len (len / 2); 
  runTask (reverso2lst (lst_y len) (lst_x (len / 2))); 
  Format.printf "\n\n"
  ) lens
;;
*)

(*
let _ = List.iter (fun len -> 
  Format.printf "appendo direct 'y'*%d and 'x'*%d\n" len len; 
  runTask (appendo_direct (lst_y len) (lst_x len)); 
  Format.printf "\n\n"
  ) lens;
  close_out output
;;
*)


let _ = List.iter (fun len -> 
  Format.printf "10 appendo 'y'*%d and 'x'*%d\n" len len; 
  runTask (appendo10 (lst_y len) (lst_x len)); 
  Format.printf "\n\n"
  ) lens;
  close_out output
;;
