open Lib
open Eio.Std

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
let output = open_out "./answer.txt"  (* unicanren/_build/default/lib *)
(*   _ _ _ _ _    *)

let rec makerev f i acc sym = if i = 0 then acc else makerev f (i - 1) (f acc sym) sym
let funct x symbol = Term.Cons (Symbol symbol, x)

(* lists *)
let lst_y len = makerev funct len Nil "y"
let lst_x len = makerev funct len Nil "x"

(* sizes of lists *)
let lens = [700]

(* try with Promise and Fibers -- not parallel, need to fix somehow
let sum_to n =
  traceln "Starting CPU-intensive task...";
  let total = ref 0 in
  for i = 1 to n do
    total := !total + i;
    if (i mod 10000) == 0 then traceln "%d" n
  done;
  traceln "Finished %d" n;
  !total

let main2 ~domain_mgr =
  let goal1 = Call ("reverso", [lst_y 700; Var "xs"]) in
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
  let test n =
    traceln "sum 1..%d = %d" n
      (Eio.Domain_manager.run domain_mgr
        (fun () -> sum_to n))
  in
  let startDomain f = 
    (Eio.Domain_manager.run domain_mgr
        (fun () -> f)) in
  let time = Sys.time() in
  let promise1, resolver1 = Promise.create () in
  let promise2, resolver2 = Promise.create () in
  Fiber.both
    (fun () -> 
      traceln "Start goal1";
      startDomain (
      (match (wrap goal1) with
      | Result.Ok a -> 
          traceln "Got result! in: %f" (Sys.time() -. time);
          a
      | _ -> failwithf "%s %d" __FILE__ __LINE__)
      |> Promise.resolve resolver1
      ) 
    )
    (fun () -> startDomain(
      traceln "Start goal2";
      (match (wrap goal1) with
      | Result.Ok a -> 
          traceln "Got result2! in: %f" (Sys.time() -. time);
          a
      | _ -> failwithf "%s %d" __FILE__ __LINE__)
      |> Promise.resolve resolver2 ));
  let x = Promise.await promise1 in
  let y = Promise.await promise2 in
  Stream.mplus x y 
  |> Stream.take ~n:(-1)
  |> (fun xs ->
        traceln "ET: %f" (Sys.time() -. time);
        List.iter (fun st ->
            let str = Format.asprintf "%a\n" Value.pp (Value.walk st (Value.var 10)) in 
            Printf.fprintf output "%s" str;
        ) xs
      );;
*)

(* reverso 'x', reverso 'y'*)
let reverso2 ~domain_mgr = 
  let lst1 = lst_x 700 in
  let lst2 = lst_y 700 in
  let time = Sys.time() in
  let test lst () = 
    let goal1 = Call ("reverso", [lst; Var "xs"]) in
    let state = State.(
      empty
      |> "xs" --> Var 10
      |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
      |> add_rel "reverso" [ "xy"; "yx" ] reverso_body
    ) in
    let wrap g = 
      let s = StateMonad.run (eval g) state in
      s
    in (match (wrap goal1) with
    | Result.Ok a -> 
        a
    | _ -> failwithf "%s %d" __FILE__ __LINE__)
    in
  let stream1 = Eio.Domain_manager.run domain_mgr (test lst1) in
  let stream2 = Eio.Domain_manager.run domain_mgr (test lst2) in
  Stream.mplus stream1 stream2 
  |> Stream.take ~n:(-1)
  |> (fun xs ->
        traceln "ET: %f, Answers: %d" (Sys.time() -. time) (List.length xs);
        List.iter (fun st ->
            let str = Format.asprintf "%a\n" Value.pp (Value.walk st (Value.var 10)) in 
            Printf.fprintf output "%s" str;
        ) xs
      )
;;

(* append 'x'+'y' and 'y' + 'x'*)
let append2 ~domain_mgr = 
  let lstX = lst_x 16384 in
  let lstY = lst_y 16384 in
  let time = Sys.time() in
  let test lst1 lst2() = 
    let goal1 = Call ("appendo", [lst1; lst2; Var "xs"]) in
    let state = State.(
      empty
      |> "xs" --> Var 10
      |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
    ) in
    let wrap g = 
      let s = StateMonad.run (eval g) state in
      s
    in (match (wrap goal1) with
    | Result.Ok a -> 
        a
    | _ -> failwithf "%s %d" __FILE__ __LINE__)
    in
  let stream1 = Eio.Domain_manager.run domain_mgr (test lstX lstY) in
  let stream2 = Eio.Domain_manager.run domain_mgr (test lstY lstX) in
  Stream.mplus stream1 stream2 
  |> Stream.take ~n:(-1)
  |> (fun xs ->
        traceln "ET: %f, Answers: %d" (Sys.time() -. time) (List.length xs);
        List.iter (fun st ->
            let str = Format.asprintf "%a\n" Value.pp (Value.walk st (Value.var 10)) in 
            Printf.fprintf output "%s" str;
        ) xs
      )
;;

(* 10 append 'x'+'y'*)
let append10 ~domain_mgr = 
  let lstX = lst_x 1000 in
  let lstY = lst_y 1000 in
  let time = Sys.time() in
  let test lst1 lst2() = 
    let goal1 = Call ("appendo", [lst1; lst2; Var "xs"]) in
    let state = State.(
      empty
      |> "xs" --> Var 10
      |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
    ) in
    let wrap g = 
      let s = StateMonad.run (eval g) state in
      s
    in (match (wrap goal1) with
    | Result.Ok a -> 
        a
    | _ -> failwithf "%s %d" __FILE__ __LINE__)
    in
  (* i will optimise it with Lists and fold *)
  let a1 = Eio.Domain_manager.run domain_mgr (test lstX lstY) in
  let a2 = Eio.Domain_manager.run domain_mgr (test lstX lstY) in
  let a3 = Eio.Domain_manager.run domain_mgr (test lstX lstY) in
  let a4 = Eio.Domain_manager.run domain_mgr (test lstX lstY) in
  let a5 = Eio.Domain_manager.run domain_mgr (test lstX lstY) in
  let a6 = Eio.Domain_manager.run domain_mgr (test lstX lstY) in
  let a7 = Eio.Domain_manager.run domain_mgr (test lstX lstY) in
  let a8 = Eio.Domain_manager.run domain_mgr (test lstX lstY) in
  let a9 = Eio.Domain_manager.run domain_mgr (test lstX lstY) in
  let a10 = Eio.Domain_manager.run domain_mgr (test lstX lstY) in
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
  |> (fun xs ->
        traceln "10append ET: %f, Answers: %d" (Sys.time() -. time) (List.length xs);
        List.iter (fun st ->
            let str = Format.asprintf "%a\n" Value.pp (Value.walk st (Value.var 10)) in 
            Printf.fprintf output "%s" str;
        ) xs
      )
;;

(* reverso x x*)
let reverso2var ~domain_mgr =
  let time = Sys.time() in
  let test () = 
    let goal1 = Call ("reverso", [Var "xs"; Var "xs"]) in
    let state = State.(
      empty
      |> "xs" --> Var 10
      |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
      |> add_rel "reverso" [ "xy"; "yx" ] reverso_body
    ) in
    let wrap g = 
      let s = StateMonad.run (eval g) state in
      s
    in (match (wrap goal1) with
    | Result.Ok a -> 
        a
    | _ -> failwithf "%s %d" __FILE__ __LINE__)
    in
  let stream1 = Eio.Domain_manager.run domain_mgr (test) in
  let stream2 = Eio.Domain_manager.run domain_mgr (test) in
  Stream.mplus stream1 stream2 
  |> Stream.take ~n:1
  |> (fun xs ->
        traceln "ET: %f, Answers: %d" (Sys.time() -. time) (List.length xs);
        List.iter (fun st ->
            let str = Format.asprintf "%a\n" Value.pp (Value.walk st (Value.var 10)) in 
            Printf.fprintf output "%s" str;
        ) xs
      )
;;


let _ = Eio_main.run @@ fun env ->
  reverso2var ~domain_mgr:(Eio.Stdenv.domain_mgr env);;
