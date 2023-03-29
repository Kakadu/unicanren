open Eio.Std

(* non parallel *)
let main1 _env =
  Fiber.both
    (fun () -> for x = 1 to 10 do traceln "x = %d" x;  done)
    (fun () -> for y = 1 to 10 do traceln "y = %d" y;  done);;
(*
let _ = Eio_main.run main;;
*)

(* real parallelism *)

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
  let test n =
    traceln "sum 1..%d = %d" n
      (Eio.Domain_manager.run domain_mgr
        (fun () -> sum_to n))
  in
  Fiber.both
    (fun () -> test 1000000)
    (fun () -> test 50000)

    (*
let _ = Eio_main.run @@ fun env ->
  main ~domain_mgr:(Eio.Stdenv.domain_mgr env);;
*)


(* promises *)
(* 
let main3 = Eio_main.run @@ fun _ ->
  let promise, resolver = Promise.create () in
  Fiber.both
    (fun () ->
      traceln "Waiting for promise...";
      let x = Promise.await promise in
      traceln "x = %d" x
    )
    (fun () ->
      traceln "Resolving promise";
      Promise.resolve resolver 42
    );; *)
    