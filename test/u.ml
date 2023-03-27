open Unicanren.Lib

let _ =
  match
    StateMonad.run (eval ~trace_uni:true (Unify (Var "x", Symbol "y"))) State.empty
  with
  | _ -> print_endline "OK"
;;

(*
let testConde2Rev = 
  let time = Sys.time() in 
  let _ = Format.printf "Conde of reverso of list ['y'] * %d\n" len in
  let goal = Conde [ Call ("reverso", [ lst; Var "xs" ]) ; 
                    Call ("reverso", [ lst; Var "xs" ]) ]
  in
  let env = State.(
    empty
    |> "xs" --> Var 1
    |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
    |> add_rel "reverso" [ "xy"; "yx" ] reverso_body
  ) in
  let a = StateMonad.run (eval goal) env in 
  (match a with
  | Result.Ok a -> a
    |> Stream.take~n:(-1)
    |> fun xs -> Format.printf "Conde Got %d answers\n%!" (List.length xs)
  | Error _ -> failwithf "%s %d" __FILE__ __LINE__);
  Format.printf "Conde Execution time: %f\n" (Sys.time() -. time)


  let run_optimistically g st =
    match StateMonad.run (eval g) st with
    | Result.Ok r -> Stream.take ~n:(-1) r
    | Result.Error e -> failwiths "Error: %a" pp_error e
  ;;
  
  let testConde2Rev2 =
  let goal =
    Conde [
      Call
        ( "reverso"
        , [ lst ; Var "xs" ] );
      Call
        ( "reverso"
        , [ lst ; Var "yxs" ] )
    ]
  in
  (run_optimistically
     goal
     State.(
       empty
       |> "xs" --> Var 10
       |> "yxs" --> Var 11
       |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body
       |> add_rel "reverso" [ "xy"; "yx" ] reverso_body)
  |> fun xs ->
  Format.printf "Conde2: %d \n" (List.length xs);
  List.iter
    (fun st -> Format.printf "Answer: %a\n" Value.pp (Value.walk st (Value.var 10)))
    xs;
    List.iter
    (fun st -> Format.printf "11Answer: %a\n" Value.pp (Value.walk st (Value.var 11)))
    xs;);
  
;;
*)

