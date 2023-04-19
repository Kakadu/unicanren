open Unicanren.Lib

let st =
  State.(
    empty
    |> "xs" --> Var 10
    |> "ys" --> Var 11
    |> "zs" --> Var 12
    |> add_rel "appendo" [ "xs"; "ys"; "xys" ] Unicanren.Lib_tests.appendo_body)
;;

let goal = Call ("appendo", [ Var "xs"; Var "ys"; Var "zs" ])

(*   (run_optimistically
     goal
     State.(
       empty
       |> "xs" --> Var 10
       |> "ys" --> Var 11
       |> add_rel "appendo" [ "xs"; "ys"; "xys" ] appendo_body)
  |> fun xs -> *)
(* printf "@[<v>";
  List.iter
    (fun st -> printf "@[Answer: %a@]\n" Value.pp (Value.walk st (Value.var 10)))
    xs;
  printf "@]%!"); *)

let _ =
  match StateMonad.run (eval ~trace_uni:true goal) st with
  | Result.Ok st -> print_endline "OK"
  | Result.Error e -> Format.printf "%a\n%!" pp_error e
;;
