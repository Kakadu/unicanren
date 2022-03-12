open Unicanren.Lib
open Unicanren.Test_numbers

let () = Format.set_margin 1000
let () = Format.set_max_indent 1000

let () =
  let goal = Call ("pluso", [ build_num 1; build_num 3; Var "q" ]) in
  run_optimistically goal State.(default_env |> "q" --> Var 10)
  |> List.iteri (fun n st ->
         (* Format.printf "@[<h>%d: %a@]%!" n pp_subst st; *)
         Format.printf "1+3 = %a\n%!" Value.pp (Value.walk st (Var 10)))
;;

let __ () =
  let goal = Call ("poso", [ build_num 1 ]) in
  run_optimistically goal State.(default_env)
  |> List.iteri (fun n st -> Format.printf "@[<h>%d: %a@]%!" n pp_subst st)
;;

let _ = exit 1

let () =
  let goal = Call ("addero", [ Symbol "1"; build_num 0; build_num 1; Var "q" ]) in
  run_optimistically goal State.(default_env |> "q" --> Var 10)
  |> List.iteri (fun n st ->
         (* Format.printf "@[<h>%d: %a@]%!" n pp_subst st; *)
         Format.printf "(addero 1 () (1)) = %a\n%!" Value.pp (Value.walk st (Var 10)))
;;

let () =
  let goal = Call ("addero", [ Symbol "0"; build_num 1; build_num 1; Var "q" ]) in
  run_optimistically goal State.(default_env |> "q" --> Var 10)
  |> List.iteri (fun n st ->
         (* Format.printf "@[<h>%d: %a@]%!" n pp_subst st; *)
         Format.printf "(addero 0 (1) (1)) = %a\n%!" Value.pp (Value.walk st (Var 10)))
;;

let __ () =
  let goal = Call ("pluso", [ build_num 0; build_num 2; Var "q" ]) in
  run_optimistically goal State.(default_env |> "q" --> Var 10)
  |> List.iteri (fun n st ->
         (* Format.printf "@[<h>%d: %a@]%!" n pp_subst st; *)
         Format.printf "0+2 = %a\n%!" Value.pp (Value.walk st (Var 10)))
;;

let __ () =
  let goal =
    Call ("full-addero", [ Symbol "0"; Symbol "1"; Symbol "1"; Var "q"; Var "r" ])
  in
  run_optimistically goal State.(default_env |> "q" --> Var 10 |> "r" --> Var 11)
  |> List.iteri (fun _ st ->
         Format.printf
           "\t q = %a, r=%a\n%!"
           Value.pp
           (Value.walk st (Var 10))
           Value.pp
           (Value.walk st (Var 11)))
;;
