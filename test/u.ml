open Unicanren.Lib

let _ =
  match
    StateMonad.run (eval ~trace_uni:true (Unify (Var "x", Symbol "y"))) State.empty
  with
  | _ -> print_endline "OK"
;;
