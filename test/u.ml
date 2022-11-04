open Unicanren.Lib;;

StateMonad.run (eval ~trace_uni:true (Unify (Var "x", Symbol "y")))
