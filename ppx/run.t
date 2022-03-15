  $ ./pp_deep.exe -impl - <<EOF
  > [%%deeper let rec appendo xs ys zs =
  >   conde
  >     [ (xs === Std.nil ()) &&& (ys === zs)
  >     ; fresh (u us tmp)
  >         (xs === u%us)
  >         (zs === u%tmp)
  >         (appendo us ys tmp) ]]
  > EOF
  include
    struct
      let rec appendo xs ys zs =
        conde
          [(xs === (Std.nil ())) &&& (ys === zs);
          fresh (u us tmp) (xs === (u % us)) (zs === (u % tmp))
            (appendo us ys tmp)]
      let repr_appendo =
        Def
          (["xs"; "ys"; "zs"],
            (Conde
               ((Conj
                   ((Unify ((Var "xs"), (Constr ("[]", [])))),
                     (Unify ((Var "ys"), (Var "zs"))))),
                 (Fresh
                    ([u; us; tmp],
                      (Unify
                         ((Var "xs"), (Constr ("Cons", [Var "u"; Var "us"])))),
                      (Unify
                         ((Var "zs"), (Constr ("Cons", [Var "u"; Var "tmp"])))),
                      (Call ("appendo", [Var "us"; Var "ys"; Var "tmp"])))))))
    end
  $ ./pp_deep.exe -impl - <<EOF
  > EOF
