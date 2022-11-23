(*
 *
 * Copyright (C) 2022
 *   Dmitrii Kosarev a.k.a. Kakadu
 * St.Petersburg State University, JetBrains Research
 *)

open Base
open Ppxlib

let make_caml_list ~loc =
  List.fold_right ~init:[%expr []] ~f:(fun x acc -> [%expr [%e x] :: [%e acc]])

module Ast_pattern = struct
  include Ppxlib.Ast_pattern

  let lambdas next =
    of_func
      (let rec helper ctx loc e cb acc =
         to_func
           (pexp_fun nolabel none (ppat_var __) __)
           ctx loc e
           (fun lab rhs ->
             let acc = lab :: acc in
             try helper ctx loc rhs cb acc
             with Ppxlib__Ast_pattern0.Expected (_, "fun") ->
               to_func next ctx loc rhs (cb (List.rev acc)))
       in
       fun ctx loc e cb -> helper ctx loc e cb [])

  let%test "lambdas2" =
    let loc = Location.none in
    let ast = [%expr fun x y -> 1] in
    parse
      (lambdas (pexp_constant (pconst_integer (string "1") none)))
      loc ast
      (fun xs -> List.equal String.equal xs [ "x"; "y" ])

  let%test "lambdas2" =
    let loc = Location.none in
    let ast = [%expr fun x y -> 1] in
    parse
      (lambdas (pexp_constant (pconst_integer (string "2") none)))
      loc ast
      ~on_error:(fun () -> true)
      (fun xs -> false)

  let fresh_vars () =
    let id () = pexp_ident (lident __) in

    let lst () =
      of_func
        (let rec helper ctx loc xs cb acc =
           match xs with
           | [] -> cb (List.rev acc)
           | (_, h) :: tl ->
               to_func (id ()) ctx loc h (fun name ->
                   helper ctx loc tl cb (name :: acc))
         in

         fun ctx loc e cb -> helper ctx loc e cb [])
    in
    map2 (pexp_apply (id ()) (lst ())) ~f:List.cons

  let%test "fresh vars succ" =
    let loc = Location.none in
    let ast = [%expr x y z] in
    parse (fresh_vars ()) loc ast (fun xs ->
        List.equal String.equal xs [ "x"; "y"; "z" ])

  let%test "fresh vars fail" =
    let loc = Location.none in
    let ast = [%expr x y 1 z] in
    parse (fresh_vars ()) loc ast (fun _ -> false) ~on_error:(fun () -> true)
end

module Rewrite = struct
  let loc = Location.none

  let rec term () =
    Ast_pattern.of_func
      (let rec helper : _ -> _ -> _ -> (expression -> 'a) -> 'a =
        fun ctx loc e sk ->
         match e with
         | [%expr Std.nil ()] | [%expr []] -> sk [%expr Constr ("[]", [])]
         | {
          pexp_desc = Pexp_apply ([%expr ( % )], [ (Nolabel, l); (Nolabel, r) ]);
          _;
         } ->
             Ast_pattern.to_func (term ()) ctx loc l (fun l ->
                 Ast_pattern.to_func (term ()) ctx loc r (fun r ->
                     sk [%expr Constr ("Cons", [ [%e l]; [%e r] ])]))
         | { pexp_desc = Pexp_ident { txt = Lident s; _ }; _ } ->
             sk
               [%expr
                 Var
                   [%e
                     let open Ast_builder.Default in
                     pexp_constant ~loc (Pconst_string (s, loc, None))]]
             (*
         | {
          pexp_desc =
            Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident f; _ } }, args);
          _;
         } ->
             helper_list ctx loc args (fun es ->
                 let open Ast_builder.Default in
                 sk
                   [%expr
                     Call
                       ( [%e pexp_constant ~loc (Pconst_string (f, loc, None))],
                         [%e
                           List.fold_right es ~init:[%expr []] ~f:(fun t acc ->
                               [%expr [%e t] :: [%e acc]])] )])
             (* Gives not enough polymorphic types *)
             (*
             let es = helper_list ctx loc args Fn.id in
             let open Ast_builder.Default in
             sk
               [%expr
                 Call
                   ( [%e pexp_constant ~loc (Pconst_string (f, loc, None))],
                     [%e
                       List.fold_right es ~init:[%expr []] ~f:(fun t acc ->
                           [%expr [%e t] :: [%e acc]])] )]
                           *)
                           *)
         | _ -> Ppxlib__Ast_pattern0.fail loc "term"
       and helper_list :
           Ast_pattern.context ->
           location ->
           (Ppxlib.Asttypes.arg_label * expression) list ->
           (expression list -> 'b) ->
           'b =
        fun ctx loc xs sk ->
         match xs with
         | [] -> sk []
         | (_, h) :: tl ->
             helper ctx loc h (fun h ->
                 helper_list ctx loc tl (fun tl -> sk (h :: tl)))
       in
       helper)

  let%expect_test "deeping var " =
    let loc = Location.none in
    Ast_pattern.parse (term ()) loc [%expr x]
      (fun e -> Caml.Format.printf "%a\n%!" Pprintast.expression e)
      ~on_error:(fun () -> Caml.print_endline "error");
    [%expect {| Var "x" |}]

  let%expect_test "deeping cons" =
    let loc = Location.none in
    Ast_pattern.parse (term ()) loc [%expr x % y]
      (fun e -> Caml.Format.printf "%a\n%!" Pprintast.expression e)
      ~on_error:(fun () -> Caml.print_endline "error");
    [%expect {| Constr ("Cons", [Var "x"; Var "y"])  |}]

  let%expect_test "deeping nil" =
    let loc = Location.none in
    Ast_pattern.parse (term ()) loc [%expr x % []]
      (fun e -> Caml.Format.printf "%a\n%!" Pprintast.expression e)
      ~on_error:(fun () -> Caml.print_endline "error");
    [%expect {| Constr ("Cons", [Var "x"; Constr ("[]", [])])  |}]

  let caml_list_of pat =
    let open Ast_pattern in
    of_func
      (let rec helper ctx loc e sk =
         match e with
         | [%expr []] -> sk []
         | {
          pexp_desc =
            Pexp_construct
              ({ txt = Lident "::" }, Some { pexp_desc = Pexp_tuple [ l; r ] });
          _;
         } ->
             to_func pat ctx loc l (fun l ->
                 helper ctx loc r (fun r -> sk (l :: r)))
         | _ -> Ppxlib__Ast_pattern0.fail loc "caml_list_of"
       in
       helper)

  let%expect_test "caml_list_of 1" =
    let loc = Location.none in
    Ast_pattern.(parse (caml_list_of (pexp_constant (pconst_integer __ none))))
      loc [%expr [ 1; 2; 3 ]]
      (fun xs ->
        Caml.Format.(
          printf "%a\n%!"
            (pp_print_list
               ~pp_sep:(fun ppf () -> fprintf ppf " ")
               pp_print_string))
          xs)
      ~on_error:(fun () -> Caml.print_endline "error");
    [%expect {| 1 2 3   |}]

  let unify () =
    let open Ast_pattern in
    pexp_apply
      (pexp_ident (lident (string "unify" ||| string "===")))
      ((nolabel ** term ()) ^:: (nolabel ** term ()) ^:: nil)
    |> map2 ~f:(fun l r ->
           let loc = Location.none in
           [%expr Unify ([%e l], [%e r])])

  let%expect_test "unify 1" =
    let loc = Location.none in
    Ast_pattern.parse (unify ()) loc [%expr unify x y]
      (fun e -> Caml.Format.printf "%a\n%!" Pprintast.expression e)
      ~on_error:(fun () -> Caml.print_endline "error");
    [%expect {| Unify ((Var "x"), (Var "y")) |}]

  let logic : 'b. unit -> (expression, expression -> 'b, 'b) Ast_pattern.t =
   fun () ->
    let open Ast_pattern in
    of_func
      (let rec helper : 'a. _ -> _ -> _ -> (expression -> 'a) -> 'a =
        fun ctx loc e sk ->
         (* Caml.Format.printf "Calling helper on '%a'\n%!" Pprintast.expression e; *)
         let conde () =
           pexp_apply
             (pexp_ident (lident (string "conde")))
             ((nolabel ** caml_list_of (of_func helper)) ^:: nil)
           |> map1 ~f:(fun es ->
                  let loc = Location.none in
                  let open Ast_builder.Default in
                  pexp_construct ~loc
                    (Located.mk ~loc (Lident "Conde"))
                    (Some (pexp_tuple ~loc es)))
         in
         let conj () =
           pexp_apply
             (pexp_ident (lident (string "&&&")))
             ((nolabel ** of_func helper)
             ^:: (nolabel ** of_func helper)
             ^:: nil)
           |> map2 ~f:(fun l r ->
                  let loc = Location.none in
                  let open Ast_builder.Default in
                  pexp_construct ~loc
                    (Located.mk ~loc (Lident "Conj"))
                    (Some (pexp_tuple ~loc [ l; r ])))
         in
         let fresh () =
           pexp_apply
             (pexp_ident (lident (string "fresh")))
             ((nolabel ** fresh_vars ()) ^:: many (nolabel ** of_func helper))
           |> map2 ~f:(fun (names : string list) conjs ->
                  let loc = Location.none in
                  let open Ast_builder.Default in
                  let names_repr =
                    List.fold_right names ~init:[%expr []] ~f:(fun x acc ->
                        [%expr
                          [%e pexp_ident ~loc (Located.mk ~loc (Lident x))]
                          :: [%e acc]])
                  in
                  pexp_construct ~loc
                    (Located.mk ~loc (Lident "Fresh"))
                    (Some (pexp_tuple ~loc (names_repr :: conjs))))
         in
         let call () =
           pexp_apply (pexp_ident (lident __)) (many (nolabel ** term ()))
           |> map2 ~f:(fun name args ->
                  let loc = Location.none in
                  let open Ast_builder.Default in
                  [%expr
                    Call
                      ( [%e pexp_constant ~loc (Pconst_string (name, loc, None))],
                        [%e
                          List.fold_right args ~init:[%expr []] ~f:(fun x acc ->
                              [%expr [%e x] :: [%e acc]])] )])
         in

         to_func
           (unify () ||| conj () ||| conde () ||| fresh () ||| call ())
           ctx loc e sk
       in
       helper)

  let%expect_test "conde1" =
    let loc = Location.none in
    Ast_pattern.parse (logic ()) loc
      [%expr conde [ x === []; xs === y % ys ]]
      (fun e -> Caml.Format.printf "%a\n%!" Pprintast.expression e)
      ~on_error:(fun () -> Caml.print_endline "error");
    [%expect
      {|
      Conde
        ((Unify ((Var "x"), (Constr ("[]", [])))),
          (Unify ((Var "xs"), (Constr ("Cons", [Var "y"; Var "ys"]))))) |}]

  let%expect_test "fresh1" =
    let loc = Location.none in
    Ast_pattern.parse (logic ()) loc
      [%expr conde [ xs === []; fresh (y ys) (xs === y % ys) ]]
      (fun e -> Caml.Format.printf "%a\n%!" Pprintast.expression e)
      ~on_error:(fun () -> Caml.print_endline "error");
    [%expect
      {|
      Conde
        ((Unify ((Var "xs"), (Constr ("[]", [])))),
          (Fresh
             ([y; ys],
               (Unify ((Var "xs"), (Constr ("Cons", [Var "y"; Var "ys"]))))))) |}]

  let%expect_test "call1" =
    let loc = Location.none in
    Ast_pattern.parse (logic ()) loc [%expr appendo ys zs tmp]
      (fun e -> Caml.Format.printf "%a\n%!" Pprintast.expression e)
      ~on_error:(fun () -> Caml.print_endline "error");
    [%expect {| Call ("appendo", [Var "ys"; Var "zs"; Var "tmp"]) |}]

  let%expect_test "pre_appendo" =
    let loc = Location.none in
    Ast_pattern.parse (logic ()) loc
      [%expr
        conde
          [
            xs === [] &&& (ys === zs);
            fresh (y ys tmp)
              (xs === y % ys)
              (zs === y % tmp)
              (appendo ys zs tmp);
          ]]
      (fun e -> Caml.Format.printf "%a\n%!" Pprintast.expression e)
      ~on_error:(fun () -> Caml.print_endline "error");
    [%expect
      {|
      Conde
        ((Conj
            ((Unify ((Var "xs"), (Constr ("[]", [])))),
              (Unify ((Var "ys"), (Var "zs"))))),
          (Fresh
             ([y; ys; tmp],
               (Unify ((Var "xs"), (Constr ("Cons", [Var "y"; Var "ys"])))),
               (Unify ((Var "zs"), (Constr ("Cons", [Var "y"; Var "tmp"])))),
               (Call ("appendo", [Var "ys"; Var "zs"; Var "tmp"]))))) |}]

  let main _ vbs =
    match vbs with
    | [] -> assert false
    | [ vb ] ->
        let loc = vb.pvb_loc in
        Ast_pattern.(
          parse (value_binding ~pat:(ppat_var __) ~expr:(lambdas (logic ()))))
          loc vb
          ~on_error:(fun () -> Location.raise_errorf ~loc "value binding")
          (fun name args rhs ->
            let open Ast_builder.Default in
            [
              [%stri
                let [%p
                      ppat_var ~loc
                        (Located.mk ~loc (Printf.sprintf "repr_%s" name))] =
                  Def
                    ( [%e
                        make_caml_list ~loc
                        @@ List.map
                             ~f:(fun s ->
                               pexp_constant ~loc (Pconst_string (s, loc, None)))
                             args],
                      [%e rhs] )];
            ])
    | _ :: _ :: _ -> assert false
end

let () =
  let extensions =
    let pattern =
      let open Ast_pattern in
      pstr (pstr_value __ __ ^:: nil)
    in

    [
      Extension.declare "deeper" Extension.Context.structure_item pattern
        (fun ~loc ~path flg vbs ->
          let open Ppxlib.Ast_builder.Default in
          let items = Rewrite.main flg vbs in
          pstr_include ~loc
            (include_infos ~loc
               (pmod_structure ~loc (List.cons (pstr_value ~loc flg vbs) items))));
    ]
  in
  Ppxlib.Driver.register_transformation ~extensions "deeper"
