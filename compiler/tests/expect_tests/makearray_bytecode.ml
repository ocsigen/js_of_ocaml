open Util
module Jsoo = Js_of_ocaml_compiler
module J = Jsoo.Javascript

let print_var_decl program n =
  let {var_decls; _} =
    find_javascript
      ~var_decl:(function
        | J.S {name; _}, _ when name = n -> true
        | _ -> false)
      program
  in
  print_string (Format.sprintf "var %s = " n);
  match var_decls with
  | [(_, Some (expression, _))] -> print_string (expression_to_string expression)
  | _ -> print_endline "not found"

let%expect_test _ =
  let cmo =
    compile_ocaml_to_bytecode
      {|
    let lr = ref (List.init 2 Obj.repr)
    let black_box v = lr := (Obj.repr v) :: !lr

    type r = {x: int; y: string}

    let ex = {x = 5; y = "hello"} ;;
    black_box ex
    let ax = [|1;2;3;4|] ;;
    black_box ax
    let bx = [|1.0;2.0;3.0;4.0|] ;;
    black_box bx ;;

    print_int ((List.length !lr) + (List.length !lr))
  |}
  in
  let program = parse_js (print_compiled_js ~pretty:true cmo) in
  print_var_decl program "ex";
  print_var_decl program "ax";
  print_var_decl program "bx";
  [%expect
    {|
    var ex = new Block_0_2(5,caml_new_string("hello"));
    var ax = new Block_0_4(1,2,3,4);
    var bx = new Block_254_4(1,2,3,4); |}]
