module Jsoo = Js_of_ocaml_compiler
module J = Jsoo.Javascript

type finder_fun =
  { expression : J.expression -> unit
  ; statement : J.statement -> unit
  ; variable_decl : J.variable_declaration -> unit }

class finder ff =
  object
    inherit Jsoo.Js_traverse.map as super

    method! variable_declaration v =
      ff.variable_decl v;
      super#variable_declaration v

    method! expression x =
      ff.expression x;
      super#expression x

    method! statement s =
      ff.statement s;
      super#statement s
  end

exception
  Found of
    [ `Expression of J.expression
    | `Statement of J.statement
    | `Var_decl of J.variable_declaration ]

let find
    program
    ?(expression = fun _ -> false)
    ?(statement = fun _ -> false)
    ?(variable_decl = fun _ -> false)
    () =
  let b v c x = if v x then raise (Found (c x)) else () in
  let expression = b expression (fun a -> `Expression a) in
  let statement = b statement (fun a -> `Statement a) in
  let variable_decl = b variable_decl (fun a -> `Var_decl a) in
  let t = {expression; statement; variable_decl} in
  let trav = new finder t in
  try
    ignore (trav#program program);
    None
  with Found a -> Some a

let expression_to_string ?(compact = false) e =
  let e = [J.Statement (J.Expression_statement e), J.N] in
  let buffer = Buffer.create 17 in
  let pp = Jsoo.Pretty_print.to_buffer buffer in
  Jsoo.Pretty_print.set_compact pp compact;
  Jsoo.Js_output.program pp e;
  Buffer.contents buffer

let print_translated cmo_channel =
  let buffer = Buffer.create 100 in
  let pp = Jsoo.Pretty_print.to_buffer buffer in
  Jsoo.Config.Flag.disable "genprim";
  Jsoo.Config.Flag.disable "shortvar";
  Jsoo.Config.Flag.disable "share";
  Jsoo.Config.Flag.disable "excwrap";
  Jsoo.Config.Flag.disable "genprim";
  Jsoo.Config.Flag.enable "pretty";
  Jsoo.Stdlib.quiet := true;
  let program, _, debug_data, _ =
    Jsoo.Parse_bytecode.from_channel ~debug:`Names cmo_channel
  in
  Jsoo.Driver.f pp debug_data program;
  Jsoo.Parse_js.(parse (lexer_from_string (Buffer.contents buffer)))

let compile_ocaml_to_bytecode source =
  let temp_file = Filename.temp_file "jsoo_test" ".ml" in
  let out = open_out temp_file in
  Printf.fprintf out "%s" source;
  close_out out;
  let proc =
    Unix.open_process
      (Format.sprintf "ocamlfind ocamlc -g %s -I Stdlib -o %s.cmo" temp_file temp_file)
  in
  (match Unix.close_process proc with
  | WEXITED 0 -> ()
  | _ -> failwith "compilation failed");
  open_in (Format.sprintf "%s.cmo" temp_file)

let find_var_decl program n =
  let found =
    find
      program
      ~variable_decl:(function
        | J.S {name; _}, _ when name = n -> true
        | _ -> false)
      ()
  in
  match found with
  | Some (`Var_decl v) -> Some v
  | None -> None
  | Some _ -> assert false

let print_var_decl program name =
  print_string (Format.sprintf "var %s = " name);
  match find_var_decl program name with
  | Some (_, Some (expression, _)) -> print_string (expression_to_string expression)
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
    black_box bx

    ;;

    print_int ((List.length !lr) + (List.length !lr))
  |}
  in
  let program = print_translated cmo in
  print_var_decl program "ex";
  print_var_decl program "ax";
  print_var_decl program "bx";
  [%expect {|
    var ex = new Block_0_2(5,caml_new_string("hello"));
    var ax = new Block_0_4(1,2,3,4);
    var bx = new Block_254_4(1,2,3,4); |}]
