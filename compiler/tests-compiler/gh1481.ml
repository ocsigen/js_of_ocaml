let%expect_test _ =
  let prog =
    {|
type test = [ `B | `C | `D | `A ]

let to_string (tag : test) =
  match tag with
  | `A -> ("`A")
  | `B -> ("`B")
  | `C -> ("`C")
  | `D -> ("`D")

let correct x y =
  let z =
    match x, y with
    | (`A, v) | (v, `A) -> v
    | `B, _ | _, `B -> `B
    | `C, _ | _, `C -> `C
    | `D, `D -> `D
  in
  z

let incorrect x y =
  match x, y with
  | (`A, v) | (v, `A) -> v
  | `B, _ | _, `B -> `B
  | `C, _ | _, `C -> `C
  | `D, `D -> `D

let () =
  let a = `C in
  Printf.printf "[a] is: %s\n" (to_string a);

  let b = `A in
  Printf.printf "[b] is: %s\n" (to_string b);

  let c = correct a b in
  Printf.printf "[correct a b] is: %s\n" (to_string c);

  let d = incorrect a b in
  Printf.printf "[incorrect a b] is: %s\n" (to_string d);

  |}
  in
  Util.compile_and_run ~debug:false ~flags:[ "--disable"; "inline" ] prog;
  [%expect
    {|
    [a] is: `C
    [b] is: `A
    [correct a b] is: `C
    [incorrect a b] is: `C |}]
