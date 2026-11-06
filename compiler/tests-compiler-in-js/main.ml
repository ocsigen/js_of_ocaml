(* Float constants inside structured constants: when the compiler runs in
   JavaScript, they are unboxed numbers and must not be taken for
   integers. *)

type r =
  { name : string
  ; value : float
  }

let l =
  [ 1, 0.2888
  ; 2, -0.
  ; 3, 1e100
  ; 4, 3.
  ; 5, nan
  ; 6, infinity
  ; 7, 2147483648.
  ; 8, -2147483649.
  ; 9, 0.
  ]

let r = { name = "x"; value = 1.5 }

let () =
  List.iter (fun (i, f) -> Printf.printf "%d %h\n" i f) l;
  Printf.printf "%s %h\n" r.name r.value
