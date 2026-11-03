The IR interpreter (--debug interp) runs the program after each optimization
pass and fails if its behavior changes. Its output should match node's.

  $ cat > test.ml <<EOF
  > (* Assign: a mutable variable updated in a try body, read in the handler *)
  > let assign () =
  >   let x = ref 0 in
  >   (try
  >      for i = 1 to 10 do
  >        x := !x + i;
  >        if i = 5 then raise Exit
  >      done
  >    with Exit -> Printf.printf "assign %d\n" !x);
  >   !x
  > 
  > (* Closures created in a loop capture the current value *)
  > let closures_in_loop () =
  >   let l = ref [] in
  >   for i = 1 to 3 do
  >     l := (fun () -> i) :: !l
  >   done;
  >   List.iter (fun f -> Printf.printf "loop %d\n" (f ())) !l
  > 
  > (* Mutually recursive closures *)
  > let rec even n = if n = 0 then true else odd (n - 1)
  > 
  > and odd n = if n = 0 then false else even (n - 1)
  > 
  > (* Partial and over-application *)
  > let add3 a b c = a + b + c
  > 
  > let curry () =
  >   let f = add3 1 in
  >   let g = f 2 in
  >   Printf.printf "partial %d %d\n" (g 3) (f 4 5);
  >   let h a = fun b -> a * b in
  >   Printf.printf "over %d\n" (h 6 7)
  > 
  > (* Recursive values *)
  > let rec ones = 1 :: twos
  > 
  > and twos = 2 :: ones
  > 
  > let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1)
  > 
  > (* Objects *)
  > class counter = object
  >   val mutable n = 0
  >   method incr = n <- n + 1
  >   method get = n
  > end
  > 
  > (* Lazy values *)
  > let lz = lazy (print_endline "forced"; 42)
  > 
  > (* Exceptions with arguments, nested handlers, reraise *)
  > exception E of int * string
  > 
  > let exns () =
  >   (try
  >      try raise (E (1, "a")) with Not_found -> print_endline "wrong"
  >    with E (n, s) -> Printf.printf "exn %d %s\n" n s);
  >   try ignore (List.assoc 3 [1, 2]) with Not_found -> print_endline "not found"
  > 
  > (* Integer overflow and division *)
  > let ints () =
  >   Printf.printf "%d %d %d\n" (max_int + 1) (min_int / -1) (-7 mod 3);
  >   try ignore (1 / (Sys.opaque_identity 0)) with Division_by_zero -> print_endline "div"
  > 
  > let () =
  >   ignore (assign ());
  >   closures_in_loop ();
  >   Printf.printf "%b %b\n" (even 10) (odd 7);
  >   curry ();
  >   Printf.printf "%d %d %d\n" (List.hd ones) (List.hd (List.tl ones)) (fact 10);
  >   let c = new counter in
  >   c#incr;
  >   c#incr;
  >   Printf.printf "obj %d\n" c#get;
  >   Printf.printf "lazy %d %d\n" (Lazy.force lz) (Lazy.force lz);
  >   exns ();
  >   ints ();
  >   let h = Hashtbl.create 16 in
  >   for i = 0 to 100 do
  >     Hashtbl.replace h (string_of_int i) i
  >   done;
  >   Printf.printf "hashtbl %d %d\n" (Hashtbl.find h "42") (Hashtbl.length h);
  >   Printf.printf "%s %.3f %g %h\n" (string_of_float 0.1) (sqrt 2.) 1e20 1.5;
  >   Printf.printf "%Ld %lx %nd\n" 12345678901234L 255l 42n;
  >   print_endline (String.concat "," (List.map string_of_int (List.sort compare [3; 1; 2])));
  >   Format.printf "@[<v>%a@]@." (Format.pp_print_list Format.pp_print_int) [1; 2; 3]
  > EOF

The program is compiled without -g, so that mutable variables used in
exception handlers are represented by [Assign] instructions.

  $ ocamlc test.ml -o test.bc

  $ js_of_ocaml --debug interp --debug interp-output test.bc -o test.js > interp.out 2> interp.log

  $ grep -v ': ok (' interp.log | sed 's/ (.*//'
  interp: input: finished

  $ node test.js > node.out

  $ diff interp.out node.out

  $ cat interp.out
  assign 15
  loop 3
  loop 2
  loop 1
  true true
  partial 6 10
  over 42
  1 2 3628800
  obj 2
  forced
  lazy 42 42
  exn 1 a
  not found
  -2147483648 -2147483648 -1
  div
  hashtbl 42 101
  0.1 1.414 1e+20 0x1.8p+0
  12345678901234 ff 42
  1,2,3
  1
  2
  3

Effect handlers: before the Effects pass, they are implemented with the
effect handlers of the compiler (OCaml 5); after it, the CPS code calls
a transcription of the JavaScript runtime.

  $ cat > effects.ml <<EOF
  > open Effect
  > open Effect.Deep
  > type _ Effect.t += Get : int Effect.t | Put : int -> unit Effect.t | Other : unit Effect.t
  > 
  > let state f (init : int) =
  >   let r = ref init in
  >   match_with f ()
  >     { retc = (fun x -> (x, !r));
  >       exnc = raise;
  >       effc = (fun (type a) (e : a Effect.t) : ((a, _) continuation -> _) option -> match e with
  >         | Get -> Some (fun k -> continue k !r)
  >         | Put n -> Some (fun k -> r := n; continue k ())
  >         | _ -> None) }
  > 
  > let () =
  >   let v, s = state (fun () ->
  >       for i = 1 to 10 do perform (Put (perform Get + i)) done; perform Get * 2) 0 in
  >   Printf.printf "%d %d\n" v s
  > 
  > (* nested handlers: inner does not handle Get, it is reperformed *)
  > let () =
  >   let res, _ = state (fun () ->
  >       try_with (fun () -> perform (Put 5); perform Get + 1) ()
  >         { effc = (fun (type a) (e : a Effect.t) -> match e with
  >             | Other -> Some (fun (k : (a, _) continuation) -> continue k ())
  >             | _ -> None) }) 0 in
  >   Printf.printf "nested %d\n" res
  > 
  > (* discontinue *)
  > exception Stop
  > let () =
  >   let r = try_with (fun () -> try perform Other; 0 with Stop -> 42) ()
  >       { effc = (fun (type a) (e : a Effect.t) -> match e with
  >           | Other -> Some (fun (k : (a, _) continuation) -> discontinue k Stop)
  >           | _ -> None) } in
  >   Printf.printf "discontinue %d\n" r
  > 
  > (* unhandled *)
  > let () =
  >   try perform Other with Effect.Unhandled _ -> print_endline "unhandled"
  > 
  > let () =
  >   let r = try_with (fun () -> try perform Get with Effect.Unhandled _ -> -1) ()
  >       { effc = (fun (type a) (e : a Effect.t) -> match e with
  >           | Other -> Some (fun (k : (a, _) continuation) -> continue k ())
  >           | _ -> None) } in
  >   Printf.printf "unhandled in handler %d\n" r
  > 
  > (* resumed twice *)
  > let () =
  >   let saved : (unit, unit) continuation option ref = ref None in
  >   (try_with (fun () -> perform Other; print_endline "resumed") ()
  >     { effc = (fun (type a) (e : a Effect.t) -> match e with
  >         | Other -> Some (fun (k : (a, _) continuation) -> saved := Some (k : (unit, unit) continuation); continue k ())
  >         | _ -> None) });
  >   match !saved with
  >   | Some k -> (try continue k () with Continuation_already_resumed -> print_endline "already")
  >   | None -> ()
  > 
  > (* shallow: generator *)
  > let () =
  >   let open Effect.Shallow in
  >   let gen () = for i = 1 to 3 do perform (Put i) done in
  >   let rec loop : type a. (a, unit) continuation -> a -> unit = fun k v ->
  >     continue_with k v
  >       { retc = (fun () -> print_endline "done");
  >         exnc = raise;
  >         effc = (fun (type b) (e : b Effect.t) -> match e with
  >           | Put n -> Some (fun (k : (b, unit) continuation) -> Printf.printf "got %d\n" n; loop k ())
  >           | _ -> None) } in
  >   loop (fiber gen) ()
  > EOF

  $ ocamlc effects.ml -o effects.bc

  $ js_of_ocaml --effects=cps --debug interp --debug interp-output effects.bc -o effects.js > interp.out 2> interp.log

  $ grep -v ': ok (' interp.log | sed 's/ (.*//'
  interp: input: finished

  $ grep -E ': (effects|lambda_lifting):' interp.log | sed 's/ (.*//'
  interp: effects: ok
  interp: lambda_lifting: ok

  $ node effects.js > node.out

  $ diff interp.out node.out

  $ js_of_ocaml --effects=double-translation --debug interp --debug interp-output effects.bc -o effects.js > interp.out 2> interp.log

  $ grep -v ': ok (' interp.log | sed 's/ (.*//'
  interp: input: finished

  $ grep -E ': (effects|lambda_lifting|generate_closure):' interp.log | sed 's/ (.*//'
  interp: effects: ok
  interp: lambda_lifting: ok
  interp: generate_closure: ok

  $ node effects.js > node.out

  $ diff interp.out node.out

  $ cat interp.out
  110 55
  nested 6
  discontinue 42
  unhandled
  unhandled in handler -1
  resumed
  already
  got 1
  got 2
  got 3
  done
