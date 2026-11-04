The default methods of a mapper recurse through a record of closures
([sub.expr sub e]). With the type checker's hints, the record is
immutable, so the flow analysis knows its fields: inlining [expr] where
the mapper is known exposes a call to [expr] itself. Such functions must
not be inlined as functor-like code (at all their call sites), or each
round of optimization unrolls the recursion once more.

  $ cat > prog.ml << EOF
  > type e =
  >   | Const of int
  >   | Var of string
  >   | Add of e * e
  >   | Let of string * e * e
  > 
  > type mapper =
  >   { expr : mapper -> e -> e
  >   ; name : mapper -> string -> string
  >   }
  > 
  > let expr sub e =
  >   match e with
  >   | Const n -> Const n
  >   | Var s -> Var (sub.name sub s)
  >   | Add (a, b) -> Add (sub.expr sub a, sub.expr sub b)
  >   | Let (s, a, b) -> Let (sub.name sub s, sub.expr sub a, sub.expr sub b)
  > 
  > let name _ s = s
  > 
  > let default = { expr; name }
  > 
  > let rename = { default with name = (fun _ s -> s ^ "'") }
  > 
  > let double =
  >   { default with
  >     expr = (fun sub e -> match e with Const n -> Const (2 * n) | e -> default.expr sub e)
  >   }
  > 
  > let rec to_string e =
  >   match e with
  >   | Const n -> string_of_int n
  >   | Var s -> s
  >   | Add (a, b) -> "(" ^ to_string a ^ " + " ^ to_string b ^ ")"
  >   | Let (s, a, b) -> "let " ^ s ^ " = " ^ to_string a ^ " in " ^ to_string b
  > 
  > let () =
  >   let e = Let ("x", Add (Const 1, Const 2), Add (Var "x", Const 3)) in
  >   print_endline (to_string (rename.expr rename e));
  >   print_endline (to_string (double.expr double e))
  > EOF
  $ ocamlc -g -bytecode-hints prog.ml -o prog.bc

[expr] is not inlined:

  $ wasm_of_ocaml --opt 2 --pretty --debug inlining prog.bc -o prog.js 2>&1 \
  >   | grep '{expr}(expr): true' | wc -l
  0
  $ node prog.js
  let x' = (1 + 2) in (x' + 3)
  let x = (2 + 4) in (x + 6)
