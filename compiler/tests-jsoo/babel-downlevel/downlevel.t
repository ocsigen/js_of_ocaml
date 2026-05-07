Validates the recipe in manual/browser-compat.wiki: running js_of_ocaml
output through Babel + core-js + esbuild yields an ES5-compatible,
browser-ready bundle that is functionally equivalent to the baseline.

Resolve paths to the babel/esbuild/es-check binaries and core-js
installed at the repository root (via `npm install`). JSOO_ROOT is set
by the `make test-babel-downlevel` target and points to the source
root. A symlink is used so esbuild's module resolver can find core-js.

  $ BABEL="$JSOO_ROOT/node_modules/.bin/babel"
  $ ESBUILD="$JSOO_ROOT/node_modules/.bin/esbuild"
  $ ESCHECK="$JSOO_ROOT/node_modules/.bin/es-check"
  $ ln -s "$JSOO_ROOT/node_modules" node_modules

Write a test program exercising print, Hashtbl, Marshal, Weak (the last
covers the runtime's WeakRef feature-detection path), and a
closure-in-loop pattern that compiles to `let` per iteration — Babel
must rewrite the binding for ES5 because each closure captures its own
copy, not the final value.

  $ cat > test.ml <<'EOF'
  > let () =
  >   print_endline "hello";
  >   let h = Hashtbl.create 4 in
  >   Hashtbl.add h "a" 1;
  >   Hashtbl.add h "b" 2;
  >   Printf.printf "hashtbl: a=%d b=%d\n"
  >     (Hashtbl.find h "a") (Hashtbl.find h "b");
  >   let buf = Marshal.to_string (42, "x", [ 1; 2; 3 ]) [] in
  >   let (a, b, c) : int * string * int list = Marshal.from_string buf 0 in
  >   Printf.printf "marshal: %d %s [%s]\n" a b
  >     (String.concat ";" (List.map string_of_int c));
  >   let w = Weak.create 1 in
  >   Weak.set w 0 (Some "held");
  >   (match Weak.get w 0 with
  >    | Some s -> Printf.printf "weak: %s\n" s
  >    | None -> print_endline "weak: collected");
  >   let funs = ref [] in
  >   for i = 0 to 2 do funs := (fun () -> i) :: !funs done;
  >   List.iter (fun f -> Printf.printf "fn=%d " (f ())) (List.rev !funs);
  >   print_newline ()
  > EOF

`--pretty` keeps the generated JavaScript readable and emits `let` for
the per-iteration capture in the loop above (without it, the same
binding lowers to `var` plus a hoisted helper).

  $ ocamlc test.ml -o test.bc
  $ dune exec -- js_of_ocaml --pretty test.bc -o test.js 2>&1

Baseline (pre-Babel) output runs and produces expected results:



  $ node test.js 
  hello
  hashtbl: a=1 b=2
  marshal: 42 x [1;2;3]
  weak: held
  fn=0 fn=1 fn=2 

Transpile with Babel (targets ES5 to stress the full pipeline):

  $ cat > babel.config.json <<'EOF'
  > {
  >   "presets": [
  >     ["@babel/preset-env", {
  >       "targets": "ie 11",
  >       "useBuiltIns": "usage",
  >       "corejs": "3.6.5"
  >     }]
  >   ]
  > }
  > EOF
  $ NODE_PATH="$JSOO_ROOT/node_modules" \
  >   "$BABEL" test.js -o test.es5.js 2>&1 | grep -v "^Successfully" | cat

Bundle with esbuild (resolves core-js polyfill imports into a single
browser-ready file). The runtime contains `require("node:*")` calls
guarded by `fs_node_supported()` etc.; mark them external so esbuild
leaves them in place rather than failing to resolve them at bundle
time:

  $ "$ESBUILD" test.es5.js --bundle --platform=browser \
  >   --external:node:* \
  >   --target=es5 --log-level=error --outfile=test.bundle.js

Bundle is ES5-compatible:

  $ "$ESCHECK" es5 test.bundle.js 2>&1 | grep -oE 'passed![^$]*compatible\.'
  passed! All files are ES5 compatible.

Bundle runs and matches the baseline:

  $ node test.bundle.js
  hello
  hashtbl: a=1 b=2
  marshal: 42 x [1;2;3]
  weak: held
  fn=0 fn=1 fn=2 

Weak still works (as a strong ref) when WeakRef / FinalizationRegistry
are unavailable — this verifies the documented feature-detection path:

  $ node -e 'delete globalThis.WeakRef; delete globalThis.FinalizationRegistry; require("./test.bundle.js")'
  hello
  hashtbl: a=1 b=2
  marshal: 42 x [1;2;3]
  weak: held
  fn=0 fn=1 fn=2 
