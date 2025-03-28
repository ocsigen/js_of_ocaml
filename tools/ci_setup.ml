module StringSet = Set.Make (String)

(****)

let jane_root, wasmoo_root =
  match Sys.argv with
  | [| _; jane_root; wasmoo_root |] -> jane_root, wasmoo_root
  | _ -> "janestreet", "wasm_of_ocaml"

let repo = Filename.concat jane_root "opam-repository/packages"

let roots = [ "bonsai_web_components"; "string_dict"; "ppx_html"; "bonsai_bench" ]

let omitted_others = StringSet.of_list []

let omitted_js = StringSet.of_list [ "sexplib0" ]

let do_pin =
  StringSet.of_list
    [ "base"
    ; "ppx_expect"
    ; "ppx_inline_test"
    ; "time_now"
    ; "ocaml_intrinsics_kernel"
    ; "bigstringaf"
    ]

let forked_packages =
  StringSet.of_list
    [ "async_js"
    ; "base"
    ; "base_bigstring"
    ; "bin_prot"
    ; "bonsai"
    ; "bonsai_test"
    ; "bonsai_web_components"
    ; "bonsai_web_test"
    ; "core"
    ; "core_kernel"
    ; "ocaml_intrinsics_kernel"
    ; "ppx_expect"
    ; "ppx_inline_test"
    ; "ppx_module_timer"
    ; "string_dict"
    ; "time_now"
    ; "virtual_dom"
    ; "virtual_dom_toplayer"
    ; "zarith_stubs_js"
    ]

let dune_workspace =
  {|(lang dune 3.17)
(env
 (_
  (env-vars (TESTING_FRAMEWORK inline-test))
  (js_of_ocaml (enabled_if false))
  (flags :standard -alert -all -warn-error -7-8-27-30-32-34-37-49-52-55 -w -7-27-30-32-34-37-49-52-55-58-67-69)))
|}

let node_wrapper =
  [ ( "node_wrapper/dune"
    , {|(executable
 (public_name node)
 (name node_wrapper)
       (libraries unix))|} )
  ; "node_wrapper/node_wrapper_per_profile.ml", {|let args = []|}
  ; "node_wrapper/dune-project", "(lang dune 3.17)"
  ; "node_wrapper/node_wrapper.opam", ""
  ]

let patches =
  [ ( "sexp_grammar"
    , {|
diff --git a/sexp_grammar_validation.opam b/sexp_grammar_validation.opam
new file mode 100644
index 0000000..e69de29
diff --git a/validation/src/dune b/validation/src/dune
index 91933ec..849e4d7 100644
--- a/validation/src/dune
+++ b/validation/src/dune
@@ -1,5 +1,6 @@
 (library
  (name sexp_grammar_validation)
+ (public_name sexp_grammar_validation)
  (libraries bignum.bigint core
    expect_test_helpers_core.expect_test_helpers_base sexp_grammar)
  (preprocess
|}
    )
  ; ( "bignum"
    , {bignum|
diff --git a/dune-project b/dune-project
index e563d7e..b87e356 100644
--- a/dune-project
+++ b/dune-project
@@ -1 +1 @@
-(lang dune 3.11)
+(lang dune 3.17)
diff --git a/test/src/dune b/test/src/dune
index f93ae3f..3f00557 100644
--- a/test/src/dune
+++ b/test/src/dune
@@ -2,5 +2,6 @@
  (name bignum_test)
  (libraries bigint bignum core expect_test_helpers_core
    sexp_grammar_validation zarith)
+ (inline_tests (flags -drop-tag no-js -drop-tag no-wasm -drop-tag 64-bits-only) (modes js wasm))
  (preprocess
   (pps ppx_jane)))
diff --git a/test/src/test_bignum.ml b/test/src/test_bignum.ml
index c6d09fb..61b1e5b 100644
--- a/test/src/test_bignum.ml
+++ b/test/src/test_bignum.ml
@@ -3,6 +3,11 @@ open! Expect_test_helpers_core
 open Bignum
 open Bignum.For_testing
 
+module Zarith = struct
+  module Q = Q
+  module Z = Z
+end
+
 let%expect_test "Bignum.abs" =
   let test t =
     let t' = require_no_allocation (fun () -> abs t) in
|bignum}
    )
  ; ( "bin_prot"
    , {|
diff --git a/test/dune b/test/dune
index 6c0ef2f..9968f59 100644
--- a/test/dune
+++ b/test/dune
@@ -5,11 +5,3 @@
    float_array base.md5 sexplib splittable_random stdio)
  (preprocess
   (pps ppx_jane)))
-
-(rule
- (alias runtest)
- (deps core/blob_stability_tests.ml integers_repr_tests_64bit.ml
-   integers_repr_tests_js.ml integers_repr_tests_wasm.ml)
- (action
-  (bash
-    "diff <(\necho '869e6b3143f14201f406eac9c05c4cdb  core/blob_stability_tests.ml'\necho '2db396dfced6ae8d095f308acb4c80eb  integers_repr_tests_64bit.ml'\necho '9f7b6332177a4ae9547d37d17008d7ef  integers_repr_tests_js.ml'\necho '22f653bfba79ce30c22fe378c596df54  integers_repr_tests_wasm.ml'\n  ) <(md5sum %{deps})")))
    |}
    )
  ; ( "base_bigstring"
    , {|
diff --git a/src/base_bigstring_stubs.c b/src/base_bigstring_stubs.c
index 164c393..6cf4835 100644
--- a/src/base_bigstring_stubs.c
+++ b/src/base_bigstring_stubs.c
@@ -17,6 +17,50 @@
 #include <assert.h>
 #include <stdint.h>
 
+
+static inline void * mymemrchr(const void * s, int c, size_t n)
+{
+  const unsigned char * p = (const unsigned char *)s + n;
+
+  while (n--) {
+    if (*(--p) == (unsigned char) c) {
+      return (void *)p;
+    }
+  }
+
+  return NULL;
+}
+static inline void *mymemmem(const void *haystack, size_t haystack_len,
+                const void *needle, size_t needle_len)
+{
+	const char *begin = haystack;
+	const char *last_possible = begin + haystack_len - needle_len;
+	const char *tail = needle;
+	char point;
+
+	/*
+	 * The first occurrence of the empty string is deemed to occur at
+	 * the beginning of the string.
+	 */
+	if (needle_len == 0)
+		return (void *)begin;
+
+	/*
+	 * Sanity check, otherwise the loop might search through the whole
+	 * memory.
+	 */
+	if (haystack_len < needle_len)
+		return NULL;
+
+	point = *tail++;
+	for (; begin <= last_possible; begin++) {
+		if (*begin == point && !memcmp(begin + 1, tail, needle_len - 1))
+			return (void *)begin;
+	}
+
+	return NULL;
+}
+
 #ifdef __APPLE__
 #include <libkern/OSByteOrder.h>
 #define bswap_16 OSSwapInt16
@@ -239,7 +283,7 @@ CAMLprim value bigstring_rfind(value v_str, value v_needle,
   char *start, *r;
 
   start = get_bstr(v_str, v_pos);
-  r = (char*) memrchr(start, Int_val(v_needle), Long_val(v_len));
+  r = (char*) mymemrchr(start, Int_val(v_needle), Long_val(v_len));
 
   return ptr_to_offset(start, v_pos, r);
 }
@@ -250,7 +294,7 @@ CAMLprim value bigstring_memmem(value v_haystack, value v_needle,
 {
   const char *haystack = get_bstr(v_haystack, v_haystack_pos);
   const char *needle = get_bstr(v_needle, v_needle_pos);
-  const char *result = memmem(haystack, Long_val(v_haystack_len),
+  const char *result = mymemmem(haystack, Long_val(v_haystack_len),
                               needle, Long_val(v_needle_len));
 
   return ptr_to_offset(haystack, v_haystack_pos, result);
|}
    )
  ]

let removes =
  [ "core/core/test/test_sys.ml"
  ; "core/core/test/test_sys.mli"
  ; "core/core/test/test_timezone.ml"
  ; "core/core/test/test_timezone.mli"
  ]
(****)

let read_opam_file filename =
  OpamPp.parse
    OpamPp.Op.(OpamFormat.I.file -| OpamPp.map_snd OpamFile.OPAM.pp_raw_fields)
    ~pos:{ filename; start = 0, 0; stop = 0, 0 }
    (OpamParser.FullPos.file (Filename.concat (Filename.concat repo filename) "opam"))

let dependencies (_, { OpamFile.OPAM.depends; _ }) =
  let open OpamFormula in
  depends
  |> map (fun (nm, _) -> Atom (nm, None))
  |> of_atom_formula
  |> atoms
  |> List.map fst
  |> List.map OpamPackage.Name.to_string

let packages =
  repo
  |> Sys.readdir
  |> Array.to_list
  |> List.map (fun s -> String.sub s 0 (String.index s '.'), read_opam_file s)

let rec traverse visited p =
  if StringSet.mem p visited
  then visited
  else
    let visited = StringSet.add p visited in
    match List.assoc p packages with
    | exception Not_found -> visited
    | opam ->
        let l = dependencies opam in
        List.fold_left traverse visited l

let is_forked p = StringSet.mem p forked_packages

let exec_async cmd =
  let p = Unix.open_process_out cmd in
  fun () -> ignore (Unix.close_process_out p)

let ( let* ) (f : unit -> 'a) (g : 'a -> unit -> 'b) : unit -> 'b = fun () -> g (f ()) ()

let sync_exec f l =
  let l = List.map f l in
  List.iter (fun f -> f ()) l

let pin nm =
  exec_async
    (Printf.sprintf
       "opam pin add -n %s https://github.com/ocaml-wasm/%s.git#wasm-v0.18"
       nm
       nm)

let pin_packages () = sync_exec pin (StringSet.elements do_pin)

let install_others others =
  let others = StringSet.elements (StringSet.diff others omitted_others) in
  ignore (Sys.command ("opam install -y " ^ String.concat " " others))

let clone ?branch ?(depth = 1) nm src =
  exec_async
    (Printf.sprintf
       "git clone -q --depth %d %s%s %s/lib/%s"
       depth
       (match branch with
       | None -> ""
       | Some b -> Printf.sprintf "-b %s " b)
       src
       jane_root
       nm)

let clone' ?branch ?commit nm src =
  match commit with
  | None -> clone ?branch nm src
  | Some commit ->
      let* () = clone ?branch ~depth:100 nm src in
      exec_async
        (Printf.sprintf "cd %s/lib/%s && git checkout -b wasm %s" jane_root nm commit)

let () =
  let write f contents =
    Out_channel.(with_open_bin f @@ fun ch -> output_string ch contents)
  in
  let copy f f' =
    let contents = In_channel.(with_open_bin f @@ input_all) in
    Out_channel.(with_open_bin f' @@ fun ch -> output_string ch contents)
  in
  write (Filename.concat jane_root "dune-workspace") dune_workspace;
  Unix.mkdir (Filename.concat jane_root "node_wrapper") 0o755;
  List.iter
    (fun (f, contents) -> write (Filename.concat jane_root f) contents)
    node_wrapper;
  copy
    (Filename.concat wasmoo_root "tools/node_wrapper.ml")
    (Filename.concat jane_root "node_wrapper/node_wrapper.ml")

let () =
  let js, others =
    List.fold_left traverse StringSet.empty roots
    |> StringSet.partition (fun p -> List.mem_assoc p packages)
  in
  pin_packages ();
  install_others others;
  sync_exec (fun () -> clone "ocaml-uri" "https://github.com/mirage/ocaml-uri") [ () ];
  sync_exec (fun () -> exec_async "opam install uri --deps-only") [ () ];
  sync_exec
    (fun nm ->
      let branch = if is_forked nm then Some "wasm-v0.18" else None in
      let commit =
        if is_forked nm
        then None
        else
          Some
            (let _, opam = List.assoc nm packages in
             let url = OpamUrl.to_string (Option.get (OpamFile.OPAM.get_url opam)) in
             let tar_file = Filename.basename url in
             String.sub tar_file 0 (String.index tar_file '.'))
      in
      clone'
        ?branch
        ?commit
        nm
        (Printf.sprintf
           "https://github.com/%s/%s"
           (if is_forked nm then "ocaml-wasm" else "janestreet")
           nm))
    (StringSet.elements (StringSet.diff js omitted_js))

let () =
  List.iter
    (fun (dir, patch) ->
      let p = if Sys.win32 then "patch --binary" else "patch" in
      let ch =
        Unix.open_process_out
          (Printf.sprintf "cd %s/lib/%s && %s -p 1 --" jane_root dir p)
      in
      let patch =
        if Sys.win32
        then String.concat "\r\n" (String.split_on_char '\n' patch)
        else patch
      in
      output_string ch patch;
      match Unix.close_process_out ch with
      | WEXITED 0 -> ()
      | e ->
          let name, i =
            match e with
            | WEXITED n -> "exit", n
            | WSIGNALED n -> "signal", n
            | WSTOPPED n -> "stop", n
          in
          failwith (Printf.sprintf "%s %d while patching %s" name i dir))
    patches;
  List.iter (fun p -> Sys.remove (Printf.sprintf "%s/lib/%s" jane_root p)) removes
