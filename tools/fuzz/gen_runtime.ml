(* Random programs with user-supplied runtime files.

   The runtime files use //Provides (with each kind), //Requires (chains
   across files), //Alias, //Weakdef, //If, //Version, //Always,
   //Deprecated, macros (FIELD, BLOCK, ISBLOCK, TAG, LENGTH, FLAG, CONFIG),
   named values and builtin runtime primitives. Primitive bodies also
   compare random JavaScript expressions with [eval] of their source.

   The oracle is the same program where each primitive is an OCaml function
   with the same semantics, run with ocamlrun; it reads the configuration
   from its command line (see [oracle_args]). Modelled linker semantics:
   a definition is ignored when its //If or //Version condition fails, or
   when it is specific to another target environment; a definition specific
   to the target environment beats the isomorphic ones; otherwise the last
   definition loaded wins (//Weakdef only silences the warning). //Always
   fragments run at load time, in load order. *)

let sprintf = Printf.sprintf

let mask = "0xffff"

let prelude =
  {|let t n f = print_endline (Printf.sprintf "%d: %s" n (try f () with
  | Failure s -> "Failure " ^ s | Invalid_argument s -> "Invalid_argument " ^ s
  | Not_found -> "Not_found" | e -> "exception " ^ Printexc.to_string e))
let my_cb_impl x y = (x * 7 + y) land 0xffff
let () = Callback.register "my_cb" my_cb_impl
|}

(* JavaScript-only primitives: name, OCaml type, JavaScript definition,
   OCaml model *)
let js_only =
  [ ( "my_sum_list"
    , "int list -> int"
    , "//Provides: my_sum_list const\n\
       function my_sum_list(l) {\n\
      \  var s = 0;\n\
      \  while (ISBLOCK(l)) { s = (s + FIELD(l, 0)) & 0xffff; l = FIELD(l, 1); }\n\
      \  return s;\n\
       }\n"
    , "let my_sum_list l = List.fold_left (fun s x -> (s + x) land 0xffff) 0 l" )
  ; ( "my_pair"
    , "int -> int -> int * int"
    , "//Provides: my_pair const\nfunction my_pair(a, b) { return BLOCK(0, a, b); }\n"
    , "let my_pair a b = (a, b)" )
  ; ( "my_opt_get"
    , "int option -> int -> int"
    , "//Provides: my_opt_get const\n\
       function my_opt_get(o, d) { return ISBLOCK(o) ? FIELD(o, 0) : d; }\n"
    , "let my_opt_get o d = match o with Some x -> x | None -> d" )
  ; ( "my_tag"
    , "Obj.t -> int"
    , "//Provides: my_tag const\n\
       function my_tag(v) { return ISBLOCK(v) ? TAG(v) * 100 + LENGTH(v) : v; }\n"
    , "let my_tag (v : Obj.t) = if Obj.is_block v then Obj.tag v * 100 + Obj.size v else \
       (Obj.obj v : int)" )
  ; ( "my_flag"
    , "unit -> int"
    , "//Provides: my_flag const\n\
       function my_flag() { return FLAG(\"pretty\") * 2 + FLAG(\"debuginfo\"); }\n"
    , "let my_flag () = (if cfg_pretty then 2 else 0) + (if cfg_debuginfo then 1 else 0)"
    )
  ; ( "my_config"
    , "unit -> int"
    , "//Provides: my_config const\n\
       function my_config() { return CONFIG(\"effects\").length; }\n"
    , "let my_config () = String.length cfg_effects_name" )
  ; ( "my_callback"
    , "int -> int"
    , "//Provides: my_callback\n\
       //Requires: caml_named_value, caml_callback\n\
       function my_callback(x) {\n\
      \  var f = caml_named_value(\"my_cb\");\n\
      \  return caml_callback(f, [x, (x + 1) & 0xffff]);\n\
       }\n"
    , "let my_callback x = my_cb_impl x ((x + 1) land 0xffff)" )
  ; ( "my_check"
    , "int -> int"
    , "//Provides: my_check\n\
       //Requires: caml_failwith, caml_invalid_argument\n\
       function my_check(x) {\n\
      \  if (x > 200) caml_failwith(\"too big\");\n\
      \  if (x < 0) caml_invalid_argument(\"negative\");\n\
      \  return x;\n\
       }\n"
    , "let my_check x = if x > 200 then failwith \"too big\" else if x < 0 then \
       invalid_arg \"negative\" else x" )
  ; ( "my_strlen"
    , "string -> int"
    , "//Provides: my_strlen const\n\
       //Requires: caml_jsstring_of_string\n\
       function my_strlen(s) { return caml_jsstring_of_string(s).length; }\n"
    , "let my_strlen s = String.length s" )
  ; ( "my_concat"
    , "string -> string -> string"
    , "//Provides: my_concat const\n\
       //Requires: caml_jsstring_of_string, caml_string_of_jsstring\n\
       function my_concat(a, b) {\n\
      \  return caml_string_of_jsstring(caml_jsstring_of_string(a) + \"-\" + \
       caml_jsstring_of_string(b));\n\
       }\n"
    , "let my_concat a b = a ^ \"-\" ^ b" )
  ]

type variant =
  { cond : string  (** OCaml condition under which the definition is loaded *)
  ; text : string  (** the fragment, to find its position *)
  ; model : string  (** OCaml model of the body *)
  ; env_specific : bool
  }

type prim =
  { name : string
  ; arity : int
  ; aliases : string list
  ; mutable variants : variant list
  }

type t =
  { st : Rng.t
  ; nfiles : int
  ; files : string list array  (** fragments of each runtime file *)
  ; mutable helpers : (string * int) list  (** pure helpers, callable by later code *)
  ; mutable models : string list  (** OCaml models of the helpers, in order *)
  ; mutable always : (string * string) list  (** //Always fragments and their models *)
  ; mutable prims : prim list
  ; mutable init_state : int
  }

let pick g l = Rng.pick g.st l

let chance g p = Rng.chance g.st p

let int g lo hi = Rng.range g.st lo hi

(* Insert a fragment at a random position of a random file *)
let add g text =
  let f = int g 0 (g.nfiles - 1) in
  let l = g.files.(f) in
  let i = int g 0 (List.length l) in
  g.files.(f) <-
    List.filteri (fun j _ -> j < i) l @ [ text ] @ List.filteri (fun j _ -> j >= i) l

let load_pos g text =
  let rec find f =
    if f = g.nfiles
    then raise Not_found
    else
      let rec idx i = function
        | [] -> None
        | t :: r -> if t == text then Some i else idx (i + 1) r
      in
      match idx 0 g.files.(f) with
      | Some i -> f, i
      | None -> find (f + 1)
  in
  find 0

(* An integer expression in JavaScript and in OCaml. Values stay within 16
   bits, so that JavaScript doubles and OCaml integers agree. *)
let rec expr g d args state =
  if d <= 0 || chance g 0.25
  then
    let k = Random.State.float g.st 1. in
    if args <> [] && k < 0.6
    then
      let a = pick g args in
      a, a
    else if state && k < 0.75
    then "my_state.v", "!my_state"
    else
      let c = string_of_int (int g 0 300) in
      c, c
  else
    let e () = expr g (d - 1) args state in
    let a = e () in
    let b = e () in
    match int g 0 (if g.helpers <> [] then 6 else 5) with
    | 0 ->
        ( sprintf "((%s + %s) & %s)" (fst a) (fst b) mask
        , sprintf "((%s + %s) land %s)" (snd a) (snd b) mask )
    | 1 ->
        ( sprintf "((%s - %s) & %s)" (fst a) (fst b) mask
        , sprintf "((%s - %s) land %s)" (snd a) (snd b) mask )
    | 2 ->
        ( sprintf "((%s * %s) & %s)" (fst a) (fst b) mask
        , sprintf "((%s * %s) land %s)" (snd a) (snd b) mask )
    | 3 ->
        let s = int g 0 5 in
        ( sprintf "((%s << %d) & %s)" (fst a) s mask
        , sprintf "((%s lsl %d) land %s)" (snd a) s mask )
    | 4 ->
        let c = e () in
        let dd = e () in
        ( sprintf "(%s > %s ? %s : %s)" (fst a) (fst b) (fst c) (fst dd)
        , sprintf "(if %s > %s then %s else %s)" (snd a) (snd b) (snd c) (snd dd) )
    | 5 ->
        ( sprintf "((%s ^ %s) & %s)" (fst a) (fst b) mask
        , sprintf "((%s lxor %s) land %s)" (snd a) (snd b) mask )
    | _ ->
        let name, ar = pick g g.helpers in
        let xs = [ a; b ] @ List.init (ar - 2) (fun _ -> e ()) in
        ( sprintf "%s(%s)" name (String.concat ", " (List.map fst xs))
        , sprintf "(%s %s)" name (String.concat " " (List.map snd xs)) )

let requires g js =
  let req =
    List.filter_map
      (fun (h, _) -> if Harness.contains js (h ^ "(") then Some h else None)
      g.helpers
  in
  List.sort_uniq
    compare
    (if Harness.contains js "my_state" then "my_state" :: req else req)

(* Statements comparing a random JavaScript expression with [eval] of its
   source: the compiler parses, renames and prints runtime files *)
let js_check g =
  let src = Gen_js.num g.st (pick g [ 1; 2 ]) [] in
  sprintf
    "  var junk = (function () { try { return %s } catch (e) { return \"throws\" } })();\n\
    \  var junk2 = (function () { try { return eval?.(%s) } catch (e) { return \
     \"throws\" } })();\n\
    \  if (!(Object.is(junk, junk2) || String(junk) === String(junk2))) return 99999;\n"
    src
    (Gen_js.string_literal src)

let gen_helpers g =
  for i = 0 to int g 0 4 - 1 do
    let name = sprintf "my_helper%d" i in
    let ar = int g 2 3 in
    let args = List.init ar (sprintf "x%d") in
    let js, ml = expr g 2 args false in
    let req = requires g js in
    let text =
      sprintf "//Provides: %s%s\n" name (pick g [ ""; " const"; " pure" ])
      ^ (if req = [] then "" else sprintf "//Requires: %s\n" (String.concat ", " req))
      ^
      match int g 0 2 with
      | 0 ->
          sprintf
            "function %s(%s) {\n%s  return %s;\n}\n"
            name
            (String.concat ", " args)
            (if chance g 0.3 then js_check g else "")
            js
      | 1 ->
          sprintf
            "var %s = function (%s) { return %s; };\n"
            name
            (String.concat ", " args)
            js
      | _ -> sprintf "var %s = (%s) => %s;\n" name (String.concat ", " args) js
    in
    add g text;
    g.models <- g.models @ [ sprintf "let %s %s = %s" name (String.concat " " args) ml ];
    g.helpers <- g.helpers @ [ name, ar ]
  done

let gen_state g =
  g.init_state <- int g 0 100;
  add g (sprintf "//Provides: my_state\nvar my_state = { v: %d };\n" g.init_state);
  for _ = 1 to int g 0 2 do
    let js, ml = expr g 2 [] true in
    let req = List.sort_uniq compare ("my_state" :: requires g js) in
    let text =
      sprintf "//Always\n//Requires: %s\nmy_state.v = %s;\n" (String.concat ", " req) js
    in
    add g text;
    g.always <- g.always @ [ text, ml ]
  done

let gen_int_prim g i =
  let name = sprintf "my_prim%d" i in
  let arity = int g 1 3 in
  let args = List.init arity (sprintf "a%d") in
  let kind = pick g [ "mutator"; "const"; "mutable" ] in
  let aliases = if chance g 0.2 then [ name ^ "_alias" ] else [] in
  let p = { name; arity; aliases; variants = [] } in
  let emit cond annots =
    let js, ml = expr g (pick g [ 1; 2; 3 ]) args (kind <> "const") in
    let pre_js, pre_ml =
      if kind = "mutator" && chance g 0.7
      then
        let ujs, uml = expr g 2 args true in
        sprintf "  my_state.v = %s;\n" ujs, sprintf "my_state := %s; " uml
      else "", ""
    in
    let check = if chance g 0.3 then js_check g else "" in
    let req = requires g (js ^ pre_js) in
    let body = check ^ pre_js ^ sprintf "  return %s;\n" js in
    let text =
      sprintf
        "//Provides: %s%s\n"
        name
        (match kind with
        | "const" -> " const"
        | "mutable" -> " mutable"
        | _ -> "")
      ^ (if req = [] then "" else sprintf "//Requires: %s\n" (String.concat ", " req))
      ^ String.concat
          ""
          (List.map
             (fun a -> a ^ "\n")
             (annots @ List.map (sprintf "//Alias: %s") aliases))
      ^
      if chance g 0.5
      then sprintf "function %s(%s) {\n%s}\n" name (String.concat ", " args) body
      else sprintf "var %s = function (%s) {\n%s};\n" name (String.concat ", " args) body
    in
    add g text;
    p.variants <-
      p.variants
      @ [ { cond
          ; text
          ; model = pre_ml ^ ml
          ; env_specific =
              Harness.contains text "//If: nodejs"
              || Harness.contains text "//If: browser"
          }
        ]
  in
  let k = Random.State.float g.st 1. in
  if k < 0.15
  then (
    let flag = pick g [ "effects"; "wasm"; "doubletranslate" ] in
    emit ("cfg_" ^ flag) [ "//If: " ^ flag ];
    emit ("not cfg_" ^ flag) [ "//If: !" ^ flag ])
  else if k < 0.25
  then (
    emit "true" [ "//Version: >= 4.14" ];
    emit "false" [ "//Version: < 4.14" ])
  else if k < 0.35
  then
    for _ = 1 to int g 2 3 do
      emit "true" (if chance g 0.5 then [ "//Weakdef" ] else [])
    done
  else if k < 0.45
  then (
    let env = pick g [ "nodejs"; "browser" ] in
    emit (sprintf "cfg_env = %S" env) [ "//If: " ^ env ];
    emit "true" [])
  else emit "true" (if chance g 0.1 then [ "//Deprecated: use something else" ] else []);
  g.prims <- g.prims @ [ p ]

let rec arg g d wasm_ok =
  if d <= 0 || chance g 0.3
  then
    let v = int g (-50) 300 in
    if v < 0 then sprintf "(%d)" v else string_of_int v
  else if (not wasm_ok) && chance g 0.2
  then js_call g d
  else
    let p = pick g g.prims in
    sprintf
      "(%s %s)"
      (pick g (p.name :: p.aliases))
      (String.concat " " (List.init p.arity (fun _ -> arg g (d - 1) wasm_ok)))

and js_call g d =
  let a () = arg g (d - 1) false in
  match int g 0 9 with
  | 0 -> sprintf "(my_sum_list [%s; %s; %s])" (a ()) (a ()) (a ())
  | 1 -> sprintf "(let (x, y) = my_pair %s %s in (x - y) land 0xffff)" (a ()) (a ())
  | 2 -> sprintf "(my_opt_get (if %s > 100 then Some %s else None) 7)" (a ()) (a ())
  | 3 ->
      sprintf
        "(my_tag (Obj.repr (Some %s)) + my_tag (Obj.repr %s) + my_tag (Obj.repr [| 1; 2; \
         3 |]))"
        (a ())
        (a ())
  | 4 -> "(my_flag ())"
  | 5 -> "(my_config ())"
  | 6 -> sprintf "(my_callback %s)" (a ())
  | 7 ->
      sprintf
        "(try my_check %s with Failure _ -> 1000 | Invalid_argument _ -> 2000)"
        (a ())
  | 8 -> sprintf "(my_strlen (String.make (%s land 7) 'x') + my_strlen \"hey\")" (a ())
  | _ -> sprintf "(String.length (my_concat (string_of_int %s) \"ab\"))" (a ())

let test_expr g wasm_ok =
  let e () = arg g (pick g [ 1; 2; 3 ]) wasm_ok in
  match int g 0 6 with
  | 0 -> e ()
  | 1 -> sprintf "(let x = %s in let y = %s in (x * 3 + y) land 0xffff)" (e ()) (e ())
  | 2 -> sprintf "(ignore %s; %s)" (e ()) (e ())
  | 3 ->
      sprintf
        "(let s = ref 0 in for i = 0 to 4 do s := (!s + %s + i) land 0xffff done; !s)"
        (e ())
  | 4 -> (
      match List.filter (fun p -> p.arity = 2) g.prims with
      | [] -> e ()
      | ps -> sprintf "(List.fold_left %s %s [1; 2; 3])" (pick g ps).name (e ()))
  | 5 -> sprintf "(if %s > 100 then %s else %s)" (e ()) (e ()) (e ())
  | _ -> sprintf "(let f = (fun x -> (x + %s) land 0xffff) in f %s)" (e ()) (e ())

type program =
  { runtime_files : (string * string) list  (** in load order *)
  ; wat : string  (** Wasm primitives *)
  ; main : string  (** the program *)
  ; oracle : string  (** its OCaml model *)
  ; wasm_lines : int list  (** test lines calling integer primitives only *)
  }

(* Command-line arguments of the oracle *)
let oracle_args ~effects ~wasm ~env ~pretty ~debuginfo =
  let b x = if x then "1" else "0" in
  [ b (effects = "cps" || effects = "double-translation")
  ; b (effects = "double-translation")
  ; b wasm
  ; env
  ; b pretty
  ; b debuginfo
  ; effects
  ]

let generate seed =
  let st = Rng.make seed in
  let nfiles = Rng.range st 1 3 in
  let g =
    { st
    ; nfiles
    ; files = Array.make nfiles []
    ; helpers = []
    ; models = []
    ; always = []
    ; prims = []
    ; init_state = 0
    }
  in
  gen_helpers g;
  gen_state g;
  for i = 0 to int g 2 6 - 1 do
    gen_int_prim g i
  done;
  List.iter (fun (_, _, js, _) -> add g js) js_only;
  let wasm_lines = ref [] in
  let tests =
    List.init (int g 10 20) (fun i ->
        let wasm_ok = chance g 0.6 in
        if wasm_ok then wasm_lines := i :: !wasm_lines;
        let e = sprintf "string_of_int %s" (test_expr g wasm_ok) in
        (* JavaScript-only lines are skipped in Wasm, where the primitives
           they call are dummies, declared pure by the JavaScript runtime
           files, whose exceptions may be optimized away *)
        let e = if wasm_ok then e else sprintf "if is_wasm then \"skipped\" else %s" e in
        sprintf "let () = t %d (fun () -> %s)" i e)
  in
  let externals =
    List.concat_map
      (fun p ->
        let ty = String.concat " -> " (List.init (p.arity + 1) (fun _ -> "int")) in
        List.map (fun nm -> sprintf "external %s : %s = %S" nm ty nm) (p.name :: p.aliases))
      g.prims
    @ List.map (fun (n, ty, _, _) -> sprintf "external %s : %s = %S" n ty n) js_only
  in
  let main =
    String.concat "\n" externals
    ^ "\nlet is_wasm = Sys.backend_type = Other \"wasm_of_ocaml\"\n"
    ^ prelude
    ^ String.concat "\n" tests
    ^ "\n"
  in
  let models =
    [ "let cfg_effects = Sys.argv.(1) = \"1\""
    ; "let cfg_doubletranslate = Sys.argv.(2) = \"1\""
    ; "let cfg_wasm = Sys.argv.(3) = \"1\""
    ; "let cfg_env = Sys.argv.(4)"
    ; "let cfg_pretty = Sys.argv.(5) = \"1\""
    ; "let cfg_debuginfo = Sys.argv.(6) = \"1\""
    ; "let cfg_effects_name = Sys.argv.(7)"
    ; "let is_wasm = cfg_wasm"
    ; sprintf "let my_state = ref %d" g.init_state
    ; prelude
    ]
    @ g.models
    @ List.map
        (fun (_, upd) -> sprintf "let () = my_state := %s" upd)
        (List.sort
           (fun (t, _) (t', _) -> compare (load_pos g t) (load_pos g t'))
           g.always)
    @ List.concat_map
        (fun p ->
          let vs =
            List.sort
              (fun v v' -> compare (load_pos g v.text) (load_pos g v'.text))
              p.variants
          in
          (* Later definitions override earlier ones; environment-specific
             definitions override isomorphic ones *)
          let sel =
            List.fold_left
              (fun sel v ->
                sprintf "(if %s then (fun () -> %s) else %s)" v.cond v.model sel)
              "(fun () -> failwith \"no definition\")"
              (List.filter (fun v -> not v.env_specific) vs
              @ List.filter (fun v -> v.env_specific) vs)
          in
          let args = String.concat " " (List.init p.arity (sprintf "a%d")) in
          List.map
            (fun nm -> sprintf "let %s %s = %s ()" nm args sel)
            (p.name :: p.aliases))
        g.prims
    (* In Wasm, the JavaScript-only primitives are dummies raising an
       exception *)
    @ List.map
        (fun (_, _, _, model) ->
          match String.index_opt model '=' with
          | Some i ->
              String.sub model 0 (i + 1)
              ^ " if cfg_wasm then failwith \"not implemented\" else"
              ^ String.sub model (i + 1) (String.length model - i - 1)
          | None -> assert false)
        js_only
  in
  let oracle = String.concat "\n" models ^ "\n" ^ String.concat "\n" tests ^ "\n" in
  let wat =
    let imports =
      List.map
        (fun p ->
          sprintf
            "  (import \"js\" %S (func $js_%s (param %s) (result i32)))"
            p.name
            p.name
            (String.concat " " (List.init p.arity (fun _ -> "i32"))))
        g.prims
    in
    let funcs =
      List.concat_map
        (fun p ->
          List.map
            (fun nm ->
              sprintf
                "  (func (export %S) %s (result (ref eq))\n\
                \    (ref.i31 (call $js_%s %s)))"
                nm
                (String.concat " " (List.init p.arity (sprintf "(param $a%d (ref eq))")))
                p.name
                (String.concat
                   " "
                   (List.init
                      p.arity
                      (sprintf "(i31.get_s (ref.cast (ref i31) (local.get $a%d)))"))))
            (p.name :: p.aliases))
        g.prims
    in
    String.concat "\n" (("(module" :: imports) @ funcs @ [ ")" ]) ^ "\n"
  in
  { runtime_files =
      List.init nfiles (fun f ->
          ( sprintf "r%d.js" f
          , sprintf "// Generated runtime file %d\n\n" f ^ String.concat "\n" g.files.(f)
          ))
  ; wat
  ; main
  ; oracle
  ; wasm_lines = List.rev !wasm_lines
  }
