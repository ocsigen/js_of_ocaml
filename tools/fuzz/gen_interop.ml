(* Random programs using the js_of_ocaml library (JavaScript interop).

   Two kinds of test lines, each printing "N: result":
   - calls in both directions between OCaml and JavaScript: callbacks
     (with partial and over-application), objects (ppx syntax and
     [Js.Unsafe]), arrays, strings, [Opt]/[Optdef], exceptions crossing the
     boundary; the result is compared between configurations;
   - JavaScript expressions given to [Js.Unsafe.js_expr], which the
     compiler parses and prints back: the same source is also evaluated
     with [eval], and the line reports DIFF when the two disagree. *)

let sprintf = Printf.sprintf

let prelude =
  {|open Js_of_ocaml
let js_describe : Js.Unsafe.any = Js.Unsafe.js_expr {js|(function (x) {
  switch (typeof x) {
  case "function": return "function";
  case "symbol": return "symbol";
  case "bigint": return "bigint:" + String(x);
  case "object":
    if (x === null) return "null";
    try { return "object:" + JSON.stringify(x); } catch (e) { return "object"; }
  default: return typeof x + ":" + String(x);
  }})|js}
let js_same : Js.Unsafe.any = Js.Unsafe.js_expr {js|(function (a, b) {
  if (typeof a !== typeof b) return false;
  if (Object.is(a, b)) return true;
  if (typeof a === "function") return true;
  var d = function (x) { try { return JSON.stringify(x) } catch (e) { return "?" } };
  return d(a) === d(b); })|js}
let describe x =
  Js.to_string (Js.Unsafe.fun_call js_describe [| Js.Unsafe.inject x |] : Js.js_string Js.t)
let jscheck (src : string) (f : unit -> Js.Unsafe.any) =
  let v = try `V (f ()) with Js_error.Exn _ -> `E in
  let w = try `V (Js.Unsafe.eval_string src) with Js_error.Exn _ -> `E in
  match v, w with
  | `V v, `V w ->
    if Js.to_bool (Js.Unsafe.fun_call js_same [| v; w |] : bool Js.t)
    then describe v else "DIFF " ^ describe v ^ " / " ^ describe w
  | `E, `E -> "throws"
  | `V v, `E -> "DIFF " ^ describe v ^ " / throws"
  | `E, `V w -> "DIFF throws / " ^ describe w
let add2 : Js.Unsafe.any = Js.Unsafe.js_expr "(function (a, b) { return (a + b) | 0; })"
let call0 : Js.Unsafe.any = Js.Unsafe.js_expr "(function (f) { return f(); })"
let call1 : Js.Unsafe.any = Js.Unsafe.js_expr "(function (f, a) { return f(a); })"
let call2 : Js.Unsafe.any = Js.Unsafe.js_expr "(function (f, a, b) { return f(a, b); })"
let call2c : Js.Unsafe.any = Js.Unsafe.js_expr "(function (f, a, b) { return f(a)(b); })"
let call3 : Js.Unsafe.any = Js.Unsafe.js_expr "(function (f, a, b, c) { return f(a, b, c); })"
let catcher : Js.Unsafe.any =
  Js.Unsafe.js_expr "(function (f) { try { return f(); } catch (e) { return -1; } })"
let thrower : Js.Unsafe.any = Js.Unsafe.js_expr "(function (s) { throw new Error(s); })"
let math : Js.Unsafe.any = Js.Unsafe.global##._Math
let ctor : (int -> < x : int Js.readonly_prop; get : int Js.meth > Js.t) Js.constr =
  Js.Unsafe.js_expr
    "(function C(x) { this.x = x * 2; this.get = function () { return this.x + 1; }; })"
let exn_name e = match e with
  | Js_error.Exn e -> "jserror " ^ Js_error.message e
  | Not_found -> "Not_found" | Exit -> "Exit" | Failure s -> "Failure " ^ s
  | Invalid_argument s -> "Invalid_argument " ^ s
  | Division_by_zero -> "Division_by_zero" | _ -> "other exception"
let t n f = print_endline (Printf.sprintf "%d: %s" n (try f () with e -> "exn " ^ exn_name e))
let opaque = Array.length Sys.argv > 1000
(* [o x] is [x], but the compiler cannot know it *)
let o x = if opaque then Obj.magic 0 else x
|}

let int_lit st =
  let v =
    Rng.pick st [ 0; 1; 2; 3; 7; 10; 42; 100; -1; -5; 255; 1000; 65535; 1 lsl 20 ]
  in
  let v = if v < 0 then sprintf "(%d)" v else string_of_int v in
  if Rng.chance st 0.4 then sprintf "(o %s)" v else v

let str_lit st =
  Rng.pick
    st
    [ "\"\""
    ; "\"a\""
    ; "\"hello\""
    ; "\"h\\195\\169llo\""
    ; "\"\\226\\130\\172uro\""
    ; "\"a b c\""
    ; "\"x,y,z\""
    ; "\"\\240\\159\\152\\128\""
    ; "\"ABC\""
    ]

let rec int_expr st d env =
  if d <= 0 || Rng.chance st 0.15
  then if env <> [] && Rng.chance st 0.6 then Rng.pick st env else int_lit st
  else
    let e () = int_expr st (d - 1) env in
    let e2 extra = int_expr st (d - 1) (extra @ env) in
    let s () = str_lit st in
    let a = sprintf "a%d" d and b = sprintf "b%d" d in
    match Rng.range st 0 45 with
    | 0 -> sprintf "(%s + %s)" (e ()) (e ())
    | 1 -> sprintf "((%s * %s) land 0xffff)" (e ()) (e ())
    | 2 ->
        sprintf
          "(Js.Unsafe.fun_call add2 [| Js.Unsafe.inject %s; Js.Unsafe.inject %s |] : int)"
          (e ())
          (e ())
    | 3 ->
        sprintf
          "(Js.Unsafe.fun_call add2 (Array.map Js.Unsafe.inject [| %s; %s |]) : int)"
          (e ())
          (e ())
    | 4 ->
        sprintf
          "(Js.Unsafe.fun_call call2 [| Js.Unsafe.inject (Js.wrap_callback (fun %s %s -> \
           %s)); Js.Unsafe.inject %s; Js.Unsafe.inject %s |] : int)"
          a
          b
          (e2 [ a; b ])
          (e ())
          (e ())
    | 5 ->
        sprintf
          "(Js.Unsafe.fun_call call2c [| Js.Unsafe.inject (Js.wrap_callback (fun %s %s \
           -> %s)); Js.Unsafe.inject %s; Js.Unsafe.inject %s |] : int)"
          a
          b
          (e2 [ a; b ])
          (e ())
          (e ())
    | 6 ->
        sprintf
          "(Js.Unsafe.fun_call call2 [| Js.Unsafe.inject (Js.wrap_callback (fun %s -> \
           let c = %s in fun %s -> %s + c)); Js.Unsafe.inject %s; Js.Unsafe.inject %s |] \
           : int)"
          a
          (e2 [ a ])
          b
          (e2 [ a; b ])
          (e ())
          (e ())
    | 7 ->
        sprintf
          "(Js.Unsafe.fun_call call3 [| Js.Unsafe.inject (Js.wrap_callback (fun %s %s -> \
           fun c -> %s - c)); Js.Unsafe.inject %s; Js.Unsafe.inject %s; Js.Unsafe.inject \
           %s |] : int)"
          a
          b
          (e2 [ a; b ])
          (e ())
          (e ())
          (e ())
    | 8 ->
        sprintf
          "(Js.Unsafe.fun_call call2 [| Js.Unsafe.inject (Js.Unsafe.callback_with_arity \
           2 (fun %s %s -> %s)); Js.Unsafe.inject %s; Js.Unsafe.inject %s |] : int)"
          a
          b
          (e2 [ a; b ])
          (e ())
          (e ())
    | 9 ->
        sprintf
          "(Js.Unsafe.fun_call call1 [| Js.Unsafe.inject (Js.wrap_callback (fun %s -> \
           %s)); Js.Unsafe.inject %s |] : int)"
          a
          (e2 [ a ])
          (e ())
    | 10 ->
        sprintf
          "(Js.Unsafe.fun_call call0 [| Js.Unsafe.inject (Js.wrap_callback (fun () -> \
           %s)) |] : int)"
          (e ())
    | 11 ->
        sprintf
          "(Js.Unsafe.meth_call (Js.Unsafe.obj [| \"v\", Js.Unsafe.inject %s; \"m\", \
           Js.Unsafe.inject (Js.wrap_meth_callback (fun this %s -> (Js.Unsafe.get this \
           \"v\" : int) + %s)) |]) \"m\" [| Js.Unsafe.inject %s |] : int)"
          (e ())
          a
          (e2 [ a ])
          (e ())
    | 12 ->
        sprintf
          "(let ob = object%%js (self) val mutable v = %s method get = self##.v method \
           add x = self##.v := self##.v + x; self##.v end in ignore (ob##add %s); \
           ob##add (ob##get))"
          (e ())
          (e ())
    | 13 ->
        sprintf
          "(let ob = object%%js val x = %s val y = %s method sum z = z + 1 end in ob##.x \
           * 3 + ob##.y + ob##sum %s)"
          (e ())
          (e ())
          (e ())
    | 14 ->
        sprintf
          "(let ob = Js.Unsafe.obj [| \"a\", Js.Unsafe.inject %s |] in Js.Unsafe.set ob \
           \"b\" %s; Js.Unsafe.delete ob \"a\"; Js.Optdef.get (Js.Unsafe.get ob \"a\") \
           (fun () -> 100) + Js.Unsafe.get ob \"b\")"
          (e ())
          (e ())
    | 15 ->
        sprintf
          "(let ar = Js.array [| %s; %s; %s |] in Js.array_set ar 1 %s; ignore (ar##push \
           %s); ar##.length * 1000 + Js.Optdef.get (Js.array_get ar 1) (fun () -> -1) + \
           Js.Optdef.get (Js.array_get ar 7) (fun () -> -2))"
          (e ())
          (e ())
          (e ())
          (e ())
          (e ())
    | 16 ->
        sprintf
          "(Array.fold_left (+) 0 (Js.to_array ((Js.array [| %s; %s |])##map \
           (Js.wrap_callback (fun %s i _ -> %s + i)))))"
          (e ())
          (e ())
          a
          (e2 [ a ])
    | 17 -> sprintf "((Js.string %s)##.length + %s)" (s ()) (e ())
    | 18 -> sprintf "((Js.string %s)##indexOf (Js.string %s))" (s ()) (s ())
    | 19 -> sprintf "(String.length (Js.to_string ((Js.string %s)##toUpperCase)))" (s ())
    | 20 ->
        sprintf "(Js.str_array ((Js.string %s)##split (Js.string \",\")))##.length" (s ())
    | 21 ->
        sprintf
          "(String.length (Js.to_string (Js.string %s)) + String.length \
           (Js.to_bytestring (Js.bytestring %s)))"
          (s ())
          (s ())
    | 22 ->
        sprintf
          "(Js.Opt.get (if %s > 3 then Js.some %s else Js.null) (fun () -> 7))"
          (e ())
          (e ())
    | 23 ->
        sprintf
          "(Js.Opt.case (Js.Opt.map (Js.some %s) (fun x -> x + 1)) (fun () -> 0) (fun x \
           -> x * 2))"
          (e ())
    | 24 ->
        sprintf
          "(Js.Optdef.get (Js.Optdef.option (if %s > 3 then Some %s else None)) (fun () \
           -> 9))"
          (e ())
          (e ())
    | 25 ->
        sprintf
          "(if Js.equals (Js.Unsafe.inject %s) (Js.Unsafe.inject (Js.string \
           (string_of_int %s))) then 1 else 0)"
          (e ())
          (e ())
    | 26 ->
        sprintf
          "(if Js.strict_equals (Js.Unsafe.inject %s) (Js.Unsafe.inject %s) then 1 else \
           0)"
          (e ())
          (e ())
    | 27 ->
        sprintf
          "(String.length (Js.to_string (Js.typeof (Js.Unsafe.inject %s))))"
          (Rng.pick
             st
             [ e ()
             ; sprintf "(Js.string %s)" (s ())
             ; "(Js.array [||])"
             ; "(Js.wrap_callback (fun x -> x + 1))"
             ; "Js.null"
             ; "Js.undefined"
             ; "(Js.number_of_float 1.5)"
             ])
    | 28 ->
        sprintf
          "(if Js.instanceof (Js.array [| %s |]) (Js.Unsafe.global##._Array : _ \
           Js.constr) then 1 else 0)"
          (e ())
    | 29 -> sprintf "((new%%js ctor %s)##.x + (new%%js ctor %s)##get)" (e ()) (e ())
    | 30 ->
        sprintf
          "(Js.Unsafe.get (Js.Unsafe.new_obj (Js.Unsafe.js_expr \"(class { \
           constructor(x, y) { this.s = x - y } })\") [| Js.Unsafe.inject %s; \
           Js.Unsafe.inject %s |]) \"s\" : int)"
          (e ())
          (e ())
    | 31 ->
        sprintf
          "(try ignore (Js.Unsafe.fun_call call0 [| Js.Unsafe.inject (Js.wrap_callback \
           (fun () -> if %s > 3 then raise Exit else 0)) |] : int); 0 with Exit -> 1)"
          (e ())
    | 32 ->
        sprintf
          "(Js.Unsafe.fun_call catcher [| Js.Unsafe.inject (Js.wrap_callback (fun () -> \
           if %s > 3 then raise Not_found else %s)) |] : int)"
          (e ())
          (e ())
    | 33 ->
        sprintf
          "(try Js.Unsafe.fun_call thrower [| Js.Unsafe.inject (Js.string %s) |] with \
           Js_error.Exn ex -> String.length (Js_error.message ex))"
          (s ())
    | 34 -> sprintf "(try %s with Js_error.Exn _ -> -7)" (e ())
    | 35 ->
        sprintf
          "(int_of_float (Js.float_of_number (Js.Unsafe.meth_call math \"floor\" [| \
           Js.Unsafe.inject (Js.number_of_float (float_of_int %s /. 3.)) |])))"
          (e ())
    | 36 ->
        sprintf
          "(int_of_float (Js.float_of_number (Js.Unsafe.meth_call math \"max\" \
           (Array.map (fun x -> Js.Unsafe.inject (Js.number_of_float (float_of_int x))) \
           [| %s; %s; %s |]))))"
          (e ())
          (e ())
          (e ())
    | 37 ->
        sprintf
          "(int_of_float (Js.float_of_number (Js.Unsafe.variable \"Math.PI\") *. 1000.) \
           + %s)"
          (e ())
    | 38 ->
        sprintf
          "(Js.Unsafe.fun_call (Js.Unsafe.get Js.Unsafe.global \"parseInt\") [| \
           Js.Unsafe.inject (Js.string (string_of_int %s)); Js.Unsafe.inject 16 |] : \
           int)"
          (e ())
    | 39 ->
        sprintf
          "(Js.Unsafe.call (Js.Unsafe.js_expr \"(function (a) { return this.k + a; })\") \
           (Js.Unsafe.obj [| \"k\", Js.Unsafe.inject %s |]) [| Js.Unsafe.inject %s |] : \
           int)"
          (e ())
          (e ())
    | 40 ->
        sprintf
          "(let ob : < v : int Js.prop > Js.t = Js.Unsafe.obj [| \"v\", Js.Unsafe.inject \
           %s |] in ob##.v := ob##.v + %s; ob##.v)"
          (e ())
          (e ())
    | 41 ->
        sprintf
          "(Js.Unsafe.js_expr %s : int)"
          (Rng.pick
             st
             [ "\"(3 + 4) * 2 | 0\""
             ; "\"[1,2,3].length\""
             ; "\"\\\"abc\\\".charCodeAt(1)\""
             ; "\"(function(){ return 5 })()\""
             ])
    | 42 -> sprintf "(if Js.to_bool (Js.bool (%s > %s)) then 1 else 0)" (e ()) (e ())
    | 43 ->
        sprintf
          "(Js.Unsafe.fun_call (Js.Unsafe.pure_js_expr \"Math.abs\") [| Js.Unsafe.inject \
           %s |] : int)"
          (e ())
    | _ ->
        sprintf
          "(let f = Js.wrap_callback (fun %s %s -> %s) in (Js.Unsafe.fun_call call2 [| \
           Js.Unsafe.inject f; Js.Unsafe.inject %s; Js.Unsafe.inject %s |] : int) + \
           (Js.Unsafe.fun_call call2c [| Js.Unsafe.inject f; Js.Unsafe.inject %s; \
           Js.Unsafe.inject %s |] : int))"
          a
          b
          (e2 [ a; b ])
          (e ())
          (e ())
          (e ())
          (e ())

let str_expr st d =
  let e () = int_expr st (d - 1) [] in
  let s () = str_lit st in
  match Rng.range st 0 13 with
  | 0 -> sprintf "(Js.to_string ((Js.string %s)##concat (Js.string %s)))" (s ()) (s ())
  | 1 -> sprintf "(Js.to_string ((Js.string %s)##toUpperCase))" (s ())
  | 2 -> sprintf "(Js.to_string ((Js.string %s)##substring %s %s))" (s ()) (e ()) (e ())
  | 3 ->
      sprintf
        "(Js.to_string (Js.number_of_float (float_of_int %s /. 7.))##toString)"
        (e ())
  | 4 ->
      sprintf
        "(Js.to_string ((Js.number_of_float (float_of_int %s /. 7.))##toFixed 3))"
        (e ())
  | 5 ->
      sprintf
        "(String.concat \"|\" (Array.to_list (Array.map Js.to_string (Js.to_array \
         (Js.str_array ((Js.string %s)##split (Js.string \"\")))))))"
        (s ())
  | 6 ->
      sprintf
        "(Js.to_string (Js._JSON##stringify (Js.Unsafe.obj [| \"a\", Js.Unsafe.inject \
         %s; \"b\", Js.Unsafe.inject (Js.string %s) |])))"
        (e ())
        (s ())
  | 7 -> "(describe (Js.Unsafe.js_expr \"({ x: [1, 2], y: 'z' })\"))"
  | 8 -> sprintf "(describe (Js.array [| %s; %s |]))" (e ()) (e ())
  | 9 ->
      sprintf
        "(Js.to_string (Js.typeof (Js.Unsafe.inject %s)))"
        (Rng.pick
           st
           [ e ()
           ; sprintf "(Js.string %s)" (s ())
           ; "Js.null"
           ; "Js.undefined"
           ; "(Js.wrap_callback (fun x -> x))"
           ; "(Js.number_of_float 1.5)"
           ])
  | 10 ->
      sprintf
        "(describe (Js.Unsafe.inject %s))"
        (Rng.pick
           st
           [ e ()
           ; sprintf "(Js.string %s)" (s ())
           ; "(Js.number_of_float 1.5)"
           ; "Js._true"
           ; "(Js.bool true)"
           ; "(Js.number_of_float (-0.))"
           ; "(Js.number_of_float nan)"
           ])
  | 11 -> sprintf "(Js.to_string (Js.string %s))" (s ())
  | 12 -> sprintf "(Js.to_bytestring (Js.bytestring %s))" (s ())
  | _ ->
      "(Js.to_string (Js.Unsafe.fun_call (Js.Unsafe.js_expr \"(function (f) { return \
       String(f('x', 2)); })\") [| Js.Unsafe.inject (Js.wrap_callback (fun x n -> \
       Js.string (String.concat \"\" (List.init n (fun _ -> Js.to_string x))))) |]))"

let program seed =
  let st = Rng.make seed in
  let tests =
    List.init (Rng.range st 15 30) (fun i ->
        let k = Random.State.float st 1. in
        if k < 0.45
        then
          sprintf
            "let () = t %d (fun () -> string_of_int (%s))"
            i
            (int_expr st (Rng.pick st [ 1; 2; 3 ]) [])
        else if k < 0.6
        then sprintf "let () = t %d (fun () -> %s)" i (str_expr st (Rng.pick st [ 1; 2 ]))
        else
          let src = sprintf "{js|%s|js}" (Gen_js.expr st) in
          sprintf
            "let () = t %d (fun () -> jscheck %s (fun () -> Js.Unsafe.js_expr %s))"
            i
            src
            src)
  in
  prelude ^ String.concat "\n" tests ^ "\n"
