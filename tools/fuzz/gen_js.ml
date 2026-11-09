(* Random JavaScript expressions, for code that the compiler parses and
   prints back: [Js.Unsafe.js_expr] strings and runtime files. Their value
   is compared with the one obtained with [eval] on the same source, which
   the compiler does not see. The expressions exercise precedence,
   automatic semicolon insertion, scoping and recent syntax. *)

let sprintf = Printf.sprintf

let paren e = "(" ^ e ^ ")"

let rec num st d env =
  let pick l = Rng.pick st l in
  if d <= 0 || Rng.chance st 0.15
  then
    if env <> [] && Rng.chance st 0.5
    then pick env
    else
      pick
        [ "0"
        ; "1"
        ; "2"
        ; "3"
        ; "7"
        ; "10"
        ; "42"
        ; "0x1f"
        ; "1e3"
        ; ".5"
        ; "5."
        ; "0b101"
        ; "0o17"
        ; "1_000"
        ; "2.5e-3"
        ; "255"
        ]
  else
    let n () = num st (d - 1) env in
    let nb v = num st (d - 1) (v :: env) in
    let s () = str st (d - 1) env in
    let v = sprintf "v%d" d in
    let op () =
      pick [ "+"; "-"; "*"; "/"; "%"; "&"; "|"; "^"; "<<"; ">>"; ">>>"; "**" ]
    in
    match Rng.range st 0 61 with
    | 0 -> sprintf "%s %s %s" (paren (n ())) (op ()) (paren (n ()))
    | 1 ->
        sprintf
          "%s %s %s %s %s"
          (paren (n ()))
          (op ())
          (paren (n ()))
          (pick [ "+"; "-"; "*"; "**"; "|"; "<<" ])
          (paren (n ()))
    | 2 -> sprintf "%s(%s)" (pick [ "-"; "+"; "~"; "- -"; "+-"; "!"; "!!" ]) (n ())
    | 3 ->
        sprintf
          "(%s %s %s ? %s : %s)"
          (n ())
          (pick [ "<"; ">"; "=="; "==="; "!="; "<="; "in [0,1,2,3,4,5,6,7,8,9,10] &&" ])
          (n ())
          (n ())
          (n ())
    | 4 -> sprintf "(%s, %s)" (n ()) (n ())
    | 5 -> sprintf "((%s) => %s * 2)(%s)" v (nb v) (n ())
    | 6 -> sprintf "((%s, w = 3) => %s + w)(%s)" v (nb v) (n ())
    | 7 ->
        sprintf
          "(() => { let %s = %s\n  let w = %s + 1\n  return w * 2 })()"
          v
          (n ())
          (nb v)
    | 8 ->
        sprintf
          "(() => { var %s = %s; %s; return %s })()"
          v
          (n ())
          (stmt st (d - 1) (v :: env) v)
          v
    | 9 -> sprintf "({a: %s, b: %s}).%s" (n ()) (n ()) (pick [ "a"; "b" ])
    | 10 -> sprintf "({[\"k\" + 1]: %s}).k1" (n ())
    | 11 -> sprintf "({...{a: %s, c: 1}, b: 2}).a" (n ())
    | 12 ->
        sprintf "({ class: %s, if: 2, new: 3 }).%s" (n ()) (pick [ "class"; "if"; "new" ])
    | 13 -> sprintf "[%s, %s, %s][%s]" (n ()) (n ()) (n ()) (pick [ "0"; "1"; "2"; "3" ])
    | 14 -> sprintf "[...[1, 2], %s, ...\"ab\"].length" (n ())
    | 15 -> sprintf "(%s).length" (s ())
    | 16 -> sprintf "Math.max(%s, %s)" (n ()) (n ())
    | 17 -> sprintf "Math.max(...[%s, %s], 0)" (n ()) (n ())
    | 18 -> sprintf "(({a: {b: %s}})?.a?.b ?? 0)" (n ())
    | 19 -> sprintf "(null ?? %s) + (undefined?.x ?? %s)" (n ()) (n ())
    | 20 -> sprintf "`${%s}`.length" (n ())
    | 21 -> sprintf "((%s) && (%s)) + ((%s) || (%s))" (n ()) (n ()) (n ()) (n ())
    | 22 ->
        sprintf
          "(new (class { constructor(x) { this.v = x } get w() { return this.v * 2 } \
           })(%s)).w"
          (n ())
    | 23 ->
        sprintf
          "(new (class { #p = %s; static s = 1; static { this.t = this.s + 1 } m() { \
           return this.#p } })()).m()"
          (n ())
    | 24 ->
        sprintf
          "(() => { class A { m() { return %s } } class B extends A { m() { return \
           super.m() + 1 } } return new B().m() })()"
          (n ())
    | 25 -> sprintf "(function f(k) { return k <= 0 ? 0 : k + f(k - 1) })(%s & 15)" (n ())
    | 26 -> sprintf "((%s) => ((%s) => %s + 1)(%s * 2))(%s)" v v (nb v) v (n ())
    | 27 ->
        sprintf
          "(() => { let s = 0; outer: for (let i = 0; i < 3; i++) { for (let j = 0; j < \
           3; j++) { if (j == i) continue outer; s += %s } } return s })()"
          (n ())
    | 28 -> sprintf "(/a+/.test(\"caaat\") ? %s : %s)" (n ()) (n ())
    | 29 -> sprintf "\"a/b/c\".split(\"/\").length / 2 / %s" (paren (n ()))
    | 30 -> sprintf "(\"a\" in {a: 1} ? %s : %s)" (n ()) (n ())
    | 31 -> sprintf "(void 0 === undefined ? %s : 0)" (n ())
    | 32 -> sprintf "typeof %s === \"number\" ? 1 : 0" (paren (n ()))
    | 33 -> sprintf "Number((%sn ** 3n) %% 1000n)" (pick [ "2"; "10"; "7" ])
    | 34 -> sprintf "(/* c */ %s // d\n + 1)" (n ())
    | 35 ->
        sprintf
          "\"%s\".length"
          (pick [ "\\u00e9"; "\\x41"; "\\'"; "\\\\n"; "\\u{1F600}"; "a\\tb" ])
    | 36 -> sprintf "({ get a() { return %s }, set a(x) {} }).a" (n ())
    | 37 -> sprintf "(([a, , b = 4], {c: d = 1} = {}) => a + b + d)([%s, 2])" (n ())
    | 38 -> sprintf "[...(function* () { yield %s; yield* [1, 2] })()].length" (n ())
    | 39 ->
        sprintf
          "(() => { let x = 0; x ||= %s; x &&= x + 1; x ??= 5; x **= 2; return x })()"
          (n ())
    | 40 ->
        sprintf
          "(() => { let x = %s; x += 2; x -= 1; x *= 3; x >>>= 1; x <<= 2; x |= 1; x ^= \
           3; x &= 255; x %%= 100; return x })()"
          (n ())
    | 41 -> sprintf "((f) => f?.(%s) ?? -1)((x) => x + 1)" (n ())
    | 42 ->
        sprintf
          "(() => { let s = 0; for (const x of [%s, %s]) s += x; for (const k in {p: 1, \
           q: 2}) s += k.length; let i = 0; do { s++ } while (++i < 3); return s })()"
          (n ())
          (n ())
    | 43 ->
        sprintf
          "(() => { switch (%s %% 3) { case 0: return 10; case 1: case 2: { return 20 } \
           default: return 30 } })()"
          (paren (n ()))
    | 44 ->
        sprintf
          "(() => { try { throw %s } catch ({message = 0}) { return message } finally { \
           } })()"
          (n ())
    | 45 -> sprintf "(() => { try { null.x } catch { return %s } })()" (n ())
    | 46 ->
        sprintf "(() => { let r = 0; try { return r = %s } finally { r++ } })()" (n ())
    | 47 ->
        sprintf
          "((x) => { if (x > 5) return 1\n else if (x > 2) return 2\n return 3 })(%s)"
          (n ())
    | 48 -> sprintf "(function () { return arguments.length + %s })(1, 2)" (n ())
    | 49 -> sprintf "((...rest) => rest.length + %s)(1, 2, 3)" (n ())
    | 50 -> sprintf "(function () { return new.target === undefined ? %s : 0 })()" (n ())
    | 51 -> sprintf "(x => x)(%s) - -%s" (n ()) (paren (n ()))
    | 52 -> sprintf "%s - -%s" (paren (n ())) (paren (n ()))
    | 53 -> sprintf "%s + +%s" (paren (n ())) (paren (n ()))
    | 54 -> sprintf "%s ** -%s" (paren (n ())) (paren (n ()))
    | 55 -> sprintf "(-%s) ** 2" (paren (n ()))
    | 56 -> sprintf "(2).toString().length + 1..toString().length + %s" (paren (n ()))
    | 57 ->
        sprintf "(() => { label: { if (%s) break label; return 1 } return 2 })()" (n ())
    | 58 ->
        sprintf
          "(() => { let i = 0; while (true) { if (!(i++ <= %s %% 5)) break } return i \
           })()"
          (paren (n ()))
    | 59 ->
        sprintf
          "(() => { const {a, ...r} = {a: %s, b: 2, c: 3}; return a + \
           Object.keys(r).length })()"
          (n ())
    | 60 -> sprintf "(() => { var x = %s; { let x = 1; x++ } return x })()" (n ())
    | _ ->
        if Rng.chance st 0.5
        then
          sprintf
            "(() => { function g() { return h() + 1 } function h() { return %s } return \
             g() })()"
            (n ())
        else
          sprintf
            "(() => { let fs = []; for (let i = 0; i < 3; i++) fs.push(() => i); return \
             fs[0]() + fs[2]() + %s })()"
            (n ())

and stmt st d env v =
  let n () = num st d env in
  match Rng.range st 0 3 with
  | 0 -> sprintf "%s += %s" v (n ())
  | 1 -> sprintf "if (%s > 3) %s = %s; else %s--" (n ()) v (n ()) v
  | 2 -> sprintf "for (var i = 0; i < 3; i++) %s += i" v
  | _ -> sprintf "%s = (%s, %s)" v (n ()) (n ())

and str st d env =
  let n () = num st (d - 1) env in
  match Rng.range st 0 8 with
  | 0 -> sprintf "\"ab\" + %s" (paren (n ()))
  | 1 -> sprintf "`x${%s}y`" (n ())
  | 2 -> sprintf "String(%s)" (n ())
  | 3 -> sprintf "[%s, %s].join(\"-\")" (n ()) (n ())
  | 4 -> "'q' + \"\\\"\" + `\\``"
  | 5 -> "\"\\u00e9\\u{1F600}\""
  | 6 -> sprintf "JSON.stringify({a: [%s], 'b c': null})" (n ())
  | 7 -> sprintf "typeof %s" (paren (n ()))
  | _ -> sprintf "(%s).toString(%s)" (n ()) (Rng.pick st [ "2"; "16"; "36" ])

let expr st =
  let d = Rng.pick st [ 1; 2; 2; 3 ] in
  if Rng.chance st 0.2 then str st d [] else num st d []

(* A JavaScript string literal for [s] *)
let string_literal s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\t' -> Buffer.add_string b "\\t"
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b
