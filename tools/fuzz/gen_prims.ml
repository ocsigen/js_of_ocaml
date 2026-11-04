(* Random programs testing runtime primitives on random inputs.

   Each test prints "N: result" (or the exception raised). The output must
   be the same with ocamlrun, js_of_ocaml and wasm_of_ocaml: integers whose
   width matters stay within 31 bits, nativeint is not used, and only
   floating-point functions that are exactly rounded on every platform are
   called. Tests whose result legitimately differs in JavaScript (hash and
   marshal of int32 and floats, which are JavaScript numbers there) are
   marked with the comment [(* jsdiff *)]. *)

let sprintf = Printf.sprintf

let prelude =
  {|let opaque = Array.length Sys.argv > 1000
(* [o x] is [x], but the compiler cannot know it *)
let o x = if opaque then Obj.magic 0 else x
let t n f =
  match f () with
  | s -> Printf.printf "%d: %s\n%!" n s
  | exception e -> Printf.printf "%d: exception %s\n%!" n (Printexc.to_string e)
let sl l = "[" ^ String.concat ";" l ^ "]"
(* The sign of NaN results is platform-dependent *)
let fh x = if Float.is_nan x then "nan" else Printf.sprintf "%h" x
|}

type t =
  { st : Rng.t
  ; mutable jsdiff : bool
  }

let pick g l = Rng.pick g.st l

let chance g p = Rng.chance g.st p

let int g lo hi = Rng.range g.st lo hi

(* Literal or opaque value, so that both constant folding and the runtime
   are tested *)
let wrap g lit = if chance g 0.4 then lit else sprintf "(o %s)" lit

let paren_if_neg s = if s <> "" && s.[0] = '-' then "(" ^ s ^ ")" else s

(* An OCaml float literal *)
let float_literal x =
  let s = sprintf "%.17g" x in
  let s =
    if String.exists (fun c -> c = '.' || c = 'e' || c = 'n' || c = 'i') s
    then s
    else s ^ "."
  in
  paren_if_neg s

let escape_bytes s =
  "\""
  ^ String.concat
      ""
      (List.map (fun c -> sprintf "\\%03d" (Char.code c)) (List.of_seq (String.to_seq s)))
  ^ "\""

(* Literals *)

let int_lit ?(nonneg = false) g =
  let r = Random.State.float g.st 1. in
  let v =
    if r < 0.3
    then pick g [ 0; 1; 2; 7; 10; 255; 256; 65535; 65536; 1 lsl 29; (1 lsl 30) - 1 ]
    else if r < 0.7
    then int g 0 1000
    else int g 0 ((1 lsl 30) - 1)
  in
  let v =
    if (not nonneg) && chance g 0.3 then if chance g 0.1 then -(1 lsl 30) else -v else v
  in
  sprintf "(%d)" v

let i32_lit g =
  let v =
    pick
      g
      [ 0
      ; 1
      ; -1
      ; 0x7FFFFFFF
      ; -0x80000000
      ; 1 lsl 15
      ; -(1 lsl 15)
      ; 1 lsl 16
      ; 255
      ; int g (-0x80000000) 0x7FFFFFFF
      ; int g (-1000) 1000
      ]
  in
  sprintf "(%dl)" v

let i64_lit g =
  let v =
    pick
      g
      [ 0L
      ; 1L
      ; -1L
      ; Int64.max_int
      ; Int64.min_int
      ; 0x80000000L
      ; -0x80000001L
      ; 0x100000000L
      ; Int64.add (Int64.shift_left 1L 53) 1L
      ; Random.State.bits64 g.st
      ; Rng.range64 g.st (-0x10000000000L) 0x10000000000L
      ; Int64.of_int (int g (-1000) 1000)
      ]
  in
  sprintf "(%LdL)" v

let float_lit g =
  let r = Random.State.float g.st 1. in
  if r < 0.35
  then
    pick
      g
      [ "0."
      ; "(-0.)"
      ; "nan"
      ; "infinity"
      ; "neg_infinity"
      ; "1."
      ; "(-1.)"
      ; "0.1"
      ; "0.5"
      ; "1.5"
      ; "2.5"
      ; "(-2.5)"
      ; "1e-308"
      ; "5e-324"
      ; "1.7976931348623157e308"
      ; "2.2250738585072014e-308"
      ; "123456789.123"
      ; "9007199254740993."
      ; "1e21"
      ; "1e22"
      ; "0.3"
      ; "(1. /. 3.)"
      ; "100."
      ; "1e15"
      ; "1e16"
      ; "1e-7"
      ; "999999.5"
      ; "0.000123"
      ]
  else if r < 0.6
  then
    (* Random bit patterns, but no NaN payload *)
    let rec bits () =
      let b = Random.State.bits64 g.st in
      let exp = Int64.(to_int (logand (shift_right_logical b 52) 0x7ffL)) in
      if exp <> 0x7ff || Int64.(equal (logand b 0xFFFFFFFFFFFFFL) 0L) then b else bits ()
    in
    sprintf "(Int64.float_of_bits (%LdL))" (bits ())
  else if r < 0.8
  then float_literal (float_of_int (int g (-1_000_000) 1_000_000))
  else
    float_literal
      (Rng.float g.st (-1e6) 1e6 *. Float.pow 10. (float_of_int (int g (-20) 20)))

(* Finite floats in the range of 31-bit integers *)
let small_float_lit g =
  let x = if chance g 0.7 then Rng.float g.st (-1e8) 1e8 else Rng.float g.st (-10.) 10. in
  let x = if chance g 0.2 then Float.round x +. pick g [ 0.5; -0.5; 0. ] else x in
  float_literal x

let char_lit g = sprintf "'\\%03d'" (int g 0 255)

let str_lit g =
  let r = Random.State.float g.st 1. in
  let s =
    if r < 0.3
    then
      pick
        g
        [ ""
        ; "a"
        ; "abc"
        ; "Hello, World!"
        ; "  spaces  "
        ; "\t\n"
        ; "a,b,,c"
        ; "\xc3\xa9"
        ; "\xe6\x97\xa5\xe6\x9c\xac"
        ; "\xff\xfe"
        ; String.make 40 'x'
        ; "0123456789"
        ]
    else if r < 0.6
    then String.init (int g 0 12) (fun _ -> Char.chr (int g 0 255))
    else if r < 0.8
    then
      let chars = "abcxyz019 ,.-_" in
      String.init (int g 0 20) (fun _ -> chars.[int g 0 (String.length chars - 1)])
    else
      let s =
        pick
          g
          [ "\xe2\x82\xac"
          ; "a\xc3\xa9\xe6\x97\xa5\xf0\x9f\x8e\x89"
          ; "\xc3\xbf"
          ; "\xf4\x8f\xbf\xbf"
          ; "x\xcc\x80"
          ]
      in
      (* Invalid UTF-8 *)
      if chance g 0.3 then String.sub s 0 (String.length s - 1) else s
  in
  escape_bytes s

let binary v =
  if v = 0
  then "0"
  else
    let rec loop v acc =
      if v = 0 then acc else loop (v lsr 1) (string_of_int (v land 1) ^ acc)
    in
    loop v ""

(* Strings for int_of_string whose value, if valid, fits in 31 bits *)
let int_string g =
  let v = int g 0 (1 lsl 29) in
  let base = pick g [ ""; "0x"; "0X"; "0o"; "0b"; "0u" ] in
  let digits =
    match base with
    | "0x" -> sprintf "%x" v
    | "0X" -> sprintf "%X" v
    | "0o" -> sprintf "%o" v
    | "0b" -> binary v
    | _ -> string_of_int v
  in
  let digits =
    if chance g 0.2 && String.length digits > 1
    then
      let i = int g 1 (String.length digits - 1) in
      String.sub digits 0 i ^ "_" ^ String.sub digits i (String.length digits - i)
    else digits
  in
  let s = pick g [ ""; ""; "-"; "+" ] ^ base ^ digits in
  let s =
    if chance g 0.1
    then
      pick
        g
        [ ""
        ; "-"
        ; "0x"
        ; "_1"
        ; "1_"
        ; " 1"
        ; "1 "
        ; "abc"
        ; "0xg"
        ; "1e3"
        ; "--1"
        ; "0b2"
        ; "0o8"
        ; "12345678901234567890"
        ]
    else s
  in
  sprintf "%S" s

let wide_int_string g bits =
  let r = Random.State.float g.st 1. in
  if bits = 32
  then
    if r < 0.3
    then
      let v =
        pick
          g
          [ 0x7FFFFFFF; -0x80000000; 0x80000000; 0xFFFFFFFF; 0x100000000; -0x80000001 ]
      in
      if chance g 0.5 && v >= 0 then sprintf "\"0x%x\"" v else sprintf "\"%d\"" v
    else if r < 0.5
    then sprintf "\"0u%d\"" (int g 0 0x100000000)
    else
      let v = int g (-0x80000000) 0x7FFFFFFF in
      pick
        g
        [ sprintf "\"%d\"" v
        ; (if v < 0 then sprintf "\"-0x%x\"" (-v) else sprintf "\"0x%x\"" v)
        ; (if v < 0 then sprintf "\"-0o%o\"" (-v) else sprintf "\"0o%o\"" v)
        ]
  else if r < 0.3
  then
    pick
      g
      [ "\"9223372036854775807\""
      ; "\"-9223372036854775808\""
      ; "\"9223372036854775808\""
      ; "\"18446744073709551615\""
      ; "\"18446744073709551616\""
      ; "\"-9223372036854775809\""
      ; "\"0x7fffffffffffffff\""
      ; "\"0xffffffffffffffff\""
      ]
  else if r < 0.5
  then sprintf "\"0u%Lu\"" (Random.State.bits64 g.st)
  else
    let v = Random.State.bits64 g.st in
    let neg = Int64.compare v 0L < 0 in
    pick
      g
      [ sprintf "\"%Ld\"" v
      ; (if neg then sprintf "\"-0x%Lx\"" (Int64.neg v) else sprintf "\"0x%Lx\"" v)
      ; (if neg then sprintf "\"-0o%Lo\"" (Int64.neg v) else sprintf "\"0o%Lo\"" v)
      ]

let float_string g =
  if chance g 0.4
  then
    sprintf
      "%S"
      (pick
         g
         [ "0"
         ; "-0"
         ; "1."
         ; ".5"
         ; "5."
         ; "1e3"
         ; "1E-3"
         ; "1e"
         ; "e1"
         ; "1_000.5"
         ; "_1"
         ; "nan"
         ; "-nan"
         ; "NaN"
         ; "inf"
         ; "-inf"
         ; "infinity"
         ; "Infinity"
         ; "0x1p3"
         ; "0x1.8p-1"
         ; "0X1P1"
         ; "0x.8p1"
         ; "0x1p"
         ; "1e400"
         ; "-1e400"
         ; "1e-400"
         ; "  1.5"
         ; "1.5  "
         ; "+1.5"
         ; ""
         ; "0x1.fffffffffffffp1023"
         ; "4.9e-324"
         ; "2.4703282292062328e-324"
         ; "1.0000000000000000001"
         ; "123456789012345678901234567890"
         ; "0.1e1_0"
         ; "1__0"
         ])
  else
    let x = Int64.float_of_bits (Random.State.bits64 g.st) in
    let fmts : (float -> string, unit, string) format list =
      [ "%.17g"; "%.3g"; "%.12g"; "%h" ]
    in
    sprintf "%S" (sprintf (pick g fmts) x)

(* Values for compare / hash / marshal: an expression and its type *)
let rec value ?(d = 3) ?kind g =
  let kind =
    match kind with
    | Some k -> k
    | None ->
        pick
          g
          [ "int"
          ; "float"
          ; "str"
          ; "list"
          ; "pair"
          ; "opt"
          ; "i64"
          ; "i32"
          ; "arr"
          ; "farr"
          ; "variant"
          ]
  in
  let kind =
    if d <= 0 && List.mem kind [ "list"; "pair"; "opt"; "arr"; "variant" ]
    then "int"
    else kind
  in
  match kind with
  | "int" -> int_lit g, "int"
  | "float" -> float_lit g, "float"
  | "str" -> str_lit g, "string"
  | "i64" -> i64_lit g, "int64"
  | "i32" -> i32_lit g, "int32"
  | "farr" ->
      ( sprintf
          "[|%s|]"
          (String.concat "; " (List.init (int g 0 3) (fun _ -> float_lit g)))
      , "float array" )
  | "list" ->
      let ek = pick g [ "int"; "float"; "str"; "i64" ] in
      let items = List.init (int g 0 3) (fun _ -> fst (value ~d:(d - 1) ~kind:ek g)) in
      let ty =
        match ek with
        | "int" -> "int"
        | "float" -> "float"
        | "str" -> "string"
        | _ -> "int64"
      in
      sprintf "[%s]" (String.concat "; " items), sprintf "(%s) list" ty
  | "arr" ->
      ( sprintf "[|%s|]" (String.concat "; " (List.init (int g 0 3) (fun _ -> int_lit g)))
      , "int array" )
  | "pair" ->
      let a, ta = value ~d:(d - 1) g in
      let b, tb = value ~d:(d - 1) g in
      sprintf "(%s, %s)" a b, sprintf "(%s) * (%s)" ta tb
  | "opt" ->
      if chance g 0.3
      then "None", "int option"
      else sprintf "(Some %s)" (int_lit g), "int option"
  | _ ->
      ( pick
          g
          [ "`A"
          ; sprintf "(`B %s)" (int_lit g)
          ; sprintf "(`C (%s, %s))" (str_lit g) (float_lit g)
          ]
      , "[`A | `B of int | `C of string * float]" )

let same_type_values g =
  let a, ty = value g in
  let rec loop n =
    if n = 0
    then a, a, ty
    else
      let b, ty' = value g in
      if ty' = ty then a, b, ty else loop (n - 1)
  in
  loop 20

(* Tests *)

let int_conv = [ "d"; "i"; "u"; "x"; "X"; "o" ]

let float_conv = [ "f"; "F"; "e"; "E"; "g"; "G"; "h"; "H" ]

let remove c s = String.concat "" (String.split_on_char c s)

(* Flags, width and precision, following the rules of OCaml formats *)
let flags g conv =
  let f =
    String.concat "" (List.filter (fun _ -> chance g 0.15) [ "-"; "0"; "+"; " "; "#" ])
  in
  let width = if chance g 0.5 then string_of_int (int g 1 25) else "" in
  let prec =
    if chance g 0.4
    then "." ^ string_of_int (pick g [ 0; 1; 2; 3; 6; 10; 17; 20; 30 ])
    else ""
  in
  let has c = String.contains f c in
  let f = if has '-' && has '0' then remove '0' f else f in
  let f = if width = "" then remove '-' (remove '0' f) else f in
  let f = if String.contains f '+' && String.contains f ' ' then remove ' ' f else f in
  let integer = List.mem conv int_conv in
  let f =
    if integer && List.mem conv [ "u"; "x"; "X"; "o" ]
    then remove '+' (remove ' ' f)
    else f
  in
  let f = if integer && not (List.mem conv [ "x"; "X"; "o" ]) then remove '#' f else f in
  let f = if integer && prec <> "" then remove '0' f else f in
  let f = if List.mem conv [ "F"; "h"; "H" ] then remove '#' f else f in
  f ^ width ^ prec

let test_format_int g =
  let conv = pick g int_conv in
  let nonneg = List.mem conv [ "u"; "x"; "X"; "o" ] in
  sprintf "Printf.sprintf \"%%%s%s\" %s" (flags g conv) conv (wrap g (int_lit ~nonneg g))

let test_format_wide g =
  let conv = pick g int_conv in
  let f = flags g conv in
  if chance g 0.5
  then sprintf "Printf.sprintf \"%%%sl%s\" %s" f conv (wrap g (i32_lit g))
  else sprintf "Printf.sprintf \"%%%sL%s\" %s" f conv (wrap g (i64_lit g))

let test_format_float g =
  let conv = pick g float_conv in
  sprintf "Printf.sprintf \"%%%s%s\" %s" (flags g conv) conv (wrap g (float_lit g))

let test_float_to_string g =
  let x = wrap g (float_lit g) in
  sprintf
    (pick
       g
       ([ "string_of_float %s"
        ; "Float.to_string %s"
        ; "Printf.sprintf \"%%S %%C %%s\" \"\" 'x' (string_of_float %s)"
        ; "Printf.sprintf \"%%.17g\" %s"
        ; "Printf.sprintf \"%%F\" %s"
        ]
         : (string -> string, unit, string) format list))
    x

let test_parse g =
  match int g 0 3 with
  | 0 -> sprintf "string_of_int (int_of_string %s)" (wrap g (int_string g))
  | 1 -> sprintf "Int32.to_string (Int32.of_string %s)" (wrap g (wide_int_string g 32))
  | 2 -> sprintf "Int64.to_string (Int64.of_string %s)" (wrap g (wide_int_string g 64))
  | _ -> sprintf "fh (float_of_string %s)" (wrap g (float_string g))

let ops =
  [ "add"
  ; "sub"
  ; "mul"
  ; "div"
  ; "rem"
  ; "logand"
  ; "logor"
  ; "logxor"
  ; "unsigned_div"
  ; "unsigned_rem"
  ; "min"
  ; "max"
  ]

let shifts = [ "shift_left"; "shift_right"; "shift_right_logical" ]

let test_int64 g =
  let a = wrap g (i64_lit g) and b = wrap g (i64_lit g) in
  match int g 0 5 with
  | k when k <= 2 -> sprintf "Int64.to_string (Int64.%s %s %s)" (pick g ops) a b
  | 3 -> sprintf "Int64.to_string (Int64.%s %s (%d))" (pick g shifts) a (int g 0 63)
  | 4 ->
      pick
        g
        [ sprintf "string_of_int (Int64.compare %s %s)" a b
        ; sprintf "string_of_int (Int64.unsigned_compare %s %s)" a b
        ; sprintf "fh (Int64.to_float %s)" a
        ; sprintf "fh (Int64.float_of_bits %s)" a
        ; sprintf "Int64.to_string (Int64.bits_of_float %s)" (wrap g (float_lit g))
        ; sprintf "Int64.to_string (Int64.of_float %s)" (wrap g (small_float_lit g))
        ; sprintf "Int64.to_string (Int64.of_int32 %s)" (wrap g (i32_lit g))
        ; sprintf "Int32.to_string (Int64.to_int32 %s)" a
        ]
  | _ ->
      sprintf
        "Int64.to_string (Int64.%s %s)"
        (pick g [ "neg"; "abs"; "succ"; "pred"; "lognot" ])
        a

let test_int32 g =
  let a = wrap g (i32_lit g) and b = wrap g (i32_lit g) in
  match int g 0 4 with
  | k when k <= 2 -> sprintf "Int32.to_string (Int32.%s %s %s)" (pick g ops) a b
  | 3 -> sprintf "Int32.to_string (Int32.%s %s (%d))" (pick g shifts) a (int g 0 31)
  | _ ->
      pick
        g
        [ sprintf "string_of_int (Int32.unsigned_compare %s %s)" a b
        ; sprintf "fh (Int32.float_of_bits %s)" a
        ; sprintf "Int32.to_string (Int32.bits_of_float %s)" (wrap g (float_lit g))
        ; sprintf "Int32.to_string (Int32.of_float %s)" (wrap g (small_float_lit g))
        ; sprintf "fh (Int32.to_float %s)" a
        ]

let test_float g =
  let x = wrap g (float_lit g)
  and y = wrap g (float_lit g)
  and z = wrap g (float_lit g) in
  pick
    g
    [ sprintf "fh (Float.round %s)" x
    ; sprintf "fh (Float.trunc %s)" x
    ; sprintf "fh (ceil %s)" x
    ; sprintf "fh (floor %s)" x
    ; sprintf "fh (Float.rem %s %s)" x y
    ; sprintf "fh (mod_float %s %s)" x y
    ; sprintf "fh (Float.fma %s %s %s)" x y z
    ; sprintf "fh (ldexp %s (%d))" x (int g (-1100) 1100)
    ; sprintf "let m, e = frexp %s in Printf.sprintf \"%%h %%d\" m e" x
    ; sprintf "let f, i = modf %s in Printf.sprintf \"%%h %%h\" f i" x
    ; sprintf "fh (Float.copy_sign %s %s)" x y
    ; sprintf "fh (sqrt %s)" x
    ; sprintf "Printf.sprintf \"%%h %%h\" (Float.succ %s) (Float.pred %s)" x x
    ; sprintf
        "Printf.sprintf \"%%h %%h %%h\" (Float.min %s %s) (Float.max %s %s) \
         (Float.min_num %s nan)"
        x
        y
        x
        y
        x
    ; sprintf
        "Printf.sprintf \"%%b %%d %%b\" (Float.equal %s %s) (Float.compare %s %s) \
         (Float.is_integer %s)"
        x
        y
        x
        y
        x
    ; sprintf "string_of_int (int_of_float %s)" (wrap g (small_float_lit g))
    ; sprintf "fh (float_of_int %s)" (wrap g (int_lit g))
    ; sprintf "Printf.sprintf \"%%h %%h\" (%s +. %s) (%s /. %s)" x y x y
    ; sprintf
        "(match classify_float %s with FP_normal -> \"normal\" | FP_subnormal -> \
         \"subnormal\" | FP_zero -> \"zero\" | FP_infinite -> \"inf\" | FP_nan -> \
         \"nan\")"
        x
    ]

let test_compare g =
  let a, b, ty = same_type_values g in
  let a = wrap g a and b = wrap g b in
  sprintf
    (pick
       g
       ([ "string_of_int (compare (%s : %s) %s)"
        ; "string_of_bool ((%s : %s) = %s)"
        ; "string_of_bool ((%s : %s) < %s)"
        ; "string_of_bool ((%s : %s) <> %s)"
        ; "string_of_bool ((%s : %s) >= %s)"
        ]
         : (string -> string -> string -> string, unit, string) format list))
    a
    ty
    b

(* In JavaScript, int32 values and floats are numbers, which cannot be
   distinguished from integers by the hash function or Marshal *)
let js_repr g ty =
  if Harness.contains ty "int32" || Harness.contains ty "float" then g.jsdiff <- true

let test_hash g =
  let v, ty = value g in
  js_repr g ty;
  let v = wrap g v in
  pick
    g
    [ sprintf "string_of_int (Hashtbl.hash (%s : %s))" v ty
    ; sprintf "string_of_int (Hashtbl.seeded_hash %d (%s : %s))" (int g 0 1000) v ty
    ; sprintf
        "string_of_int (Hashtbl.hash_param %d %d (%s : %s))"
        (int g 1 20)
        (int g 1 50)
        v
        ty
    ]

let test_marshal g =
  let v, ty = value g in
  let v = wrap g v in
  match int g 0 3 with
  | 0 ->
      js_repr g ty;
      sprintf
        "Digest.to_hex (Digest.string (Marshal.to_string (%s : %s) [Marshal.No_sharing]))"
        v
        ty
  | 1 ->
      js_repr g ty;
      sprintf
        "string_of_int (String.length (Marshal.to_string (%s : %s) [Marshal.No_sharing]))"
        v
        ty
  | 2 ->
      sprintf
        "let v = (%s : %s) in string_of_bool (compare (Marshal.from_string \
         (Marshal.to_string v []) 0) v = 0)"
        v
        ty
  | _ ->
      js_repr g ty;
      sprintf
        "let v = (%s : %s) in let s = Marshal.to_string v [Marshal.No_sharing] in \
         Printf.sprintf \"%%d %%d\" (Marshal.header_size) (Marshal.total_size \
         (Bytes.of_string s) 0)"
        v
        ty

let test_string g =
  let s = wrap g (str_lit g) and s2 = wrap g (str_lit g) in
  let c = wrap g (char_lit g) in
  let i = sprintf "(%d)" (int g (-2) 12) and j = int g (-2) 12 in
  pick
    g
    [ sprintf "String.escaped %s" s
    ; sprintf "Printf.sprintf \"%%S\" %s" s
    ; sprintf "String.escaped (String.uppercase_ascii %s)" s
    ; sprintf "String.escaped (String.capitalize_ascii (String.lowercase_ascii %s))" s
    ; sprintf "String.escaped (String.sub %s %s (%d))" s i j
    ; sprintf
        "(match String.index_opt %s %s with None -> \"none\" | Some i -> string_of_int i)"
        s
        c
    ; sprintf
        "(match String.rindex_from_opt %s %s %s with None -> \"none\" | Some i -> \
         string_of_int i)"
        s
        i
        c
    ; sprintf "sl (List.map String.escaped (String.split_on_char %s %s))" c s
    ; sprintf "String.escaped (String.trim %s)" s
    ; sprintf "String.escaped (String.concat %s [%s; %s])" s2 s s2
    ; sprintf "string_of_int (String.compare %s %s)" s s2
    ; sprintf "string_of_bool (String.starts_with ~prefix:%s %s)" s2 s
    ; sprintf "string_of_bool (String.is_valid_utf_8 %s)" s
    ; sprintf
        "let d = String.get_utf_8_uchar %s %s in Printf.sprintf \"%%b %%d %%d\" \
         (Uchar.utf_decode_is_valid d) (Uchar.utf_decode_length d) (Uchar.to_int \
         (Uchar.utf_decode_uchar d))"
        s
        i
    ; sprintf "Int32.to_string (String.get_int32_le %s %s)" s i
    ; sprintf "Int64.to_string (String.get_int64_be %s %s)" s i
    ; sprintf "string_of_int (String.get_uint16_be %s %s)" s i
    ; sprintf "string_of_int (String.get_int16_le %s %s)" s i
    ; sprintf "string_of_int (String.get_int8 %s %s)" s i
    ; sprintf "Char.escaped (String.get %s %s)" s i
    ; sprintf "Char.escaped %s ^ Char.escaped (Char.uppercase_ascii %s)" c c
    ; sprintf
        "string_of_int (String.fold_left (fun a c -> (a * 31 + Char.code c) land \
         0xFFFFFF) 0 %s)"
        s
    ; sprintf "string_of_int (Hashtbl.hash %s)" s
    ]

let test_bytes g =
  let s = wrap g (str_lit g) in
  let n = int g 0 10 and i = sprintf "(%d)" (int g (-1) 10) and k = int g (-1) 6 in
  pick
    g
    [ sprintf
        "let b = Bytes.make %d 'a' in Bytes.blit_string %s 0 b %s (%d); String.escaped \
         (Bytes.to_string b)"
        n
        s
        i
        k
    ; sprintf
        "let b = Bytes.of_string %s in Bytes.fill b %s (%d) 'z'; String.escaped \
         (Bytes.to_string b)"
        s
        i
        k
    ; sprintf
        "String.escaped (Bytes.to_string (Bytes.extend (Bytes.of_string %s) (%d) (%d)))"
        s
        (int g (-3) 5)
        (int g (-3) 5)
    ; sprintf
        "let b = Bytes.make 8 '\\000' in Bytes.set_int64_le b 0 %s; String.escaped \
         (Bytes.to_string b)"
        (wrap g (i64_lit g))
    ; sprintf
        "let b = Bytes.make 6 '\\000' in Bytes.set_int32_be b %s %s; String.escaped \
         (Bytes.to_string b)"
        i
        (wrap g (i32_lit g))
    ; sprintf
        "let b = Bytes.make 4 '\\000' in Bytes.set_int16_le b %s %s; String.escaped \
         (Bytes.to_string b)"
        i
        (wrap g (int_lit g))
    ; sprintf
        "let b = Bytes.make 3 'x' in Bytes.set_uint8 b %s %s; String.escaped \
         (Bytes.to_string b)"
        i
        (wrap g (int_lit g))
    ; sprintf
        "let b = Bytes.of_string %s in Bytes.blit b 0 b %s (%d); String.escaped \
         (Bytes.to_string b)"
        s
        i
        k
    ; sprintf
        "let b = Buffer.create 4 in Buffer.add_string b %s; Buffer.add_utf_8_uchar b \
         (Uchar.of_int %d); Buffer.add_int16_be b %s; Buffer.truncate b (max 0 \
         (Buffer.length b - %d)); String.escaped (Buffer.contents b)"
        s
        (pick g [ 0; 0x41; 0xe9; 0x20ac; 0x1f389; 0x10ffff ])
        (wrap g (int_lit g))
        (int g 0 3)
    ]

let test_random g =
  let seed = int g 0 (1 lsl 30) in
  pick
    g
    [ sprintf
        "let s = Random.State.make [| %d |] in sl (List.init 5 (fun _ -> string_of_int \
         (Random.State.int s %d)))"
        seed
        (int g 1 ((1 lsl 30) - 1))
    ; sprintf
        "let s = Random.State.make [| %d; %d |] in sl (List.init 3 (fun _ -> \
         string_of_int (Random.State.bits s)))"
        seed
        (int g 0 1000)
    ; sprintf
        "let s = Random.State.make [| %d |] in sl (List.init 3 (fun _ -> Int64.to_string \
         (Random.State.int64 s %dL)))"
        seed
        (int g 1 max_int)
    ; sprintf
        "let s = Random.State.make [| %d |] in sl (List.init 3 (fun _ -> fh \
         (Random.State.float s 1.)))"
        seed
    ; sprintf
        "let s = Random.State.make [| %d |] in sl (List.init 3 (fun _ -> Int32.to_string \
         (Random.State.int32 s %dl)))"
        seed
        (int g 1 0x7FFFFFFF)
    ; sprintf
        "let s = Random.State.make [| %d |] in sl (List.init 5 (fun _ -> string_of_bool \
         (Random.State.bool s)))"
        seed
    ]

let test_digest g =
  let s = wrap g (str_lit g) in
  pick
    g
    [ sprintf "Digest.to_hex (Digest.string %s)" s
    ; sprintf "Digest.to_hex (Digest.substring %s 1 2)" s
    ; sprintf
        "Digest.to_hex (Digest.string (String.make %d 'a'))"
        (pick g [ 0; 55; 56; 63; 64; 65; 1000 ])
    ]

let test_bigarray g =
  let kind, conv, lit =
    pick
      g
      [ "float64", "fh", float_lit
      ; "float32", "fh", float_lit
      ; ("int8_signed", "string_of_int", fun g -> int_lit g)
      ; ("int8_unsigned", "string_of_int", fun g -> int_lit g)
      ; ("int16_signed", "string_of_int", fun g -> int_lit g)
      ; ("int16_unsigned", "string_of_int", fun g -> int_lit g)
      ; "int32", "Int32.to_string", i32_lit
      ; "int64", "Int64.to_string", i64_lit
      ; "char", "Char.escaped", char_lit
      ]
  in
  let layout = pick g [ "c_layout"; "fortran_layout" ] in
  let base = if layout = "c_layout" then 0 else 1 in
  let n = int g 1 6 in
  let sets =
    String.concat
      "; "
      (List.init n (fun i ->
           sprintf "Bigarray.Array1.set a %d %s" (base + i) (wrap g (lit g))))
  in
  let mk =
    sprintf
      "let a = Bigarray.Array1.create Bigarray.%s Bigarray.%s %d in %s; "
      kind
      layout
      n
      sets
  in
  match int g 0 3 with
  | 0 ->
      mk
      ^ sprintf
          "sl (List.init %d (fun i -> %s (Bigarray.Array1.get a (i + %d))))"
          n
          conv
          base
  | 1 -> mk ^ sprintf "%s (Bigarray.Array1.get a (%d))" conv (base + int g (-1) n)
  | 2 ->
      let off = int g base (base + n - 1) in
      let len = int g 0 (n + base - off) in
      mk
      ^ sprintf
          "let s = Bigarray.Array1.sub a %d %d in sl (List.init (Bigarray.Array1.dim s) \
           (fun i -> %s (Bigarray.Array1.get s (i + %d))))"
          off
          len
          conv
          base
  | _ ->
      mk
      ^ sprintf
          "let b = Bigarray.Array1.create Bigarray.%s Bigarray.%s %d in \
           Bigarray.Array1.fill b %s; Bigarray.Array1.blit a b; string_of_bool (a = b) ^ \
           string_of_int (compare a b)"
          kind
          layout
          n
          (wrap g (lit g))

let tests =
  [ test_format_int
  ; test_format_wide
  ; test_format_float
  ; test_float_to_string
  ; test_parse
  ; test_int64
  ; test_int32
  ; test_float
  ; test_compare
  ; test_hash
  ; test_marshal
  ; test_string
  ; test_bytes
  ; test_random
  ; test_digest
  ; test_bigarray
  ]

let jsdiff_marker = "(* jsdiff *)"

let program ?(ntests = 60) seed =
  let g = { st = Rng.make seed; jsdiff = false } in
  let lines =
    List.init ntests (fun n ->
        g.jsdiff <- false;
        let e = (pick g tests) g in
        sprintf
          "let () = t %d (fun () -> %s)%s"
          n
          e
          (if g.jsdiff then " " ^ jsdiff_marker else ""))
  in
  prelude ^ String.concat "\n" lines ^ "\n"
