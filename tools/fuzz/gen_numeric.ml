(* Random numeric kernels: loops, references, exception handlers, closures,
   arrays and bigarrays over floats, int32, int64, nativeint and integers.
   They exercise the placement of the conversions between boxed or tagged
   values and machine numbers in wasm_of_ocaml, which the programs of
   [Gen_ocaml] hardly reach (they only print their boxed numbers).

   Invariants that keep the output identical with ocamlrun (63-bit
   integers and nativeints), js_of_ocaml (32-bit) and wasm_of_ocaml
   (31-bit integers, 32-bit nativeints):
   - integer and nativeint values are in [0, 2^30). Integer arithmetic
     that may overflow (addition, subtraction, multiplication, left
     shift, bitwise operations) is computed on any value and then masked
     to 30 bits, since its low bits do not depend on the word size.
     Division, right shifts and comparisons only see masked values;
   - int32 and int64 have the same width everywhere: any value;
   - floats only use exactly rounded operations; NaNs are printed as
     "nan" (their sign is not portable);
   - float conversions to integers are applied to values in range;
   - loops have constant bounds, recursion is bounded by a fuel
     argument, and the calls between kernels are limited so that the
     running time stays small;
   - exceptions are only raised inside handlers that catch them. *)

let sprintf = Printf.sprintf

type ty =
  | I
  | F
  | I32
  | I64
  | N
  | B

let all_tys = [ I; F; I32; I64; N; B ]

let num_tys = [ I; F; I32; I64; N ]

let ty_name = function
  | I -> "int"
  | F -> "float"
  | I32 -> "int32"
  | I64 -> "int64"
  | N -> "nativeint"
  | B -> "bool"

let printer = function
  | I -> "pi"
  | F -> "pf"
  | I32 -> "p32"
  | I64 -> "p64"
  | N -> "pn"
  | B -> "pb"

let prelude =
  {|exception E of int
type fr = { x : float; mutable y : float }
let pi x = Printf.printf "%d\n" x
let pf x = if Float.is_nan x then print_endline "nan" else Printf.printf "%h\n" x
let p32 x = Printf.printf "%ld\n" x
let p64 x = Printf.printf "%Ld\n" x
let pn x = Printf.printf "%nd\n" x
let pb x = Printf.printf "%b\n" x|}

(* Containers whose elements can be read and written *)
type cont =
  | Arr of string * ty * int  (** an int or float array, of the given length *)
  | Ba of string * ty * int * bool
      (** a one-dimensional bigarray of the given length; float32
          elements when the boolean is true *)
  | Rec of string  (** a [fr] record: [x] is immutable, [y] mutable *)
  | Pair of string * ty * ty

type func =
  { fname : string
  ; params : ty list
  ; ret : ty
  ; fuel : bool  (** the first argument is the fuel *)
  }

type env =
  { vars : (string * ty * bool) list  (** name, type, is a reference *)
  ; conts : cont list
  ; clos : (string * ty * ty) list  (** local closures *)
  ; in_loop : bool
  ; in_try : bool
  ; self : func option  (** the enclosing kernel, when it can be called *)
  ; nesting : int  (** loop nesting *)
  }

type g =
  { st : Rng.t
  ; mutable n : int
  ; mutable helpers : func list
  ; mutable kernels : func list
  ; mutable kernel_calls : int  (** calls to other kernels in the current one *)
  ; mutable self_calls : int
  }

let fresh g p =
  g.n <- g.n + 1;
  sprintf "%s%d" p g.n

let chance g p = Rng.chance g.st p

let pick g l = Rng.pick g.st l

let int g lo hi = Rng.range g.st lo hi

let mask = "0x3fffffff"

let vars_of env ty = List.filter (fun (_, t, _) -> t = ty) env.vars

let read (x, _, is_ref) = if is_ref then "!" ^ x else x

(* Literals, including values at the edges of the representations *)
let literal g ty =
  match ty with
  | I ->
      pick
        g
        [ "0"; "1"; "2"; "7"; "100"; "255"; "1000"; "65535"; "0x3fffffff"; "0x20000000" ]
  | F ->
      pick
        g
        [ "0."
        ; "(-0.)"
        ; "1."
        ; "(-1.5)"
        ; "0.1"
        ; "2.5"
        ; "3.25"
        ; "1e16"
        ; "1e300"
        ; "(-1e-300)"
        ; "infinity"
        ; "neg_infinity"
        ; "0x1p-1074"
        ; "4503599627370497."
        ]
  | I32 ->
      pick
        g
        [ "0l"
        ; "1l"
        ; "(-1l)"
        ; "7l"
        ; "1000l"
        ; "0x7fffl"
        ; "Int32.max_int"
        ; "Int32.min_int"
        ; "0x40000000l"
        ; "(-0x40000001l)"
        ]
  | I64 ->
      pick
        g
        [ "0L"
        ; "1L"
        ; "(-1L)"
        ; "1000L"
        ; "0x7fffffffL"
        ; "0x80000000L"
        ; "0x100000000L"
        ; "Int64.max_int"
        ; "Int64.min_int"
        ; "0x3fffffffffffffffL"
        ]
  | N -> pick g [ "0n"; "1n"; "5n"; "1000n"; "0x3fffffffn"; "0x20000000n" ]
  | B -> pick g [ "true"; "false" ]

(* Values of any width, whose low 30 bits are meaningful; only used below
   a mask *)
let ring_literal g =
  pick
    g
    [ "0x7fffffff"
    ; "0x40000000"
    ; "(-1)"
    ; "0xffffffff"
    ; "(-0x40000000)"
    ; "0x1_0000_0001"
    ; "max_int"
    ; "min_int"
    ]

let leaf g env ty =
  let vs = vars_of env ty in
  if vs <> [] && chance g 0.75 then read (pick g vs) else literal g ty

let rec expr g env ty d =
  if d <= 0
  then leaf g env ty
  else
    let d' = d - 1 in
    let sub ty = expr g env ty d' in
    let conts = env.conts in
    let reads =
      List.concat_map
        (fun c ->
          match c with
          | Arr (a, t, len) when t = ty ->
              [ (fun () -> sprintf "%s.(%s mod %d)" a (sub I) len) ]
          | Ba (a, t, len, _) when t = ty ->
              [ (fun () -> sprintf "(Bigarray.Array1.get %s (%s mod %d))" a (sub I) len) ]
          | Rec r when ty = F -> [ (fun () -> r ^ ".x"); (fun () -> r ^ ".y") ]
          | Pair (p, t, _) when t = ty -> [ (fun () -> sprintf "(fst %s)" p) ]
          | Pair (p, _, t) when t = ty -> [ (fun () -> sprintf "(snd %s)" p) ]
          | _ -> [])
        conts
    in
    let calls =
      List.filter_map
        (fun (h, a, r) ->
          if r = ty then Some (fun () -> sprintf "(%s %s)" h (sub a)) else None)
        env.clos
      @ List.filter_map
          (fun f -> if f.ret = ty then Some (fun () -> call g env f d') else None)
          g.helpers
      @ (if env.in_loop || g.kernel_calls >= 2
         then []
         else
           List.filter_map
             (fun f ->
               if f.ret = ty
               then
                 Some
                   (fun () ->
                     g.kernel_calls <- g.kernel_calls + 1;
                     call g env f d')
               else None)
             g.kernels)
      @
      match env.self with
      | Some f when f.ret = ty && (not env.in_loop) && g.self_calls < 2 ->
          [ (fun () ->
              g.self_calls <- g.self_calls + 1;
              call g env f d')
          ]
      | _ -> []
    in
    let cond () = sprintf "(if %s then %s else %s)" (sub B) (sub ty) (sub ty) in
    let specific =
      match ty with
      | I ->
          [ (fun () -> sprintf "((%s) land %s)" (ring g env d') mask)
          ; (fun () -> sprintf "((%s) land %s)" (ring g env d') mask)
          ; (fun () ->
              (* no overflow: the result is not masked *)
              sprintf "((%s land 0xffff) + (%s land 0xffff))" (sub I) (sub I))
          ; (fun () -> sprintf "((%s land 0x3fff) * (%s land 0x3fff))" (sub I) (sub I))
          ; (fun () ->
              sprintf "(0x10000 + (%s land 0xffff) - (%s land 0xffff))" (sub I) (sub I))
          ; (fun () -> sprintf "((%s land 0xfffffff) + %d)" (sub I) (int g 1 100))
          ; (fun () -> sprintf "(%s / (%s lor 1))" (sub I) (sub I))
          ; (fun () -> sprintf "(%s mod (%s lor 1))" (sub I) (sub I))
          ; (fun () -> sprintf "(%s lsr %d)" (sub I) (int g 0 30))
          ; (fun () -> sprintf "(%s asr %d)" (sub I) (int g 0 30))
          ; (fun () -> sprintf "(min %s %s)" (sub I) (sub I))
          ; (fun () -> sprintf "(Int32.to_int (Int32.logand %s 0x3fffffffl))" (sub I32))
          ; (fun () -> sprintf "(Int64.to_int (Int64.logand %s 0x3fffffffL))" (sub I64))
          ; (fun () -> sprintf "(Nativeint.to_int %s)" (sub N))
          ; (fun () ->
              let x = fresh g "f" in
              sprintf
                "(let %s = %s in if Float.is_nan %s || Float.abs %s = infinity then 3 \
                 else int_of_float (Float.rem %s 1e9) land %s)"
                x
                (sub F)
                x
                x
                x
                mask)
          ; (fun () -> sprintf "(compare %s %s + 1)" (sub F) (sub F))
          ; (fun () ->
              let t = pick g [ I32; I64; N ] in
              sprintf "(compare %s %s + 1)" (sub t) (sub t))
          ; (fun () -> sprintf "(if %s then 1 else 0)" (sub B))
          ]
      | F ->
          [ (fun () -> sprintf "(%s +. %s)" (sub F) (sub F))
          ; (fun () -> sprintf "(%s -. %s)" (sub F) (sub F))
          ; (fun () -> sprintf "(%s *. %s)" (sub F) (sub F))
          ; (fun () -> sprintf "(%s /. %s)" (sub F) (sub F))
          ; (fun () -> sprintf "(Float.sqrt %s)" (sub F))
          ; (fun () -> sprintf "(Float.abs %s)" (sub F))
          ; (fun () -> sprintf "(-. %s)" (sub F))
          ; (fun () -> sprintf "(Float.min %s %s)" (sub F) (sub F))
          ; (fun () -> sprintf "(Float.max %s %s)" (sub F) (sub F))
          ; (fun () -> sprintf "(Float.rem %s %s)" (sub F) (sub F))
          ; (fun () ->
              sprintf
                "(%s %s)"
                (pick g [ "Float.trunc"; "Float.floor"; "Float.ceil"; "Float.round" ])
                (sub F))
          ; (fun () -> sprintf "(float_of_int %s)" (sub I))
          ; (fun () -> sprintf "(Int32.to_float %s)" (sub I32))
          ; (fun () ->
              sprintf "(Int64.to_float (Int64.logand %s 0xfffffffffffffL))" (sub I64))
          ; (fun () -> sprintf "(Nativeint.to_float %s)" (sub N))
          ]
      | I32 ->
          [ (fun () -> sprintf "(Int32.add %s %s)" (sub I32) (sub I32))
          ; (fun () -> sprintf "(Int32.sub %s %s)" (sub I32) (sub I32))
          ; (fun () -> sprintf "(Int32.mul %s %s)" (sub I32) (sub I32))
          ; (fun () -> sprintf "(Int32.div %s (Int32.logor %s 1l))" (sub I32) (sub I32))
          ; (fun () -> sprintf "(Int32.rem %s (Int32.logor %s 1l))" (sub I32) (sub I32))
          ; (fun () ->
              sprintf
                "(Int32.%s %s %s)"
                (pick g [ "logand"; "logor"; "logxor" ])
                (sub I32)
                (sub I32))
          ; (fun () ->
              sprintf
                "(Int32.%s %s %d)"
                (pick g [ "shift_left"; "shift_right"; "shift_right_logical" ])
                (sub I32)
                (int g 0 31))
          ; (fun () -> sprintf "(Int32.neg %s)" (sub I32))
          ; (fun () -> sprintf "(Int32.of_int %s)" (sub I))
          ; (fun () -> sprintf "(Int64.to_int32 %s)" (sub I64))
          ; (fun () -> sprintf "(Nativeint.to_int32 %s)" (sub N))
          ; (fun () ->
              let x = fresh g "f" in
              sprintf
                "(let %s = %s in if Float.is_nan %s || Float.abs %s >= 1e9 then 5l else \
                 Int32.of_float %s)"
                x
                (sub F)
                x
                x
                x)
          ]
      | I64 ->
          [ (fun () -> sprintf "(Int64.add %s %s)" (sub I64) (sub I64))
          ; (fun () -> sprintf "(Int64.sub %s %s)" (sub I64) (sub I64))
          ; (fun () -> sprintf "(Int64.mul %s %s)" (sub I64) (sub I64))
          ; (fun () -> sprintf "(Int64.div %s (Int64.logor %s 1L))" (sub I64) (sub I64))
          ; (fun () -> sprintf "(Int64.rem %s (Int64.logor %s 1L))" (sub I64) (sub I64))
          ; (fun () ->
              sprintf
                "(Int64.%s %s %s)"
                (pick g [ "logand"; "logor"; "logxor" ])
                (sub I64)
                (sub I64))
          ; (fun () ->
              sprintf
                "(Int64.%s %s %d)"
                (pick g [ "shift_left"; "shift_right"; "shift_right_logical" ])
                (sub I64)
                (int g 0 63))
          ; (fun () -> sprintf "(Int64.neg %s)" (sub I64))
          ; (fun () -> sprintf "(Int64.of_int %s)" (sub I))
          ; (fun () -> sprintf "(Int64.of_int32 %s)" (sub I32))
          ; (fun () -> sprintf "(Int64.of_nativeint %s)" (sub N))
          ; (fun () ->
              let x = fresh g "f" in
              sprintf
                "(let %s = %s in if Float.is_nan %s || Float.abs %s >= 1e18 then 9L else \
                 Int64.of_float %s)"
                x
                (sub F)
                x
                x
                x)
          ]
      | N ->
          [ (fun () -> sprintf "(Nativeint.logand (%s) 0x3fffffffn)" (nring g env d'))
          ; (fun () -> sprintf "(Nativeint.logand (%s) 0x3fffffffn)" (nring g env d'))
          ; (fun () ->
              sprintf "(Nativeint.div %s (Nativeint.logor %s 1n))" (sub N) (sub N))
          ; (fun () ->
              sprintf "(Nativeint.rem %s (Nativeint.logor %s 1n))" (sub N) (sub N))
          ; (fun () ->
              sprintf
                "(Nativeint.%s %s %d)"
                (pick g [ "shift_right"; "shift_right_logical" ])
                (sub N)
                (int g 0 30))
          ; (fun () -> sprintf "(Nativeint.of_int %s)" (sub I))
          ]
      | B ->
          [ (fun () ->
              let t = pick g num_tys in
              sprintf
                "(%s %s %s)"
                (sub t)
                (pick g [ "<"; "<="; "="; "<>"; ">"; ">=" ])
                (sub t))
          ; (fun () -> sprintf "(%s land 1 = 0)" (sub I))
          ; (fun () -> sprintf "(Float.is_nan %s)" (sub F))
          ; (fun () -> sprintf "(not %s)" (sub B))
          ; (fun () -> sprintf "(%s && %s)" (sub B) (sub B))
          ; (fun () -> sprintf "(%s || %s)" (sub B) (sub B))
          ; (fun () -> sprintf "(Int32.equal %s %s)" (sub I32) (sub I32))
          ; (fun () -> sprintf "(Int64.compare %s %s < 0)" (sub I64) (sub I64))
          ]
    in
    let choices =
      List.concat
        [ specific
        ; reads
        ; reads
        ; calls
        ; calls
        ; [ cond; (fun () -> leaf g env ty); (fun () -> leaf g env ty) ]
        ]
    in
    (pick g choices) ()

(* Integer expressions of any width; only their low 30 bits are used *)
and ring g env d =
  if d <= 0
  then if chance g 0.3 then ring_literal g else leaf g env I
  else
    let sub () = ring g env (d - 1) in
    match int g 0 9 with
    | 0 -> sprintf "(%s + %s)" (sub ()) (sub ())
    | 1 -> sprintf "(%s - %s)" (sub ()) (sub ())
    | 2 | 3 -> sprintf "(%s * %s)" (sub ()) (sub ())
    | 4 -> sprintf "(%s lsl %d)" (sub ()) (int g 0 29)
    | 5 -> sprintf "(%s %s %s)" (sub ()) (pick g [ "land"; "lor"; "lxor" ]) (sub ())
    | 6 -> sprintf "(lnot %s)" (sub ())
    | 7 -> sprintf "(- %s)" (sub ())
    | 8 -> ring_literal g
    | _ -> expr g env I (d - 1)

and nring g env d =
  if d <= 0
  then if chance g 0.3 then "0xffffffffn" else leaf g env N
  else
    let sub () = nring g env (d - 1) in
    match int g 0 7 with
    | 0 -> sprintf "(Nativeint.add %s %s)" (sub ()) (sub ())
    | 1 -> sprintf "(Nativeint.sub %s %s)" (sub ()) (sub ())
    | 2 -> sprintf "(Nativeint.mul %s %s)" (sub ()) (sub ())
    | 3 -> sprintf "(Nativeint.shift_left %s %d)" (sub ()) (int g 0 29)
    | 4 ->
        sprintf
          "(Nativeint.%s %s %s)"
          (pick g [ "logand"; "logor"; "logxor" ])
          (sub ())
          (sub ())
    | 5 -> sprintf "(Nativeint.neg %s)" (sub ())
    | 6 -> sprintf "(Nativeint.of_int32 %s)" (expr g env I32 (d - 1))
    | _ -> expr g env N (d - 1)

and call g env f d =
  let args = List.map (fun t -> expr g env t (min d 2)) f.params in
  let args =
    if f.fuel
    then
      (match env.self with
      | Some s when s.fname = f.fname -> "(fuel - 1)"
      | _ -> string_of_int (int g 0 3))
      :: args
    else args
  in
  sprintf "(%s %s)" f.fname (String.concat " " args)

(* Statements, of type unit *)
(* The depth of the expressions in statements, independent of the
   nesting of statements *)
let edepth g = int g 1 3

let rec stmt g env d =
  let d' = d - 1 in
  let refs = List.filter (fun (_, _, r) -> r) env.vars in
  let writes =
    List.concat_map
      (fun c ->
        match c with
        | Arr (a, t, len) ->
            [ (fun () ->
                sprintf
                  "%s.(%s mod %d) <- %s"
                  a
                  (expr g env I 1)
                  len
                  (expr g env t (edepth g)))
            ]
        | Ba (a, t, len, _) ->
            [ (fun () ->
                sprintf
                  "Bigarray.Array1.set %s (%s mod %d) %s"
                  a
                  (expr g env I 1)
                  len
                  (expr g env t (edepth g)))
            ]
        | Rec r -> [ (fun () -> sprintf "%s.y <- %s" r (expr g env F (edepth g))) ]
        | Pair _ -> [])
      env.conts
  in
  let assigns =
    List.map (fun (x, t, _) () -> sprintf "%s := %s" x (expr g env t (edepth g))) refs
  in
  let accumulates =
    List.map
      (fun (x, t, _) () ->
        let e = expr g env t (edepth g) in
        let r = "!" ^ x in
        sprintf
          "%s := %s"
          x
          (match t with
          | I -> sprintf "(%s %s %s) land %s" r (pick g [ "+"; "*"; "lxor"; "-" ]) e mask
          | F -> sprintf "%s %s %s" r (pick g [ "+."; "*."; "-."; "/." ]) e
          | I32 -> sprintf "Int32.%s %s %s" (pick g [ "add"; "mul"; "logxor"; "sub" ]) r e
          | I64 -> sprintf "Int64.%s %s %s" (pick g [ "add"; "mul"; "logxor"; "sub" ]) r e
          | N ->
              sprintf
                "Nativeint.logand (Nativeint.%s %s %s) 0x3fffffffn"
                (pick g [ "add"; "mul"; "logxor"; "sub" ])
                r
                e
          | B -> sprintf "%s %s %s" r (pick g [ "&&"; "||"; "<>" ]) e))
      refs
  in
  let simple = assigns @ accumulates @ accumulates @ writes in
  let simple =
    if simple = [] then [ (fun () -> "ignore " ^ expr g env I (edepth g)) ] else simple
  in
  if d <= 0
  then (pick g simple) ()
  else
    let loop_env = { env with in_loop = true; nesting = env.nesting + 1 } in
    let compound =
      [ (fun () ->
          sprintf
            "if %s then (%s) else (%s)"
            (expr g env B (edepth g))
            (stmt g env d')
            (stmt g env d'))
      ; (fun () ->
          sprintf
            "(match %s mod 3 with 0 -> %s | 1 -> %s | _ -> %s)"
            (expr g env I (edepth g))
            (stmt g env d')
            (stmt g env d')
            (stmt g env d'))
      ; (fun () ->
          let t = pick g num_tys in
          let x = fresh g "x" in
          let e = expr g env t (edepth g) in
          sprintf
            "(let %s = %s in %s)"
            x
            e
            (stmt g { env with vars = (x, t, false) :: env.vars } d'))
      ; (fun () -> sprintf "%s; %s" (stmt g env d') (stmt g env d'))
      ]
      @ (if env.nesting < 3
         then
           [ (fun () ->
               let i = fresh g "i" in
               sprintf
                 "for %s = 0 to %d do %s done"
                 i
                 (int g 0 7)
                 (stmt g { loop_env with vars = (i, I, false) :: env.vars } d'))
           ; (fun () ->
               let c = fresh g "c" in
               sprintf
                 "(let %s = ref 0 in while !%s < %d do %s; incr %s done)"
                 c
                 c
                 (int g 1 6)
                 (* the counter is read-only in the body *)
                 (stmt g { loop_env with vars = ("!" ^ c, I, false) :: env.vars } d')
                 c)
           ]
         else [])
      @ (if env.nesting < 3
         then
           List.filter_map
             (fun c ->
               let scan t len get =
                 (* a scan over the elements, with the index in a reference
                    and the element available in the body and the condition *)
                 let i = fresh g "j" in
                 let elt = get i in
                 let body_env =
                   { loop_env with
                     vars = ("!" ^ i, I, false) :: (elt, t, false) :: env.vars
                   }
                 in
                 let cond = expr g { body_env with in_loop = true } B (edepth g) in
                 if chance g 0.5
                 then
                   sprintf
                     "(let %s = ref %d in while !%s < %d && %s do %s; incr %s done)"
                     i
                     (int g 0 (len - 1))
                     i
                     len
                     cond
                     (stmt g body_env d')
                     i
                 else
                   sprintf
                     "(let %s = ref %d in while !%s >= 0 && %s do %s; decr %s done)"
                     i
                     (int g 0 (len - 1))
                     i
                     cond
                     (stmt g body_env d')
                     i
               in
               let partition a t len =
                 (* the loops of quicksort's partition: the indices flow
                    through several loop headers. The outer loop
                    terminates since [i] increases and [j] never does. *)
                 let i = fresh g "j" and j = fresh g "j" in
                 let ai = sprintf "%s.(!%s)" a i and aj = sprintf "%s.(!%s)" a j in
                 let env' =
                   { loop_env with
                     vars = ("!" ^ i, I, false) :: ("!" ^ j, I, false) :: env.vars
                   }
                 in
                 let inner_env x =
                   { env' with vars = (x, t, false) :: env'.vars; nesting = 3 }
                 in
                 let cond x = expr g (inner_env x) B (edepth g) in
                 let body x = stmt g (inner_env x) 0 in
                 sprintf
                   "(let %s = ref 0 in let %s = ref %d in while !%s < !%s do while !%s < \
                    %d && %s do %s; incr %s done; while !%s > 0 && %s do %s; decr %s \
                    done; if !%s < !%s then (let t = %s in %s <- %s; %s <- t); incr %s \
                    done)"
                   i
                   j
                   (len - 1)
                   i
                   j
                   i
                   (len - 1)
                   (cond ai)
                   (body ai)
                   i
                   j
                   (cond aj)
                   (body aj)
                   j
                   i
                   j
                   ai
                   ai
                   aj
                   aj
                   i
               in
               match c with
               | Arr (a, t, len) ->
                   Some
                     (fun () ->
                       if chance g 0.5
                       then partition a t len
                       else scan t len (fun i -> sprintf "%s.(!%s)" a i))
               | Ba (a, t, len, _) ->
                   Some
                     (fun () ->
                       scan t len (fun i -> sprintf "(Bigarray.Array1.get %s !%s)" a i))
               | Rec _ | Pair _ -> None)
             env.conts
         else [])
      @ (if env.in_loop || chance g 0.3
         then
           [ (fun () ->
               let k = fresh g "k" in
               let r = if refs = [] then None else Some (pick g refs) in
               sprintf
                 "(try %s with E %s -> %s)"
                 (stmt g { env with in_try = true } d')
                 k
                 (match r with
                 | Some (x, t, _) ->
                     sprintf
                       "%s := %s"
                       x
                       (expr g { env with vars = (k, I, false) :: env.vars } t 1)
                 | None -> "ignore " ^ k))
           ]
         else [])
      @
      if env.in_try
      then
        [ (fun () ->
            sprintf "if %s then raise (E %s)" (expr g env B (edepth g)) (expr g env I 1))
        ]
      else []
    in
    (pick g (simple @ compound @ compound)) ()

(* Local definitions followed by an expression of type [ret] *)
let rec block g env ret d n =
  if n = 0
  then expr g env ret d
  else
    let def, env =
      match int g 0 11 with
      | 0 | 1 ->
          let t = pick g num_tys in
          let x = fresh g "x" in
          ( sprintf "let %s = %s in" x (expr g env t d)
          , { env with vars = (x, t, false) :: env.vars } )
      | 2 | 3 ->
          let t = pick g num_tys in
          let x = fresh g "r" in
          ( sprintf "let %s = ref %s in" x (expr g env t d)
          , { env with vars = (x, t, true) :: env.vars } )
      | 4 | 5 | 6 -> sprintf "%s;" (stmt g env d), env
      | 7 ->
          let t = pick g [ I; F ] in
          let a = fresh g "a" in
          let len = int g 1 8 in
          let init =
            if chance g 0.5
            then sprintf "Array.make %d %s" len (expr g env t (d - 1))
            else
              let i = fresh g "i" in
              sprintf
                "Array.init %d (fun %s -> %s)"
                len
                i
                (expr
                   g
                   { env with in_loop = true; vars = (i, I, false) :: env.vars }
                   t
                   (d - 1))
          in
          ( sprintf "let %s = %s in" a init
          , { env with conts = Arr (a, t, len) :: env.conts } )
      | 8 ->
          let t, kind, f32 =
            pick
              g
              [ F, "float64", false
              ; F, "float32", true
              ; I32, "int32", false
              ; I64, "int64", false
              ; N, "nativeint", false
              ; I, "int", false
              ]
          in
          let a = fresh g "ba" in
          let len = int g 1 8 in
          ( sprintf
              "let %s = Bigarray.Array1.create Bigarray.%s Bigarray.c_layout %d in \
               Bigarray.Array1.fill %s %s;"
              a
              kind
              len
              a
              (expr g env t (d - 1))
          , { env with conts = Ba (a, t, len, f32) :: env.conts } )
      | 9 ->
          let r = fresh g "rc" in
          ( sprintf "let %s = { x = %s; y = %s } in" r (expr g env F d) (expr g env F d)
          , { env with conts = Rec r :: env.conts } )
      | 10 ->
          let t1 = pick g num_tys and t2 = pick g num_tys in
          let p = fresh g "p" in
          ( sprintf "let %s = (%s, %s) in" p (expr g env t1 d) (expr g env t2 d)
          , { env with conts = Pair (p, t1, t2) :: env.conts } )
      | _ ->
          let a = pick g num_tys and r = pick g num_tys in
          let h = fresh g "h" and y = fresh g "y" in
          let body_env =
            { env with vars = (y, a, false) :: env.vars; in_loop = true; in_try = false }
          in
          ( sprintf "let %s = fun (%s : %s) -> %s in" h y (ty_name a) (expr g body_env r d)
          , { env with clos = (h, a, r) :: env.clos } )
    in
    def ^ "\n  " ^ block g env ret d (n - 1)

let empty_env =
  { vars = []
  ; conts = []
  ; clos = []
  ; in_loop = false
  ; in_try = false
  ; self = None
  ; nesting = 0
  }

let params g n = List.init n (fun _ -> pick g num_tys)

let header f names =
  sprintf
    "%s %s"
    f.fname
    (String.concat
       " "
       ((if f.fuel then [ "(fuel : int)" ] else [])
       @ List.map2 (fun x t -> sprintf "(%s : %s)" x (ty_name t)) names f.params))

(* Straight-line helpers, whose parameters are typically used unboxed *)
let helper g =
  let f =
    { fname = fresh g "h"
    ; params = params g (int g 1 3)
    ; ret = pick g num_tys
    ; fuel = false
    }
  in
  let names = List.map (fun _ -> fresh g "a") f.params in
  let env =
    (* [in_loop]: no calls to kernels *)
    { empty_env with
      vars = List.map2 (fun x t -> x, t, false) names f.params
    ; in_loop = true
    }
  in
  let body = expr g env f.ret 3 in
  g.helpers <- g.helpers @ [ f ];
  sprintf "let %s : %s = %s" (header f names) (ty_name f.ret) body

let kernel g globals =
  let recursive = chance g 0.3 in
  let f =
    { fname = fresh g "k"
    ; params = params g (int g 0 3)
    ; ret = pick g all_tys
    ; fuel = true
    }
  in
  let names = List.map (fun _ -> fresh g "a") f.params in
  let env =
    { empty_env with
      vars = List.map2 (fun x t -> x, t, false) names f.params @ globals
    ; self = (if recursive then Some f else None)
    }
  in
  g.kernel_calls <- (if recursive then 2 else 0);
  g.self_calls <- 0;
  let body = block g env f.ret 2 (int g 2 6) in
  let body =
    if recursive
    then
      sprintf
        "if fuel <= 0 then %s\n  else (\n  %s)"
        (expr g { env with self = None } f.ret 2)
        body
    else body
  in
  g.kernels <- g.kernels @ [ f ];
  sprintf
    "let %s%s : %s =\n  %s"
    (if recursive then "rec " else "")
    (header f names)
    (ty_name f.ret)
    body

let program seed =
  let g =
    { st = Rng.make seed
    ; n = 0
    ; helpers = []
    ; kernels = []
    ; kernel_calls = 0
    ; self_calls = 0
    }
  in
  let globals =
    List.init (int g 0 3) (fun _ ->
        let t = pick g num_tys in
        fresh g "g", t)
  in
  let global_defs =
    List.map (fun (x, t) -> sprintf "let %s = %s" x (expr g empty_env t 2)) globals
  in
  let globals = List.map (fun (x, t) -> x, t, false) globals in
  let items = ref [] in
  for _ = 1 to int g 4 7 do
    items := (if chance g 0.4 then helper g else kernel g globals) :: !items
  done;
  let tests =
    List.map
      (fun f ->
        g.kernel_calls <- 0;
        sprintf
          "let () = try %s %s with E k -> Printf.printf \"E %%d\\n\" k"
          (printer f.ret)
          (call g { empty_env with vars = globals; in_loop = true } f 2))
      (g.kernels @ g.helpers)
  in
  String.concat "\n\n" ((prelude :: global_defs) @ List.rev !items @ tests) ^ "\n"
