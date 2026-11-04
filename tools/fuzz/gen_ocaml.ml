(* Random OCaml programs whose output does not depend on the target.

   Invariants that keep the output identical with ocamlrun (63-bit
   integers), js_of_ocaml (32-bit) and wasm_of_ocaml (31-bit):
   - integer values stay in [0, 2^15): every arithmetic result is masked;
   - recursion is bounded by a fuel argument;
   - floats only use exactly rounded operations, and are printed in
     hexadecimal;
   - no physical equality, no unspecified evaluation order: every operand
     with a side effect is let-bound first. *)

let sprintf = Printf.sprintf

let m = "32767"

let prelude =
  {|exception E of int
exception F
type v = A | B of int | C of int * int
type r = { a : int; mutable b : int }
type fr = { x : float; mutable y : float }
let pi x = Printf.printf "%d\n" x
let pf x = Printf.printf "%h\n" x
let ps x = Printf.printf "%S\n" x
let pb x = Printf.printf "%b\n" x
let pl l = print_endline (String.concat "," (List.map string_of_int l))
module type S = sig val f : int -> int end
module Mk (X : S) = struct
  let g x = (X.f x + 1) land 32767
  let h l = List.map g l
end
module IM = Map.Make (Int)
module SM = Map.Make (String)
type mr = { m : int; mutable fl : float }
let p32 x = Printf.printf "%ld\n" x
let p64 x = Printf.printf "%Ld\n" x
type _ Effect.t += Get : int Effect.t | Put : int -> unit Effect.t | Abort : int Effect.t
let run_state (f : unit -> int) (init : int) : int =
  let s = ref init in
  Effect.Deep.match_with f ()
    { retc = (fun x -> (x + !s) land 32767);
      exnc = raise;
      effc =
        (fun (type a) (e : a Effect.t) : ((a, int) Effect.Deep.continuation -> int) option ->
          match e with
          | Get -> Some (fun k -> Effect.Deep.continue k !s)
          | Put n -> Some (fun k -> s := n; Effect.Deep.continue k ())
          | Abort -> Some (fun k -> Effect.Deep.discontinue k (E 99))
          | _ -> None) }|}

type ty =
  | Int
  | Bool
  | Float
  | List
  | Arr
  | F1
  | F2
  | Pair
  | Opt
  | V
  | R
  | Fr
  | Ref
  | Str
  | I32
  | I64
  | Lazy
  | Farr

let all_types =
  [ Int
  ; Bool
  ; Float
  ; List
  ; Arr
  ; F1
  ; F2
  ; Pair
  ; Opt
  ; V
  ; R
  ; Fr
  ; Ref
  ; Str
  ; I32
  ; I64
  ; Lazy
  ; Farr
  ]

(* Variables in scope; [eff]: an effect handler is installed *)
type env =
  { vars : (string * ty) list
  ; eff : bool
  }

let empty = { vars = []; eff = false }

let ( +: ) env l = { env with vars = l @ env.vars }

let no_eff env = { env with eff = false }

type kind =
  | Pure
  | Rec
  | Mutual
  | Try
  | Hof
  | Label
  | Optional

type t =
  { st : Rng.t
  ; size : int
  ; mutable n : int
  ; mutable funs : (string * kind * int) list  (** toplevel functions *)
  ; mutable uses_effects : bool
  }

let fresh g p =
  g.n <- g.n + 1;
  sprintf "%s%d" p g.n

let chance g p = Rng.chance g.st p

let pick g l = Rng.pick g.st l

let int g lo hi = Rng.range g.st lo hi

let var g env ty =
  match List.filter (fun (_, t) -> t = ty) env.vars with
  | [] -> None
  | l -> Some (fst (pick g l))

(* Leaves *)

let lit_int g =
  let r = Random.State.float g.st 1. in
  string_of_int
    (if r < 0.3 then int g 0 3 else if r < 0.8 then int g 0 100 else int g 0 32767)

let lit_float g =
  pick g [ "0."; "1."; "0.5"; "2.25"; "3."; "1e3"; "0.125"; "7.5"; "1e-3" ]

let lits g f n = String.concat "; " (List.init (int g 0 n) (fun _ -> f g))

let pure_funs g arity =
  List.filter_map (fun (f, k, a) -> if k = Pure && a = arity then Some f else None) g.funs

let leaf g ty env =
  match var g env ty with
  | Some v when chance g 0.6 -> v
  | _ -> (
      match ty with
      | Int -> if env.eff && chance g 0.2 then "(Effect.perform Get)" else lit_int g
      | Bool -> pick g [ "true"; "false" ]
      | Float -> lit_float g
      | List -> sprintf "[%s]" (lits g lit_int 3)
      | Arr -> sprintf "[|%s|]" (lits g lit_int 3)
      | F1 -> (
          match pure_funs g 1 with
          | _ :: _ as l when chance g 0.5 -> pick g l
          | _ ->
              let x = fresh g "x" in
              sprintf "(fun %s -> (%s + %s) land %s)" x x (lit_int g) m)
      | F2 -> (
          match pure_funs g 2 with
          | _ :: _ as l when chance g 0.5 -> pick g l
          | _ ->
              let x = fresh g "x" and y = fresh g "x" in
              sprintf "(fun %s %s -> (%s * 3 + %s) land %s)" x y x y m)
      | Pair -> sprintf "(%s, %s)" (lit_int g) (lit_int g)
      | Opt -> pick g [ "None"; sprintf "(Some %s)" (lit_int g) ]
      | V ->
          pick
            g
            [ "A"
            ; sprintf "(B %s)" (lit_int g)
            ; sprintf "(C (%s, %s))" (lit_int g) (lit_int g)
            ]
      | R -> sprintf "{ a = %s; b = %s }" (lit_int g) (lit_int g)
      | Fr -> sprintf "{ x = %s; y = %s }" (lit_float g) (lit_float g)
      | Ref -> sprintf "(ref %s)" (lit_int g)
      | Str -> sprintf "%S" (pick g [ ""; "a"; "abc"; "hello"; "x y"; "\n" ])
      | I32 ->
          sprintf
            "(%s)"
            (pick
               g
               [ "0l"
               ; "1l"
               ; "-1l"
               ; "2147483647l"
               ; "-2147483648l"
               ; sprintf "%dl" (int g (-100000) 100000)
               ])
      | I64 ->
          sprintf
            "(%s)"
            (pick
               g
               [ "0L"
               ; "1L"
               ; "-1L"
               ; "9223372036854775807L"
               ; "-9223372036854775808L"
               ; sprintf "%dL" (int g (-1_000_000_000_000) 1_000_000_000_000)
               ])
      | Lazy -> sprintf "(lazy %s)" (lit_int g)
      | Farr -> sprintf "[|%s|]" (lits g lit_float 3))

(* Expressions *)

let rec gen g ty env d =
  if d <= 0 || chance g 0.15
  then leaf g ty env
  else
    let d = d - 1 in
    match ty with
    | Int -> gen_int g env d
    | Bool -> gen_bool g env d
    | Float -> gen_float g env d
    | List -> gen_list g env d
    | Arr -> gen_arr g env d
    | F1 -> gen_f1 g env d
    | F2 -> gen_f2 g env d
    | Pair -> gen_pair g env d
    | Opt -> gen_opt g env d
    | V -> gen_v g env d
    | R -> gen_r g env d
    | Fr -> sprintf "{ x = %s; y = %s }" (gen g Float env d) (gen g Float env d)
    | Ref -> sprintf "(ref %s)" (gen g Int env d)
    | Str -> gen_str g env d
    | I32 -> gen_i32 g env d
    | I64 -> gen_i64 g env d
    | Lazy ->
        let c = fresh g "c" in
        sprintf
          "(let %s = ref 0 in lazy (incr %s; (%s + ! %s) land %s))"
          c
          c
          (gen g Int (no_eff env) d)
          c
          m
    | Farr -> gen_farr g env d

and perform g env d =
  let e ty = gen g ty env d in
  match int g 0 3 with
  | 0 -> "(Effect.perform Get)"
  | 1 -> sprintf "(Effect.perform (Put %s); %s)" (e Int) (e Int)
  | 2 ->
      let x = fresh g "x" in
      sprintf "(try Effect.perform Abort with E %s -> (%s + %s) land %s)" x x (e Int) m
  | _ -> sprintf "(if %s then Effect.perform Abort else %s)" (e Bool) (e Int)

and gen_int g env d =
  let e ty = gen g ty env d in
  let with_vars l ty = gen g ty (env +: l) d in
  if env.eff && chance g 0.25
  then perform g env d
  else
    match int g 0 30 with
    | c when c <= 5 ->
        let op = pick g [ "+"; "-"; "*"; "lxor"; "land"; "lor" ] in
        sprintf "((%s %s %s) land %s)" (e Int) op (e Int) m
    | 6 -> sprintf "(%s %s (%s lor 1))" (e Int) (pick g [ "/"; "mod" ]) (e Int)
    | 7 ->
        let a = e Int and b = e Int in
        pick
          g
          [ sprintf "(%s lsr (%s land 7))" a b
          ; sprintf "((%s lsl (%s land 3)) land %s)" a b m
          ; sprintf "(min %s %s)" a b
          ; sprintf "(max %s %s)" a b
          ]
    | 8 -> sprintf "(if %s then %s else %s)" (e Bool) (e Int) (e Int)
    | 9 ->
        let ty = pick g all_types in
        let x = fresh g "x" in
        let v = gen g ty env d in
        sprintf "(let %s = %s in %s)" x v (with_vars [ x, ty ] Int)
    | 10 -> sprintf "(%s %s)" (e F1) (e Int)
    | 11 ->
        if chance g 0.5
        then sprintf "(%s %s %s)" (e F2) (e Int) (e Int)
        else
          let h = fresh g "g" in
          sprintf "(let %s = %s %s in %s %s)" h (e F2) (e Int) h (e Int)
    | 12 when List.exists (fun (_, k, _) -> k <> Pure) g.funs ->
        call g (pick g (List.filter (fun (_, k, _) -> k <> Pure) g.funs)) env d
    | 13 ->
        let x = fresh g "x" in
        sprintf
          "(match %s with None -> %s | Some %s -> %s)"
          (e Opt)
          (e Int)
          x
          (with_vars [ x, Int ] Int)
    | 14 ->
        let x = fresh g "x" and y = fresh g "x" and z = fresh g "x" in
        sprintf
          "(match %s with A -> %s | B %s -> %s | C (%s, %s) -> %s)"
          (e V)
          (e Int)
          x
          (with_vars [ x, Int ] Int)
          y
          z
          (with_vars [ y, Int; z, Int ] Int)
    | 15 ->
        let x = fresh g "x" and l = fresh g "l" in
        sprintf
          "(match %s with [] -> %s | %s :: %s -> %s)"
          (e List)
          (e Int)
          x
          l
          (with_vars [ x, Int; l, List ] Int)
    | 16 ->
        let acc = fresh g "acc" and x = fresh g "x" in
        sprintf
          "(List.fold_left (fun %s %s -> %s) %s %s)"
          acc
          x
          (with_vars [ acc, Int; x, Int ] Int)
          (e Int)
          (e List)
    | 17 ->
        if chance g 0.5
        then sprintf "(List.length %s)" (e List)
        else sprintf "(Array.length %s)" (e Arr)
    | 18 ->
        let a = fresh g "a" in
        sprintf
          "(let %s = %s in if Array.length %s = 0 then %s else %s.(%s mod Array.length \
           %s))"
          a
          (e Arr)
          a
          (e Int)
          a
          (e Int)
          a
    | 19 -> sprintf "(%s %s)" (pick g [ "fst"; "snd" ]) (e Pair)
    | 20 ->
        pick
          g
          [ sprintf "(! %s)" (e Ref); sprintf "(%s).a" (e R); sprintf "(%s).b" (e R) ]
    | 21 ->
        let x = fresh g "x" in
        sprintf
          "(try %s with E %s -> %s | F -> %s)"
          (e Int)
          x
          (with_vars [ x, Int ] Int)
          (e Int)
    | 22 ->
        sprintf
          "(if %s then raise %s else %s)"
          (e Bool)
          (pick g [ "F"; sprintf "(E %s)" (e Int) ])
          (e Int)
    | 23 -> (
        (* Side effects *)
        match int g 0 2 with
        | 0 ->
            let rv = fresh g "r" in
            sprintf
              "(let %s = %s in %s.b <- %s; %s)"
              rv
              (e R)
              rv
              (e Int)
              (with_vars [ rv, R ] Int)
        | 1 ->
            let rv = fresh g "c" in
            sprintf
              "(let %s = %s in %s := %s; %s)"
              rv
              (e Ref)
              rv
              (e Int)
              (with_vars [ rv, Ref ] Int)
        | _ ->
            let a = fresh g "a" in
            sprintf
              "(let %s = %s in if Array.length %s > 0 then %s.(%s mod Array.length %s) \
               <- %s; %s)"
              a
              (e Arr)
              a
              a
              (e Int)
              a
              (e Int)
              (with_vars [ a, Arr ] Int))
    | 24 ->
        let acc = fresh g "acc" and i = fresh g "i" in
        sprintf
          "(let %s = ref %s in for %s = 0 to %s land 7 do %s := (! %s + %s) land %s \
           done; ! %s)"
          acc
          (e Int)
          i
          (e Int)
          acc
          acc
          (with_vars [ i, Int ] Int)
          m
          acc
    | 25 ->
        let i = fresh g "i" and acc = fresh g "acc" in
        sprintf
          "(let %s = ref 0 and %s = ref %s in while ! %s < %s land 7 do %s := (! %s * 3 \
           + %s) land %s; incr %s done; ! %s)"
          i
          acc
          (e Int)
          i
          (e Int)
          acc
          acc
          (with_vars [ acc, Ref ] Int)
          m
          i
          acc
    | 26 -> sprintf "(String.length %s)" (e Str)
    | 27 ->
        let f = fresh g "f" in
        sprintf
          "(let %s = %s in if Float.is_finite %s then int_of_float (Float.rem %s 1000.) \
           land %s else 0)"
          f
          (e Float)
          f
          f
          m
    | 28 ->
        if chance g 0.5
        then sprintf "(compare %s %s + 1)" (e Int) (e Int)
        else sprintf "(compare %s %s + 1)" (e List) (e List)
    | 29 ->
        sprintf
          "(Hashtbl.hash %s land %s)"
          (e (pick g [ Int; Str; List; Pair; V; Opt ]))
          m
    | _ -> gen_int_more g env d

and gen_int_more g env d =
  let e ty = gen g ty env d in
  let with_vars l ty = gen g ty (env +: l) d in
  match int g 0 15 with
  | 0 -> sprintf "(Int32.to_int (Int32.logand %s 0x7FFFl))" (e I32)
  | 1 -> sprintf "(Int64.to_int (Int64.logand %s 0x7FFFL))" (e I64)
  | 2 -> sprintf "(Lazy.force %s)" (e Lazy)
  | 3 ->
      let o = fresh g "o" in
      sprintf
        "(let %s = object val mutable v = %s method get = v method set x = v <- x end in \
         %s#set %s; %s#get)"
        o
        (e Int)
        o
        (e Int)
        o
  | 4 ->
      let b = fresh g "b" in
      sprintf
        "(let %s = Bytes.make (%s land 7 + 1) 'a' in Bytes.set %s (%s mod Bytes.length \
         %s) 'z'; Hashtbl.hash (Bytes.to_string %s) land %s)"
        b
        (e Int)
        b
        (e Int)
        b
        b
        m
  | 5 when not env.eff ->
      g.uses_effects <- true;
      let env2 = { env with eff = true } in
      let x = fresh g "x" in
      let body =
        match int g 0 3 with
        | 0 ->
            sprintf
              "let %s = Effect.perform Get in %s"
              x
              (gen g Int (env2 +: [ x, Int ]) d)
        | 1 -> sprintf "Effect.perform (Put %s); %s" (gen g Int env2 d) (gen g Int env2 d)
        | 2 ->
            sprintf
              "(try (if %s then Effect.perform Abort else %s) with E %s -> %s)"
              (gen g Bool env2 d)
              (gen g Int env2 d)
              x
              (gen g Int (env2 +: [ x, Int ]) d)
        | _ -> gen g Int env2 d
      in
      sprintf "(run_state (fun () -> %s) %s)" body (e Int)
  | 7 ->
      let x = fresh g "x" and mv = fresh g "m" and k = fresh g "x" in
      sprintf
        "(match IM.find_opt %s (List.fold_left (fun %s %s -> IM.add %s %s %s) IM.empty \
         %s) with None -> %s | Some %s -> %s)"
        (e Int)
        mv
        x
        x
        x
        mv
        (e List)
        (e Int)
        k
        k
  | 8 ->
      sprintf
        "(SM.cardinal (SM.add %s %s (SM.add %s %s SM.empty)))"
        (e Str)
        (e Int)
        (e Str)
        (e Int)
  | 9 ->
      let x = fresh g "x" in
      sprintf
        "(match (if %s then `P else `Q %s) with `P -> %s | `Q %s -> %s)"
        (e Bool)
        (e Int)
        (e Int)
        x
        (with_vars [ x, Int ] Int)
  | 10 ->
      let ex = fresh g "L" and x = fresh g "x" in
      sprintf
        "(let exception %s of int in try (if %s then raise (%s %s) else %s) with %s %s \
         -> %s)"
        ex
        (e Bool)
        ex
        (e Int)
        (e Int)
        ex
        x
        x
  | 11 ->
      let rv = fresh g "r" in
      sprintf
        "(let %s = { m = %s; fl = %s } in %s.fl <- %s.fl +. 1.; if Float.is_finite %s.fl \
         then int_of_float (Float.rem %s.fl 100.) land %s else %s.m)"
        rv
        (e Int)
        (e Float)
        rv
        rv
        rv
        rv
        m
        rv
  | 12 ->
      let lv = fresh g "l" in
      let body =
        sprintf "(if %s then raise F else %s)" (e Bool) (gen g Int (no_eff env) d)
      in
      sprintf
        "(let %s = lazy %s in ((try Lazy.force %s with F -> %s) + (try Lazy.force %s \
         with F -> %s)) land %s)"
        lv
        body
        lv
        (e Int)
        lv
        (e Int)
        m
  | 13 ->
      let lp = fresh g "loop" and i = fresh g "i" and acc = fresh g "acc" in
      sprintf
        "(let rec %s %s %s = if %s <= 0 then %s else %s (%s - 1) ((%s + %s) land %s) in \
         %s (%s land 15) %s)"
        lp
        i
        acc
        i
        acc
        lp
        i
        acc
        (with_vars [ i, Int; acc, Int ] Int)
        m
        lp
        (e Int)
        (e Int)
  | 14 ->
      let a = fresh g "a" and f = fresh g "f" in
      sprintf
        "(let %s = %s in if Array.length %s = 0 then 0 else (let %s = %s.(%s mod \
         Array.length %s) in if Float.is_finite %s then int_of_float (Float.rem %s \
         1000.) land %s else 0))"
        a
        (e Farr)
        a
        f
        a
        (e Int)
        a
        f
        f
        m
  | 15 ->
      let acc = fresh g "acc" and x = fresh g "x" in
      sprintf
        "(Seq.fold_left (fun %s %s -> (%s + %s) land %s) 0 (Seq.take (%s land 7) \
         (Seq.ints %s)))"
        acc
        x
        acc
        x
        m
        (e Int)
        (e Int)
  | _ -> if env.eff then perform g env d else sprintf "(%s).a" (e R)

and call g (name, kind, _) env d =
  let e ty = gen g ty env d in
  match kind with
  | Rec -> sprintf "(%s (%s land 15) %s)" name (e Int) (e Int)
  | Mutual ->
      let fuel = if chance g 0.2 then "100000" else sprintf "(%s land 31)" (e Int) in
      sprintf "(%s %s %s)" name fuel (e Int)
  | Try -> sprintf "(%s %s)" name (e Int)
  | Hof -> sprintf "(%s %s %s)" name (e F1) (e Int)
  | Label ->
      if chance g 0.5
      then sprintf "(%s ~b:%s ~a:%s)" name (e Int) (e Int)
      else
        let h = fresh g "g" in
        sprintf "(let %s = %s ~a:%s in %s ~b:%s)" h name (e Int) h (e Int)
  | Optional ->
      if chance g 0.5
      then sprintf "(%s %s)" name (e Int)
      else sprintf "(%s ~d:%s %s)" name (e Int) (e Int)
  | Pure -> assert false

and gen_bool g env d =
  let e ty = gen g ty env d in
  match int g 0 7 with
  | c when c <= 2 ->
      sprintf "(%s %s %s)" (e Int) (pick g [ "="; "<>"; "<"; "<=" ]) (e Int)
  | 3 -> sprintf "(%s %s %s)" (e Bool) (pick g [ "&&"; "||" ]) (e Bool)
  | 4 -> sprintf "(not %s)" (e Bool)
  | 5 -> sprintf "(List.mem %s %s)" (e Int) (e List)
  | 6 ->
      let ty = pick g [ V; Pair; Opt; List; Str ] in
      sprintf "(%s = %s)" (e ty) (e ty)
  | _ -> sprintf "(%s < %s)" (e Float) (e Float)

and gen_float g env d =
  let e ty = gen g ty env d in
  match int g 0 6 with
  | c when c <= 1 ->
      sprintf "(%s %s %s)" (e Float) (pick g [ "+."; "-."; "*." ]) (e Float)
  | 2 -> sprintf "(float_of_int %s)" (e Int)
  | 3 -> sprintf "(sqrt (Float.abs %s))" (e Float)
  | 4 -> sprintf "(if %s then %s else %s)" (e Bool) (e Float) (e Float)
  | 5 -> sprintf "(%s).%s" (e Fr) (pick g [ "x"; "y" ])
  | _ ->
      if chance g 0.3
      then sprintf "(Array.fold_left ( +. ) 0. %s)" (e Farr)
      else sprintf "(%s /. (Float.abs %s +. 1.))" (e Float) (e Float)

and gen_list g env d =
  let e ty = gen g ty env d in
  match int g 0 7 with
  | 0 -> sprintf "(%s :: %s)" (e Int) (e List)
  | 1 -> sprintf "(List.map %s %s)" (e F1) (e List)
  | 2 ->
      let x = fresh g "x" in
      sprintf
        "(List.filter (fun %s -> %s) %s)"
        x
        (gen g Bool (env +: [ x, Int ]) d)
        (e List)
  | 3 -> sprintf "(List.rev %s)" (e List)
  | 4 -> sprintf "(%s @ %s)" (e List) (e List)
  | 5 -> sprintf "(List.init (%s land 7) %s)" (e Int) (e F1)
  | 6 -> (
      let mods =
        List.filter_map
          (fun (f, _, _) ->
            if String.ends_with ~suffix:".g" f
            then Some (String.sub f 0 (String.length f - 2))
            else None)
          g.funs
      in
      match mods with
      | _ :: _ when chance g 0.5 -> sprintf "(%s.h %s)" (pick g mods) (e List)
      | _ -> sprintf "(Array.to_list %s)" (e Arr))
  | _ ->
      if chance g 0.3
      then
        let x = fresh g "x" in
        sprintf
          "(List.of_seq (Seq.unfold (fun %s -> if %s <= 0 then None else Some (%s, %s - \
           1)) (%s land 7)))"
          x
          x
          x
          x
          (e Int)
      else if chance g 0.3
      then sprintf "(List.of_seq (Seq.map %s (List.to_seq %s)))" (e F1) (e List)
      else sprintf "(List.sort compare %s)" (e List)

and gen_arr g env d =
  let e ty = gen g ty env d in
  match int g 0 3 with
  | 0 -> sprintf "(Array.make (%s land 7) %s)" (e Int) (e Int)
  | 1 -> sprintf "(Array.of_list %s)" (e List)
  | 2 -> sprintf "(Array.map %s %s)" (e F1) (e Arr)
  | _ -> sprintf "(Array.init (%s land 7) %s)" (e Int) (e F1)

and gen_f1 g env d =
  let env = no_eff env in
  let e ty = gen g ty env d in
  let x = fresh g "x" in
  match int g 0 4 with
  | c when c <= 1 -> sprintf "(fun %s -> %s)" x (gen g Int (env +: [ x, Int ]) d)
  | 2 -> sprintf "(%s %s)" (e F2) (e Int)
  | 3 ->
      let f = fresh g "g" and h = fresh g "g" in
      sprintf
        "(let %s = %s and %s = %s in fun %s -> %s (%s %s))"
        f
        (e F1)
        h
        (e F1)
        x
        f
        h
        x
  | _ ->
      if chance g 0.5
      then
        let k = fresh g "k" and h = fresh g "h" and y = fresh g "x" in
        sprintf
          "(let %s = %s in fun %s -> (let %s = fun %s -> (%s + %s + %s) land %s in %s \
           (%s %s)))"
          k
          (e Int)
          x
          h
          y
          x
          y
          k
          m
          h
          h
          x
      else
        let cv = fresh g "c" in
        sprintf
          "(let %s = ref %s in fun %s -> %s := (! %s + %s) land %s; ! %s)"
          cv
          (e Int)
          x
          cv
          cv
          x
          m
          cv

and gen_f2 g env d =
  let env = no_eff env in
  let x = fresh g "x" and y = fresh g "x" in
  sprintf "(fun %s %s -> %s)" x y (gen g Int (env +: [ x, Int; y, Int ]) d)

and gen_pair g env d =
  let e ty = gen g ty env d in
  if chance g 0.7
  then sprintf "(%s, %s)" (e Int) (e Int)
  else
    let x = fresh g "x" and y = fresh g "x" in
    sprintf "(let (%s, %s) = %s in (%s, %s))" x y (e Pair) y x

and gen_opt g env d =
  let e ty = gen g ty env d in
  match int g 0 2 with
  | 0 -> sprintf "(Some %s)" (e Int)
  | 1 ->
      let x = fresh g "x" in
      sprintf
        "(List.find_opt (fun %s -> %s) %s)"
        x
        (gen g Bool (env +: [ x, Int ]) d)
        (e List)
  | _ -> sprintf "(if %s then Some %s else None)" (e Bool) (e Int)

and gen_v g env d =
  let e ty = gen g ty env d in
  match int g 0 2 with
  | 0 -> sprintf "(B %s)" (e Int)
  | 1 -> sprintf "(C (%s, %s))" (e Int) (e Int)
  | _ ->
      let x = fresh g "x" in
      sprintf
        "(match %s with A -> B 0 | B %s -> C (%s, %s) | C (_, %s) -> B %s)"
        (e V)
        x
        x
        x
        x
        x

and gen_r g env d =
  let e ty = gen g ty env d in
  if chance g 0.6
  then sprintf "{ a = %s; b = %s }" (e Int) (e Int)
  else sprintf "{ %s with a = %s }" (e R) (e Int)

and gen_i32 g env d =
  let e ty = gen g ty env d in
  match int g 0 4 with
  | 0 ->
      sprintf
        "(Int32.%s %s %s)"
        (pick g [ "add"; "sub"; "mul"; "logxor"; "logand"; "logor" ])
        (e I32)
        (e I32)
  | 1 ->
      sprintf
        "(Int32.%s %s (Int32.logor %s 1l))"
        (pick g [ "div"; "rem" ])
        (e I32)
        (e I32)
  | 2 ->
      sprintf
        "(Int32.%s %s (%s land 31))"
        (pick g [ "shift_left"; "shift_right"; "shift_right_logical" ])
        (e I32)
        (e Int)
  | 3 -> sprintf "(Int32.of_int %s)" (e Int)
  | _ -> sprintf "(Int64.to_int32 %s)" (e I64)

and gen_i64 g env d =
  let e ty = gen g ty env d in
  match int g 0 4 with
  | 0 ->
      sprintf
        "(Int64.%s %s %s)"
        (pick g [ "add"; "sub"; "mul"; "logxor"; "logand"; "logor" ])
        (e I64)
        (e I64)
  | 1 ->
      sprintf
        "(Int64.%s %s (Int64.logor %s 1L))"
        (pick g [ "div"; "rem" ])
        (e I64)
        (e I64)
  | 2 ->
      sprintf
        "(Int64.%s %s (%s land 63))"
        (pick g [ "shift_left"; "shift_right"; "shift_right_logical" ])
        (e I64)
        (e Int)
  | 3 -> sprintf "(Int64.of_int32 %s)" (e I32)
  | _ -> sprintf "(Int64.of_int %s)" (e Int)

and gen_farr g env d =
  let e ty = gen g ty env d in
  match int g 0 2 with
  | 0 -> sprintf "(Array.make (%s land 7) %s)" (e Int) (e Float)
  | 1 ->
      let x = fresh g "x" in
      sprintf "(Array.map (fun %s -> %s *. 0.5) %s)" x x (e Farr)
  | _ -> sprintf "(Array.of_list (List.map float_of_int %s))" (e List)

and gen_str g env d =
  let e ty = gen g ty env d in
  match int g 0 4 with
  | 0 -> sprintf "(string_of_int %s)" (e Int)
  | 1 -> sprintf "(%s ^ %s)" (e Str) (e Str)
  | 2 -> sprintf "(String.make (%s land 7) 'x')" (e Int)
  | 3 -> sprintf "(Printf.sprintf \"%%d-%%s\" %s %s)" (e Int) (e Str)
  | _ ->
      let s = fresh g "s" in
      sprintf
        "(let %s = %s in String.sub %s 0 (min (String.length %s) %s))"
        s
        (e Str)
        s
        s
        (e Int)

(* Toplevel functions *)

let toplevel_item g d =
  let ints l = { empty with vars = List.map (fun x -> x, Int) l } in
  let name = fresh g "f" in
  let add f = g.funs <- f :: g.funs in
  match int g 0 10 with
  | 0 ->
      let x = fresh g "x" in
      let body = gen g Int (ints [ x ]) d in
      add (name, Pure, 1);
      sprintf "let %s %s = %s" name x body
  | 1 ->
      let x = fresh g "x" and y = fresh g "x" in
      let body = gen g Int (ints [ x; y ]) d in
      add (name, Pure, 2);
      sprintf "let %s %s %s = %s" name x y body
  | 2 ->
      let n = fresh g "n" and x = fresh g "x" in
      let env = ints [ x ] in
      let body = gen g Int env d in
      let step = sprintf "(%s (%s - 1) %s)" name n (gen g Int env (d - 1)) in
      let base = gen g Int env (d - 1) in
      add (name, Rec, 2);
      sprintf
        "let rec %s %s %s = if %s <= 0 then %s else ((%s + %s) land %s)"
        name
        n
        x
        n
        base
        step
        body
        m
  | 3 ->
      let other = fresh g "f" in
      let n = fresh g "n" and x = fresh g "x" and n2 = fresh g "n" and y = fresh g "x" in
      add (name, Mutual, 2);
      (* Tail calls only, so that a large fuel does not overflow the stack *)
      sprintf
        "let rec %s %s %s = if %s <= 0 then %s else %s (%s - 1) ((%s + 1) land %s)\n\
         and %s %s %s = if %s <= 0 then (%s * 2) land %s else %s (%s - 1) ((%s * 3) land \
         %s)"
        name
        n
        x
        n
        x
        other
        n
        x
        m
        other
        n2
        y
        n2
        y
        m
        name
        n2
        y
        m
  | 4 ->
      let x = fresh g "x" and rr = fresh g "r" and i = fresh g "i" in
      let step = gen g Int (ints [ x; i ]) (d - 1) in
      let cond = gen g Bool { empty with vars = [ x, Int; i, Int; rr, Ref ] } (d - 1) in
      add (name, Try, 1);
      sprintf
        "let %s %s =\n\
        \  let %s = ref %s in\n\
        \  (try for %s = 0 to 5 do %s := (! %s + %s) land %s; if %s then raise F done; ! \
         %s\n\
        \   with F -> (! %s + 1) land %s)"
        name
        x
        rr
        x
        i
        rr
        rr
        step
        m
        cond
        rr
        rr
        m
  | 5 ->
      let f = fresh g "g" and x = fresh g "x" in
      add (name, Hof, 2);
      sprintf "let %s (%s : int -> int) %s = %s (%s %s)" name f x f f x
  | 6 ->
      let ma = fresh g "RA" and mb = fresh g "RB" in
      let n = fresh g "n" and x = fresh g "x" in
      add (ma ^ ".f", Rec, 2);
      sprintf
        "module rec %s : sig val f : int -> int -> int end = struct\n\
        \  let f %s %s = if %s <= 0 then %s else %s.g (%s - 1) ((%s + 1) land %s)\n\
         end\n\
         and %s : sig val g : int -> int -> int end = struct\n\
        \  let g %s %s = (%s.f %s ((%s * 3) land %s) + 1) land %s\n\
         end"
        ma
        n
        x
        n
        x
        mb
        n
        x
        m
        mb
        n
        x
        ma
        n
        x
        m
        m
  | 7 ->
      let fb = fresh g "f" and fc = fresh g "f" in
      let n1 = fresh g "n"
      and x1 = fresh g "x"
      and n2 = fresh g "n"
      and x2 = fresh g "x" in
      let n3 = fresh g "n" and x3 = fresh g "x" and y = fresh g "x" and h = fresh g "h" in
      let base = gen g Int (ints [ x1 ]) (d - 1) in
      add (name, Rec, 2);
      sprintf
        "let rec %s %s %s = if %s <= 0 then %s else (%s (%s - 1) %s + 1) land %s\n\
         and %s %s %s = if %s <= 0 then (%s * 2) land %s else %s (%s - 1) ((%s + 3) land \
         %s)\n\
         and %s %s %s = let %s %s = %s (%s - 1) %s in if %s <= 0 then %s else %s ((%s * \
         5) land %s)"
        name
        n1
        x1
        n1
        base
        fb
        n1
        x1
        m
        fb
        n2
        x2
        n2
        x2
        m
        fc
        n2
        x2
        m
        fc
        n3
        x3
        h
        y
        name
        n3
        y
        n3
        x3
        h
        x3
        m
  | 8 ->
      add (name, Label, 2);
      sprintf "let %s ~a ~b = ((a * 3 + b) land %s)" name m
  | 9 ->
      let md = fresh g "M" and x = fresh g "x" in
      let body = gen g Int (ints [ x ]) d in
      add (md ^ ".g", Pure, 1);
      sprintf "module %s = Mk (struct let f %s = %s end)" md x body
  | _ ->
      let x = fresh g "x" in
      add (name, Optional, 1);
      sprintf "let %s ?(d = %s) %s = (%s * d + 1) land %s" name (lit_int g) x x m

let test g d =
  let ty, printer =
    pick
      g
      [ Int, "pi"
      ; Int, "pi"
      ; Int, "pi"
      ; Float, "pf"
      ; Str, "ps"
      ; Bool, "pb"
      ; List, "pl"
      ; I32, "p32"
      ; I64, "p64"
      ]
  in
  sprintf
    "let () =\n\
    \  try %s %s\n\
    \  with E k -> Printf.printf \"E %%d\\n\" k | F -> print_endline \"F\""
    printer
    (gen g ty empty d)

type parts =
  { prelude : string
  ; items : string list  (** toplevel definitions *)
  ; tests : string list
  ; uses_effects : bool  (** the program installs effect handlers *)
  }

let parts ~seed ~size =
  let g = { st = Rng.make seed; size; n = 0; funs = []; uses_effects = false } in
  let items = List.init (int g 3 8) (fun _ -> toplevel_item g g.size) in
  let tests = List.init (int g 10 25) (fun _ -> test g g.size) in
  { prelude; items; tests; uses_effects = g.uses_effects }

let program p = String.concat "\n\n" ((p.prelude :: p.items) @ p.tests) ^ "\n"
