(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* A reference interpreter for the intermediate code, following the
   semantics of the JavaScript backend: integers are 32-bit, [int32]
   and [nativeint] values are integers, strings are immutable values
   compared by content, and integers and floats are both numbers.

   Closures capture their environment by value when they are
   created, except that a group of consecutive closures (as defined
   by [Generate.collect_closures]) share an environment, so that they
   can be mutually recursive. [Assign (x, y)] updates the binding of
   [x] in the current frame: it is only read by the arguments of an
   exception handler continuation (see [Code.check_assigned_variables]),
   which are evaluated when the exception is caught.

   Effect handlers have two semantics. Before the [Effects] pass, the
   effect primitives ([%perform], [%resume], ...) are implemented with
   the effect handlers of the host, so this requires the compiler to be
   built with OCaml 5. Once the program has been transformed into CPS,
   calls that do not have the right number of arguments follow the CPS
   conventions of [caml_call_gen], calls in [trampolined_calls] bounce
   when the stack is too deep, and the runtime functions used by the
   CPS code (runtime/js/effect.js) are transcribed here; with double
   translation, functions can have a CPS version, and calls in [in_cps]
   use it.

   Behaviours that the passes are not supposed to introduce are reported
   as errors: unbound variables, exact calls with the wrong number of
   arguments, out-of-bounds field accesses, conditionals on values that
   are not integers, ... *)

open! Stdlib
open Code

let debug = Debug.find "interp"

let debug_output = Debug.find "interp-output"

let debug_program = Debug.find "interp-program"

exception Error of string

exception Unsupported of string

exception Out_of_fuel

exception Exit_program of int

let error fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

module V = struct
  type t =
    | Int of Targetint.t
    | Float of float
    | String of string
    | Native_string of string
    | Bytes of bytes
    | Block of block
    | Int64 of int64
    | Closure of closure
    | Partial of partial
    | Prim_closure of string
    | Host of host  (** A function of the runtime *)
    | Channel of channel
    | Js_array of t array  (** A JavaScript array, built by [%js_array] *)
    | Trampoline of t * t * bool
        (** Function, arguments, and whether the function is called in
            direct style (see [caml_trampoline_return]) *)
    | Stack of fiber  (** Effect handlers *)
    | Null

  and block =
    { mutable tag : int
    ; mutable fields : t array
    ; mutable fun_ : t option  (** Set by [caml_update_dummy] for recursive functions *)
    }

  and closure =
    { fn : func
    ; env : t array  (** Values of the free variables of the function *)
    ; mutable cps : t option
          (** With double translation, the CPS version of the function *)
    }

  and host =
    { arity : int
    ; hfn : t list -> t
    ; mutable host_cps : t option
    }

  (* Functions and blocks are compiled before execution: variables are
     replaced by integers, which are indices in the array of local
     variables of the current function when nonnegative, and [-1 - i]
     for the [i]-th free variable *)
  and func =
    { params : int array
    ; entry : jump
    ; nlocals : int
    ; local_names : Var.t array
    ; free_names : Var.t array
    }

  and jump =
    { pc : Addr.t
    ; args : int array
    }

  and partial =
    { pf : t
    ; pargs : t list
    ; remaining : int
    }

  and channel = { fd : int }

  (* A fiber, as allocated by [caml_alloc_stack]. A continuation is a
     block of tag 245 containing the innermost and the outermost fibers
     of a chain of fibers, linked by [parent]. *)
  and fiber =
    { mutable handlers : t array  (** Value, exception and effect handlers *)
    ; mutable resume : ((unit -> t) -> t) option
          (** Resume the suspended computation of the fiber, running the
              function at the point where the computation was suspended;
              [None] if the fiber has not been started *)
    ; mutable parent : fiber option  (** Next fiber in a continuation *)
    ; mutable depth : int  (** Call depth of the suspended computation *)
    ; mutable parent_depth : int  (** Call depth where the fiber was resumed *)
    ; mutable k : t  (** In CPS: low-level continuation *)
    ; mutable traps : t list  (** In CPS: exception handlers *)
    }
  [@@warning "-69"]
  (* Direct-style effect handlers are not supported with OCaml 4 *)
end

exception Ml_exn of V.t

module C = struct
  type arg =
    | Arg_var of int
    | Arg_value of V.t
    | Arg_alloc of constant

  type expr =
    | Apply of
        { f : int
        ; args : int array
        ; exact : bool
        ; trampolined : bool
              (** In CPS, the call returns a trampoline when the stack is
                  too deep *)
        ; in_cps : bool  (** With double translation, a call in CPS *)
        }
    | Block of int * int array
    | Field of int * int
    | Value of V.t
    | Alloc of constant  (** A constant which is allocated each time it is evaluated *)
    | Prim of prim * arg array
    | Extern of (V.t list -> V.t) * arg array
    | Special of string

  type closure =
    { dst : int
    ; fn : V.func
    ; captures : int array
    }

  type instr =
    | Let of int * expr
    | Closures of closure array * (int * int * int) array
        (** Mutually recursive closures, and variables bound by
            [caml_cps_closure] to one of these closures (destination, direct
            and CPS versions) *)
    | Assign of int * int
    | Set_field of int * int * int
    | Offset_ref of int * int
    | Array_set of int * int * int

  type last =
    | Return of int
    | Raise of int
    | Stop
    | Branch of V.jump
    | Cond of int * V.jump * V.jump
    | Switch of int * V.jump array
    | Pushtrap of V.jump * int * V.jump
    | Poptrap of V.jump

  type block =
    { params : int array
    ; body : instr array
    ; tail_call : (int * int * int array * bool) option
          (** Last instruction [x = f(args)], when the block returns [x] *)
    ; last : last
    }
end

type state =
  { mutable code : C.block option array
  ; mutable fuel : int
  ; mutable depth : int
  ; stdout : Buffer.t
  ; stderr : Buffer.t
  ; named_values : V.t String.Hashtbl.t
  ; globals : V.t String.Hashtbl.t
  ; mutable oo_id : int
  ; mutable cache_id : int
  ; mutable out_channels : V.t list
  ; mutable dls : V.t
  ; mutable pc : Addr.t  (** Block being executed, for error messages *)
  ; cps : bool  (** Whether the program has been transformed into CPS *)
  ; dt : bool  (** Same, with double translation *)
  ; mutable stack_depth : int  (** [caml_stack_depth] *)
  ; mutable current : V.fiber  (** [caml_current_stack] *)
  }

(* Set once the [Effects] pass has transformed the program into CPS *)
let trampolined_calls = ref None

let in_cps_calls = ref Var.Set.empty

let toplevel_fiber () : V.fiber =
  { handlers = [||]
  ; resume = None
  ; parent = None
  ; depth = 0
  ; parent_depth = 0
  ; k = Int Targetint.zero
  ; traps = []
  }

let host arity hfn = V.Host { arity; hfn; host_cps = None }

let check_depth st =
  st.stack_depth <- st.stack_depth - 1;
  st.stack_depth > 0

let max_depth = 100_000

(****)

let unit = V.Int Targetint.zero

let bool b = V.Int (if b then Targetint.one else Targetint.zero)

let int n = V.Int (Targetint.of_int32_truncate (Int32.of_int n))

let int32 n = V.Int (Targetint.of_int32_truncate n)

let block ?(tag = 0) fields = V.Block { tag; fields; fun_ = None }

let as_int (v : V.t) =
  match v with
  | Int n -> n
  | _ -> error "integer expected"

let to_int v = Targetint.to_int_exn (as_int v)

let to_int32 v = Targetint.to_int32 (as_int v)

let as_float (v : V.t) =
  match v with
  | Float f -> f
  | Int n -> Targetint.to_float n
  | _ -> error "float expected"

let as_block (v : V.t) =
  match v with
  | Block b -> b
  | _ -> error "block expected"

let as_string (v : V.t) =
  match v with
  | String s | Native_string s -> s
  | Bytes b -> Bytes.to_string b
  | _ -> error "string expected"

let as_bytes (v : V.t) =
  match v with
  | Bytes b -> b
  | _ -> error "bytes expected"

let as_int64 (v : V.t) =
  match v with
  | Int64 n -> n
  | _ -> error "int64 expected"

let field v i =
  let b = as_block v in
  if i < 0 || i >= Array.length b.fields
  then error "field %d out of bounds (size %d)" i (Array.length b.fields);
  b.fields.(i)

let set_field v i x =
  let b = as_block v in
  if i < 0 || i >= Array.length b.fields
  then error "field %d out of bounds (size %d)" i (Array.length b.fields);
  b.fields.(i) <- x

let rec list_of_values l =
  match l with
  | [] -> V.Int Targetint.zero
  | x :: r -> block [| x; list_of_values r |]

let array_of_list (v : V.t) =
  let rec loop acc (v : V.t) =
    match v with
    | Int _ -> List.rev acc
    | Block { fields = [| x; r |]; _ } -> loop (x :: acc) r
    | _ -> error "list expected"
  in
  loop [] v

let rec constant (c : constant) : V.t =
  match c with
  | String s -> String s
  | NativeString (Byte s) | NativeString (Utf (Utf8 s)) -> Native_string s
  | Float f | Float32 f -> Float (Int64.float_of_bits f)
  | Float_array (a, _) ->
      block ~tag:254 (Array.map a ~f:(fun f -> V.Float (Int64.float_of_bits f)))
  | Int n -> Int n
  | Int32 n | NativeInt n -> Int (Targetint.of_int32_truncate n)
  | Int64 n -> Int64 n
  | Tuple (tag, a, _, _) -> block ~tag (Array.map a ~f:constant)
  | Null_ -> Null

(****)

(* Physical equality, which is [===] in JavaScript *)
let phys_equal_value (a : V.t) (b : V.t) =
  match a, b with
  | Int x, Int y -> Targetint.equal x y
  | (Int _ | Float _), (Int _ | Float _) -> Float.( = ) (as_float a) (as_float b)
  | String x, String y | Native_string x, Native_string y -> String.equal x y
  | Bytes x, Bytes y -> phys_equal x y
  | Block x, Block y -> phys_equal x y
  | Closure x, Closure y -> phys_equal x y
  | Partial x, Partial y -> phys_equal x y
  | Prim_closure x, Prim_closure y -> String.equal x y
  | Host _, Host _ -> phys_equal a b
  | Int64 _, Int64 _ -> phys_equal a b
  | Channel x, Channel y -> phys_equal x y
  | Js_array x, Js_array y -> phys_equal x y
  | Trampoline _, Trampoline _ -> phys_equal a b
  | Stack x, Stack y -> phys_equal x y
  | Null, Null -> true
  | ( ( Int _
      | Float _
      | String _
      | Native_string _
      | Bytes _
      | Block _
      | Closure _
      | Partial _
      | Prim_closure _
      | Host _
      | Int64 _
      | Channel _
      | Js_array _
      | Trampoline _
      | Stack _
      | Null )
    , _ ) -> false

(* Exceptions *)

let predef st name =
  match String.Hashtbl.find_opt st.globals name with
  | Some v -> v
  | None -> error "predefined exception %s not registered" name

let raise_with_string st name s = raise (Ml_exn (block [| predef st name; String s |]))

(* Polymorphic comparison *)

let unordered = min_int

let sign c = if c < 0 then -1 else if c > 0 then 1 else 0

let compare_floats ~total x y =
  if Float.( < ) x y
  then -1
  else if Float.( > ) x y
  then 1
  else if Float.( = ) x y
  then 0
  else if not total
  then unordered
  else if Float.is_nan x
  then if Float.is_nan y then 0 else -1
  else 1

let ml_tag st (v : V.t) =
  ignore st;
  match v with
  | Block b -> b.tag
  | String _ | Native_string _ | Bytes _ -> 252
  | Int64 _ | Channel _ -> 255
  | Js_array _ | Trampoline _ | Stack _ -> 1000
  | Closure _ | Partial _ | Prim_closure _ | Host _ -> 247
  | Int _ | Float _ -> 1000
  | Null -> 1010

(* Same algorithm as [caml_compare_val] in the JavaScript runtime, with
   an explicit stack *)
let compare_values st ~total (a : V.t) (b : V.t) =
  let rec compare_one stack (a : V.t) (b : V.t) =
    if total && phys_equal_value a b
    then next stack
    else
      match a, b with
      | Block { tag = 250; fields = [| a |]; _ }, _ -> compare_one stack a b
      | _, Block { tag = 250; fields = [| b |]; _ } -> compare_one stack a b
      | Int x, Int y ->
          let c = Targetint.compare x y in
          if c <> 0 then sign c else next stack
      | (Int _ | Float _), (Int _ | Float _) ->
          let c = compare_floats ~total (as_float a) (as_float b) in
          if c <> 0 then c else next stack
      | Null, Null -> next stack
      | Null, _ -> -1
      | _, Null -> 1
      | (Int _ | Float _), _ -> -1
      | _, (Int _ | Float _) -> 1
      | (String _ | Native_string _ | Bytes _), (String _ | Native_string _ | Bytes _) ->
          let c = String.compare (as_string a) (as_string b) in
          if c <> 0 then sign c else next stack
      | Int64 x, Int64 y ->
          let c = Int64.compare x y in
          if c <> 0 then sign c else next stack
      | (Closure _ | Partial _ | Prim_closure _ | Host _), _
      | _, (Closure _ | Partial _ | Prim_closure _ | Host _) ->
          raise_with_string st "Invalid_argument" "compare: functional value"
      | Block x, Block y ->
          if x.tag <> y.tag
          then sign (compare x.tag y.tag)
          else if x.tag = 251
          then raise_with_string st "Invalid_argument" "equal: abstract value"
          else if x.tag = 245
          then raise_with_string st "Invalid_argument" "compare: continuation value"
          else if x.tag = 248
          then
            let c = compare_one [] x.fields.(1) y.fields.(1) in
            if c <> 0 then c else next stack
          else
            let n = Array.length x.fields and n' = Array.length y.fields in
            if n <> n'
            then sign (compare n n')
            else if n = 0
            then next stack
            else
              compare_one
                (if n > 1 then (x.fields, y.fields, 1) :: stack else stack)
                x.fields.(0)
                y.fields.(0)
      | _ ->
          let c = compare (ml_tag st a) (ml_tag st b) in
          if c <> 0 then sign c else next stack
  and next stack =
    match stack with
    | [] -> 0
    | (a, b, i) :: rem ->
        let stack = if i + 1 < Array.length a then (a, b, i + 1) :: rem else rem in
        compare_one stack a.(i) b.(i)
  in
  compare_one [] a b

let raise_constant st name = raise (Ml_exn (predef st name))

let ml_invalid_arg st s = raise_with_string st "Invalid_argument" s

let ml_failure st s = raise_with_string st "Failure" s

let bound_error st = ml_invalid_arg st "index out of bounds"

(* Hashing: any deterministic function will do *)

let hash_value ~count ~limit ~seed (v : V.t) =
  let h = ref seed in
  let mix d = h := ((!h * 65599) + d) land 0x3FFFFFFF in
  let mix_string s = String.iter s ~f:(fun c -> mix (Char.code c)) in
  let queue = Queue.create () in
  Queue.push v queue;
  let num = ref count and size = ref limit in
  while (not (Queue.is_empty queue)) && !num > 0 do
    (match Queue.pop queue with
    | Int n ->
        decr num;
        mix (Targetint.to_int_exn n)
    | Float f ->
        decr num;
        mix (Hashtbl.hash f)
    | String s | Native_string s ->
        decr num;
        mix_string s
    | Bytes b ->
        decr num;
        mix_string (Bytes.to_string b)
    | Int64 n ->
        decr num;
        mix (Hashtbl.hash n)
    | Block b ->
        mix b.tag;
        Array.iter b.fields ~f:(fun x ->
            if !size > 0
            then (
              decr size;
              Queue.push x queue))
    | Closure _
    | Partial _
    | Prim_closure _
    | Host _
    | Channel _
    | Js_array _
    | Trampoline _
    | Stack _
    | Null -> ());
    ()
  done;
  !h

(* Formatting *)

let num_bits_mask () = (1 lsl Targetint.num_bits ()) - 1

(* Remove the length modifier of an integer conversion *)
let strip_length fmt =
  let i = String.length fmt - 1 in
  if i >= 1
  then
    match fmt.[i - 1] with
    | 'l' | 'L' | 'n' -> String.sub fmt ~pos:0 ~len:(i - 1) ^ String.make 1 fmt.[i]
    | _ -> fmt
  else fmt

let format_int st fmt n =
  let fmt = strip_length fmt in
  let conv = fmt.[String.length fmt - 1] in
  let n =
    match conv with
    | 'x' | 'X' | 'o' | 'u' -> n land num_bits_mask ()
    | _ -> n
  in
  match Printf.sprintf (Scanf.format_from_string fmt "%d") n with
  | s -> V.String s
  | exception (Scanf.Scan_failure _ | Failure _ | Invalid_argument _) ->
      ignore st;
      unsupported "format %S" fmt

let format_int64 fmt n =
  let fmt = strip_length fmt in
  let i = String.length fmt - 1 in
  let fmt = String.sub fmt ~pos:0 ~len:i ^ "L" ^ String.make 1 fmt.[i] in
  match Printf.sprintf (Scanf.format_from_string fmt "%Ld") n with
  | s -> V.String s
  | exception (Scanf.Scan_failure _ | Failure _ | Invalid_argument _) ->
      unsupported "format %S" fmt

let format_float fmt f =
  match Printf.sprintf (Scanf.format_from_string fmt "%f") f with
  | s -> V.String s
  | exception (Scanf.Scan_failure _ | Failure _ | Invalid_argument _) ->
      unsupported "format %S" fmt

(****)

(* Primitives: name -> (arity, implementation) *)
let prims : (int * (state -> V.t list -> V.t)) String.Hashtbl.t =
  String.Hashtbl.create 256

let bad_arity name = error "wrong number of arguments for primitive %s" name

let register name arity f = String.Hashtbl.replace prims name (arity, f)

(* Primitives taking a unit argument, which the bytecode parser sometimes omits *)
let reg0 name f =
  register name 1 (fun st l ->
      match l with
      | [] | [ _ ] -> f st
      | _ -> bad_arity name)

let reg1 name f =
  register name 1 (fun st l ->
      match l with
      | [ x ] -> f st x
      | _ -> bad_arity name)

let reg2 name f =
  register name 2 (fun st l ->
      match l with
      | [ x; y ] -> f st x y
      | _ -> bad_arity name)

let reg3 name f =
  register name 3 (fun st l ->
      match l with
      | [ x; y; z ] -> f st x y z
      | _ -> bad_arity name)

let reg4 name f =
  register name 4 (fun st l ->
      match l with
      | [ x; y; z; t ] -> f st x y z t
      | _ -> bad_arity name)

let reg5 name f =
  register name 5 (fun st l ->
      match l with
      | [ x; y; z; t; u ] -> f st x y z t u
      | _ -> bad_arity name)

let alias names f = List.iter names ~f

let find_prim name =
  match String.Hashtbl.find_opt prims name with
  | Some p -> p
  | None -> (
      match String.Hashtbl.find_opt prims (Primitive.resolve name) with
      | Some p -> p
      | None -> unsupported "primitive %s" name)

(* Integers *)

let int_bin names f =
  alias names (fun name -> reg2 name (fun _ x y -> V.Int (f (as_int x) (as_int y))))

let int_un names f = alias names (fun name -> reg1 name (fun _ x -> V.Int (f (as_int x))))

let shift_amount y = to_int y land (Targetint.num_bits () - 1)

let checked_div st f x y =
  if Targetint.is_zero (as_int y)
  then raise_constant st "Division_by_zero"
  else V.Int (f (as_int x) (as_int y))

let () =
  int_bin [ "%int_add"; "caml_int32_add"; "caml_nativeint_add" ] Targetint.add;
  int_bin [ "%int_sub"; "caml_int32_sub"; "caml_nativeint_sub" ] Targetint.sub;
  int_bin
    [ "%int_mul"; "%direct_int_mul"; "caml_mul"; "caml_int32_mul"; "caml_nativeint_mul" ]
    Targetint.mul;
  alias [ "%int_div"; "caml_div"; "caml_int32_div"; "caml_nativeint_div" ] (fun name ->
      reg2 name (fun st x y -> checked_div st Targetint.div x y));
  alias [ "%int_mod"; "caml_mod"; "caml_int32_mod"; "caml_nativeint_mod" ] (fun name ->
      reg2 name (fun st x y -> checked_div st Targetint.rem x y));
  (* These are only produced for divisors known to be non-zero *)
  reg2 "%direct_int_div" (fun _ x y ->
      if Targetint.is_zero (as_int y) then error "%%direct_int_div by zero";
      V.Int (Targetint.div (as_int x) (as_int y)));
  reg2 "%direct_int_mod" (fun _ x y ->
      if Targetint.is_zero (as_int y) then error "%%direct_int_mod by zero";
      V.Int (Targetint.rem (as_int x) (as_int y)));
  int_bin [ "%int_and"; "caml_int32_and"; "caml_nativeint_and" ] Targetint.logand;
  int_bin [ "%int_or"; "caml_int32_or"; "caml_nativeint_or" ] Targetint.logor;
  int_bin [ "%int_xor"; "caml_int32_xor"; "caml_nativeint_xor" ] Targetint.logxor;
  alias [ "%int_lsl"; "caml_int32_shift_left"; "caml_nativeint_shift_left" ] (fun name ->
      reg2 name (fun _ x y -> V.Int (Targetint.shift_left (as_int x) (shift_amount y))));
  alias
    [ "%int_lsr"
    ; "caml_int32_shift_right_unsigned"
    ; "caml_nativeint_shift_right_unsigned"
    ]
    (fun name ->
      reg2 name (fun _ x y ->
          V.Int (Targetint.shift_right_logical (as_int x) (shift_amount y))));
  alias
    [ "%int_asr"; "caml_int32_shift_right"; "caml_nativeint_shift_right" ]
    (fun name ->
      reg2 name (fun _ x y -> V.Int (Targetint.shift_right (as_int x) (shift_amount y))));
  int_un [ "%int_neg"; "caml_int32_neg"; "caml_nativeint_neg" ] Targetint.neg;
  alias
    [ "%identity"
    ; "caml_int32_of_int"
    ; "caml_int32_to_int"
    ; "caml_nativeint_of_int"
    ; "caml_nativeint_to_int"
    ; "caml_nativeint_to_int32"
    ; "caml_nativeint_of_int32"
    ; "caml_checked_nativeint_to_int"
    ; "caml_checked_int32_to_int"
    ; "caml_array_of_iarray"
    ; "caml_iarray_of_array"
    ; "caml_array_of_uniform_array"
    ; "caml_make_array"
    ; "caml_process_pending_actions_with_root"
    ]
    (fun name -> reg1 name (fun _ x -> x));
  alias
    [ "caml_int_compare"; "caml_int32_compare"; "caml_nativeint_compare" ]
    (fun name ->
      reg2 name (fun _ x y -> int (Targetint.compare (as_int x) (as_int y) |> sign)));
  alias [ "caml_int32_unsigned_compare"; "caml_nativeint_unsigned_compare" ] (fun name ->
      reg2 name (fun _ x y ->
          let x = as_int x and y = as_int y in
          int
            (if Targetint.unsigned_lt x y
             then -1
             else if Targetint.unsigned_lt y x
             then 1
             else 0)));
  reg1 "%caml_format_int_special" (fun _ x -> String (Targetint.to_string (as_int x)));
  alias [ "caml_format_int"; "caml_int32_format"; "caml_nativeint_format" ] (fun name ->
      reg2 name (fun st f x -> format_int st (as_string f) (to_int x)));
  alias
    [ "caml_int_of_string"; "caml_int32_of_string"; "caml_nativeint_of_string" ]
    (fun name ->
      reg1 name (fun st s ->
          match Int32.of_string (as_string s) with
          | n -> int32 n
          | exception Failure _ -> ml_failure st "int_of_string"));
  reg1 "caml_bswap16" (fun _ x ->
      let n = to_int x in
      int (((n land 0xff) lsl 8) lor ((n lsr 8) land 0xff)));
  alias [ "caml_int32_bswap"; "caml_nativeint_bswap" ] (fun name ->
      reg1 name (fun _ x ->
          let n = to_int32 x in
          let b i = Int32.(logand (shift_right_logical n (8 * i)) 0xffl) in
          int32
            Int32.(
              logor
                (shift_left (b 0) 24)
                (logor (shift_left (b 1) 16) (logor (shift_left (b 2) 8) (b 3))))))

(* Floats *)

let float_bin names f =
  alias names (fun name -> reg2 name (fun _ x y -> V.Float (f (as_float x) (as_float y))))

let float_un names f =
  alias names (fun name -> reg1 name (fun _ x -> V.Float (f (as_float x))))

let float_cmp name f = reg2 name (fun _ x y -> bool (f (as_float x) (as_float y)))

(* JavaScript [x | 0] *)
let to_int32_wrap f =
  if Float.is_finite f
  then Int64.to_int32 (Int64.of_float (Float.rem (Float.trunc f) 4294967296.))
  else 0l

let () =
  float_bin [ "caml_add_float" ] Float.add;
  float_bin [ "caml_sub_float" ] Float.sub;
  float_bin [ "caml_mul_float" ] Float.mul;
  float_bin [ "caml_div_float" ] Float.div;
  float_bin [ "caml_fmod_float" ] Float.rem;
  float_bin [ "caml_power_float" ] Float.pow;
  float_bin [ "caml_atan2_float" ] Float.atan2;
  float_bin [ "caml_hypot_float" ] Float.hypot;
  float_bin [ "caml_copysign_float" ] Float.copy_sign;
  float_bin [ "caml_nextafter_float" ] (fun x y ->
      if Float.is_nan x || Float.is_nan y
      then Float.nan
      else if Float.( = ) x y
      then y
      else if Float.( < ) x y
      then Float.succ x
      else Float.pred x);
  float_un [ "caml_neg_float" ] Float.neg;
  float_un [ "caml_abs_float" ] Float.abs;
  float_un [ "caml_sqrt_float" ] Float.sqrt;
  float_un [ "caml_exp_float" ] Float.exp;
  float_un [ "caml_expm1_float" ] Float.expm1;
  float_un [ "caml_log_float" ] Float.log;
  float_un [ "caml_log1p_float" ] Float.log1p;
  float_un [ "caml_log10_float" ] Float.log10;
  float_un [ "caml_log2_float" ] Float.log2;
  float_un [ "caml_exp2_float" ] Float.exp2;
  float_un [ "caml_cbrt_float" ] Float.cbrt;
  float_un [ "caml_sin_float" ] Float.sin;
  float_un [ "caml_cos_float" ] Float.cos;
  float_un [ "caml_tan_float" ] Float.tan;
  float_un [ "caml_asin_float" ] Float.asin;
  float_un [ "caml_acos_float" ] Float.acos;
  float_un [ "caml_atan_float" ] Float.atan;
  float_un [ "caml_sinh_float" ] Float.sinh;
  float_un [ "caml_cosh_float" ] Float.cosh;
  float_un [ "caml_tanh_float" ] Float.tanh;
  float_un [ "caml_asinh_float" ] Float.asinh;
  float_un [ "caml_acosh_float" ] Float.acosh;
  float_un [ "caml_atanh_float" ] Float.atanh;
  float_un [ "caml_ceil_float" ] Float.ceil;
  float_un [ "caml_floor_float" ] Float.floor;
  float_un [ "caml_trunc_float" ] Float.trunc;
  float_un [ "caml_round_float" ] Float.round;
  float_un [ "caml_erf_float" ] Float.erf;
  float_un [ "caml_erfc_float" ] Float.erfc;
  float_un
    [ "caml_float_of_int"; "caml_int32_to_float"; "caml_nativeint_to_float" ]
    Fun.id;
  reg3 "caml_fma_float" (fun _ x y z ->
      Float (Float.fma (as_float x) (as_float y) (as_float z)));
  float_cmp "caml_eq_float" Float.( = );
  float_cmp "caml_neq_float" Float.( <> );
  float_cmp "caml_lt_float" Float.( < );
  float_cmp "caml_le_float" Float.( <= );
  float_cmp "caml_gt_float" Float.( > );
  float_cmp "caml_ge_float" Float.( >= );
  reg1 "caml_signbit_float" (fun _ x -> bool (Float.sign_bit (as_float x)));
  reg2 "caml_float_compare" (fun _ x y ->
      int (compare_floats ~total:true (as_float x) (as_float y)));
  alias
    [ "caml_int_of_float"; "caml_int32_of_float"; "caml_nativeint_of_float" ]
    (fun name -> reg1 name (fun _ x -> int32 (to_int32_wrap (as_float x))));
  reg1 "caml_classify_float" (fun _ x ->
      int
        (match Float.classify_float (as_float x) with
        | FP_normal -> 0
        | FP_subnormal -> 1
        | FP_zero -> 2
        | FP_infinite -> 3
        | FP_nan -> 4));
  reg2 "caml_ldexp_float" (fun _ x n -> Float (Float.ldexp (as_float x) (to_int n)));
  reg1 "caml_frexp_float" (fun _ x ->
      let m, e = Float.frexp (as_float x) in
      block [| Float m; int e |]);
  reg1 "caml_modf_float" (fun _ x ->
      let f, i = Float.modf (as_float x) in
      block [| Float f; Float i |]);
  reg2 "caml_format_float" (fun _ f x -> format_float (as_string f) (as_float x));
  reg3 "caml_hexstring_of_float" (fun _ x prec style ->
      let x = as_float x and prec = to_int prec in
      let s = if prec < 0 then Printf.sprintf "%h" x else Printf.sprintf "%.*h" prec x in
      String
        (match Char.chr (to_int style) with
        | ('+' | ' ') as c when not (Float.sign_bit x || Float.is_nan x) ->
            String.make 1 c ^ s
        | _ -> s));
  reg1 "caml_float_of_string" (fun st s ->
      match float_of_string (as_string s) with
      | f -> Float f
      | exception Failure _ -> ml_failure st "float_of_string");
  reg1 "caml_int32_bits_of_float" (fun _ x -> int32 (Int32.bits_of_float (as_float x)));
  reg1 "caml_int32_float_of_bits" (fun _ x -> Float (Int32.float_of_bits (to_int32 x)));
  reg1 "caml_int64_bits_of_float" (fun _ x -> Int64 (Int64.bits_of_float (as_float x)));
  reg1 "caml_int64_float_of_bits" (fun _ x -> Float (Int64.float_of_bits (as_int64 x)))

(* Int64 *)

let int64_bin name f = reg2 name (fun _ x y -> V.Int64 (f (as_int64 x) (as_int64 y)))

let () =
  int64_bin "caml_int64_add" Int64.add;
  int64_bin "caml_int64_sub" Int64.sub;
  int64_bin "caml_int64_mul" Int64.mul;
  int64_bin "caml_int64_and" Int64.logand;
  int64_bin "caml_int64_or" Int64.logor;
  int64_bin "caml_int64_xor" Int64.logxor;
  reg2 "caml_int64_div" (fun st x y ->
      if Int64.equal (as_int64 y) 0L
      then raise_constant st "Division_by_zero"
      else Int64 (Int64.div (as_int64 x) (as_int64 y)));
  reg2 "caml_int64_mod" (fun st x y ->
      if Int64.equal (as_int64 y) 0L
      then raise_constant st "Division_by_zero"
      else Int64 (Int64.rem (as_int64 x) (as_int64 y)));
  reg2 "caml_int64_shift_left" (fun _ x n ->
      Int64 (Int64.shift_left (as_int64 x) (to_int n land 63)));
  reg2 "caml_int64_shift_right" (fun _ x n ->
      Int64 (Int64.shift_right (as_int64 x) (to_int n land 63)));
  reg2 "caml_int64_shift_right_unsigned" (fun _ x n ->
      Int64 (Int64.shift_right_logical (as_int64 x) (to_int n land 63)));
  reg1 "caml_int64_neg" (fun _ x -> Int64 (Int64.neg (as_int64 x)));
  alias
    [ "caml_int64_of_int"; "caml_int64_of_int32"; "caml_int64_of_nativeint" ]
    (fun name -> reg1 name (fun _ x -> Int64 (Int64.of_int32 (to_int32 x))));
  alias
    [ "caml_int64_to_int"; "caml_int64_to_int32"; "caml_int64_to_nativeint" ]
    (fun name -> reg1 name (fun _ x -> int32 (Int64.to_int32 (as_int64 x))));
  reg1 "caml_int64_of_float" (fun _ x ->
      let f = as_float x in
      Int64 (if Float.is_nan f then 0L else Int64.of_float f));
  reg1 "caml_int64_to_float" (fun _ x -> Float (Int64.to_float (as_int64 x)));
  reg2 "caml_int64_compare" (fun _ x y ->
      int (Int64.compare (as_int64 x) (as_int64 y) |> sign));
  reg2 "caml_int64_ult" (fun _ x y ->
      bool (Int64.unsigned_compare (as_int64 x) (as_int64 y) < 0));
  reg2 "caml_int64_format" (fun _ f x -> format_int64 (as_string f) (as_int64 x));
  reg1 "caml_int64_of_string" (fun st s ->
      match Int64.of_string (as_string s) with
      | n -> Int64 n
      | exception Failure _ -> ml_failure st "Int64.of_string");
  reg1 "caml_int64_bswap" (fun _ x ->
      let n = as_int64 x in
      let r = ref 0L in
      for i = 0 to 7 do
        r :=
          Int64.logor
            (Int64.shift_left !r 8)
            (Int64.logand (Int64.shift_right_logical n (8 * i)) 0xffL)
      done;
      Int64 !r)

(* Polymorphic comparison and hashing *)

let () =
  reg2 "caml_compare" (fun st x y -> int (compare_values st ~total:true x y));
  reg2 "caml_equal" (fun st x y -> bool (compare_values st ~total:false x y = 0));
  reg2 "caml_notequal" (fun st x y -> bool (compare_values st ~total:false x y <> 0));
  reg2 "caml_lessthan" (fun st x y ->
      let c = compare_values st ~total:false x y in
      bool (c < 0 && c <> unordered));
  reg2 "caml_lessequal" (fun st x y ->
      let c = compare_values st ~total:false x y in
      bool (c <= 0 && c <> unordered));
  reg2 "caml_greaterthan" (fun st x y -> bool (compare_values st ~total:false x y > 0));
  reg2 "caml_greaterequal" (fun st x y -> bool (compare_values st ~total:false x y >= 0));
  reg4 "caml_hash" (fun _ count limit seed v ->
      int (hash_value ~count:(to_int count) ~limit:(to_int limit) ~seed:(to_int seed) v));
  reg2 "caml_string_hash" (fun _ seed v ->
      int (hash_value ~count:1 ~limit:1 ~seed:(to_int seed) v))

(* Strings and bytes *)

let check_sub st len ofs l msg =
  if ofs < 0 || l < 0 || ofs > len - l then ml_invalid_arg st msg

let string_cmp name f =
  reg2 name (fun _ x y -> bool (f (String.compare (as_string x) (as_string y))))

let () =
  alias [ "caml_ml_string_length"; "caml_ml_bytes_length" ] (fun name ->
      reg1 name (fun _ s ->
          match s with
          | Bytes b -> int (Bytes.length b)
          | _ -> int (String.length (as_string s))));
  reg1 "caml_create_bytes" (fun st n ->
      let n = to_int n in
      if n < 0 then ml_invalid_arg st "Bytes.create";
      Bytes (Bytes.make n '\000'));
  alias [ "caml_string_get"; "caml_bytes_get" ] (fun name ->
      reg2 name (fun st s i ->
          let s = as_string s and i = to_int i in
          if i < 0 || i >= String.length s then bound_error st;
          int (Char.code s.[i])));
  alias [ "caml_string_unsafe_get"; "caml_bytes_unsafe_get" ] (fun name ->
      reg2 name (fun _ s i ->
          let s = as_string s and i = to_int i in
          if i < 0 || i >= String.length s then error "%s out of bounds" name;
          int (Char.code s.[i])));
  reg3 "caml_bytes_set" (fun st b i c ->
      let b = as_bytes b and i = to_int i in
      if i < 0 || i >= Bytes.length b then bound_error st;
      Bytes.set b i (Char.unsafe_chr (to_int c land 0xff));
      unit);
  reg3 "caml_bytes_unsafe_set" (fun _ b i c ->
      let b = as_bytes b and i = to_int i in
      if i < 0 || i >= Bytes.length b then error "caml_bytes_unsafe_set out of bounds";
      Bytes.set b i (Char.unsafe_chr (to_int c land 0xff));
      unit);
  alias [ "caml_blit_string"; "caml_blit_bytes" ] (fun name ->
      reg5 name (fun st src src_pos dst dst_pos len ->
          let src = as_string src and dst = as_bytes dst in
          let src_pos = to_int src_pos
          and dst_pos = to_int dst_pos
          and len = to_int len in
          check_sub st (String.length src) src_pos len "String.blit";
          check_sub st (Bytes.length dst) dst_pos len "Bytes.blit";
          Bytes.blit_string ~src ~src_pos ~dst ~dst_pos ~len;
          unit));
  reg4 "caml_fill_bytes" (fun st b pos len c ->
      let b = as_bytes b and pos = to_int pos and len = to_int len in
      check_sub st (Bytes.length b) pos len "Bytes.fill";
      Bytes.fill b ~pos ~len (Char.unsafe_chr (to_int c land 0xff));
      unit);
  alias [ "caml_bytes_of_string"; "caml_bytes_of_jsbytes" ] (fun name ->
      reg1 name (fun _ s -> Bytes (Bytes.of_string (as_string s))));
  alias
    [ "caml_string_of_bytes"
    ; "caml_string_of_jsbytes"
    ; "caml_string_of_jsstring"
    ; "caml_bytes_to_string"
    ]
    (fun name -> reg1 name (fun _ s -> String (as_string s)));
  alias [ "caml_jsbytes_of_string"; "caml_jsstring_of_string" ] (fun name ->
      reg1 name (fun _ s -> Native_string (as_string s)));
  reg2 "caml_string_concat" (fun _ x y -> String (as_string x ^ as_string y));
  alias [ "caml_string_equal"; "caml_bytes_equal" ] (fun name ->
      reg2 name (fun _ x y -> bool (String.equal (as_string x) (as_string y))));
  alias [ "caml_string_notequal"; "caml_bytes_notequal" ] (fun name ->
      reg2 name (fun _ x y -> bool (not (String.equal (as_string x) (as_string y)))));
  alias [ "caml_string_compare"; "caml_bytes_compare" ] (fun name ->
      reg2 name (fun _ x y -> int (String.compare (as_string x) (as_string y) |> sign)));
  alias [ "caml_string_lessthan"; "caml_bytes_lessthan" ] (fun name ->
      string_cmp name (fun c -> c < 0));
  alias [ "caml_string_lessequal"; "caml_bytes_lessequal" ] (fun name ->
      string_cmp name (fun c -> c <= 0));
  alias [ "caml_string_greaterthan"; "caml_bytes_greaterthan" ] (fun name ->
      string_cmp name (fun c -> c > 0));
  alias [ "caml_string_greaterequal"; "caml_bytes_greaterequal" ] (fun name ->
      string_cmp name (fun c -> c >= 0))

(* Little-endian accesses to strings and bytes *)

let get_bytes st name s i n =
  let s = as_string s and i = to_int i in
  if i < 0 || i > String.length s - n then ml_invalid_arg st name;
  let r = ref 0L in
  for j = n - 1 downto 0 do
    r := Int64.logor (Int64.shift_left !r 8) (Int64.of_int (Char.code s.[i + j]))
  done;
  !r

let set_bytes st name b i n v =
  let b = as_bytes b and i = to_int i in
  if i < 0 || i > Bytes.length b - n then ml_invalid_arg st name;
  for j = 0 to n - 1 do
    Bytes.set
      b
      (i + j)
      (Char.unsafe_chr
         (Int64.to_int (Int64.logand (Int64.shift_right_logical v (8 * j)) 0xffL)))
  done;
  unit

let () =
  alias [ "caml_string_get16"; "caml_bytes_get16" ] (fun name ->
      reg2 name (fun st s i ->
          int (Int64.to_int (get_bytes st "index out of bounds" s i 2))));
  alias [ "caml_string_get32"; "caml_bytes_get32" ] (fun name ->
      reg2 name (fun st s i ->
          int32 (Int64.to_int32 (get_bytes st "index out of bounds" s i 4))));
  alias [ "caml_string_get64"; "caml_bytes_get64" ] (fun name ->
      reg2 name (fun st s i -> Int64 (get_bytes st "index out of bounds" s i 8)));
  reg3 "caml_bytes_set16" (fun st b i v ->
      set_bytes st "index out of bounds" b i 2 (Int64.of_int (to_int v)));
  reg3 "caml_bytes_set32" (fun st b i v ->
      set_bytes st "index out of bounds" b i 4 (Int64.of_int32 (to_int32 v)));
  reg3 "caml_bytes_set64" (fun st b i v ->
      set_bytes st "index out of bounds" b i 8 (as_int64 v))

(* Arrays *)

let array_get st a i =
  let b = as_block a and i = to_int i in
  if i < 0 || i >= Array.length b.fields then bound_error st;
  b.fields.(i)

let array_set st a i v =
  let b = as_block a and i = to_int i in
  if i < 0 || i >= Array.length b.fields then bound_error st;
  b.fields.(i) <- v;
  unit

let () =
  alias [ "caml_make_vect"; "caml_array_make" ] (fun name ->
      reg2 name (fun st len v ->
          let len = to_int len in
          if len < 0 then ml_invalid_arg st "Array.make";
          block (Array.make len v)));
  alias
    [ "caml_make_float_vect"; "caml_array_create_float"; "caml_floatarray_create" ]
    (fun name ->
      reg1 name (fun st len ->
          let len = to_int len in
          if len < 0 then ml_invalid_arg st "Array.create_float";
          block ~tag:254 (Array.make len (V.Float 0.))));
  alias
    [ "caml_array_get"
    ; "caml_array_get_addr"
    ; "caml_array_get_float"
    ; "caml_floatarray_get"
    ]
    (fun name -> reg2 name array_get);
  alias
    [ "caml_array_set"
    ; "caml_array_set_addr"
    ; "caml_array_set_float"
    ; "caml_floatarray_set"
    ]
    (fun name -> reg3 name array_set);
  alias
    [ "caml_array_unsafe_get"
    ; "caml_array_unsafe_get_float"
    ; "caml_floatarray_unsafe_get"
    ]
    (fun name -> reg2 name (fun _ a i -> field a (to_int i)));
  alias
    [ "caml_array_unsafe_set"
    ; "caml_array_unsafe_set_float"
    ; "caml_floatarray_unsafe_set"
    ; "caml_array_unsafe_set_addr"
    ]
    (fun name ->
      reg3 name (fun _ a i v ->
          set_field a (to_int i) v;
          unit));
  reg2 "caml_check_bound" (fun st a i ->
      let len = Array.length (as_block a).fields and i = to_int i in
      if i < 0 || i >= len then bound_error st;
      a);
  alias [ "caml_array_sub"; "caml_floatarray_sub" ] (fun name ->
      reg3 name (fun st a pos len ->
          let b = as_block a and pos = to_int pos and len = to_int len in
          check_sub st (Array.length b.fields) pos len "Array.sub";
          block ~tag:b.tag (Array.sub b.fields ~pos ~len)));
  alias [ "caml_array_append"; "caml_floatarray_append" ] (fun name ->
      reg2 name (fun _ a a' ->
          let b = as_block a and b' = as_block a' in
          let tag = if b.tag = 254 || b'.tag = 254 then 254 else 0 in
          block ~tag (Array.append b.fields b'.fields)));
  reg1 "caml_array_concat" (fun _ l ->
      let l = List.map (array_of_list l) ~f:as_block in
      let tag = if List.exists l ~f:(fun b -> b.V.tag = 254) then 254 else 0 in
      block ~tag (Array.concat (List.map l ~f:(fun b -> b.V.fields))));
  alias [ "caml_array_blit"; "caml_floatarray_blit" ] (fun name ->
      reg5 name (fun st src src_pos dst dst_pos len ->
          let src = as_block src and dst = as_block dst in
          let src_pos = to_int src_pos
          and dst_pos = to_int dst_pos
          and len = to_int len in
          check_sub st (Array.length src.fields) src_pos len "Array.blit";
          check_sub st (Array.length dst.fields) dst_pos len "Array.blit";
          Array.blit ~src:src.fields ~src_pos ~dst:dst.fields ~dst_pos ~len;
          unit));
  alias [ "caml_array_fill"; "caml_floatarray_fill" ] (fun name ->
      reg4 name (fun st a pos len v ->
          let b = as_block a and pos = to_int pos and len = to_int len in
          check_sub st (Array.length b.fields) pos len "Array.fill";
          Array.fill b.fields ~pos ~len v;
          unit))

(* Objects, lazy values, recursive values *)

let update_tag v o n =
  let b = as_block v in
  if b.tag = o
  then (
    b.tag <- n;
    true)
  else false

let () =
  reg1 "caml_obj_tag" (fun st v -> int (ml_tag st v));
  reg1 "%direct_obj_tag" (fun _ v -> int (as_block v).tag);
  reg2 "caml_obj_block" (fun _ tag size ->
      block ~tag:(to_int tag) (Array.make (to_int size) unit));
  reg1 "caml_obj_dup" (fun _ v ->
      match v with
      | Block b -> V.Block { b with fields = Array.copy b.fields }
      | Bytes b -> Bytes (Bytes.copy b)
      | _ -> v);
  reg2 "caml_obj_with_tag" (fun _ tag v ->
      let b = as_block v in
      V.Block { b with tag = to_int tag; fields = Array.copy b.fields });
  reg2 "caml_obj_raw_field" (fun _ v i -> field v (to_int i));
  reg3 "caml_obj_set_raw_field" (fun _ v i x ->
      set_field v (to_int i) x;
      unit);
  reg1 "caml_lazy_make_forward" (fun _ v -> block ~tag:250 [| v |]);
  reg1 "caml_obj_make_forward" (fun _ v -> block ~tag:250 [| v |]);
  reg1 "caml_lazy_update_to_forcing" (fun _ v ->
      match v with
      | Block _ when update_tag v 246 244 -> unit
      | _ -> int 1);
  reg1 "caml_lazy_update_to_forward" (fun _ v ->
      ignore (update_tag v 244 250);
      unit);
  reg1 "caml_lazy_reset_to_lazy" (fun _ v ->
      ignore (update_tag v 244 246);
      unit);
  reg1 "caml_lazy_read_result" (fun st v -> if ml_tag st v = 250 then field v 0 else v);
  alias
    [ "caml_alloc_dummy"; "caml_alloc_dummy_float"; "caml_alloc_dummy_function" ]
    (fun name -> reg1 name (fun _ _ -> block [||]));
  reg2 "caml_alloc_dummy_mixed" (fun _ _ _ -> block [||]);
  reg2 "caml_update_dummy" (fun _ x y ->
      let b = as_block x in
      (match y with
      | Block b' -> (
          match b'.fun_ with
          | Some _ -> b.fun_ <- b'.fun_
          | None ->
              b.tag <- b'.tag;
              b.fields <- Array.copy b'.fields)
      | Closure _ | Partial _ | Prim_closure _ | Host _ -> b.fun_ <- Some y
      | _ -> error "caml_update_dummy");
      unit);
  reg1 "caml_fresh_oo_id" (fun st _ ->
      let id = st.oo_id in
      st.oo_id <- id + 1;
      int id);
  reg1 "caml_set_oo_id" (fun st b ->
      set_field b 1 (int st.oo_id);
      st.oo_id <- st.oo_id + 1;
      b);
  reg0 "caml_oo_cache_id" (fun st ->
      let id = st.cache_id in
      st.cache_id <- id + 1;
      int id)

(* Atomics and domains *)

let () =
  reg1 "caml_atomic_load" (fun _ r -> field r 0);
  reg2 "caml_atomic_exchange" (fun _ r v ->
      let old = field r 0 in
      set_field r 0 v;
      old);
  reg2 "caml_atomic_set" (fun _ r v ->
      set_field r 0 v;
      unit);
  reg3 "caml_atomic_cas" (fun _ r o n ->
      if phys_equal_value (field r 0) o
      then (
        set_field r 0 n;
        int 1)
      else int 0);
  reg2 "caml_atomic_fetch_add" (fun _ r n ->
      let old = field r 0 in
      set_field r 0 (V.Int (Targetint.add (as_int old) (as_int n)));
      old);
  reg1 "caml_atomic_make_contended" (fun _ v -> block [| v |]);
  reg0 "caml_domain_dls_get" (fun st -> st.dls);
  reg1 "caml_domain_dls_set" (fun st v ->
      st.dls <- v;
      unit);
  reg2 "caml_domain_dls_compare_and_set" (fun st o n ->
      if phys_equal_value st.dls o
      then (
        st.dls <- n;
        int 1)
      else int 0);
  reg0 "caml_ml_domain_id" (fun _ -> int 0);
  reg0 "caml_ml_domain_index" (fun _ -> int 0);
  reg0 "caml_recommended_domain_count" (fun _ -> int 1)

let () =
  reg2 "caml_atomic_load_field" (fun _ r i -> field r (to_int i));
  reg3 "caml_atomic_set_field" (fun _ r i v ->
      set_field r (to_int i) v;
      unit);
  reg3 "caml_atomic_exchange_field" (fun _ r i v ->
      let i = to_int i in
      let old = field r i in
      set_field r i v;
      old);
  reg4 "caml_atomic_cas_field" (fun _ r i o n ->
      let i = to_int i in
      if phys_equal_value (field r i) o
      then (
        set_field r i n;
        int 1)
      else int 0);
  reg3 "caml_atomic_fetch_add_field" (fun _ r i n ->
      let i = to_int i in
      let old = field r i in
      set_field r i (V.Int (Targetint.add (as_int old) (as_int n)));
      old)

(* Ephemerons, without weakness: field 0 holds the data and the
   following fields the keys, [Null] meaning that there is no value *)

let ephe_get e i =
  match field e i with
  | Null -> unit
  | v -> block [| v |]

let ephe_blit st e1 o1 e2 o2 len =
  let b1 = as_block e1 and b2 = as_block e2 in
  check_sub st (Array.length b1.fields) o1 len "Weak.blit";
  check_sub st (Array.length b2.fields) o2 len "Weak.blit";
  Array.blit ~src:b1.fields ~src_pos:o1 ~dst:b2.fields ~dst_pos:o2 ~len;
  unit

let () =
  reg1 "caml_ephe_create" (fun _ n -> block (Array.make (to_int n + 1) V.Null));
  alias [ "caml_ephe_get_key"; "caml_ephe_get_key_copy" ] (fun name ->
      reg2 name (fun _ e i -> ephe_get e (to_int i + 1)));
  reg2 "caml_ephe_check_key" (fun _ e i ->
      bool
        (match field e (to_int i + 1) with
        | Null -> false
        | _ -> true));
  reg3 "caml_ephe_set_key" (fun _ e i v ->
      set_field e (to_int i + 1) v;
      unit);
  reg2 "caml_ephe_unset_key" (fun _ e i ->
      set_field e (to_int i + 1) V.Null;
      unit);
  reg5 "caml_ephe_blit_key" (fun st e1 o1 e2 o2 len ->
      ephe_blit st e1 (to_int o1 + 1) e2 (to_int o2 + 1) (to_int len));
  alias [ "caml_ephe_get_data"; "caml_ephe_get_data_copy" ] (fun name ->
      reg1 name (fun _ e -> ephe_get e 0));
  reg1 "caml_ephe_check_data" (fun _ e ->
      bool
        (match field e 0 with
        | Null -> false
        | _ -> true));
  reg2 "caml_ephe_set_data" (fun _ e v ->
      set_field e 0 v;
      unit);
  reg1 "caml_ephe_unset_data" (fun _ e ->
      set_field e 0 V.Null;
      unit);
  reg2 "caml_ephe_blit_data" (fun st e1 e2 -> ephe_blit st e1 0 e2 0 1)

(* Global data and runtime system *)

let () =
  alias [ "caml_register_global"; "caml_register_global_predef" ] (fun name ->
      reg2 name (fun st v n ->
          String.Hashtbl.replace st.globals (as_string n) v;
          unit));
  reg2 "caml_register_named_value" (fun st n v ->
      String.Hashtbl.replace st.named_values (as_string n) v;
      unit);
  reg1 "caml_named_value" (fun st n ->
      match String.Hashtbl.find_opt st.named_values (as_string n) with
      | Some v -> block [| v |]
      | None -> unit);
  alias
    [ "caml_fs_init"
    ; "caml_set_link_info"
    ; "caml_register_block_descs"
    ; "caml_ensure_stack_capacity"
    ; "caml_record_backtrace"
    ; "caml_restore_raw_backtrace"
    ; "caml_ml_enable_runtime_warnings"
    ; "caml_gc_full_major"
    ; "caml_gc_major"
    ; "caml_gc_minor"
    ; "caml_gc_compaction"
    ; "caml_final_register"
    ; "caml_final_register_called_without_value"
    ; "caml_final_release"
    ; "debugger"
    ]
    (fun name -> register name 1 (fun _ _ -> unit));
  reg0 "caml_backtrace_status" (fun _ -> int 0);
  reg0 "caml_ml_runtime_warnings_enabled" (fun _ -> int 0);
  reg0 "caml_get_exception_raw_backtrace" (fun _ -> block [||]);
  reg1 "caml_convert_raw_backtrace" (fun _ _ -> unit);
  reg0 "caml_sys_const_big_endian" (fun _ -> int 0);
  reg0 "caml_sys_const_word_size" (fun _ -> int 32);
  reg0 "caml_sys_const_int_size" (fun _ -> int (Targetint.num_bits ()));
  reg0 "caml_sys_const_max_wosize" (fun _ -> int (0x7fffffff / 4));
  reg0 "caml_sys_const_ostype_unix" (fun _ -> int 1);
  reg0 "caml_sys_const_ostype_win32" (fun _ -> int 0);
  reg0 "caml_sys_const_ostype_cygwin" (fun _ -> int 0);
  reg0 "caml_sys_const_backend_type" (fun _ -> block [| String "js_of_ocaml" |]);
  reg0 "caml_sys_const_naked_pointers_checked" (fun _ -> int 0);
  reg0 "caml_sys_get_config" (fun _ -> block [| String "Unix"; int 32; int 0 |]);
  let argv = V.String "a.out" in
  reg0 "caml_sys_get_argv" (fun _ -> block [| argv; block [| argv |] |]);
  reg0 "caml_sys_argv" (fun _ -> block [| argv |]);
  reg0 "caml_sys_executable_name" (fun _ -> argv);
  reg1 "caml_sys_getenv" (fun st _ -> raise_constant st "Not_found");
  reg1 "caml_sys_getenv_opt" (fun _ _ -> unit);
  reg3 "caml_sys_open" (fun st _ _ _ -> raise_with_string st "Sys_error" "file system");
  alias
    [ "caml_sys_convert_signal_number"; "caml_sys_rev_convert_signal_number" ]
    (fun name -> reg1 name (fun _ n -> n));
  reg0 "caml_sys_time" (fun _ -> Float 0.);
  reg0 "caml_sys_time_include_children" (fun _ -> Float 0.);
  reg0 "caml_sys_random_seed" (fun _ -> block [| int 1; int 2; int 3; int 4 |]);
  reg1 "caml_sys_file_exists" (fun _ _ -> int 0);
  reg0 "caml_sys_getcwd" (fun _ -> String "/");
  reg0 "caml_sys_io_buffer_size" (fun _ -> int 65536);
  reg0 "caml_runtime_variant" (fun _ -> String "");
  reg0 "caml_runtime_parameters" (fun _ -> String "");
  reg2 "caml_install_signal_handler" (fun _ _ _ -> unit);
  reg1 "caml_sys_exit" (fun _ n -> raise (Exit_program (to_int n)));
  reg0 "caml_gc_counters" (fun _ -> block [| Float 0.; Float 0.; Float 0. |])

(* Mutexes and conditions: there is a single thread *)

let () =
  reg0 "caml_ml_mutex_new" (fun _ -> block [| int 0 |]);
  reg1 "caml_ml_mutex_lock" (fun st m ->
      if not (Targetint.is_zero (as_int (field m 0)))
      then raise_with_string st "Sys_error" "Mutex.lock: Resource deadlock avoided";
      set_field m 0 (int 1);
      unit);
  reg1 "caml_ml_mutex_try_lock" (fun _ m ->
      if Targetint.is_zero (as_int (field m 0))
      then (
        set_field m 0 (int 1);
        int 1)
      else int 0);
  reg1 "caml_ml_mutex_unlock" (fun _ m ->
      set_field m 0 (int 0);
      unit);
  reg0 "caml_ml_condition_new" (fun _ -> block [||]);
  reg2 "caml_ml_condition_wait" (fun _ _ _ -> unsupported "Condition.wait");
  alias [ "caml_ml_condition_signal"; "caml_ml_condition_broadcast" ] (fun name ->
      reg1 name (fun _ _ -> unit))

(* Objects *)

let get_public_method obj tag =
  let meths = as_block (field obj 0) in
  let get i = meths.fields.(i - 1) in
  let tag = as_int tag in
  let li = ref 3 and hi = ref ((to_int (get 1) * 2) + 1) in
  while !li < !hi do
    let mi = ((!li + !hi) asr 1) lor 1 in
    if Targetint.( < ) tag (as_int (get (mi + 1))) then hi := mi - 2 else li := mi
  done;
  match get (!li + 1) with
  | Int t when Targetint.equal t tag -> get !li
  | _ -> unit

let () =
  reg3 "caml_get_public_method" (fun _ obj tag _ -> get_public_method obj tag);
  reg3 "caml_get_cached_method" (fun _ obj tag _ -> get_public_method obj tag)

(* Miscellaneous *)

let () =
  reg0 "caml_sys_temp_dir_name" (fun _ -> String "/tmp");
  reg0 "caml_gc_minor_words" (fun _ -> Float 0.);
  reg3 "caml_md5_string" (fun st s pos len ->
      let s = as_string s and pos = to_int pos and len = to_int len in
      check_sub st (String.length s) pos len "Digest.substring";
      String (Digest.string (String.sub s ~pos ~len)));
  reg1 "caml_unix_inet_addr_of_string" (fun _ s -> String (as_string s));
  reg0 "caml_alloc_dummy_lazy" (fun _ -> block [||])

(* Functions *)

let rec split n l =
  if n = 0
  then [], l
  else
    match l with
    | x :: r ->
        let l1, l2 = split (n - 1) r in
        x :: l1, l2
    | [] -> assert false

let not_a_function (v : V.t) =
  error
    "call to a value which is not a function (%s)"
    (match v with
    | Int n -> "integer " ^ Targetint.to_string n
    | Float _ -> "float"
    | String _ | Native_string _ | Bytes _ -> "string"
    | Block b -> Printf.sprintf "block of tag %d" b.tag
    | Int64 _ -> "int64"
    | Channel _ -> "channel"
    | Js_array _ -> "JavaScript array"
    | Trampoline _ -> "trampoline"
    | Stack _ -> "stack"
    | Null -> "null"
    | Closure _ | Partial _ | Prim_closure _ | Host _ -> "function")

let rec cps_of (v : V.t) =
  match v with
  | Closure { cps; _ } | Host { host_cps = cps; _ } -> cps
  | Block { fun_ = Some f; _ } -> cps_of f
  | _ -> None

let rec set_cps (v : V.t) c =
  match v with
  | Closure f -> f.cps <- Some c
  | Host h -> h.host_cps <- Some c
  | Block { fun_ = Some f; _ } -> set_cps f c
  | _ -> error "caml_cps_closure: not a function"

let rec arity_of (v : V.t) =
  match v with
  | Closure c -> Array.length c.fn.params
  | Partial p -> p.remaining
  | Prim_closure name -> fst (find_prim name)
  | Host { arity; _ } -> arity
  | Block { fun_ = Some f; _ } -> arity_of f
  | _ -> not_a_function v

(* Trampolines (see [Generate_closure]) *)

let apply_ref =
  ref (fun _ (_ : V.t) (_ : V.t list) ~exact:(_ : bool) : V.t -> assert false)

let call_gen_cps_ref = ref (fun _ (_ : V.t) (_ : V.t list) : V.t -> assert false)

let () =
  register "%js_array" 0 (fun _ l -> Js_array (Array.of_list l));
  register "caml_trampoline_return" 2 (fun _ l ->
      match l with
      | [ f; args ] -> Trampoline (f, args, false)
      | [ f; args; direct ] -> Trampoline (f, args, phys_equal_value direct (int 1))
      | _ -> bad_arity "caml_trampoline_return");
  reg1 "caml_trampoline" (fun st r ->
      let rec loop (r : V.t) =
        match r with
        | Trampoline (f, Js_array args, _) ->
            (* [f.apply(null, args)] in JavaScript *)
            loop (!apply_ref st f (Array.to_list args) ~exact:true)
        | Trampoline _ -> error "caml_trampoline"
        | _ -> r
      in
      loop r)

(* Double translation: the CPS version is attached to the direct-style
   closure, which is only called directly here *)
let () =
  reg2 "caml_cps_closure" (fun _ direct cps ->
      set_cps direct cps;
      direct)

(* Effect handlers. A continuation is a block of tag 245 containing the
   innermost and the outermost fibers of a chain of fibers, linked by
   their [parent] field. *)

let as_fiber (v : V.t) =
  match v with
  | Stack f -> f
  | _ -> error "fiber expected"

(* Since OCaml 5.6, effect handlers no longer take the last fiber of the
   continuation *)
let handler_takes_last_fiber = Ocaml_version.compare Ocaml_version.current [ 5; 6 ] < 0

let continuation_already_resumed st =
  match String.Hashtbl.find_opt st.named_values "Effect.Continuation_already_resumed" with
  | Some exn -> raise (Ml_exn exn)
  | None -> error "continuation already resumed"

let unhandled_effect st eff =
  match String.Hashtbl.find_opt st.named_values "Effect.Unhandled" with
  | Some exn -> block [| exn; eff |]
  | None ->
      let id = st.oo_id in
      st.oo_id <- id + 1;
      block ~tag:248 [| String "Effect.Unhandled"; int id |]

let take_continuation cont =
  let stack = field cont 0 in
  set_field cont 0 unit;
  stack

let () =
  reg1 "caml_continuation_use_noexc" (fun _ cont -> take_continuation cont);
  reg4 "caml_continuation_use_and_update_handler_noexc" (fun _ cont hv hx hf ->
      let stack = take_continuation cont in
      (match stack with
      | Stack _ -> (as_fiber (field cont 1)).handlers <- [| hv; hx; hf |]
      | _ -> ());
      stack);
  reg2 "caml_get_continuation_callstack" (fun _ _ _ -> block [||]);
  reg2 "caml_maybe_attach_backtrace" (fun _ exn _ -> exn)

(* Once the program is in CPS, effects are implemented by the runtime
   (runtime/js/effect.js and the trampolines of runtime/js/jslib.js),
   which is transcribed here *)

let call_gen st f args = !apply_ref st f args ~exact:false

(* [caml_call_gen_cps] *)
let call_gen_cps st f args =
  if st.dt then !call_gen_cps_ref st f args else call_gen st f args

let array_of_js (v : V.t) =
  match v with
  | Js_array a -> a
  | _ -> error "JavaScript array expected"

let trampoline_return ?(direct = false) f args =
  V.Trampoline (f, Js_array (Array.of_list args), direct)

let pop_fiber st =
  match st.current.parent with
  | Some c ->
      st.current.parent <- None;
      st.current <- c;
      c.k
  | None -> error "no parent fiber"

let cps_alloc_stack st hv hx hf : V.t =
  let call f x =
    let args = [ x; pop_fiber st ] in
    if check_depth st then call_gen_cps st f args else trampoline_return f args
  in
  Stack
    { (toplevel_fiber ()) with
      handlers = [| hv; hx; hf |]
    ; k =
        host 1 (function
          | [ x ] -> call st.current.handlers.(0) x
          | _ -> bad_arity "caml_alloc_stack_hval")
    ; traps =
        [ host 1 (function
            | [ e ] -> call st.current.handlers.(1) e
            | _ -> bad_arity "caml_alloc_stack_hexn")
        ]
    }

let resume_stack st (stack : V.t) (last : V.t) k =
  match stack with
  | Stack s ->
      let last =
        match last with
        | Int n when Targetint.is_zero n ->
            let rec find (f : V.fiber) =
              match f.parent with
              | None -> f
              | Some f -> find f
            in
            find s
        | _ -> as_fiber last
      in
      st.current.k <- k;
      last.parent <- Some st.current;
      st.current <- s;
      s.k
  | _ -> continuation_already_resumed st

let call_effect_handler st handler eff cont last k1 =
  let args =
    if handler_takes_last_fiber then [ eff; cont; last; k1 ] else [ eff; cont; k1 ]
  in
  if check_depth st
  then
    (* [caml_get_cps_fun]: with double translation, the effect handler is
       called in CPS if it has a CPS version. Otherwise, it is called
       directly, the continuation being an extra argument which is ignored
       by JavaScript *)
    match cps_of handler with
    | Some h when st.dt -> !apply_ref st h args ~exact:true
    | None when st.dt ->
        let args, _ = split (List.length args - 1) args in
        !apply_ref st handler args ~exact:true
    | Some _ | None -> !apply_ref st handler args ~exact:true
  else trampoline_return handler args

let cps_trampoline st f args =
  let saved_stack_depth = st.stack_depth in
  let saved_current = st.current in
  let depth = st.depth in
  st.current <- toplevel_fiber ();
  let rec loop f args =
    st.stack_depth <- 40;
    match call_gen st f args with
    | V.Trampoline (f, Js_array args, _) -> loop f (Array.to_list args)
    | Trampoline _ -> error "ill-formed trampoline"
    | res -> res
    | exception Ml_exn e -> (
        (* Exception raised by the runtime, or by a function in direct style *)
        st.depth <- depth;
        match st.current.traps with
        | [] -> raise (Ml_exn e)
        | h :: t ->
            st.current.traps <- t;
            loop h [ e ])
  in
  let restore () =
    st.stack_depth <- saved_stack_depth;
    st.current <- saved_current
  in
  match
    loop
      f
      (Array.to_list (array_of_js args)
      @ [ host 1 (function
            | [ x ] -> x
            | _ -> bad_arity "caml_cps_trampoline")
        ])
  with
  | v ->
      restore ();
      v
  | exception e ->
      restore ();
      raise e

let () =
  reg3 "caml_alloc_stack" (fun st hv hx hf ->
      if st.cps || st.dt
      then cps_alloc_stack st hv hx hf
      else unsupported "effect handlers");
  reg3 "caml_resume_stack" (fun st stack last k -> resume_stack st stack last k);
  reg0 "caml_pop_trap" (fun st ->
      match st.current.traps with
      | [] ->
          host 1 (function
            | [ x ] -> raise (Ml_exn x)
            | _ -> bad_arity "caml_pop_trap")
      | h :: t ->
          st.current.traps <- t;
          h);
  reg1 "caml_push_trap" (fun st h ->
      st.current.traps <- h :: st.current.traps;
      unit);
  reg1 "caml_raise_unhandled" (fun st eff -> raise (Ml_exn (unhandled_effect st eff)));
  reg2 "caml_perform_effect" (fun st eff k0 ->
      if Option.is_none st.current.parent then raise (Ml_exn (unhandled_effect st eff));
      let handler = st.current.handlers.(2) in
      let last = st.current in
      last.k <- k0;
      let cont = block ~tag:245 [| Stack last; Stack last |] in
      let k1 = pop_fiber st in
      call_effect_handler st handler eff cont (Stack last) k1);
  reg4 "caml_reperform_effect" (fun st eff cont last k0 ->
      (* Before OCaml 5.6, the last fiber is passed explicitly *)
      let last = if handler_takes_last_fiber then last else field cont 1 in
      if Option.is_none st.current.parent
      then (
        let exn = unhandled_effect st eff in
        let stack = take_continuation cont in
        ignore (resume_stack st stack last k0);
        raise (Ml_exn exn));
      let handler = st.current.handlers.(2) in
      let last_fiber = st.current in
      last_fiber.k <- k0;
      (as_fiber last).parent <- Some last_fiber;
      set_field cont 1 (Stack last_fiber);
      let k1 = pop_fiber st in
      call_effect_handler st handler eff cont (Stack last_fiber) k1);
  reg2 "caml_cps_trampoline" (fun st f args -> cps_trampoline st f args);
  reg0 "caml_stack_check_depth" (fun st -> bool (check_depth st));
  reg4 "caml_resume" (fun st f arg stack last ->
      let saved_stack_depth = st.stack_depth in
      let saved_current = st.current in
      let depth = st.depth in
      let restore () =
        st.stack_depth <- saved_stack_depth;
        st.current <- saved_current
      in
      let rec loop f args direct =
        st.stack_depth <- 40;
        match
          if direct then !apply_ref st f args ~exact:true else call_gen_cps st f args
        with
        | V.Trampoline (f, Js_array args, direct) -> loop f (Array.to_list args) direct
        | Trampoline _ -> error "ill-formed trampoline"
        | res -> res
        | exception Ml_exn e -> (
            st.depth <- depth;
            match st.current.traps with
            | [] -> raise (Ml_exn e)
            | h :: t ->
                st.current.traps <- t;
                loop h [ e ] true)
      in
      match
        st.current <- toplevel_fiber ();
        let k =
          resume_stack
            st
            stack
            last
            (host 1 (function
              | [ x ] -> x
              | _ -> bad_arity "caml_resume"))
        in
        loop f [ arg; k ] false
      with
      | v ->
          restore ();
          v
      | exception e ->
          restore ();
          raise e);
  reg2 "caml_direct_trampoline" (fun st f args ->
      let saved_stack_depth = st.stack_depth in
      let rec loop f args =
        st.stack_depth <- 40;
        match !apply_ref st f args ~exact:true with
        | V.Trampoline (f, Js_array args, _) -> loop f (Array.to_list args)
        | Trampoline _ -> error "ill-formed trampoline"
        | res -> res
      in
      match loop f (Array.to_list (array_of_js args)) with
      | v ->
          st.stack_depth <- saved_stack_depth;
          v
      | exception e ->
          st.stack_depth <- saved_stack_depth;
          raise e);
  reg1 "caml_assume_no_perform" (fun st f ->
      if st.cps
      then cps_trampoline st f (Js_array [| unit |])
      else !apply_ref st f [ unit ] ~exact:false)

(* Effect handlers, implemented with the effect handlers of the host
   (they are thus not supported when the compiler is built with OCaml 4).
   A fiber runs under a host handler, which catches the effects performed
   by the fiber and calls the effect handler of the fiber, and calls the
   value or exception handler when the fiber terminates. *)

module _ = struct
  type _ Effect.t +=
    | Perform : V.t -> (unit -> V.t) Effect.t
    | Reperform : V.t * V.t -> (unit -> V.t) Effect.t  (** Effect and continuation *)

  let apply st f args = !apply_ref st f args ~exact:false

  let call_effect_handler st (fiber : V.fiber) eff cont =
    apply
      st
      fiber.handlers.(2)
      (if handler_takes_last_fiber then [ eff; cont; Stack fiber ] else [ eff; cont ])

  (* The call depth is counted from the start of each fiber, as each
     fiber has its own stack in the host *)
  let suspend st (fiber : V.fiber) k =
    fiber.resume <- Some (fun body -> Effect.Deep.continue k body);
    fiber.parent <- None;
    fiber.depth <- st.depth;
    st.depth <- fiber.parent_depth

  let start st (fiber : V.fiber) body =
    fiber.parent_depth <- st.depth;
    st.depth <- 0;
    Effect.Deep.match_with
      body
      ()
      { retc =
          (fun v ->
            st.depth <- fiber.parent_depth;
            apply st fiber.handlers.(0) [ v ])
      ; exnc =
          (fun e ->
            match e with
            | Ml_exn v ->
                st.depth <- fiber.parent_depth;
                apply st fiber.handlers.(1) [ v ]
            | _ -> raise e)
      ; effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Perform eff ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    suspend st fiber k;
                    let cont = block ~tag:245 [| Stack fiber; Stack fiber |] in
                    call_effect_handler st fiber eff cont)
            | Reperform (eff, cont) ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    suspend st fiber k;
                    (* The fiber becomes the last fiber of the continuation *)
                    (as_fiber (field cont 1)).parent <- Some fiber;
                    set_field cont 1 (Stack fiber);
                    call_effect_handler st fiber eff cont)
            | _ -> None)
      }

  let resume_fiber st (fiber : V.fiber) body =
    match fiber.resume with
    | None -> start st fiber body
    | Some resume ->
        fiber.resume <- None;
        fiber.parent_depth <- st.depth;
        st.depth <- fiber.depth;
        resume body

  (* The outermost fiber is resumed first, and resumes the next fiber
     where it was suspended, down to the innermost fiber, which runs
     [body] *)
  let rec resume_chain st (fiber : V.fiber) body =
    match fiber.parent with
    | None -> resume_fiber st fiber body
    | Some parent ->
        fiber.parent <- None;
        resume_chain st parent (fun () -> resume_fiber st fiber body)

  let resume st (stack : V.t) body =
    match stack with
    | Stack fiber -> resume_chain st fiber body
    | _ -> continuation_already_resumed st

  let alloc_stack hv hx hf : V.t =
    Stack { (toplevel_fiber ()) with handlers = [| hv; hx; hf |] }

  let () =
    reg3 "caml_alloc_stack" (fun st hv hx hf ->
        if st.cps || st.dt then cps_alloc_stack st hv hx hf else alloc_stack hv hx hf);
    reg4 "%resume" (fun st stack f arg _ ->
        resume st stack (fun () -> apply st f [ arg ]));
    reg5 "%with_stack" (fun st hv hx hf f arg ->
        resume st (alloc_stack hv hx hf) (fun () -> apply st f [ arg ]));
    reg1 "%perform" (fun st eff ->
        match Effect.perform (Perform eff) with
        | body -> body ()
        | exception Effect.Unhandled _ -> raise (Ml_exn (unhandled_effect st eff)));
    reg3 "%reperform" (fun st eff cont _ ->
        match Effect.perform (Reperform (eff, cont)) with
        | body -> body ()
        | exception Effect.Unhandled _ ->
            (* Raise the exception where the effect was performed *)
            let exn = unhandled_effect st eff in
            resume st (take_continuation cont) (fun () -> raise (Ml_exn exn)))
end
[@@if ocaml_version >= (5, 0, 0)]

(* Channels: only standard output and standard error are supported *)

let output st ch s =
  match ch with
  | V.Channel { fd = 1 } -> Buffer.add_string st.stdout s
  | Channel { fd = 2 } -> Buffer.add_string st.stderr s
  | _ -> unsupported "output to a file"

let () =
  reg1 "caml_ml_open_descriptor_out" (fun st fd ->
      let ch = V.Channel { fd = to_int fd } in
      st.out_channels <- ch :: st.out_channels;
      ch);
  reg1 "caml_ml_open_descriptor_in" (fun _ fd -> V.Channel { fd = to_int fd });
  reg0 "caml_ml_out_channels_list" (fun st -> list_of_values st.out_channels);
  alias [ "caml_ml_output"; "caml_ml_output_bytes" ] (fun name ->
      reg4 name (fun st ch s pos len ->
          let s = as_string s and pos = to_int pos and len = to_int len in
          check_sub st (String.length s) pos len "output";
          output st ch (String.sub s ~pos ~len);
          unit));
  reg2 "caml_ml_output_char" (fun st ch c ->
      output st ch (String.make 1 (Char.unsafe_chr (to_int c land 0xff)));
      unit);
  reg2 "caml_ml_output_int" (fun st ch n ->
      let n = to_int n in
      output
        st
        ch
        (String.init 4 ~f:(fun i -> Char.unsafe_chr ((n lsr (8 * (3 - i))) land 0xff)));
      unit);
  alias [ "caml_ml_flush"; "caml_ml_close_channel" ] (fun name ->
      reg1 name (fun _ _ -> unit));
  reg2 "caml_ml_set_channel_name" (fun _ _ _ -> unit);
  reg2 "caml_ml_set_buffered" (fun _ _ _ -> unit);
  reg1 "caml_ml_is_buffered" (fun _ _ -> int 1);
  reg2 "caml_ml_set_binary_mode" (fun _ _ _ -> unit);
  reg1 "caml_ml_is_binary_mode" (fun _ _ -> int 0);
  reg1 "caml_channel_descriptor" (fun _ ch ->
      match ch with
      | Channel { fd } -> int fd
      | _ -> error "channel expected");
  (* Standard input is empty *)
  reg4 "caml_ml_input" (fun _ _ _ _ _ -> int 0);
  reg4 "caml_ml_input_bigarray" (fun _ _ _ _ _ -> int 0);
  reg1 "caml_ml_input_char" (fun st _ -> raise_constant st "End_of_file");
  reg1 "caml_ml_input_int" (fun st _ -> raise_constant st "End_of_file");
  reg1 "caml_ml_input_scan_line" (fun _ _ -> int 0)

(****)

(* Compilation *)

let var_name x = Format.asprintf "%a" Var.print x

let iter_expr_vars f (e : expr) =
  match e with
  | Apply { f = g; args; _ } ->
      f g;
      List.iter args ~f
  | Block (_, a, _, _, _) -> Array.iter a ~f
  | Field (x, _, _) -> f x
  | Closure _ | Constant _ | Special _ -> ()
  | Prim (_, args) ->
      List.iter args ~f:(fun a ->
          match a with
          | Pv x -> f x
          | Pc _ -> ())

let iter_block_vars f (b : Code.block) =
  List.iter b.body ~f:(fun i ->
      match i with
      | Let (_, e) -> iter_expr_vars f e
      | Assign (x, y) ->
          f x;
          f y
      | Set_field (x, _, _, y) ->
          f x;
          f y
      | Offset_ref (x, _) -> f x
      | Array_set (x, y, z) ->
          f x;
          f y;
          f z
      | Event _ -> ());
  let cont (_, args) = List.iter args ~f in
  match b.branch with
  | Return x | Raise (x, _) -> f x
  | Stop -> ()
  | Branch c | Poptrap c -> cont c
  | Cond (x, c1, c2) ->
      f x;
      cont c1;
      cont c2
  | Switch (x, a) ->
      f x;
      Array.iter a ~f:cont
  | Pushtrap (c1, _, c2) ->
      cont c1;
      cont c2

type func_info =
  { entry : cont
  ; fparams : Var.t list
  ; fblocks : Addr.t list
  ; mutable locals : Var.t list  (** In reverse order of slots *)
  ; mutable nlocals : int
  ; mutable closures : int list  (** Functions defined in this function *)
  ; mutable free : Var.t array option
  ; free_index : int Var.Hashtbl.t
  }

let compile st (p : program) =
  let trampolined x =
    match !trampolined_calls with
    | Some s -> Var.Set.mem x s
    | None -> false
  in
  let nvars = Var.count () in
  let owner = Array.make nvars (-1) in
  let slot = Array.make nvars (-1) in
  let block_owner = Addr.Hashtbl.create 1024 in
  let func_of_entry = Addr.Hashtbl.create 128 in
  let funcs = ref [] in
  let nfuncs = ref 0 in
  let add_function entry fparams =
    let pc, _ = entry in
    match Addr.Hashtbl.find_opt func_of_entry pc with
    | Some id -> id
    | None ->
        let id = !nfuncs in
        incr nfuncs;
        Addr.Hashtbl.add func_of_entry pc id;
        let fblocks =
          Code.traverse
            { fold = Code.fold_children }
            (fun pc acc ->
              (match Addr.Hashtbl.find_opt block_owner pc with
              | Some id' when id' <> id -> error "block %d belongs to two functions" pc
              | _ -> Addr.Hashtbl.replace block_owner pc id);
              pc :: acc)
            pc
            p.blocks
            []
        in
        funcs :=
          { entry
          ; fparams
          ; fblocks
          ; locals = []
          ; nlocals = 0
          ; closures = []
          ; free = None
          ; free_index = Var.Hashtbl.create 16
          }
          :: !funcs;
        id
  in
  let start = add_function (p.start, []) [] in
  Addr.Map.iter
    (fun _ (b : Code.block) ->
      List.iter b.body ~f:(fun i ->
          match i with
          | Let (_, Closure (params, cont, _)) -> ignore (add_function cont params)
          | _ -> ()))
    p.blocks;
  let funcs = Array.of_list (List.rev !funcs) in
  (* Local variables *)
  Array.iteri funcs ~f:(fun id f ->
      let define x =
        let i = Var.idx x in
        if owner.(i) = -1
        then (
          owner.(i) <- id;
          slot.(i) <- f.nlocals;
          f.nlocals <- f.nlocals + 1;
          f.locals <- x :: f.locals)
        else if owner.(i) <> id
        then error "variable %s is bound in two functions" (var_name x)
      in
      List.iter f.fparams ~f:define;
      List.iter f.fblocks ~f:(fun pc ->
          let b = Addr.Map.find pc p.blocks in
          List.iter b.params ~f:define;
          List.iter b.body ~f:(fun i ->
              match i with
              | Let (x, Closure (_, (pc', _), _)) ->
                  define x;
                  f.closures <- Addr.Hashtbl.find func_of_entry pc' :: f.closures
              | Let (x, _) -> define x
              | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ());
          match b.branch with
          | Pushtrap (_, x, _) -> define x
          | Return _ | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Poptrap _ -> ()));
  (* Free variables *)
  let rec free id =
    let f = funcs.(id) in
    match f.free with
    | Some a -> a
    | None ->
        let s = ref Var.Set.empty in
        let use x = if owner.(Var.idx x) <> id then s := Var.Set.add x !s in
        List.iter f.fblocks ~f:(fun pc -> iter_block_vars use (Addr.Map.find pc p.blocks));
        List.iter (snd f.entry) ~f:use;
        List.iter f.closures ~f:(fun id' -> Array.iter (free id') ~f:use);
        let a = Array.of_list (Var.Set.elements !s) in
        Array.iteri a ~f:(fun i x -> Var.Hashtbl.replace f.free_index x i);
        f.free <- Some a;
        a
  in
  Array.iteri funcs ~f:(fun id _ -> ignore (free id));
  let resolve id x =
    if owner.(Var.idx x) = id
    then slot.(Var.idx x)
    else -1 - Var.Hashtbl.find funcs.(id).free_index x
  in
  let local id x =
    if owner.(Var.idx x) <> id then error "assignment to a free variable %s" (var_name x);
    slot.(Var.idx x)
  in
  let jump id (pc, args) : V.jump =
    (match Addr.Hashtbl.find_opt block_owner pc with
    | Some id' when id' = id -> ()
    | _ -> error "jump to block %d of another function" pc);
    let b = Addr.Map.find pc p.blocks in
    if List.compare_lengths b.params args <> 0
    then error "wrong number of arguments for block %d" pc;
    { pc; args = Array.of_list (List.map args ~f:(resolve id)) }
  in
  let vfuncs =
    Array.mapi funcs ~f:(fun id f ->
        { V.params = Array.of_list (List.map f.fparams ~f:(resolve id))
        ; entry = jump id f.entry
        ; nlocals = f.nlocals
        ; local_names = Array.of_list (List.rev f.locals)
        ; free_names = Option.get f.free
        })
  in
  let code = Array.make p.free_pc None in
  Array.iteri funcs ~f:(fun id f ->
      let resolve = resolve id and local = local id and jump = jump id in
      let expr x (e : expr) : C.expr =
        match e with
        | Apply { f; args; exact } ->
            Apply
              { f = resolve f
              ; args = Array.of_list (List.map args ~f:resolve)
              ; exact
              ; trampolined = trampolined x
              ; in_cps = Var.Set.mem x !in_cps_calls
              }
        | Block (tag, a, _, _, _) -> Block (tag, Array.map a ~f:resolve)
        | Field (x, i, _) -> Field (resolve x, i)
        | Closure _ -> assert false
        | Constant ((Tuple _ | Float_array _ | Int64 _) as c) -> Alloc c
        | Constant c -> Value (constant c)
        | Prim (p, args) -> (
            let args =
              Array.of_list
                (List.map args ~f:(fun a : C.arg ->
                     match a with
                     | Pv x -> Arg_var (resolve x)
                     | Pc ((Tuple _ | Float_array _ | Int64 _) as c) -> Arg_alloc c
                     | Pc c -> Arg_value (constant c)))
            in
            match p with
            | Extern (name, _) ->
                let f =
                  match find_prim name with
                  | _, f -> f st
                  | exception Unsupported msg -> fun _ -> raise (Unsupported msg)
                in
                Extern (f, args)
            | _ -> Prim (p, args))
        | Special (Alias_prim name) -> Special name
      in
      let rec body l acc =
        match l with
        | [] -> List.rev acc
        | Let (_, Closure _) :: _ ->
            (* With double translation, the instructions [Let (x,
               caml_cps_closure (d, c))] pairing the two versions of a
               function are part of the group: other closures of the group
               can refer to [x] (see [Closure_conversion.iter_closures]) *)
            let rec collect l closures pairs =
              match l with
              | Let (x, Closure (_, (pc, _), _)) :: rem ->
                  let id' = Addr.Hashtbl.find func_of_entry pc in
                  collect
                    rem
                    ({ C.dst = local x
                     ; fn = vfuncs.(id')
                     ; captures = Array.map (free id') ~f:resolve
                     }
                    :: closures)
                    pairs
              | Let (x, Prim (Extern ("caml_cps_closure", _), [ Pv d; Pv c ])) :: rem
                when List.exists closures ~f:(fun (c : C.closure) -> c.dst = local d) ->
                  collect rem closures ((local x, local d, resolve c) :: pairs)
              | Event _ :: (Let (_, Closure _) :: _ as rem) -> collect rem closures pairs
              | _ -> l, closures, pairs
            in
            let rem, closures, pairs = collect l [] [] in
            body
              rem
              (C.Closures
                 (Array.of_list (List.rev closures), Array.of_list (List.rev pairs))
              :: acc)
        | i :: rem ->
            body
              rem
              (match i with
              | Let (x, e) -> C.Let (local x, expr x e) :: acc
              | Assign (x, y) -> Assign (local x, resolve y) :: acc
              | Set_field (x, i, _, y) -> Set_field (resolve x, i, resolve y) :: acc
              | Offset_ref (x, n) -> Offset_ref (resolve x, n) :: acc
              | Array_set (x, y, z) -> Array_set (resolve x, resolve y, resolve z) :: acc
              | Event _ -> acc)
      in
      List.iter f.fblocks ~f:(fun pc ->
          let b = Addr.Map.find pc p.blocks in
          let instrs, tail_call =
            match List.rev b.body, b.branch with
            | Let (x, Apply { f; args; exact }) :: rem, Return y
              when Var.equal x y
                   && (not (trampolined x))
                   && not (Var.Set.mem x !in_cps_calls) ->
                ( List.rev rem
                , Some
                    (local x, resolve f, Array.of_list (List.map args ~f:resolve), exact)
                )
            | _ -> b.body, None
          in
          let last : C.last =
            match b.branch with
            | Return x -> Return (resolve x)
            | Raise (x, _) -> Raise (resolve x)
            | Stop -> Stop
            | Branch c -> Branch (jump c)
            | Cond (x, c1, c2) -> Cond (resolve x, jump c1, jump c2)
            | Switch (x, a) -> Switch (resolve x, Array.map a ~f:jump)
            | Pushtrap (c1, x, c2) -> Pushtrap (jump c1, local x, jump c2)
            | Poptrap c -> Poptrap (jump c)
          in
          code.(pc) <-
            Some
              { C.params = Array.of_list (List.map b.params ~f:local)
              ; body = Array.of_list (body instrs [])
              ; tail_call
              ; last
              }));
  code, vfuncs.(start)

(****)

(* Execution *)

type frame =
  { locals : V.t array
  ; env : V.t array
  ; fn : V.func
  }

type flow =
  | Returned of V.t
  | Tail_call of V.t * V.t list * bool
  | Popped of V.jump

(* Initial value of variables, to detect unbound variables *)
let unset = V.Native_string "unset"

let unbound fr r =
  error
    "unbound variable %s"
    (var_name (if r >= 0 then fr.fn.local_names.(r) else fr.fn.free_names.(-1 - r)))

let get fr r =
  let v = if r >= 0 then fr.locals.(r) else fr.env.(-1 - r) in
  if phys_equal v unset then unbound fr r;
  v

let tick st =
  st.fuel <- st.fuel - 1;
  if st.fuel < 0 then raise Out_of_fuel

let is_function (v : V.t) =
  match v with
  | Closure _ | Partial _ | Prim_closure _ | Host _ | Block { fun_ = Some _; _ } -> true
  | _ -> false

(* ocamlc tests values that are either integers or blocks directly
   (for instance, lists). The truth value of floats and strings depend
   on their content in JavaScript, so we do not accept them. *)
let condition (v : V.t) =
  match v with
  | Int n -> not (Targetint.is_zero n)
  | Block _
  | Bytes _
  | Int64 _
  | Closure _
  | Partial _
  | Prim_closure _
  | Host _
  | Channel _
  | Js_array _
  | Trampoline _
  | Stack _ -> true
  | Float _ | String _ | Native_string _ | Null ->
      error "condition on a float, a string or null"

let prim p (args : V.t list) =
  match p, args with
  | Vectlength _, [ x ] -> int (Array.length (as_block x).fields)
  | Array_get _, [ x; i ] -> field x (to_int i)
  | Not, [ Int n ] when Targetint.is_zero n -> int 1
  | Not, [ Int n ] when Targetint.equal n Targetint.one -> int 0
  | Not, [ _ ] -> error "negation of a value which is not a boolean"
  | IsInt _, [ x ] -> (
      match x with
      | Int _ | Float _ -> int 1
      | _ -> int 0)
  | (Eq { int_only } | Neq { int_only }), [ x; y ] ->
      (match x, y with
      | Int _, Int _ -> ()
      | _ -> if int_only then error "integer comparison of non-integer values");
      let b = phys_equal_value x y in
      bool
        (match p with
        | Eq _ -> b
        | _ -> not b)
  | Lt, [ Int x; Int y ] -> bool (Targetint.( < ) x y)
  | Le, [ Int x; Int y ] -> bool (Targetint.( <= ) x y)
  | Ult, [ Int x; Int y ] -> bool (Targetint.unsigned_lt x y)
  | (Lt | Le), [ ((Int _ | Float _) as x); ((Int _ | Float _) as y) ] ->
      let x = as_float x and y = as_float y in
      bool
        (match p with
        | Lt -> Float.( < ) x y
        | _ -> Float.( <= ) x y)
  | (Lt | Le | Ult), [ _; _ ] -> error "integer comparison of non-integer values"
  | Extern _, _ -> (* Resolved when compiling the program *) assert false
  | (Vectlength _ | Array_get _ | Not | IsInt _ | Eq _ | Neq _ | Lt | Le | Ult), _ ->
      error "wrong number of arguments for a primitive"

let get_list fr a =
  let rec loop i acc = if i < 0 then acc else loop (i - 1) (get fr a.(i) :: acc) in
  loop (Array.length a - 1) []

let get_arg fr (a : C.arg) =
  match a with
  | Arg_var x -> get fr x
  | Arg_value v -> v
  | Arg_alloc c -> constant c

let get_args fr a =
  let rec loop i acc = if i < 0 then acc else loop (i - 1) (get_arg fr a.(i) :: acc) in
  loop (Array.length a - 1) []

let rec run_cont st fr ~in_try (j : V.jump) =
  let b =
    match st.code.(j.pc) with
    | Some b -> b
    | None -> error "unknown block %d" j.pc
  in
  (match j.args with
  | [||] -> ()
  | [| x |] -> fr.locals.(b.params.(0)) <- get fr x
  | [| x; y |] ->
      let v = get fr x in
      let v' = get fr y in
      fr.locals.(b.params.(0)) <- v;
      fr.locals.(b.params.(1)) <- v'
  | args ->
      let values = Array.map args ~f:(fun x -> get fr x) in
      for i = 0 to Array.length values - 1 do
        fr.locals.(b.params.(i)) <- values.(i)
      done);
  st.pc <- j.pc;
  tick st;
  let body = b.body in
  for i = 0 to Array.length body - 1 do
    exec_instr st fr body.(i)
  done;
  match b.tail_call with
  | Some (_, f, args, exact) when not in_try ->
      Tail_call (get fr f, get_list fr args, exact)
  | Some (_, f, args, exact) -> Returned (apply st (get fr f) (get_list fr args) ~exact)
  | None -> (
      match b.last with
      | Return x -> Returned (get fr x)
      | Raise x -> raise (Ml_exn (get fr x))
      | Stop -> Returned unit
      | Branch j -> run_cont st fr ~in_try j
      | Cond (x, j1, j2) ->
          run_cont st fr ~in_try (if condition (get fr x) then j1 else j2)
      | Switch (x, a) ->
          let i = to_int (get fr x) in
          if i < 0 || i >= Array.length a then error "switch index %d out of bounds" i;
          run_cont st fr ~in_try a.(i)
      | Pushtrap (body, x, handler) -> (
          match run_try st fr body with
          | `Flow (Popped j) -> run_cont st fr ~in_try j
          | `Flow ((Returned _ | Tail_call _) as flow) -> flow
          | `Exn v ->
              fr.locals.(x) <- v;
              run_cont st fr ~in_try handler)
      | Poptrap j ->
          if not in_try then error "Poptrap outside of an exception handler";
          Popped j)

and run_try st fr body =
  let depth = st.depth in
  try `Flow (run_cont st fr ~in_try:true body)
  with Ml_exn v ->
    st.depth <- depth;
    `Exn v

and exec_instr st fr (i : C.instr) =
  match i with
  | Let (x, e) -> fr.locals.(x) <- eval_expr st fr e
  | Closures (a, pairs) ->
      let closures =
        Array.map a ~f:(fun (c : C.closure) ->
            let env = Array.make (Array.length c.captures) unset in
            fr.locals.(c.dst) <- V.Closure { fn = c.fn; env; cps = None };
            env)
      in
      Array.iter pairs ~f:(fun (x, d, c) ->
          let f = fr.locals.(d) in
          set_cps f (get fr c);
          fr.locals.(x) <- f);
      Array.iteri a ~f:(fun i (c : C.closure) ->
          let env = closures.(i) in
          Array.iteri c.captures ~f:(fun j x -> env.(j) <- get fr x))
  | Assign (x, y) -> fr.locals.(x) <- get fr y
  | Set_field (x, i, y) -> set_field (get fr x) i (get fr y)
  | Offset_ref (x, n) ->
      let r = get fr x in
      set_field r 0 (V.Int (Targetint.add (as_int (field r 0)) (Targetint.of_int_exn n)))
  | Array_set (x, y, z) -> set_field (get fr x) (to_int (get fr y)) (get fr z)

and eval_expr st fr (e : C.expr) : V.t =
  match e with
  | Apply { f; args; exact; trampolined; in_cps } ->
      let f = get fr f in
      let args = get_list fr args in
      if trampolined && not (check_depth st)
      then trampoline_return ~direct:(not in_cps) f args
      else if in_cps && st.dt
      then apply_dt_cps st f args ~exact
      else apply st f args ~exact
  | Block (tag, a) -> block ~tag (Array.map a ~f:(fun x -> get fr x))
  | Field (x, i) -> field (get fr x) i
  | Value v -> v
  | Alloc c -> constant c
  | Extern (f, args) -> f (get_args fr args)
  | Prim (p, args) -> prim p (get_args fr args)
  | Special name -> Prim_closure name

and apply st (f : V.t) args ~exact =
  match f with
  | Closure c when List.length args = Array.length c.fn.params -> call st c args
  | Closure c -> apply_generic st f args ~exact ~arity:(Array.length c.fn.params)
  | Partial p -> apply_generic st f args ~exact ~arity:p.remaining
  | Prim_closure name -> apply_generic st f args ~exact ~arity:(fst (find_prim name))
  | Host { arity; _ } -> apply_generic st f args ~exact ~arity
  | Block { fun_ = Some f; _ } -> apply st f args ~exact
  | _ -> not_a_function f

(* Partial application, and application to too many arguments *)
and apply_generic st f args ~exact ~arity =
  let n = List.length args in
  if exact && n <> arity
  then error "exact call with %d arguments to a function of arity %d" n arity;
  if n = arity
  then apply_exact st f args
  else if st.cps
  then apply_cps st f args ~arity
  else if st.dt
  then call_gen_direct st f args
  else if n < arity
  then V.Partial { pf = f; pargs = args; remaining = arity - n }
  else
    let args, rem = split arity args in
    apply st (apply_exact st f args) rem ~exact:false

(* [caml_call_gen] once the program is in CPS: the last argument is the
   continuation *)
and apply_cps st f args ~arity =
  let n = List.length args in
  let k = List.nth args (n - 1) in
  if n > arity
  then
    let first, rest = split (arity - 1) args in
    let k' =
      host 1 (function
        | [ g ] ->
            if is_function g
            then apply st g rest ~exact:false
            else apply st k [ g ] ~exact:true
        | _ -> bad_arity "continuation")
    in
    apply_exact st f (first @ [ k' ])
  else
    let pargs, _ = split (n - 1) args in
    apply st k [ Partial { pf = f; pargs; remaining = arity - n + 1 } ] ~exact:true

(* Double translation: a call in CPS (see [Generate.apply_fun_raw]) *)
and apply_dt_cps st f args ~exact =
  let n = List.length args in
  match cps_of f with
  | Some c ->
      if exact || arity_of c = n
      then apply st c args ~exact:true
      else call_gen_cps st f args
  | None ->
      if exact || arity_of f = n - 1
      then
        let direct_args, k = split (n - 1) args in
        apply st (List.hd k) [ apply st f direct_args ~exact:true ] ~exact:true
      else call_gen_cps st f args

(* Double translation: [caml_call_gen_direct] and [caml_call_gen_cps] *)
and call_gen_direct st f args =
  let n = arity_of f in
  let m = List.length args in
  if m = n
  then apply st f args ~exact:true
  else if m > n
  then
    let first, rest = split n args in
    let g = apply st f first ~exact:true in
    if is_function g then call_gen_direct st g rest else g
  else
    let d = n - m in
    let g = host d (fun extra -> call_gen_direct st f (args @ extra)) in
    set_cps g (host (d + 1) (fun extra -> call_gen_cps st f (args @ extra)));
    g

and call_gen_cps st f args =
  let m = List.length args in
  match cps_of f with
  | None ->
      let n = arity_of f in
      if m - 1 > n
      then
        let first, rest = split n args in
        call_gen_cps st (apply st f first ~exact:true) rest
      else
        let direct_args, k = split (m - 1) args in
        apply st (List.hd k) [ call_gen_direct st f direct_args ] ~exact:true
  | Some c ->
      let n = arity_of c in
      if m = n
      then apply st c args ~exact:true
      else if m > n
      then
        let first, rest = split (n - 1) args in
        let k' =
          host 1 (function
            | [ g ] -> call_gen_cps st g rest
            | _ -> bad_arity "continuation")
        in
        apply st c (first @ [ k' ]) ~exact:true
      else
        let pargs, k = split (m - 1) args in
        let d = n - m in
        let g = host d (fun extra -> call_gen_direct st f (pargs @ extra)) in
        set_cps g (host (d + 1) (fun extra -> call_gen_cps st f (pargs @ extra)));
        apply st (List.hd k) [ g ] ~exact:true

and apply_exact st (f : V.t) args =
  match f with
  | Closure c -> call st c args
  | Partial p -> apply st p.pf (p.pargs @ args) ~exact:true
  | Prim_closure name -> (snd (find_prim name)) st args
  | Host { hfn; _ } -> hfn args
  | Block { fun_ = Some f; _ } -> apply_exact st f args
  | _ -> assert false

and call st c args =
  st.depth <- st.depth + 1;
  if st.depth > max_depth then unsupported "call depth";
  let rec loop (c : V.closure) args =
    let fn = c.fn in
    let fr = { locals = Array.make fn.nlocals unset; env = c.env; fn } in
    let rec bind i l =
      match l with
      | [] -> ()
      | v :: r ->
          fr.locals.(fn.params.(i)) <- v;
          bind (i + 1) r
    in
    bind 0 args;
    match run_cont st fr ~in_try:false fn.entry with
    | Returned v -> v
    | Tail_call (Closure c, args, _) when List.length args = Array.length c.fn.params ->
        loop c args
    | Tail_call (f, args, exact) -> apply st f args ~exact
    | Popped _ -> error "Poptrap outside of an exception handler"
  in
  let pc = st.pc in
  let v = loop c args in
  st.depth <- st.depth - 1;
  st.pc <- pc;
  v

let () =
  apply_ref := apply;
  call_gen_cps_ref := call_gen_cps

(****)

type result =
  | Finished
  | Exited of int
  | Uncaught of V.t
  | Failed of string * Addr.t  (** Error message, block where it occurred *)
  | Skipped of string

type outcome =
  { result : result
  ; stdout : string
  ; stderr : string
  ; steps : int
  }

let rec print_value ~depth f (v : V.t) =
  match v with
  | Int n -> Format.pp_print_string f (Targetint.to_string n)
  | Float x -> Format.fprintf f "%h" x
  | String s -> Format.fprintf f "%S" s
  | Native_string s -> Format.fprintf f "js%S" s
  | Bytes b -> Format.fprintf f "bytes%S" (Bytes.to_string b)
  | Int64 n -> Format.fprintf f "%LdL" n
  | Closure _ | Partial _ | Prim_closure _ | Host _ -> Format.pp_print_string f "<fun>"
  | Channel _ -> Format.pp_print_string f "<channel>"
  | Js_array _ -> Format.pp_print_string f "<js array>"
  | Trampoline _ -> Format.pp_print_string f "<trampoline>"
  | Stack _ -> Format.pp_print_string f "<stack>"
  | Null -> Format.pp_print_string f "null"
  | Block { tag = 248; fields; _ } when Array.length fields > 0 ->
      (* Exception constructors are printed without their identifier,
         which depends on the number of exceptions defined before *)
      print_value ~depth f fields.(0)
  | Block { tag; fields; _ } ->
      if depth = 0
      then Format.pp_print_string f "..."
      else
        Format.fprintf
          f
          "@[<hov 1><%d>(%a)@]"
          tag
          (Format.pp_print_list
             ~pp_sep:(fun f () -> Format.fprintf f ",@ ")
             (print_value ~depth:(depth - 1)))
          (Array.to_list fields)

let print_result f r =
  match r with
  | Finished -> Format.pp_print_string f "finished"
  | Exited n -> Format.fprintf f "exited with code %d" n
  | Uncaught v -> Format.fprintf f "uncaught exception %a" (print_value ~depth:3) v
  | Failed (s, pc) -> Format.fprintf f "error: %s (block %d)" s pc
  | Skipped s -> Format.fprintf f "not supported: %s" s

let result_to_string r = Format.asprintf "%a" print_result r

let is_effects e =
  match e, Config.effects () with
  | `Cps, `Cps | `Double_translation, `Double_translation -> true
  | ( (`Cps | `Double_translation)
    , (`Cps | `Double_translation | `Disabled | `Jspi | `Native) ) -> false

let run ?fuel (p : program) =
  let fuel =
    match fuel with
    | Some n -> n
    | None -> Config.Param.interp_fuel ()
  in
  let st =
    { code = [||]
    ; fuel
    ; depth = 0
    ; stdout = Buffer.create 256
    ; stderr = Buffer.create 256
    ; named_values = String.Hashtbl.create 16
    ; globals = String.Hashtbl.create 16
    ; oo_id = 0
    ; cache_id = 0
    ; out_channels = []
    ; dls = block [||]
    ; pc = p.start
    ; cps = Option.is_some !trampolined_calls && is_effects `Cps
    ; dt = Option.is_some !trampolined_calls && is_effects `Double_translation
    ; stack_depth = 0
    ; current = toplevel_fiber ()
    }
  in
  let result =
    match
      let code, fn = compile st p in
      st.code <- code;
      let fr =
        { locals = Array.make fn.nlocals unset
        ; env = Array.make (Array.length fn.free_names) unset
        ; fn
        }
      in
      match run_cont st fr ~in_try:false fn.entry with
      | Returned _ -> ()
      | Tail_call (f, args, exact) -> ignore (apply st f args ~exact)
      | Popped _ -> error "Poptrap outside of an exception handler"
    with
    | () -> Finished
    | exception Ml_exn v -> Uncaught v
    | exception Exit_program n -> Exited n
    | exception Error s -> Failed (s, st.pc)
    | exception Unsupported s -> Skipped s
    | exception Out_of_fuel -> Skipped "out of fuel"
    | exception ((Invalid_argument _ | Not_found | Failure _ | Division_by_zero) as e) ->
        Skipped (Printf.sprintf "internal error %s" (Printexc.to_string e))
    | exception Stack_overflow ->
        Skipped (Printf.sprintf "stack overflow (call depth %d)" st.depth)
  in
  { result
  ; stdout = Buffer.contents st.stdout
  ; stderr = Buffer.contents st.stderr
  ; steps = fuel - st.fuel
  }

let same_result r r' =
  match r, r' with
  | Failed (s, _), Failed (s', _) -> String.equal s s'
  | _ -> String.equal (result_to_string r) (result_to_string r')

let same_outcome o o' =
  same_result o.result o'.result
  && String.equal o.stdout o'.stdout
  && String.equal o.stderr o'.stderr

let print_outcome f o =
  Format.fprintf
    f
    "%a (%d steps, %d bytes of output)"
    print_result
    o.result
    o.steps
    (String.length o.stdout + String.length o.stderr)

(****)

let reference = ref None

let start p =
  reference := None;
  trampolined_calls := None;
  in_cps_calls := Var.Set.empty;
  if debug ()
  then
    match Config.target () with
    | `Wasm -> Format.eprintf "interp: the Wasm target is not supported@."
    | `JavaScript -> (
        let o = run p in
        Format.eprintf "interp: input: %a@." print_outcome o;
        if debug_output () then print_string o.stdout;
        match o.result with
        | Skipped _ -> ()
        | Finished | Exited _ | Uncaught _ | Failed _ -> reference := Some o)

let first_difference s s' =
  let n = min (String.length s) (String.length s') in
  let rec loop i = if i < n && Char.equal s.[i] s'.[i] then loop (i + 1) else i in
  loop 0

let check ~name p =
  match !reference with
  | Some r when debug () -> (
      let o = run p in
      match o.result with
      | Skipped _ -> Format.eprintf "interp: %s: %a@." name print_outcome o
      | Finished | Exited _ | Uncaught _ | Failed _ ->
          if same_outcome r o
          then Format.eprintf "interp: %s: ok (%d steps)@." name o.steps
          else
            let output name s s' =
              if not (String.equal s s')
              then
                let i = first_difference s s' in
                let excerpt s =
                  let pos = max 0 (i - 20) in
                  String.sub s ~pos ~len:(min (String.length s - pos) 60)
                in
                Format.eprintf
                  "  %s differs at byte %d:@.    before: %S@.    after:  %S@."
                  name
                  i
                  (excerpt s)
                  (excerpt s')
            in
            if debug_program ()
            then Code.Print.program Format.err_formatter (fun _ _ -> "") p;
            Format.eprintf "interp: %s: the behavior of the program changed@." name;
            (match o.result with
            | Failed (_, pc) -> (
                match Addr.Map.find_opt pc p.blocks with
                | Some b ->
                    Format.eprintf "  in block:@.";
                    Code.Print.block Format.err_formatter (fun _ _ -> "") pc b
                | None -> ())
            | Finished | Exited _ | Uncaught _ | Skipped _ -> ());
            Format.eprintf "  before: %a@." print_outcome r;
            Format.eprintf "  after:  %a@." print_outcome o;
            output "standard output" r.stdout o.stdout;
            output "standard error" r.stderr o.stderr;
            failwith (Printf.sprintf "IR interpreter: pass %s changed the behavior" name))
  | Some _ | None -> ()

let set_cps ~trampolined_calls:s ~in_cps =
  trampolined_calls := Some s;
  in_cps_calls := in_cps
