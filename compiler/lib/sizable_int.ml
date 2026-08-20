module type S = sig
  type t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val is_zero : t -> bool

  (* to *)

  val to_string : t -> string

  val to_int_exn : t -> int

  val to_float : t -> float

  val to_int32 : t -> int32

  val to_int64 : t -> int64

  (* of *)

  val of_string_exn : string -> t

  val of_int_exn : int -> t

  val of_int32_exn : int32 -> t

  val of_int64_exn : int64 -> t

  val of_int32_truncate : int32 -> t

  val of_int64_truncate : int64 -> t

  val of_int32_warning_on_overflow : int32 -> t

  val of_int64_warning_on_overflow : int64 -> t

  val of_nativeint_warning_on_overflow : nativeint -> t

  val of_int_warning_on_overflow : int -> t

  val of_float_opt : float -> t option

  (* arithmetic *)

  val succ : t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val rem : t -> t -> t

  val logand : t -> t -> t

  val logor : t -> t -> t

  val logxor : t -> t -> t

  val shift_left : t -> int -> t

  val shift_right : t -> int -> t

  val shift_right_logical : t -> int -> t

  val neg : t -> t

  val abs : t -> t

  (* constant *)

  val min_int : unit -> t

  val max_int : unit -> t

  val zero : t

  val one : t

  (* num bits *)

  val num_bits : unit -> int

  val set_num_bits : int -> unit

  (* comparison *)

  val ( >= ) : t -> t -> bool

  val ( <= ) : t -> t -> bool

  val ( < ) : t -> t -> bool

  val ( > ) : t -> t -> bool

  val ( = ) : t -> t -> bool

  val ( <> ) : t -> t -> bool

  val unsigned_lt : t -> t -> bool

  val bits_of_float : float -> t

  val float_of_bits : t -> float

  val of_float : float -> t
end

module type Arg = sig
  val name : string

  val allowed_sizes : int array
end

module Make (Arg : Arg) : S = struct
  type t = Int64.t

  let num_bits_ = ref 0

  let set_num_bits x =
    if Array.exists (( = ) x) Arg.allowed_sizes
    then num_bits_ := x
    else failwith (Printf.sprintf "%s.num_bits %d unsupported" Arg.name x)

  let num_bits () = !num_bits_

  type offset = Offset of int [@@ocaml.unboxed]

  let () = assert (Obj.is_int (Obj.repr (Offset 0)))

  let offset () = Offset (64 - num_bits ())

  let equal = Int64.equal

  let compare = Int64.compare

  let wrap (Offset offset) i = Int64.(shift_left i offset)

  let unwrap (Offset offset) i = Int64.(shift_right i offset)

  let wrap_modulo i =
    let offset = offset () in
    unwrap offset (wrap offset i)

  let max_int_ (Offset offset) = Int64.shift_right Int64.max_int offset

  let min_int_ (Offset offset) = Int64.shift_right Int64.min_int offset

  let min_int () =
    let offset = offset () in
    min_int_ offset

  let max_int () =
    let offset = offset () in
    max_int_ offset

  let to_string x = Int64.to_string x

  let to_float x = Int64.to_float x

  let to_int32 x = Int64.to_int32 x

  let to_int64 x = x

  let to_int_exn x =
    if
      Sys.int_size >= 32
      || (Int64.of_int Int.min_int <= x && x <= Int64.of_int Int.max_int)
    then Int64.to_int x
    else failwith "to_int_exn"

  let neg x =
    let offset = offset () in
    unwrap offset (Int64.neg (wrap offset x))

  let abs x =
    let offset = offset () in
    unwrap offset (Int64.abs (wrap offset x))

  let int_binop f x y = wrap_modulo (f x y)

  let add = int_binop Int64.add

  let zero = 0L

  let one = 1L

  let succ x = add x one

  let sub = int_binop Int64.sub

  let mul = int_binop Int64.mul

  let div = int_binop Int64.div

  let rem = int_binop Int64.rem

  let logand = int_binop Int64.logand

  let logor = int_binop Int64.logor

  let logxor = int_binop Int64.logxor

  let shift_op f x y =
    let offset = offset () in
    let mask =
      if
        num_bits () > 32
        (* Limit the shift offset to [0, 63], this works for both 63 and 64
        bit integers *)
      then 0x3f
      (* Limit the shift offset to [0, 31], this works for both 31 and 32
        bit integers *)
      else 0x1f
    in
    unwrap offset (f (wrap offset x) (y land mask))

  let shift_left = shift_op Int64.shift_left

  let shift_right = shift_op Int64.shift_right

  let shift_right_logical = shift_op Int64.shift_right_logical

  let is_zero x = equal x 0L

  let of_int_exn (x : int) =
    let offset = offset () in
    if
      Sys.int_size <= num_bits ()
      || (Int64.to_int (min_int_ offset) <= x && x <= Int64.to_int (max_int_ offset))
    then Int64.of_int x
    else failwith (Printf.sprintf "of_int_exn(%d)" x)

  let of_int32_exn (x : int32) =
    let x = Int64.of_int32 x in
    let offset = offset () in
    if min_int_ offset <= x && x <= max_int_ offset then x else failwith "of_int32_exn"

  let of_int64_exn (x : int64) =
    let offset = offset () in
    if min_int_ offset <= x && x <= max_int_ offset then x else failwith "of_int64_exn"

  let of_int32_truncate x = wrap_modulo (Int64.of_int32 x)

  let of_int64_truncate x = wrap_modulo x

  let of_string_exn x =
    try
      let offset = offset () in
      let x32 = Int64.of_string x in
      if min_int_ offset <= x32 && x32 <= max_int_ offset then x32 else raise Not_found
    with Not_found | _ -> failwith (Printf.sprintf "%s.of_string_exn(%s)" Arg.name x)

  let of_float_opt x =
    let offset = offset () in
    if Int64.to_float (min_int_ offset) <= x && x <= Int64.to_float (max_int_ offset)
    then Some (wrap_modulo (Int64.of_float x))
    else None

  (* The truncated value is printed with the width of its representation
     on the target *)
  let to_hex_truncated to_int64 x =
    let i = to_int64 x in
    if num_bits () <= 32
    then Printf.sprintf "%lx" (Int64.to_int32 i)
    else Printf.sprintf "%Lx" i

  let of_int_warning_on_overflow i =
    Stdlib.Int64.convert_warning_on_overflow
      "integer"
      ~to_int64:(fun i -> wrap_modulo (Int64.of_int i))
      ~of_int64:Int64.to_int
      ~equal:Int.equal
      ~to_dec:(Printf.sprintf "%d")
      ~to_hex:(Printf.sprintf "%x")
      ~to_hex_truncated:(to_hex_truncated Int64.of_int)
      i

  let of_int32_warning_on_overflow n =
    Stdlib.Int64.convert_warning_on_overflow
      "int32"
      ~to_int64:(fun i -> wrap_modulo (Int64.of_int32 i))
      ~of_int64:Int64.to_int32
      ~equal:Int32.equal
      ~to_dec:(Printf.sprintf "%ld")
      ~to_hex:(Printf.sprintf "%lx")
      ~to_hex_truncated:(to_hex_truncated Int64.of_int32)
      n

  let of_int64_warning_on_overflow n =
    Stdlib.Int64.convert_warning_on_overflow
      "int64"
      ~to_int64:(fun i -> wrap_modulo i)
      ~of_int64:Fun.id
      ~equal:Int64.equal
      ~to_dec:(Printf.sprintf "%Ld")
      ~to_hex:(Printf.sprintf "%Lx")
      ~to_hex_truncated:(to_hex_truncated Fun.id)
      n

  let of_nativeint_warning_on_overflow n =
    Stdlib.Int64.convert_warning_on_overflow
      "native integer"
      ~to_int64:(fun i -> wrap_modulo (Int64.of_nativeint i))
      ~of_int64:Int64.to_nativeint
      ~equal:Nativeint.equal
      ~to_dec:(Printf.sprintf "%nd")
      ~to_hex:(Printf.sprintf "%nx")
      ~to_hex_truncated:(to_hex_truncated Int64.of_nativeint)
      n

  external ( < ) : int64 -> int64 -> bool = "%lessthan"

  external ( <= ) : int64 -> int64 -> bool = "%lessequal"

  external ( <> ) : int64 -> int64 -> bool = "%notequal"

  external ( = ) : int64 -> int64 -> bool = "%equal"

  external ( > ) : int64 -> int64 -> bool = "%greaterthan"

  external ( >= ) : int64 -> int64 -> bool = "%greaterequal"

  let unsigned_lt n m = Int64.(sub n min_int < sub m min_int)

  let bits_of_float f =
    if Stdlib.( > ) (num_bits ()) 32
    then Int64.bits_of_float f
    else Int64.of_int32 (Int32.bits_of_float f)

  let float_of_bits x =
    if Stdlib.( > ) (num_bits ()) 32
    then Int64.float_of_bits x
    else Int32.float_of_bits (Int64.to_int32 x)

  let of_float f =
    if Stdlib.( > ) (num_bits ()) 32
    then Int64.of_float f
    else Int64.of_int32 (Int32.of_float f)
end
