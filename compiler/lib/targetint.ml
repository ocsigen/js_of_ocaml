type t = Int32.t

let num_bits_ = ref 0

let set_num_bits x = num_bits_ := x

let num_bits () =
  match !num_bits_ with
  | (31 | 32) as x -> x
  | x -> failwith (Printf.sprintf "Targetint.num_bits %d unsupported" x)

type offset = Offset of int [@@ocaml.unboxed]

let () = assert (Obj.is_int (Obj.repr (Offset 0)))

let offset () = Offset (32 - num_bits ())

let equal = Int32.equal

let compare = Int32.compare

let wrap (Offset offset) i = Int32.(shift_left i offset)

let unwrap (Offset offset) i = Int32.(shift_right i offset)

let wrap_modulo i =
  let offset = offset () in
  unwrap offset (wrap offset i)

let max_int_ (Offset offset) = Int32.shift_right Int32.max_int offset

let min_int_ (Offset offset) = Int32.shift_right Int32.min_int offset

let min_int () =
  let offset = offset () in
  min_int_ offset

let max_int () =
  let offset = offset () in
  max_int_ offset

let to_string x = Int32.to_string x

let to_float x = Int32.to_float x

let to_int32 x = x

let to_int_exn x =
  if Sys.int_size >= 32 || Int32.of_int Int.min_int <= x || x <= Int32.of_int Int.max_int
  then Int32.to_int x
  else failwith "to_int_exn"

let neg x =
  let offset = offset () in
  unwrap offset (Int32.neg (wrap offset x))

let abs x =
  let offset = offset () in
  unwrap offset (Int32.abs (wrap offset x))

let int_binop f x y = wrap_modulo (f x y)

let add = int_binop Int32.add

let zero = 0l

let one = 1l

let succ x = add x one

let sub = int_binop Int32.sub

let mul = int_binop Int32.mul

let div = int_binop Int32.div

let rem = int_binop Int32.rem

let logand = int_binop Int32.logand

let logor = int_binop Int32.logor

let logxor = int_binop Int32.logxor

let shift_op f x y =
  let offset = offset () in
  (* Limit the shift offset to [0, 31], this works for both 31 and 32
     bit integers *)
  unwrap offset (f (wrap offset x) (y land 0x1f))

let shift_left = shift_op Int32.shift_left

let shift_right = shift_op Int32.shift_right

let shift_right_logical = shift_op Int32.shift_right_logical

let is_zero x = equal x 0l

let of_int_exn (x : int) =
  let offset = offset () in
  if
    Sys.int_size <= 32
    || (Int32.to_int (min_int_ offset) <= x && x <= Int32.to_int (max_int_ offset))
  then Int32.of_int x
  else failwith (Printf.sprintf "of_int_exn(%d)" x)

let of_int32_exn (x : int32) =
  let offset = offset () in
  if min_int_ offset <= x && x <= max_int_ offset then x else failwith "of_int32_exn"

let of_string_exn x =
  try
    let offset = offset () in
    let x32 = Int32.of_string x in
    if min_int_ offset <= x32 || x32 <= max_int_ offset then x32 else raise Not_found
  with Not_found | _ -> failwith (Printf.sprintf "Targetint.of_string_exn(%s)" x)

let of_float_opt x =
  let offset = offset () in
  if Int32.to_float (min_int_ offset) <= x || x <= Int32.to_float (max_int_ offset)
  then Some (wrap_modulo (Int32.of_float x))
  else None

let of_int_warning_on_overflow i =
  Stdlib.Int32.convert_warning_on_overflow
    "integer"
    ~to_int32:(fun i -> wrap_modulo (Int32.of_int i))
    ~of_int32:Int32.to_int
    ~equal:Int.equal
    ~to_dec:(Printf.sprintf "%d")
    ~to_hex:(Printf.sprintf "%x")
    i

let of_int32_warning_on_overflow n =
  Stdlib.Int32.convert_warning_on_overflow
    "int32"
    ~to_int32:(fun i -> wrap_modulo i)
    ~of_int32:Fun.id
    ~equal:Int32.equal
    ~to_dec:(Printf.sprintf "%ld")
    ~to_hex:(Printf.sprintf "%lx")
    n

let of_nativeint_warning_on_overflow n =
  Stdlib.Int32.convert_warning_on_overflow
    "native integer"
    ~to_int32:(fun i -> wrap_modulo (Nativeint.to_int32 i))
    ~of_int32:Nativeint.of_int32
    ~equal:Nativeint.equal
    ~to_dec:(Printf.sprintf "%nd")
    ~to_hex:(Printf.sprintf "%nx")
    n

external ( < ) : int32 -> int32 -> bool = "%lessthan"

external ( <= ) : int32 -> int32 -> bool = "%lessequal"

external ( <> ) : int32 -> int32 -> bool = "%notequal"

external ( = ) : int32 -> int32 -> bool = "%equal"

external ( > ) : int32 -> int32 -> bool = "%greaterthan"

external ( >= ) : int32 -> int32 -> bool = "%greaterequal"
