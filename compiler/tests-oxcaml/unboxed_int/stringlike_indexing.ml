(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 include stdlib_stable;
 {
   native;
 }{
   flags = "-O3";
   native;
 }{
   bytecode;
 }{
   flags = "-extension layouts_alpha";
   native;
 }{
   flags = "-extension layouts_alpha -O3";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta -O3";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

let lengths = List.init 17 (fun x -> x) @ List.init 17 (fun x -> 300 + x)

type exn += Test_failed

let create_s length =
  String.init length (fun i -> i * 7 mod 256 |> char_of_int)
;;

let create_b length = create_s length |> Bytes.of_string

open struct
  open Bigarray

  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  let bigstring_of_string s =
    let a = Array1.create char c_layout (String.length s) in
    for i = 0 to String.length s - 1 do
      a.{i} <- s.[i]
    done;
    a

  let create_bs length = create_s length |> bigstring_of_string
end

module Tester (Primitives : sig
    type boxed_index
    type boxed_data
    type container

    val create : int -> container
    val generate_data : int -> boxed_data
    val to_index : int -> boxed_index
    val data_equal : boxed_data -> boxed_data -> bool

    type 'a getter := container -> 'a -> boxed_data
    type 'a setter := container -> 'a -> boxed_data -> unit

    val get_reference : int getter
    val get_safe : boxed_index getter
    val get_unsafe : boxed_index getter
    val set_reference : int setter
    val set_safe : boxed_index setter
    val set_unsafe : boxed_index setter
    val extra_bounds_checks : boxed_index list
  end) : sig end = struct
  open Primitives

  let make_tester_functions length =
    let for_reference = create length
    and for_safe = create length
    and for_unsafe = create length in
    let check_get_bounds i =
      try
        let _ = get_safe for_safe i in
        assert false
      with
      | Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      try
        let _ = set_safe for_safe i x in
        assert false
      with
      | Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = to_index i in
      try
        let res = get_reference for_reference i in
        try
          assert (data_equal res (get_safe for_safe test_i));
          assert (data_equal res (get_unsafe for_unsafe test_i))
        with
        | _ -> raise Test_failed
      with
      | Test_failed -> assert false
      | Invalid_argument _ -> check_get_bounds test_i
      | _ ->
        (try
           let _ = get_safe for_safe test_i in
           assert false
         with
         | Invalid_argument _ -> assert false
         | _ -> ())
    in
    let check_set i x =
      let test_i = to_index i in
      try
        set_reference for_reference i x;
        try
          set_safe for_safe test_i x;
          assert (data_equal x (get_reference for_safe i));
          set_unsafe for_unsafe test_i x;
          assert (data_equal x (get_reference for_unsafe i));
          (* Check that we didn't ruin adjacent indices *)
          check_get (i - 1);
          check_get (i + 1)
        with
        | _ -> raise Test_failed
      with
      | Test_failed -> assert false
      | Invalid_argument _ -> check_set_bounds test_i x
      | _ ->
        (try
           set_safe for_safe test_i x;
           assert false
         with
         | Invalid_argument _ -> assert false
         | _ -> ())
    in
    check_get_bounds, check_get, check_set_bounds, check_set
  ;;

  let test length =
    Random.init 1234;
    let check_get_bounds, check_get, check_set_bounds, check_set =
      make_tester_functions length
    in
    for i = -1 to length + 1 do
      check_get i;
      check_set i (generate_data i)
    done;
    List.iter
      (fun bound ->
        check_get_bounds bound;
        check_set_bounds bound (generate_data 1))
      extra_bounds_checks
  ;;

  let () = List.iter test lengths
end

module Tester_no_set (Primitives : sig
    type boxed_index
    type boxed_data
    type container

    val create : int -> container
    val generate_data : int -> boxed_data
    val to_index : int -> boxed_index
    val data_equal : boxed_data -> boxed_data -> bool

    type 'a getter := container -> 'a -> boxed_data

    val get_reference : int getter
    val get_safe : boxed_index getter
    val get_unsafe : boxed_index getter
    val extra_bounds_checks : boxed_index list
  end) : sig end = struct
  open Primitives

  let make_tester_functions length =
    let for_reference = create length
    and for_safe = create length
    and for_unsafe = create length in
    let check_get_bounds i =
      try
        let _ = get_safe for_safe i in
        assert false
      with
      | Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = to_index i in
      try
        let res = get_reference for_reference i in
        try
          assert (data_equal res (get_safe for_safe test_i));
          assert (data_equal res (get_unsafe for_unsafe test_i))
        with
        | _ -> raise Test_failed
      with
      | Test_failed -> assert false
      | Invalid_argument _ -> check_get_bounds test_i
      | _ ->
        (try
           let _ = get_safe for_safe test_i in
           assert false
         with
         | Invalid_argument _ -> assert false
         | _ -> ())
    in
    check_get_bounds, check_get
  ;;

  let test length =
    Random.init 1234;
    let check_get_bounds, check_get = make_tester_functions length in
    for i = -1 to length + 1 do
      check_get i;
    done;
    List.iter (fun bound -> check_get_bounds bound) extra_bounds_checks
  ;;

  let () = List.iter test lengths
end


open struct

  type boxed_index = nativeint
  type boxed_data = int

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int.zero
      | 1 -> (Int.(shift_left one) (16 - 1))
      | 2 -> 
        (let shift = 16 - 1 in
        Int.(lognot (shift_left (shift_right (lognot zero) shift) shift)))
      | _ -> Random.int_in_range ~min:Int.zero ~max:(Int.(shift_left one) (16 - 1))


  let to_index = Nativeint.of_int
  let data_equal = Int.equal
  let unbox_index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int
        = "%caml_string_get16"

      external get_safe
        :  string
        -> nativeint#
        -> int
        = "%caml_string_get16_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> int
        = "%caml_string_get16u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int
        = "%caml_bytes_get16"

      external get_safe
        :  bytes
        -> nativeint#
        -> int
        = "%caml_bytes_get16_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> int
        = "%caml_bytes_get16u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int
        -> unit
        = "%caml_bytes_set16"

      external set_safe
        :  bytes
        -> nativeint#
        -> int
        -> unit
        = "%caml_bytes_set16_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> int
        -> unit
        = "%caml_bytes_set16u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int
        = "%caml_bigstring_get16"

      external get_safe
        :  bigstring
        -> nativeint#
        -> int
        = "%caml_bigstring_get16_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> int
        = "%caml_bigstring_get16u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int
        -> unit
        = "%caml_bigstring_set16"

      external set_safe
        :  bigstring
        -> nativeint#
        -> int
        -> unit
        = "%caml_bigstring_set16_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> int
        -> unit
        = "%caml_bigstring_set16u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int32_in_range ~min:Int32.min_int ~max:Int32.max_int


  let to_index = Nativeint.of_int
  let data_equal = Int32.equal
  let unbox_index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> nativeint#
        -> int32
        = "%caml_string_get32_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> int32
        = "%caml_string_get32u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> nativeint#
        -> int32
        = "%caml_bytes_get32_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> int32
        = "%caml_bytes_get32u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> nativeint#
        -> int32
        -> unit
        = "%caml_bytes_set32_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> int32
        -> unit
        = "%caml_bytes_set32u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> nativeint#
        -> int32
        = "%caml_bigstring_get32_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> int32
        = "%caml_bigstring_get32u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> nativeint#
        -> int32
        -> unit
        = "%caml_bigstring_set32_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> int32
        -> unit
        = "%caml_bigstring_set32u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range ~min:Int64.min_int ~max:Int64.max_int


  let to_index = Nativeint.of_int
  let data_equal = Int64.equal
  let unbox_index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> nativeint#
        -> int64
        = "%caml_string_get64_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> int64
        = "%caml_string_get64u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> nativeint#
        -> int64
        = "%caml_bytes_get64_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> int64
        = "%caml_bytes_get64u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> nativeint#
        -> int64
        -> unit
        = "%caml_bytes_set64_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> int64
        -> unit
        = "%caml_bytes_set64u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> nativeint#
        -> int64
        = "%caml_bigstring_get64_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> int64
        = "%caml_bigstring_get64u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> nativeint#
        -> int64
        -> unit
        = "%caml_bigstring_set64_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> int64
        -> unit
        = "%caml_bigstring_set64u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int32_in_range ~min:Int32.min_int ~max:Int32.max_int


  let to_index = Nativeint.of_int
  let data_equal = Int32.equal
  let unbox_index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint
  let unbox_data = Stdlib_upstream_compatible.Int32_u.of_int32
  let box_data = Stdlib_upstream_compatible.Int32_u.to_int32
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> nativeint#
        -> int32#
        = "%caml_string_get32#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> int32#
        = "%caml_string_get32u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> nativeint#
        -> int32#
        = "%caml_bytes_get32#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> int32#
        = "%caml_bytes_get32u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> nativeint#
        -> int32#
        -> unit
        = "%caml_bytes_set32#_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> int32#
        -> unit
        = "%caml_bytes_set32u#_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> nativeint#
        -> int32#
        = "%caml_bigstring_get32#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> int32#
        = "%caml_bigstring_get32u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> nativeint#
        -> int32#
        -> unit
        = "%caml_bigstring_set32#_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> int32#
        -> unit
        = "%caml_bigstring_set32u#_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range ~min:Int64.min_int ~max:Int64.max_int


  let to_index = Nativeint.of_int
  let data_equal = Int64.equal
  let unbox_index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint
  let unbox_data = Stdlib_upstream_compatible.Int64_u.of_int64
  let box_data = Stdlib_upstream_compatible.Int64_u.to_int64
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> nativeint#
        -> int64#
        = "%caml_string_get64#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> int64#
        = "%caml_string_get64u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> nativeint#
        -> int64#
        = "%caml_bytes_get64#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> int64#
        = "%caml_bytes_get64u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> nativeint#
        -> int64#
        -> unit
        = "%caml_bytes_set64#_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> int64#
        -> unit
        = "%caml_bytes_set64u#_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> nativeint#
        -> int64#
        = "%caml_bigstring_get64#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> int64#
        = "%caml_bigstring_get64u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> nativeint#
        -> int64#
        -> unit
        = "%caml_bigstring_set64#_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> int64#
        -> unit
        = "%caml_bigstring_set64u#_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Stdlib_stable.Float32.of_float f

  let to_index = Nativeint.of_int
  let data_equal = Stdlib_stable.Float32.equal
  let unbox_index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> nativeint#
        -> float32
        = "%caml_string_getf32_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> float32
        = "%caml_string_getf32u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> nativeint#
        -> float32
        = "%caml_bytes_getf32_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> float32
        = "%caml_bytes_getf32u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> nativeint#
        -> float32
        -> unit
        = "%caml_bytes_setf32_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> float32
        -> unit
        = "%caml_bytes_setf32u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> nativeint#
        -> float32
        = "%caml_bigstring_getf32_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> float32
        = "%caml_bigstring_getf32u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> nativeint#
        -> float32
        -> unit
        = "%caml_bigstring_setf32_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> float32
        -> unit
        = "%caml_bigstring_setf32u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Stdlib_stable.Float32.of_float f

  let to_index = Nativeint.of_int
  let data_equal = Stdlib_stable.Float32.equal
  let unbox_index = Stdlib_upstream_compatible.Nativeint_u.of_nativeint
  let unbox_data = Stdlib_stable.Float32_u.of_float32
  let box_data = Stdlib_stable.Float32_u.to_float32
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> nativeint#
        -> float32#
        = "%caml_string_getf32#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> float32#
        = "%caml_string_getf32u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> nativeint#
        -> float32#
        = "%caml_bytes_getf32#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> float32#
        = "%caml_bytes_getf32u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> nativeint#
        -> float32#
        -> unit
        = "%caml_bytes_setf32#_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> float32#
        -> unit
        = "%caml_bytes_setf32u#_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> nativeint#
        -> float32#
        = "%caml_bigstring_getf32#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> float32#
        = "%caml_bigstring_getf32u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> nativeint#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32#_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32u#_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = int

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int.zero
      | 1 -> (Int.(shift_left one) (16 - 1))
      | 2 -> 
        (let shift = 16 - 1 in
        Int.(lognot (shift_left (shift_right (lognot zero) shift) shift)))
      | _ -> Random.int_in_range ~min:Int.zero ~max:(Int.(shift_left one) (16 - 1))


  let to_index = Int32.of_int
  let data_equal = Int.equal
  let unbox_index = Stdlib_upstream_compatible.Int32_u.of_int32
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int
        = "%caml_string_get16"

      external get_safe
        :  string
        -> int32#
        -> int
        = "%caml_string_get16_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> int
        = "%caml_string_get16u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int
        = "%caml_bytes_get16"

      external get_safe
        :  bytes
        -> int32#
        -> int
        = "%caml_bytes_get16_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> int
        = "%caml_bytes_get16u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int
        -> unit
        = "%caml_bytes_set16"

      external set_safe
        :  bytes
        -> int32#
        -> int
        -> unit
        = "%caml_bytes_set16_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> int
        -> unit
        = "%caml_bytes_set16u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int
        = "%caml_bigstring_get16"

      external get_safe
        :  bigstring
        -> int32#
        -> int
        = "%caml_bigstring_get16_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> int
        = "%caml_bigstring_get16u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int
        -> unit
        = "%caml_bigstring_set16"

      external set_safe
        :  bigstring
        -> int32#
        -> int
        -> unit
        = "%caml_bigstring_set16_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> int
        -> unit
        = "%caml_bigstring_set16u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int32_in_range ~min:Int32.min_int ~max:Int32.max_int


  let to_index = Int32.of_int
  let data_equal = Int32.equal
  let unbox_index = Stdlib_upstream_compatible.Int32_u.of_int32
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> int32#
        -> int32
        = "%caml_string_get32_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> int32
        = "%caml_string_get32u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> int32#
        -> int32
        = "%caml_bytes_get32_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> int32
        = "%caml_bytes_get32u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> int32#
        -> int32
        -> unit
        = "%caml_bytes_set32_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> int32
        -> unit
        = "%caml_bytes_set32u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> int32#
        -> int32
        = "%caml_bigstring_get32_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> int32
        = "%caml_bigstring_get32u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> int32#
        -> int32
        -> unit
        = "%caml_bigstring_set32_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> int32
        -> unit
        = "%caml_bigstring_set32u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range ~min:Int64.min_int ~max:Int64.max_int


  let to_index = Int32.of_int
  let data_equal = Int64.equal
  let unbox_index = Stdlib_upstream_compatible.Int32_u.of_int32
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> int32#
        -> int64
        = "%caml_string_get64_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> int64
        = "%caml_string_get64u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> int32#
        -> int64
        = "%caml_bytes_get64_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> int64
        = "%caml_bytes_get64u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> int32#
        -> int64
        -> unit
        = "%caml_bytes_set64_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> int64
        -> unit
        = "%caml_bytes_set64u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> int32#
        -> int64
        = "%caml_bigstring_get64_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> int64
        = "%caml_bigstring_get64u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> int32#
        -> int64
        -> unit
        = "%caml_bigstring_set64_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> int64
        -> unit
        = "%caml_bigstring_set64u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int32_in_range ~min:Int32.min_int ~max:Int32.max_int


  let to_index = Int32.of_int
  let data_equal = Int32.equal
  let unbox_index = Stdlib_upstream_compatible.Int32_u.of_int32
  let unbox_data = Stdlib_upstream_compatible.Int32_u.of_int32
  let box_data = Stdlib_upstream_compatible.Int32_u.to_int32
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> int32#
        -> int32#
        = "%caml_string_get32#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> int32#
        = "%caml_string_get32u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> int32#
        -> int32#
        = "%caml_bytes_get32#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> int32#
        = "%caml_bytes_get32u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> int32#
        -> int32#
        -> unit
        = "%caml_bytes_set32#_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> int32#
        -> unit
        = "%caml_bytes_set32u#_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> int32#
        -> int32#
        = "%caml_bigstring_get32#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> int32#
        = "%caml_bigstring_get32u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> int32#
        -> int32#
        -> unit
        = "%caml_bigstring_set32#_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> int32#
        -> unit
        = "%caml_bigstring_set32u#_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range ~min:Int64.min_int ~max:Int64.max_int


  let to_index = Int32.of_int
  let data_equal = Int64.equal
  let unbox_index = Stdlib_upstream_compatible.Int32_u.of_int32
  let unbox_data = Stdlib_upstream_compatible.Int64_u.of_int64
  let box_data = Stdlib_upstream_compatible.Int64_u.to_int64
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> int32#
        -> int64#
        = "%caml_string_get64#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> int64#
        = "%caml_string_get64u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> int32#
        -> int64#
        = "%caml_bytes_get64#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> int64#
        = "%caml_bytes_get64u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> int32#
        -> int64#
        -> unit
        = "%caml_bytes_set64#_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> int64#
        -> unit
        = "%caml_bytes_set64u#_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> int32#
        -> int64#
        = "%caml_bigstring_get64#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> int64#
        = "%caml_bigstring_get64u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> int32#
        -> int64#
        -> unit
        = "%caml_bigstring_set64#_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> int64#
        -> unit
        = "%caml_bigstring_set64u#_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Stdlib_stable.Float32.of_float f

  let to_index = Int32.of_int
  let data_equal = Stdlib_stable.Float32.equal
  let unbox_index = Stdlib_upstream_compatible.Int32_u.of_int32
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> int32#
        -> float32
        = "%caml_string_getf32_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> float32
        = "%caml_string_getf32u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> int32#
        -> float32
        = "%caml_bytes_getf32_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> float32
        = "%caml_bytes_getf32u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> int32#
        -> float32
        -> unit
        = "%caml_bytes_setf32_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> float32
        -> unit
        = "%caml_bytes_setf32u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> int32#
        -> float32
        = "%caml_bigstring_getf32_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> float32
        = "%caml_bigstring_getf32u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> int32#
        -> float32
        -> unit
        = "%caml_bigstring_setf32_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> float32
        -> unit
        = "%caml_bigstring_setf32u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Stdlib_stable.Float32.of_float f

  let to_index = Int32.of_int
  let data_equal = Stdlib_stable.Float32.equal
  let unbox_index = Stdlib_upstream_compatible.Int32_u.of_int32
  let unbox_data = Stdlib_stable.Float32_u.of_float32
  let box_data = Stdlib_stable.Float32_u.to_float32
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> int32#
        -> float32#
        = "%caml_string_getf32#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> float32#
        = "%caml_string_getf32u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> int32#
        -> float32#
        = "%caml_bytes_getf32#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> float32#
        = "%caml_bytes_getf32u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> int32#
        -> float32#
        -> unit
        = "%caml_bytes_setf32#_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> float32#
        -> unit
        = "%caml_bytes_setf32u#_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> int32#
        -> float32#
        = "%caml_bigstring_getf32#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> float32#
        = "%caml_bigstring_getf32u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> int32#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32#_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32u#_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = int

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int.zero
      | 1 -> (Int.(shift_left one) (16 - 1))
      | 2 -> 
        (let shift = 16 - 1 in
        Int.(lognot (shift_left (shift_right (lognot zero) shift) shift)))
      | _ -> Random.int_in_range ~min:Int.zero ~max:(Int.(shift_left one) (16 - 1))


  let to_index = Int64.of_int
  let data_equal = Int.equal
  let unbox_index = Stdlib_upstream_compatible.Int64_u.of_int64
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int
        = "%caml_string_get16"

      external get_safe
        :  string
        -> int64#
        -> int
        = "%caml_string_get16_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> int
        = "%caml_string_get16u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int
        = "%caml_bytes_get16"

      external get_safe
        :  bytes
        -> int64#
        -> int
        = "%caml_bytes_get16_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> int
        = "%caml_bytes_get16u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int
        -> unit
        = "%caml_bytes_set16"

      external set_safe
        :  bytes
        -> int64#
        -> int
        -> unit
        = "%caml_bytes_set16_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> int
        -> unit
        = "%caml_bytes_set16u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int
        = "%caml_bigstring_get16"

      external get_safe
        :  bigstring
        -> int64#
        -> int
        = "%caml_bigstring_get16_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> int
        = "%caml_bigstring_get16u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int
        -> unit
        = "%caml_bigstring_set16"

      external set_safe
        :  bigstring
        -> int64#
        -> int
        -> unit
        = "%caml_bigstring_set16_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> int
        -> unit
        = "%caml_bigstring_set16u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int32_in_range ~min:Int32.min_int ~max:Int32.max_int


  let to_index = Int64.of_int
  let data_equal = Int32.equal
  let unbox_index = Stdlib_upstream_compatible.Int64_u.of_int64
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> int64#
        -> int32
        = "%caml_string_get32_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> int32
        = "%caml_string_get32u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> int64#
        -> int32
        = "%caml_bytes_get32_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> int32
        = "%caml_bytes_get32u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> int64#
        -> int32
        -> unit
        = "%caml_bytes_set32_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> int32
        -> unit
        = "%caml_bytes_set32u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> int64#
        -> int32
        = "%caml_bigstring_get32_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> int32
        = "%caml_bigstring_get32u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> int64#
        -> int32
        -> unit
        = "%caml_bigstring_set32_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> int32
        -> unit
        = "%caml_bigstring_set32u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range ~min:Int64.min_int ~max:Int64.max_int


  let to_index = Int64.of_int
  let data_equal = Int64.equal
  let unbox_index = Stdlib_upstream_compatible.Int64_u.of_int64
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> int64#
        -> int64
        = "%caml_string_get64_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> int64
        = "%caml_string_get64u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> int64#
        -> int64
        = "%caml_bytes_get64_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> int64
        = "%caml_bytes_get64u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> int64#
        -> int64
        -> unit
        = "%caml_bytes_set64_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> int64
        -> unit
        = "%caml_bytes_set64u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> int64#
        -> int64
        = "%caml_bigstring_get64_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> int64
        = "%caml_bigstring_get64u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> int64#
        -> int64
        -> unit
        = "%caml_bigstring_set64_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> int64
        -> unit
        = "%caml_bigstring_set64u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int32_in_range ~min:Int32.min_int ~max:Int32.max_int


  let to_index = Int64.of_int
  let data_equal = Int32.equal
  let unbox_index = Stdlib_upstream_compatible.Int64_u.of_int64
  let unbox_data = Stdlib_upstream_compatible.Int32_u.of_int32
  let box_data = Stdlib_upstream_compatible.Int32_u.to_int32
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> int64#
        -> int32#
        = "%caml_string_get32#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> int32#
        = "%caml_string_get32u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> int64#
        -> int32#
        = "%caml_bytes_get32#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> int32#
        = "%caml_bytes_get32u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> int64#
        -> int32#
        -> unit
        = "%caml_bytes_set32#_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> int32#
        -> unit
        = "%caml_bytes_set32u#_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> int64#
        -> int32#
        = "%caml_bigstring_get32#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> int32#
        = "%caml_bigstring_get32u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> int64#
        -> int32#
        -> unit
        = "%caml_bigstring_set32#_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> int32#
        -> unit
        = "%caml_bigstring_set32u#_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range ~min:Int64.min_int ~max:Int64.max_int


  let to_index = Int64.of_int
  let data_equal = Int64.equal
  let unbox_index = Stdlib_upstream_compatible.Int64_u.of_int64
  let unbox_data = Stdlib_upstream_compatible.Int64_u.of_int64
  let box_data = Stdlib_upstream_compatible.Int64_u.to_int64
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> int64#
        -> int64#
        = "%caml_string_get64#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> int64#
        = "%caml_string_get64u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> int64#
        -> int64#
        = "%caml_bytes_get64#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> int64#
        = "%caml_bytes_get64u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> int64#
        -> int64#
        -> unit
        = "%caml_bytes_set64#_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> int64#
        -> unit
        = "%caml_bytes_set64u#_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> int64#
        -> int64#
        = "%caml_bigstring_get64#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> int64#
        = "%caml_bigstring_get64u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> int64#
        -> int64#
        -> unit
        = "%caml_bigstring_set64#_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> int64#
        -> unit
        = "%caml_bigstring_set64u#_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Stdlib_stable.Float32.of_float f

  let to_index = Int64.of_int
  let data_equal = Stdlib_stable.Float32.equal
  let unbox_index = Stdlib_upstream_compatible.Int64_u.of_int64
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> int64#
        -> float32
        = "%caml_string_getf32_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> float32
        = "%caml_string_getf32u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> int64#
        -> float32
        = "%caml_bytes_getf32_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> float32
        = "%caml_bytes_getf32u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> int64#
        -> float32
        -> unit
        = "%caml_bytes_setf32_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> float32
        -> unit
        = "%caml_bytes_setf32u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> int64#
        -> float32
        = "%caml_bigstring_getf32_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> float32
        = "%caml_bigstring_getf32u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> int64#
        -> float32
        -> unit
        = "%caml_bigstring_setf32_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> float32
        -> unit
        = "%caml_bigstring_setf32u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Stdlib_stable.Float32.of_float f

  let to_index = Int64.of_int
  let data_equal = Stdlib_stable.Float32.equal
  let unbox_index = Stdlib_upstream_compatible.Int64_u.of_int64
  let unbox_data = Stdlib_stable.Float32_u.of_float32
  let box_data = Stdlib_stable.Float32_u.to_float32
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> int64#
        -> float32#
        = "%caml_string_getf32#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> float32#
        = "%caml_string_getf32u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> int64#
        -> float32#
        = "%caml_bytes_getf32#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> float32#
        = "%caml_bytes_getf32u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> int64#
        -> float32#
        -> unit
        = "%caml_bytes_setf32#_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> float32#
        -> unit
        = "%caml_bytes_setf32u#_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> int64#
        -> float32#
        = "%caml_bigstring_getf32#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> float32#
        = "%caml_bigstring_getf32u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> int64#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32#_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32u#_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end
