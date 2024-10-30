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

module Poly = struct
  external ( < ) : 'a -> 'a -> bool = "%lessthan"

  external ( <= ) : 'a -> 'a -> bool = "%lessequal"

  external ( <> ) : 'a -> 'a -> bool = "%notequal"

  external ( = ) : 'a -> 'a -> bool = "%equal"

  external ( > ) : 'a -> 'a -> bool = "%greaterthan"

  external ( >= ) : 'a -> 'a -> bool = "%greaterequal"

  external compare : 'a -> 'a -> int = "%compare"

  external equal : 'a -> 'a -> bool = "%equal"
end

module Int_replace_polymorphic_compare = struct
  let ( < ) (x : int) y = x < y

  let ( <= ) (x : int) y = x <= y

  let ( <> ) (x : int) y = x <> y

  let ( = ) (x : int) y = x = y

  let ( > ) (x : int) y = x > y

  let ( >= ) (x : int) y = x >= y

  let compare (x : int) y = compare x y

  let equal (x : int) y = x = y

  let max (x : int) y = if x >= y then x else y

  let min (x : int) y = if x <= y then x else y
end

let phys_equal = ( == )

let ( == ) = `use_phys_equal

let ( != ) = `use_phys_equal

include Int_replace_polymorphic_compare

let quiet = ref false

let werror = ref false

let warnings = ref 0

let warn fmt =
  Format.ksprintf
    (fun s ->
      incr warnings;
      if not !quiet then Format.eprintf "%s%!" s)
    fmt

let fail = ref true

let failwith_ fmt =
  Printf.ksprintf (fun s -> if !fail then failwith s else Format.eprintf "%s@." s) fmt

let raise_ exn =
  if !fail then raise exn else Format.eprintf "%s@." (Printexc.to_string exn)

let int_num_bits = Sys.int_size

module List = struct
  include ListLabels

  let rec equal ~eq a b =
    match a, b with
    | [], [] -> true
    | x :: xs, y :: ys -> eq x y && equal ~eq xs ys
    | [], _ :: _ | _ :: _, [] -> false

  let rec find_map ~f = function
    | [] -> None
    | x :: l -> (
        match f x with
        | Some _ as result -> result
        | None -> find_map ~f l)

  let rec find_map_value ~f ~default = function
    | [] -> default
    | x :: l -> (
        match f x with
        | Some result -> result
        | None -> find_map_value ~f ~default l)

  let rec rev_append_map ~f l acc =
    match l with
    | [] -> acc
    | x :: xs -> rev_append_map ~f xs (f x :: acc)

  let slow_map l ~f = rev (rev_map ~f l)

  let max_non_tailcall =
    match Sys.backend_type with
    | Sys.Native | Sys.Bytecode -> 1_000
    | Sys.Other _ -> 50

  let rec count_map ~f l ctr =
    match l with
    | [] -> []
    | [ x1 ] ->
        let f1 = f x1 in
        [ f1 ]
    | [ x1; x2 ] ->
        let f1 = f x1 in
        let f2 = f x2 in
        [ f1; f2 ]
    | [ x1; x2; x3 ] ->
        let f1 = f x1 in
        let f2 = f x2 in
        let f3 = f x3 in
        [ f1; f2; f3 ]
    | [ x1; x2; x3; x4 ] ->
        let f1 = f x1 in
        let f2 = f x2 in
        let f3 = f x3 in
        let f4 = f x4 in
        [ f1; f2; f3; f4 ]
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
        let f1 = f x1 in
        let f2 = f x2 in
        let f3 = f x3 in
        let f4 = f x4 in
        let f5 = f x5 in
        f1
        :: f2
        :: f3
        :: f4
        :: f5
        :: (if ctr > max_non_tailcall then slow_map ~f tl else count_map ~f tl (ctr + 1))

  let map l ~f = count_map ~f l 0

  let rec take' acc n l =
    if n = 0
    then acc, l
    else
      match l with
      | [] -> acc, []
      | x :: xs -> take' (x :: acc) (pred n) xs

  let take n l =
    let x, xs = take' [] n l in
    rev x, xs

  let rec last = function
    | [] -> None
    | [ x ] -> Some x
    | _ :: xs -> last xs

  let sort_uniq ~compare l =
    let l = List.sort compare l in
    match l with
    | ([] | [ _ ]) as l -> l
    | x :: xs ->
        let rec loop prev = function
          | [] -> [ prev ]
          | x :: rest when compare x prev = 0 -> loop prev rest
          | x :: rest -> prev :: loop x rest
        in
        loop x xs

  let is_empty = function
    | [] -> true
    | _ -> false

  let partition_map t ~f =
    let rec loop t fst snd =
      match t with
      | [] -> rev fst, rev snd
      | x :: t -> (
          match f x with
          | `Fst y -> loop t (y :: fst) snd
          | `Snd y -> loop t fst (y :: snd))
    in
    loop t [] []

  let tail_append l1 l2 = rev_append (rev l1) l2

  let rec count_append l1 l2 count =
    match l2 with
    | [] -> l1
    | _ -> (
        match l1 with
        | [] -> l2
        | [ x1 ] -> x1 :: l2
        | [ x1; x2 ] -> x1 :: x2 :: l2
        | [ x1; x2; x3 ] -> x1 :: x2 :: x3 :: l2
        | [ x1; x2; x3; x4 ] -> x1 :: x2 :: x3 :: x4 :: l2
        | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
            x1
            :: x2
            :: x3
            :: x4
            :: x5
            ::
            (if count > max_non_tailcall
             then tail_append tl l2
             else count_append tl l2 (count + 1)))

  let append l1 l2 = count_append l1 l2 0

  let group l ~f =
    let rec loop (l : 'a list) (this_group : 'a list) (acc : 'a list list) : 'a list list
        =
      match l with
      | [] -> List.rev (List.rev this_group :: acc)
      | x :: xs ->
          let pred = List.hd this_group in
          if f x pred
          then loop xs (x :: this_group) acc
          else loop xs [ x ] (List.rev this_group :: acc)
    in
    match l with
    | [] -> []
    | x :: xs -> loop xs [ x ] []

  let concat_map ~f l =
    let rec aux f acc = function
      | [] -> rev acc
      | x :: l ->
          let xs = f x in
          aux f (rev_append xs acc) l
    in
    aux f [] l

  let split_last xs =
    let rec aux acc = function
      | [] -> None
      | [ x ] -> Some (rev acc, x)
      | x :: xs -> aux (x :: acc) xs
    in
    aux [] xs

  (* like [List.map] except that it calls the function with
     an additional argument to indicate whether we're mapping
     over the last element of the list *)
  let rec map_last ~f l =
    match l with
    | [] -> assert false
    | [ x ] -> [ f true x ]
    | x :: xs -> f false x :: map_last ~f xs

  (* like [List.iter] except that it calls the function with
     an additional argument to indicate whether we're iterating
     over the last element of the list *)
  let rec iter_last ~f l =
    match l with
    | [] -> ()
    | [ a ] -> f true a
    | a :: l ->
        f false a;
        iter_last ~f l
end

let ( @ ) = List.append

module Nativeint = struct
  include Nativeint

  external equal : nativeint -> nativeint -> bool = "%equal"
end

module Int32 = struct
  include Int32

  external ( < ) : int32 -> int32 -> bool = "%lessthan"

  external ( <= ) : int32 -> int32 -> bool = "%lessequal"

  external ( <> ) : int32 -> int32 -> bool = "%notequal"

  external ( = ) : int32 -> int32 -> bool = "%equal"

  external ( > ) : int32 -> int32 -> bool = "%greaterthan"

  external ( >= ) : int32 -> int32 -> bool = "%greaterequal"

  external compare : int32 -> int32 -> int = "%compare"

  external equal : int32 -> int32 -> bool = "%equal"

  let warn_overflow name ~to_dec ~to_hex i i32 =
    warn
      "Warning: integer overflow: %s 0x%s (%s) truncated to 0x%lx (%ld); the generated \
       code might be incorrect.@."
      name
      (to_hex i)
      (to_dec i)
      i32
      i32

  let convert_warning_on_overflow name ~to_int32 ~of_int32 ~equal ~to_dec ~to_hex x =
    let i32 = to_int32 x in
    let x' = of_int32 i32 in
    if not (equal x' x) then warn_overflow name ~to_dec ~to_hex x i32;
    i32

  let of_nativeint_warning_on_overflow n =
    convert_warning_on_overflow
      "native integer"
      ~to_int32:Nativeint.to_int32
      ~of_int32:Nativeint.of_int32
      ~equal:Nativeint.equal
      ~to_dec:(Printf.sprintf "%nd")
      ~to_hex:(Printf.sprintf "%nx")
      n
end

module Option = struct
  let map ~f x =
    match x with
    | None -> None
    | Some v -> Some (f v)

  let to_list = function
    | None -> []
    | Some x -> [ x ]

  let bind ~f x =
    match x with
    | None -> None
    | Some v -> f v

  let iter ~f x =
    match x with
    | None -> ()
    | Some v -> f v

  let filter ~f x =
    match x with
    | None -> None
    | Some v -> if f v then Some v else None

  let compare compare_elt a b =
    match a, b with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some a, Some b -> compare_elt a b

  let equal equal_elt a b =
    match a, b with
    | None, None -> true
    | Some a, Some b -> equal_elt a b
    | Some _, None | None, Some _ -> false

  let is_none = function
    | None -> true
    | Some _ -> false

  let is_some = function
    | None -> false
    | Some _ -> true

  let value ~default = function
    | None -> default
    | Some s -> s
end

module Int64 = struct
  include Int64

  let equal (a : int64) (b : int64) = Poly.(a = b)
end

module Float = struct
  type t = float

  let equal (_ : float) (_ : float) = `Use_ieee_equal_or_bitwise_equal

  let ieee_equal (a : float) (b : float) = Poly.equal a b

  let bitwise_equal (a : float) (b : float) =
    Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)

  (* Re-defined here to stay compatible with OCaml 4.02 *)
  external classify_float : float -> fpclass = "caml_classify_float"

  external ( < ) : t -> t -> bool = "%lessthan"

  external ( <= ) : t -> t -> bool = "%lessequal"

  external ( <> ) : t -> t -> bool = "%notequal"

  external ( = ) : t -> t -> bool = "%equal"

  external ( > ) : t -> t -> bool = "%greaterthan"

  external ( >= ) : t -> t -> bool = "%greaterequal"
end

module Bool = struct
  external ( <> ) : bool -> bool -> bool = "%notequal"

  external ( = ) : bool -> bool -> bool = "%equal"

  external ( > ) : bool -> bool -> bool = "%greaterthan"

  external equal : bool -> bool -> bool = "%equal"
end

module Char = struct
  include Char

  external ( < ) : char -> char -> bool = "%lessthan"

  external ( <= ) : char -> char -> bool = "%lessequal"

  external ( <> ) : char -> char -> bool = "%notequal"

  external ( = ) : char -> char -> bool = "%equal"

  external ( > ) : char -> char -> bool = "%greaterthan"

  external ( >= ) : char -> char -> bool = "%greaterequal"

  external compare : char -> char -> int = "%compare"

  external equal : char -> char -> bool = "%equal"

  let is_alpha = function
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false

  let is_num = function
    | '0' .. '9' -> true
    | _ -> false

  let lowercase_ascii c =
    match c with
    | 'A' .. 'Z' as c -> Char.unsafe_chr (Char.code c + 32)
    | _ -> c

  let uppercase_ascii c =
    match c with
    | 'a' .. 'z' as c -> Char.unsafe_chr (Char.code c - 32)
    | _ -> c
end

module Uchar = struct
  include Uchar

  module Utf_decode : sig
    type utf_decode [@@immediate]
    (** The type for UTF decode results. Values of this type represent the result of a
        Unicode Transformation Format decoding attempt. *)

    val utf_decode_is_valid : utf_decode -> bool
    (** [utf_decode_is_valid d] is [true] if and only if [d] holds a valid decode. *)

    val utf_decode_uchar : utf_decode -> t
    (** [utf_decode_uchar d] is the Unicode character decoded by [d] if
        [utf_decode_is_valid d] is [true] and {!Uchar.rep} otherwise. *)

    val utf_decode_length : utf_decode -> int
    (** [utf_decode_length d] is the number of elements from the source that were consumed
        by the decode [d]. This is always strictly positive and smaller or equal to [4].
        The kind of source elements depends on the actual decoder; for the decoders of the
        standard library this function always returns a length in bytes. *)

    val utf_decode : int -> t -> utf_decode
    (** [utf_decode n u] is a valid UTF decode for [u] that consumed [n] elements from the
        source for decoding. [n] must be positive and smaller or equal to [4] (this is not
        checked by the module). *)

    val utf_decode_invalid : int -> utf_decode
    (** [utf_decode_invalid n] is an invalid UTF decode that consumed [n] elements from
        the source to error. [n] must be positive and smaller or equal to [4] (this is not
        checked by the module). The resulting decode has {!rep} as the decoded Unicode
        character. *)

    val utf_8_byte_length : t -> int
    (** [utf_8_byte_length u] is the number of bytes needed to encode [u] in UTF-8. *)

    val utf_16_byte_length : t -> int
    (** [utf_16_byte_length u] is the number of bytes needed to encode [u] in UTF-16. *)
  end = struct
    (* UTF codecs tools *)

    type utf_decode = int
    (* This is an int [0xDUUUUUU] decomposed as follows:
       - [D] is four bits for decode information, the highest bit is set if the
         decode is valid. The three lower bits indicate the number of elements
         from the source that were consumed by the decode.
       - [UUUUUU] is the decoded Unicode character or the Unicode replacement
         character U+FFFD if for invalid decodes. *)

    let rep = 0xFFFD

    let valid_bit = 27

    let decode_bits = 24

    let[@inline] utf_decode_is_valid d = d lsr valid_bit = 1

    let[@inline] utf_decode_length d = (d lsr decode_bits) land 0b111

    let[@inline] utf_decode_uchar d = unsafe_of_int (d land 0xFFFFFF)

    let[@inline] utf_decode n u = ((8 lor n) lsl decode_bits) lor to_int u

    let[@inline] utf_decode_invalid n = (n lsl decode_bits) lor rep

    let utf_8_byte_length u =
      match to_int u with
      | u when u < 0 -> assert false
      | u when u <= 0x007F -> 1
      | u when u <= 0x07FF -> 2
      | u when u <= 0xFFFF -> 3
      | u when u <= 0x10FFFF -> 4
      | _ -> assert false

    let utf_16_byte_length u =
      match to_int u with
      | u when u < 0 -> assert false
      | u when u <= 0xFFFF -> 2
      | u when u <= 0x10FFFF -> 4
      | _ -> assert false
  end

  include Utf_decode
end

module Buffer = struct
  include Buffer

  let array_conv = Array.init 16 (fun i -> "0123456789abcdef".[i])

  let add_char_hex b (c : Char.t) =
    let c = Char.code c in
    Buffer.add_char b (Array.unsafe_get array_conv (c lsr 4));
    Buffer.add_char b (Array.unsafe_get array_conv (c land 0xf))
end

module Bytes = struct
  include BytesLabels

  let sub_string b ~pos:ofs ~len = unsafe_to_string (Bytes.sub b ofs len)

  let fold_left ~f ~init b =
    let r = ref init in
    for i = 0 to length b - 1 do
      r := f !r (unsafe_get b i)
    done;
    !r

  let fold_right ~f b ~init =
    let r = ref init in
    for i = length b - 1 downto 0 do
      r := f (unsafe_get b i) !r
    done;
    !r
end

module String = struct
  include StringLabels

  let equal (a : string) (b : string) = Poly.(a = b)

  let hash (a : string) = Hashtbl.hash a

  let is_empty = function
    | "" -> true
    | _ -> false

  let is_prefix ~prefix s =
    let len_a = length prefix in
    let len_s = length s in
    if len_a > len_s
    then false
    else
      let max_idx_a = len_a - 1 in
      let rec loop i =
        if i > max_idx_a
        then true
        else if not (Char.equal (unsafe_get prefix i) (unsafe_get s i))
        then false
        else loop (i + 1)
      in
      loop 0

  let is_suffix ~suffix s =
    let len_a = length suffix in
    let len_s = length s in
    if len_a > len_s
    then false
    else
      let max_idx_a = len_a - 1 in
      let rec loop i =
        if i > max_idx_a
        then true
        else if
          not
            (Char.equal
               (unsafe_get suffix (len_a - 1 - i))
               (unsafe_get s (len_s - 1 - i)))
        then false
        else loop (i + 1)
      in
      loop 0

  let drop_prefix ~prefix s =
    let plen = String.length prefix in
    if plen > String.length s
    then None
    else
      try
        for i = 0 to String.length prefix - 1 do
          if not (Char.equal s.[i] prefix.[i]) then raise Exit
        done;
        Some (String.sub s plen (String.length s - plen))
      with Exit -> None

  let for_all =
    let rec loop s ~f ~last i =
      if i > last
      then true
      else if f (String.unsafe_get s i)
      then loop s ~f ~last (i + 1)
      else false
    in
    fun s ~f -> loop s ~f ~last:(String.length s - 1) 0

  let is_ascii s =
    let res = ref true in
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '\000' .. '\127' -> ()
      | '\128' .. '\255' -> res := false
    done;
    !res

  let has_backslash s =
    let res = ref false in
    for i = 0 to String.length s - 1 do
      if Char.equal s.[i] '\\' then res := true
    done;
    !res

  let split_char ~sep p = String.split_on_char sep p

  (* copied from https://github.com/ocaml/ocaml/pull/10 *)
  let split ~sep s =
    let sep_len = String.length sep in
    if sep_len = 1
    then split_char ~sep:sep.[0] s
    else
      let sep_max = sep_len - 1 in
      if sep_max < 0
      then invalid_arg "String.split: empty separator"
      else
        let s_max = String.length s - 1 in
        if s_max < 0
        then [ "" ]
        else
          let acc = ref [] in
          let sub_start = ref 0 in
          let k = ref 0 in
          let i = ref 0 in
          (* We build the substrings by running from the start of [s] to the
             end with [i] trying to match the first character of [sep] in
             [s]. If this matches, we verify that the whole [sep] is matched
             using [k]. If this matches we extract a substring from the start
             of the current substring [sub_start] to [!i - 1] (the position
             before the [sep] we found).  We then continue to try to match
             with [i] by starting after the [sep] we just found, this is also
             becomes the start position of the next substring. If [i] is such
             that no separator can be found we exit the loop and make a
             substring from [sub_start] until the end of the string. *)
          while !i + sep_max <= s_max do
            if not (Char.equal (String.unsafe_get s !i) (String.unsafe_get sep 0))
            then incr i
            else (
              (* Check remaining [sep] chars match, access to unsafe s (!i + !k) is
                   guaranteed by loop invariant. *)
              k := 1;
              while
                !k <= sep_max
                && Char.equal (String.unsafe_get s (!i + !k)) (String.unsafe_get sep !k)
              do
                incr k
              done;
              if !k <= sep_max
              then (* no match *) incr i
              else
                let new_sub_start = !i + sep_max + 1 in
                let sub_end = !i - 1 in
                let sub_len = sub_end - !sub_start + 1 in
                acc := String.sub s !sub_start sub_len :: !acc;
                sub_start := new_sub_start;
                i := new_sub_start)
          done;
          List.rev (String.sub s !sub_start (s_max - !sub_start + 1) :: !acc)

  let apply1 f (s : string) : string =
    let b = Bytes.of_string s in
    if Bytes.length b = 0
    then s
    else (
      Bytes.unsafe_set b 0 (f (Bytes.unsafe_get b 0));
      Bytes.to_string b)

  let lsplit2 line ~on:delim =
    try
      let pos = index line delim in
      Some (sub line ~pos:0 ~len:pos, sub line ~pos:(pos + 1) ~len:(length line - pos - 1))
    with Not_found -> None

  let rsplit2 line ~on:delim =
    try
      let pos = rindex line delim in
      Some (sub line ~pos:0 ~len:pos, sub line ~pos:(pos + 1) ~len:(length line - pos - 1))
    with Not_found -> None

  let capitalize_ascii s = apply1 Char.uppercase_ascii s

  let uncapitalize_ascii s = apply1 Char.lowercase_ascii s

  let[@inline] not_in_x80_to_xBF b = b lsr 6 <> 0b10

  let[@inline] not_in_xA0_to_xBF b = b lsr 5 <> 0b101

  let[@inline] not_in_x80_to_x9F b = b lsr 5 <> 0b100

  let[@inline] not_in_x90_to_xBF b = b < 0x90 || 0xBF < b

  let[@inline] not_in_x80_to_x8F b = b lsr 4 <> 0x8

  let[@inline] utf_8_uchar_2 b0 b1 = ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F)

  let[@inline] utf_8_uchar_3 b0 b1 b2 =
    ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)

  let[@inline] utf_8_uchar_4 b0 b1 b2 b3 =
    ((b0 land 0x07) lsl 18)
    lor ((b1 land 0x3F) lsl 12)
    lor ((b2 land 0x3F) lsl 6)
    lor (b3 land 0x3F)

  external get_uint8 : string -> int -> int = "%string_safe_get"

  external unsafe_get_uint8 : string -> int -> int = "%string_unsafe_get"

  let dec_invalid = Uchar.utf_decode_invalid

  let[@inline] dec_ret n u = Uchar.utf_decode n (Uchar.unsafe_of_int u)

  let get_utf_8_uchar b i =
    let b0 = get_uint8 b i in
    (* raises if [i] is not a valid index. *)
    let get = unsafe_get_uint8 in
    let max = length b - 1 in
    match Char.unsafe_chr b0 with
    (* See The Unicode Standard, Table 3.7 *)
    | '\x00' .. '\x7F' -> dec_ret 1 b0
    | '\xC2' .. '\xDF' ->
        let i = i + 1 in
        if i > max
        then dec_invalid 1
        else
          let b1 = get b i in
          if not_in_x80_to_xBF b1 then dec_invalid 1 else dec_ret 2 (utf_8_uchar_2 b0 b1)
    | '\xE0' ->
        let i = i + 1 in
        if i > max
        then dec_invalid 1
        else
          let b1 = get b i in
          if not_in_xA0_to_xBF b1
          then dec_invalid 1
          else
            let i = i + 1 in
            if i > max
            then dec_invalid 2
            else
              let b2 = get b i in
              if not_in_x80_to_xBF b2
              then dec_invalid 2
              else dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | '\xE1' .. '\xEC' | '\xEE' .. '\xEF' ->
        let i = i + 1 in
        if i > max
        then dec_invalid 1
        else
          let b1 = get b i in
          if not_in_x80_to_xBF b1
          then dec_invalid 1
          else
            let i = i + 1 in
            if i > max
            then dec_invalid 2
            else
              let b2 = get b i in
              if not_in_x80_to_xBF b2
              then dec_invalid 2
              else dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | '\xED' ->
        let i = i + 1 in
        if i > max
        then dec_invalid 1
        else
          let b1 = get b i in
          if not_in_x80_to_x9F b1
          then dec_invalid 1
          else
            let i = i + 1 in
            if i > max
            then dec_invalid 2
            else
              let b2 = get b i in
              if not_in_x80_to_xBF b2
              then dec_invalid 2
              else dec_ret 3 (utf_8_uchar_3 b0 b1 b2)
    | '\xF0' ->
        let i = i + 1 in
        if i > max
        then dec_invalid 1
        else
          let b1 = get b i in
          if not_in_x90_to_xBF b1
          then dec_invalid 1
          else
            let i = i + 1 in
            if i > max
            then dec_invalid 2
            else
              let b2 = get b i in
              if not_in_x80_to_xBF b2
              then dec_invalid 2
              else
                let i = i + 1 in
                if i > max
                then dec_invalid 3
                else
                  let b3 = get b i in
                  if not_in_x80_to_xBF b3
                  then dec_invalid 3
                  else dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | '\xF1' .. '\xF3' ->
        let i = i + 1 in
        if i > max
        then dec_invalid 1
        else
          let b1 = get b i in
          if not_in_x80_to_xBF b1
          then dec_invalid 1
          else
            let i = i + 1 in
            if i > max
            then dec_invalid 2
            else
              let b2 = get b i in
              if not_in_x80_to_xBF b2
              then dec_invalid 2
              else
                let i = i + 1 in
                if i > max
                then dec_invalid 3
                else
                  let b3 = get b i in
                  if not_in_x80_to_xBF b3
                  then dec_invalid 3
                  else dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | '\xF4' ->
        let i = i + 1 in
        if i > max
        then dec_invalid 1
        else
          let b1 = get b i in
          if not_in_x80_to_x8F b1
          then dec_invalid 1
          else
            let i = i + 1 in
            if i > max
            then dec_invalid 2
            else
              let b2 = get b i in
              if not_in_x80_to_xBF b2
              then dec_invalid 2
              else
                let i = i + 1 in
                if i > max
                then dec_invalid 3
                else
                  let b3 = get b i in
                  if not_in_x80_to_xBF b3
                  then dec_invalid 3
                  else dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3)
    | _ -> dec_invalid 1

  let fold_utf_8 s ~f acc =
    let rec loop i s ~pos ~f acc =
      if String.length s = pos
      then acc
      else
        let r = get_utf_8_uchar s pos in
        let l = Uchar.utf_decode_length r in
        let acc = f acc i (Uchar.utf_decode_uchar r) in
        loop (i + 1) s ~pos:(pos + l) ~f acc
    in
    loop 0 s ~pos:0 ~f acc

  let fix_utf_8 s =
    let b = Buffer.create (String.length s) in
    fold_utf_8 s () ~f:(fun () _i u -> Buffer.add_utf_8_uchar b u);
    Buffer.contents b

  let is_valid_utf_8 b =
    let rec loop max b i =
      if i > max
      then true
      else
        let get = unsafe_get_uint8 in
        match Char.unsafe_chr (get b i) with
        | '\x00' .. '\x7F' -> loop max b (i + 1)
        | '\xC2' .. '\xDF' ->
            let last = i + 1 in
            if last > max || not_in_x80_to_xBF (get b last)
            then false
            else loop max b (last + 1)
        | '\xE0' ->
            let last = i + 2 in
            if
              last > max
              || not_in_xA0_to_xBF (get b (i + 1))
              || not_in_x80_to_xBF (get b last)
            then false
            else loop max b (last + 1)
        | '\xE1' .. '\xEC' | '\xEE' .. '\xEF' ->
            let last = i + 2 in
            if
              last > max
              || not_in_x80_to_xBF (get b (i + 1))
              || not_in_x80_to_xBF (get b last)
            then false
            else loop max b (last + 1)
        | '\xED' ->
            let last = i + 2 in
            if
              last > max
              || not_in_x80_to_x9F (get b (i + 1))
              || not_in_x80_to_xBF (get b last)
            then false
            else loop max b (last + 1)
        | '\xF0' ->
            let last = i + 3 in
            if
              last > max
              || not_in_x90_to_xBF (get b (i + 1))
              || not_in_x80_to_xBF (get b (i + 2))
              || not_in_x80_to_xBF (get b last)
            then false
            else loop max b (last + 1)
        | '\xF1' .. '\xF3' ->
            let last = i + 3 in
            if
              last > max
              || not_in_x80_to_xBF (get b (i + 1))
              || not_in_x80_to_xBF (get b (i + 2))
              || not_in_x80_to_xBF (get b last)
            then false
            else loop max b (last + 1)
        | '\xF4' ->
            let last = i + 3 in
            if
              last > max
              || not_in_x80_to_x8F (get b (i + 1))
              || not_in_x80_to_xBF (get b (i + 2))
              || not_in_x80_to_xBF (get b last)
            then false
            else loop max b (last + 1)
        | _ -> false
    in
    loop (length b - 1) b 0

  let fold_left ~f ~init s =
    let r = ref init in
    for i = 0 to length s - 1 do
      r := f !r (unsafe_get s i)
    done;
    !r

  let fold_right ~f s ~init =
    let r = ref init in
    for i = length s - 1 downto 0 do
      r := f (unsafe_get s i) !r
    done;
    !r
end

module Utf8_string : sig
  type t = private Utf8 of string [@@ocaml.unboxed]

  val of_string_exn : string -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool
end = struct
  type t = Utf8 of string [@@ocaml.unboxed]

  let of_string_exn s =
    if String.is_valid_utf_8 s
    then Utf8 s
    else invalid_arg "Utf8_string.of_string: invalid utf8 string"

  let compare (Utf8 x) (Utf8 y) = String.compare x y

  let equal (Utf8 x) (Utf8 y) = String.equal x y
end

module Int = struct
  type t = int

  let compare (x : int) y = compare x y

  let equal (x : t) y = x = y

  let hash (x : t) = Hashtbl.hash x
end

module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)
module Utf8_string_set = Set.Make (Utf8_string)
module Utf8_string_map = Map.Make (Utf8_string)

module BitSet : sig
  type t

  val create : unit -> t

  val create' : int -> t

  val mem : t -> int -> bool

  val set : t -> int -> unit

  val unset : t -> int -> unit

  val copy : t -> t

  val iter : f:(int -> unit) -> t -> unit

  val size : t -> int

  val next_free : t -> int -> int

  val next_mem : t -> int -> int
end = struct
  type t = { mutable arr : int array }

  let create () = { arr = Array.make 1 0 }

  let create' n = { arr = Array.make ((n / int_num_bits) + 1) 0 }

  let size t = Array.length t.arr * int_num_bits

  let mem t i =
    let arr = t.arr in
    let idx = i / int_num_bits in
    let off = i mod int_num_bits in
    idx < Array.length arr
    &&
    let x = Array.unsafe_get arr idx in
    x <> 0 && x land (1 lsl off) <> 0

  let[@ocaml.inline never] resize t idx =
    let size = Array.length t.arr in
    let size_ref = ref size in
    while idx >= !size_ref do
      size_ref := !size_ref * 2
    done;
    let a = Array.make !size_ref 0 in
    Array.blit t.arr 0 a 0 size;
    t.arr <- a

  let set t i =
    let idx = i / int_num_bits in
    let off = i mod int_num_bits in
    let size = Array.length t.arr in
    if idx >= size then resize t idx;
    Array.unsafe_set t.arr idx (Array.unsafe_get t.arr idx lor (1 lsl off))

  let unset t i =
    let idx = i / int_num_bits in
    let off = i mod int_num_bits in
    let size = Array.length t.arr in
    if idx >= size
    then ()
    else
      let b = Array.unsafe_get t.arr idx in
      let mask = 1 lsl off in
      if b <> 0 && b land mask <> 0 then Array.unsafe_set t.arr idx (b lxor mask)

  let next_free t i =
    let x = ref i in
    while mem t !x do
      incr x
    done;
    !x

  let next_mem t i =
    let x = ref i in
    while not (mem t !x) do
      incr x
    done;
    !x

  let copy t = { arr = Array.copy t.arr }

  let iter ~f t =
    for i = 0 to size t do
      if mem t i then f i
    done
end

module Array = struct
  include ArrayLabels

  let find_opt ~f:p a =
    let n = length a in
    let rec loop i =
      if i = n
      then None
      else
        let x = unsafe_get a i in
        if p x then Some x else loop (succ i)
    in
    loop 0

  let fold_right_i a ~f ~init:x =
    let r = ref x in
    for i = Array.length a - 1 downto 0 do
      r := f i (Array.unsafe_get a i) !r
    done;
    !r

  let equal eq a b =
    let len_a = Array.length a in
    if len_a <> Array.length b
    then false
    else
      let i = ref 0 in
      while !i < len_a && eq a.(!i) b.(!i) do
        incr i
      done;
      !i = len_a
end

module Filename = struct
  include Filename

  let temp_file_name =
    (* Inlined unavailable Filename.temp_file_name. Filename.temp_file gives
       us incorrect permissions. https://github.com/ocsigen/js_of_ocaml/issues/182 *)
    let prng = lazy (Random.State.make_self_init ()) in
    fun ~temp_dir prefix suffix ->
      let rnd = Random.State.bits (Lazy.force prng) land 0xFFFFFF in
      Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

  let gen_file file f =
    let f_tmp =
      temp_file_name ~temp_dir:(Filename.dirname file) (Filename.basename file) ".tmp"
    in
    try
      let ch = open_out_bin f_tmp in
      let res =
        try f ch
        with e ->
          close_out ch;
          raise e
      in
      close_out ch;
      (try Sys.remove file with Sys_error _ -> ());
      Sys.rename f_tmp file;
      res
    with exc ->
      Sys.remove f_tmp;
      raise exc
end

module Fun = struct
  include Fun

  let memoize f =
    let h = Hashtbl.create 4 in
    fun x ->
      try Hashtbl.find h x
      with Not_found ->
        let r = f x in
        Hashtbl.add h x r;
        r
end

let generated_name = function
  | "param" | "match" | "switcher" -> true
  | s -> String.is_prefix ~prefix:"cst_" s
