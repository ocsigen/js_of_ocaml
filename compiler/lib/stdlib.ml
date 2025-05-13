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

let open_in_text = open_in

let open_out_text = open_out

module Deprecated : sig
  val open_in : string -> in_channel [@@deprecated "use open_int_text/open_int_bin"]

  val open_out : string -> out_channel [@@deprecated "use open_out_text/open_out_bin"]
end = struct
  let open_in = open_in

  let open_out = open_out
end

include Deprecated

module Poly = struct
  external compare : 'a -> 'a -> int = "%compare"

  external equal : 'a -> 'a -> bool = "%equal"

  module Hashtbl = Hashtbl
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

module List = struct
  include ListLabels

  let (mem_assoc [@deprecated "use List.exists"]) = List.mem

  let (assoc [@deprecated "use List.find_map"]) = List.assoc

  let (assoc_opt [@deprecated "use List.find_map"]) = List.assoc_opt

  let (remove_assoc [@deprecated "use List.filter"]) = List.remove_assoc

  let rec mem ~eq x = function
    | [] -> false
    | a :: l -> eq a x || mem ~eq x l

  let string_assoc name l =
    List.find_map
      (fun (name', state) -> if String.equal name name' then Some state else None)
      l

  let rec rev_append_map ~f l acc =
    match l with
    | [] -> acc
    | x :: xs -> rev_append_map ~f xs (f x :: acc)

  let slow_map l ~f = rev (rev_map ~f l) [@@if ocaml_version < (4, 14, 0)]

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
  [@@if ocaml_version < (4, 14, 0)]

  let map l ~f = count_map ~f l 0 [@@if ocaml_version < (4, 14, 0)]

  let[@tail_mod_cons] rec map l ~f =
    match l with
    | [] -> []
    | x :: tl -> f x :: (map [@tailcall]) tl ~f
  [@@if ocaml_version >= (4, 14, 0)]

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

  let is_empty = function
    | [] -> true
    | _ -> false
  [@@if ocaml_version < (5, 1, 0)]

  let tail_append l1 l2 = rev_append (rev l1) l2 [@@if ocaml_version < (5, 1, 0)]

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
  [@@if ocaml_version < (5, 1, 0)]

  let append l1 l2 = count_append l1 l2 0 [@@if ocaml_version < (5, 1, 0)]

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

module Int32 = struct
  include Int32

  external ( < ) : int32 -> int32 -> bool = "%lessthan"

  external ( <= ) : int32 -> int32 -> bool = "%lessequal"

  external ( <> ) : int32 -> int32 -> bool = "%notequal"

  external ( = ) : int32 -> int32 -> bool = "%equal"

  external ( > ) : int32 -> int32 -> bool = "%greaterthan"

  external ( >= ) : int32 -> int32 -> bool = "%greaterequal"

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

module Int64 = struct
  include Int64

  external ( < ) : int64 -> int64 -> bool = "%lessthan"

  external ( <= ) : int64 -> int64 -> bool = "%lessequal"

  external ( <> ) : int64 -> int64 -> bool = "%notequal"

  external ( = ) : int64 -> int64 -> bool = "%equal"

  external ( > ) : int64 -> int64 -> bool = "%greaterthan"

  external ( >= ) : int64 -> int64 -> bool = "%greaterequal"
end

module Option = struct
  include Option

  let map ~f x =
    match x with
    | None -> None
    | Some v -> Some (f v)

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

  let value ~default = function
    | None -> default
    | Some s -> s
end

module Float = struct
  include Float

  let equal (_ : float) (_ : float) = `Use_ieee_equal_or_bitwise_equal

  let ieee_equal (a : float) (b : float) = Poly.equal a b

  let bitwise_equal (a : float) (b : float) =
    Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)

  external ( < ) : t -> t -> bool = "%lessthan"

  external ( <= ) : t -> t -> bool = "%lessequal"

  external ( <> ) : t -> t -> bool = "%notequal"

  external ( = ) : t -> t -> bool = "%equal"

  external ( > ) : t -> t -> bool = "%greaterthan"

  external ( >= ) : t -> t -> bool = "%greaterequal"
end

module Bool = struct
  include Bool

  external ( <> ) : bool -> bool -> bool = "%notequal"

  external ( = ) : bool -> bool -> bool = "%equal"

  external ( > ) : bool -> bool -> bool = "%greaterthan"
end

module Char = struct
  include Char

  external ( < ) : char -> char -> bool = "%lessthan"

  external ( <= ) : char -> char -> bool = "%lessequal"

  external ( <> ) : char -> char -> bool = "%notequal"

  external ( = ) : char -> char -> bool = "%equal"

  external ( > ) : char -> char -> bool = "%greaterthan"

  external ( >= ) : char -> char -> bool = "%greaterequal"

  let is_letter = function
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
end

module Uchar = struct
  include Uchar

  module Utf_decode : sig
    type utf_decode [@@immediate]
    (** The type for UTF decode results. Values of this type represent
    the result of a Unicode Transformation Format decoding attempt. *)

    val utf_decode_is_valid : utf_decode -> bool
    (** [utf_decode_is_valid d] is [true] if and only if [d] holds a valid
    decode. *)

    val utf_decode_uchar : utf_decode -> t
    (** [utf_decode_uchar d] is the Unicode character decoded by [d] if
    [utf_decode_is_valid d] is [true] and {!Uchar.rep} otherwise. *)

    val utf_decode_length : utf_decode -> int
    (** [utf_decode_length d] is the number of elements from the source
    that were consumed by the decode [d]. This is always strictly
    positive and smaller or equal to [4]. The kind of source elements
    depends on the actual decoder; for the decoders of the standard
    library this function always returns a length in bytes. *)

    val utf_decode : int -> t -> utf_decode
    (** [utf_decode n u] is a valid UTF decode for [u] that consumed [n]
    elements from the source for decoding. [n] must be positive and
    smaller or equal to [4] (this is not checked by the module). *)

    val utf_decode_invalid : int -> utf_decode
    (** [utf_decode_invalid n] is an invalid UTF decode that consumed [n]
    elements from the source to error. [n] must be positive and
    smaller or equal to [4] (this is not checked by the module). The
    resulting decode has {!rep} as the decoded Unicode character. *)

    val utf_8_byte_length : t -> int
    (** [utf_8_byte_length u] is the number of bytes needed to encode
    [u] in UTF-8. *)

    val utf_16_byte_length : t -> int
    (** [utf_16_byte_length u] is the number of bytes needed to encode
    [u] in UTF-16. *)
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

module Bytes = BytesLabels

module String = struct
  include StringLabels

  let hash (a : string) = Hashtbl.hash a [@@if ocaml_version < (5, 0, 0)]

  module Hashtbl = Hashtbl.Make (struct
    include String

    let hash = hash
  end)

  let is_empty = function
    | "" -> true
    | _ -> false

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
  include Int

  let hash (x : t) = x

  module Hashtbl = Hashtbl.Make (struct
    include Int

    let hash x = x
  end)
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

  let create' n = { arr = Array.make ((n / Sys.int_size) + 1) 0 }

  let size t = Array.length t.arr * Sys.int_size

  let mem t i =
    let arr = t.arr in
    let idx = i / Sys.int_size in
    let off = i mod Sys.int_size in
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
    let idx = i / Sys.int_size in
    let off = i mod Sys.int_size in
    let size = Array.length t.arr in
    if idx >= size then resize t idx;
    Array.unsafe_set t.arr idx (Array.unsafe_get t.arr idx lor (1 lsl off))

  let unset t i =
    let idx = i / Sys.int_size in
    let off = i mod Sys.int_size in
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

module In_channel = struct
  let stdlib_input_line = input_line

  (* Read up to [len] bytes into [buf], starting at [ofs]. Return total bytes
     read. *)
  let read_upto ic buf ofs len =
    let rec loop ofs len =
      if len = 0
      then ofs
      else
        let r = input ic buf ofs len in
        if r = 0 then ofs else loop (ofs + r) (len - r)
    in
    loop ofs len - ofs

  (* Best effort attempt to return a buffer with >= (ofs + n) bytes of storage,
     and such that it coincides with [buf] at indices < [ofs].

     The returned buffer is equal to [buf] itself if it already has sufficient
     free space.

     The returned buffer may have *fewer* than [ofs + n] bytes of storage if this
     number is > [Sys.max_string_length]. However the returned buffer will
     *always* have > [ofs] bytes of storage. In the limiting case when [ofs = len
     = Sys.max_string_length] (so that it is not possible to resize the buffer at
     all), an exception is raised. *)

  let ensure buf ofs n =
    let len = Bytes.length buf in
    if len >= ofs + n
    then buf
    else
      let new_len = ref len in
      while !new_len < ofs + n do
        new_len := (2 * !new_len) + 1
      done;
      let new_len = !new_len in
      let new_len =
        if new_len <= Sys.max_string_length
        then new_len
        else if ofs < Sys.max_string_length
        then Sys.max_string_length
        else
          failwith
            "In_channel.input_all: channel content is larger than maximum string length"
      in
      let new_buf = Bytes.create new_len in
      Bytes.blit ~src:buf ~src_pos:0 ~dst:new_buf ~dst_pos:0 ~len:ofs;
      new_buf

  let input_all ic =
    let chunk_size = 65536 in
    (* IO_BUFFER_SIZE *)
    let initial_size = try in_channel_length ic - pos_in ic with Sys_error _ -> -1 in
    let initial_size = if initial_size < 0 then chunk_size else initial_size in
    let initial_size =
      if initial_size <= Sys.max_string_length
      then initial_size
      else Sys.max_string_length
    in
    let buf = Bytes.create initial_size in
    let nread = read_upto ic buf 0 initial_size in
    if nread < initial_size
    then (* EOF reached, buffer partially filled *)
      Bytes.sub_string buf ~pos:0 ~len:nread
    else
      (* nread = initial_size, maybe EOF reached *)
      match input_char ic with
      | exception End_of_file ->
          (* EOF reached, buffer is completely filled *)
          Bytes.unsafe_to_string buf
      | c ->
          (* EOF not reached *)
          let rec loop buf ofs =
            let buf = ensure buf ofs chunk_size in
            let rem = Bytes.length buf - ofs in
            (* [rem] can be < [chunk_size] if buffer size close to
               [Sys.max_string_length] *)
            let r = read_upto ic buf ofs rem in
            if r < rem
            then (* EOF reached *)
              Bytes.sub_string buf ~pos:0 ~len:(ofs + r)
            else (* r = rem *)
              loop buf (ofs + rem)
          in
          let buf = ensure buf nread (chunk_size + 1) in
          Bytes.set buf nread c;
          loop buf (nread + 1)

  let input_lines ic =
    let rec aux acc =
      match input_line ic with
      | line -> aux (line :: acc)
      | exception End_of_file -> acc
    in
    List.rev (aux [])

  let input_line_exn = stdlib_input_line
end
[@@if ocaml_version < (4, 14, 0)]

module In_channel = struct
  let stdlib_input_line = input_line

  include In_channel

  (* [In_channel.input_lines] only exists in the stdlib since 5.1. *)
  let[@tail_mod_cons] rec input_lines ic =
    match stdlib_input_line ic with
    | line -> line :: input_lines ic
    | exception End_of_file -> []
  [@@if ocaml_version < (5, 1, 0)]

  let input_line_exn = stdlib_input_line
end
[@@if ocaml_version >= (4, 14, 0)]

module Seq = struct
  include Seq

  let rec mapi_aux f i xs () =
    match xs () with
    | Nil -> Nil
    | Cons (x, xs) -> Cons (f i x, mapi_aux f (i + 1) xs)

  (* Available since OCaml 4.14 *)
  let[@inline] mapi f xs = mapi_aux f 0 xs
end

let split_lines s =
  if String.equal s ""
  then []
  else
    let sep = '\n' in
    let r = ref [] in
    let j = ref (String.length s) in
    (* ignore trailing new line *)
    if Char.equal (String.unsafe_get s (!j - 1)) sep then decr j;
    for i = !j - 1 downto 0 do
      if Char.equal (String.unsafe_get s i) sep
      then (
        r := String.sub s ~pos:(i + 1) ~len:(!j - i - 1) :: !r;
        j := i)
    done;
    String.sub s ~pos:0 ~len:!j :: !r

let input_lines_read_once ic len = really_input_string ic len |> split_lines

let file_lines_bin fname =
  (* If possible, read the entire file and split it in lines.
     This is faster than reading it line by line.
     Otherwise, we fall back to a line-by-line read. *)
  let ic = open_in_bin fname in
  let len = in_channel_length ic in
  let x =
    if len < Sys.max_string_length
    then input_lines_read_once ic len
    else In_channel.input_lines ic
  in
  close_in ic;
  x

let file_lines_text file =
  let ic = open_in_text file in
  let c = In_channel.input_lines ic in
  close_in ic;
  c

let generated_name = function
  | "param" | "match" | "switcher" -> true
  | s -> String.starts_with ~prefix:"cst_" s

module Hashtbl = struct
  include Hashtbl

  let (create [@deprecated "Use Int.Hashtbl, String.Hashtbl, Var.Hashtbl, Addr.Hashtbl"])
      =
    Hashtbl.create

  let (of_seq [@deprecated "Use Int.Hashtbl, String.Hashtbl, Var.Hashtbl, Addr.Hashtbl"])
      =
    Hashtbl.of_seq
end
