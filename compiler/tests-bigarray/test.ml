open! Stdlib
open StdLabels
open Bigarray

type pack_kind =
  | Pack_kind : ('a, 'b) kind * (string -> 'a) * ('a -> string) * 'a list -> pack_kind

type pack_layout = Pack_layout : 'a layout -> pack_layout

let all_layout = [Pack_layout C_layout; Pack_layout Fortran_layout]

let layout_to_string (type t) (layout : t layout) =
  match layout with
  | C_layout -> "C_layout"
  | Fortran_layout -> "Fortran_layout"

let char_of_string s =
  if String.length s <> 1 then failwith "char_of_string";
  s.[0]

let string_of_char c = String.make 1 c

let float_sample = [nan; infinity; neg_infinity; 0.; -0.; 3.14; 1e309]

let int8_sample = [0; 128]

let int16_sample = [0; 128; 65536]

let int32_sample = [0l; 128l; 65537l; 2147483647l]

let int64_sample = [0L; 128L; 65537L; 2147483647L]

let int_sample = [0; 128; 65537; 123123423]

let nativeint_sample = [0n; 128n; 65537n; 123123423n]

let char_sample = ['\000'; '\020'; '\120'; '\255']

module Complex = struct
  type t = Complex.t =
    { re : float
    ; im : float }

  let to_string {re; im} = Printf.sprintf "%f+%fi" re im

  let of_string s =
    match String.split_on_char ~sep:'+' s with
    | [x] ->
        if x.[String.length x - 1] = 'i'
        then
          {re = 0.; im = Float.of_string (String.sub x ~pos:0 ~len:(String.length x - 1))}
        else {re = Float.of_string x; im = 0.}
    | [x; y] ->
        assert (y.[String.length y - 1] = 'i');
        { re = Float.of_string x
        ; im = Float.of_string (String.sub y ~pos:0 ~len:(String.length y - 1)) }
    | _ -> assert false
end

module Char = struct
  type t = char

  let to_string c = Char.code c |> string_of_int

  let of_string c = int_of_string c |> Char.chr
end

let all_kind =
  [ Pack_kind (Float32, float_of_string, string_of_float, float_sample)
  ; Pack_kind (Float64, float_of_string, string_of_float, float_sample)
  ; Pack_kind (Int8_signed, int_of_string, string_of_int, int8_sample)
  ; Pack_kind (Int8_unsigned, int_of_string, string_of_int, int8_sample)
  ; Pack_kind (Int16_signed, int_of_string, string_of_int, int16_sample)
  ; Pack_kind (Int16_unsigned, int_of_string, string_of_int, int16_sample)
  ; Pack_kind (Int32, Int32.of_string, Int32.to_string, int32_sample)
  ; Pack_kind (Int64, Int64.of_string, Int64.to_string, int64_sample)
  ; Pack_kind (Int, int_of_string, string_of_int, int_sample)
  ; Pack_kind (Nativeint, Nativeint.of_string, Nativeint.to_string, nativeint_sample)
  ; Pack_kind (Complex32, Complex.of_string, Complex.to_string, [])
  ; Pack_kind (Complex64, Complex.of_string, Complex.to_string, [])
  ; Pack_kind (Char, Char.of_string, Char.to_string, char_sample) ]

let kind_to_string (type a b) (kind : (a, b) kind) =
  match kind with
  | Float32 -> "Float32"
  | Float64 -> "Float64"
  | Int8_signed -> "Int8_signed"
  | Int8_unsigned -> "Int8_unsigned"
  | Int16_signed -> "Int16_signed"
  | Int16_unsigned -> "Int16_unsigned"
  | Int32 -> "Int32"
  | Int64 -> "Int64"
  | Int -> "Int"
  | Nativeint -> "Nativeint"
  | Complex32 -> "Complex32"
  | Complex64 -> "Complex64"
  | Char -> "Char"

type pack =
  | Pack :
      { layout : 'a layout
      ; kind : ('b, 'c) kind
      ; to_string : 'b -> string
      ; of_string : string -> 'b
      ; sample : 'b list }
      -> pack

let all =
  List.map all_kind ~f:(fun (Pack_kind (kind, of_string, to_string, sample)) ->
      List.map all_layout ~f:(fun (Pack_layout layout) ->
          Pack {layout; kind; to_string; of_string; sample}))
  |> List.flatten

module Genarray = struct
  module M = Genarray

  module Caml_ba_create = struct
    let a = M.create Float32 C_layout [|1|]
  end

  module Caml_ba_get_generic = struct end

  module Caml_ba_set_generic = struct end

  module Caml_ba_num_dims = struct end

  module Caml_ba_dim = struct end

  module Caml_ba_kind = struct end

  module Caml_ba_layout = struct end

  module Caml_ba_change_layout = struct end

  module Caml_ba_sub = struct end

  module Caml_ba_slice = struct end

  module Caml_ba_blit = struct end

  module Caml_ba_fill = struct end
end

module Array0 = struct
  let%expect_test "array" =
    List.iter all ~f:(fun (Pack {layout; kind; to_string; of_string; sample}) ->
        Printf.printf "Layout: %s\n" (layout_to_string layout);
        Printf.printf "Kind: %s\n" (kind_to_string kind);
        try
          let print_a a =
            Printf.printf "{| ";
            Printf.printf "%S " (to_string (Array0.get a));
            Printf.printf "|}\n"
          in
          let a = Array0.create kind layout in
          Array0.fill a (of_string "0");
          assert (kind = Array0.kind a);
          assert (layout = Array0.layout a);
          (match sample with
          | x :: y :: _ ->
              let a1 = Array0.of_value kind layout x in
              let a2 = Array0.of_value kind layout x in
              (* We expect a difference here *)
              if kind_to_string kind <> kind_to_string Int
                 && kind_to_string kind <> kind_to_string Nativeint
              then Printf.printf "Size in bits: %d\n" (Array0.size_in_bytes a1);
              print_a a1;
              if Array0.get a1 <> x
              then
                Printf.printf "ERR: %S <> %S\n" (to_string x) (to_string (Array0.get a1));
              Array0.set a1 y;
              if Array0.get a1 <> y
              then
                Printf.printf "ERR: %S <> %S\n" (to_string y) (to_string (Array0.get a1));
              print_a a1;
              print_a a2;
              Printf.printf "blit\n";
              Array0.blit a1 a2;
              print_a a1;
              print_a a2;
              Array0.fill a1 x;
              print_a a1;
              print_a a2
          | _ :: _ | [] -> ());
          Printf.printf "\n"
        with e -> Printf.printf "Error: %s\n" (Printexc.to_string e));
    [%expect
      {xxx|
      Layout: C_layout
      Kind: Float32
      Size in bits: 4
      {| "nan" |}
      ERR: "nan" <> "nan"
      {| "inf" |}
      {| "nan" |}
      blit
      {| "inf" |}
      {| "inf" |}
      {| "nan" |}
      {| "inf" |}

      Layout: Fortran_layout
      Kind: Float32
      Size in bits: 4
      {| "nan" |}
      ERR: "nan" <> "nan"
      {| "inf" |}
      {| "nan" |}
      blit
      {| "inf" |}
      {| "inf" |}
      {| "nan" |}
      {| "inf" |}

      Layout: C_layout
      Kind: Float64
      Size in bits: 8
      {| "nan" |}
      ERR: "nan" <> "nan"
      {| "inf" |}
      {| "nan" |}
      blit
      {| "inf" |}
      {| "inf" |}
      {| "nan" |}
      {| "inf" |}

      Layout: Fortran_layout
      Kind: Float64
      Size in bits: 8
      {| "nan" |}
      ERR: "nan" <> "nan"
      {| "inf" |}
      {| "nan" |}
      blit
      {| "inf" |}
      {| "inf" |}
      {| "nan" |}
      {| "inf" |}

      Layout: C_layout
      Kind: Int8_signed
      Size in bits: 1
      {| "0" |}
      ERR: "128" <> "-128"
      {| "-128" |}
      {| "0" |}
      blit
      {| "-128" |}
      {| "-128" |}
      {| "0" |}
      {| "-128" |}

      Layout: Fortran_layout
      Kind: Int8_signed
      Size in bits: 1
      {| "0" |}
      ERR: "128" <> "-128"
      {| "-128" |}
      {| "0" |}
      blit
      {| "-128" |}
      {| "-128" |}
      {| "0" |}
      {| "-128" |}

      Layout: C_layout
      Kind: Int8_unsigned
      Size in bits: 1
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: Fortran_layout
      Kind: Int8_unsigned
      Size in bits: 1
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: C_layout
      Kind: Int16_signed
      Size in bits: 2
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: Fortran_layout
      Kind: Int16_signed
      Size in bits: 2
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: C_layout
      Kind: Int16_unsigned
      Size in bits: 2
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: Fortran_layout
      Kind: Int16_unsigned
      Size in bits: 2
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: C_layout
      Kind: Int32
      Size in bits: 4
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: Fortran_layout
      Kind: Int32
      Size in bits: 4
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: C_layout
      Kind: Int64
      Size in bits: 8
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: Fortran_layout
      Kind: Int64
      Size in bits: 8
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: C_layout
      Kind: Int
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: Fortran_layout
      Kind: Int
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: C_layout
      Kind: Nativeint
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: Fortran_layout
      Kind: Nativeint
      {| "0" |}
      {| "128" |}
      {| "0" |}
      blit
      {| "128" |}
      {| "128" |}
      {| "0" |}
      {| "128" |}

      Layout: C_layout
      Kind: Complex32

      Layout: Fortran_layout
      Kind: Complex32

      Layout: C_layout
      Kind: Complex64

      Layout: Fortran_layout
      Kind: Complex64

      Layout: C_layout
      Kind: Char
      Size in bits: 1
      {| "0" |}
      {| "20" |}
      {| "0" |}
      blit
      {| "20" |}
      {| "20" |}
      {| "0" |}
      {| "20" |}

      Layout: Fortran_layout
      Kind: Char
      Size in bits: 1
      {| "0" |}
      {| "20" |}
      {| "0" |}
      blit
      {| "20" |}
      {| "20" |}
      {| "0" |}
      {| "20" |} |xxx}]

  module Caml_ba_kind = struct end

  module Caml_ba_layout = struct end

  module Caml_ba_change_layout = struct end

  module Caml_ba_blit = struct end

  module Caml_ba_fill = struct end
end

module Array1 = struct
  let%expect_test "array" =
    List.iter all ~f:(fun (Pack {layout; kind; to_string; of_string; sample}) ->
        Printf.printf "Layout: %s\n%!" (layout_to_string layout);
        Printf.printf "Kind: %s\n%!" (kind_to_string kind);
        try
          let print_a a =
            let offset =
              match Pack_layout layout with
              | Pack_layout Fortran_layout -> 1
              | Pack_layout C_layout -> 0
            in
            Printf.printf "{| ";
            for i = 0 to Array1.dim a - 1 do
              Printf.printf "%S " (to_string (Array1.get a (i + offset)))
            done;
            Printf.printf "|}\n"
          in
          let a = Array1.create kind layout 5 in
          Array1.fill a (of_string "0");
          assert (kind = Array1.kind a);
          assert (layout = Array1.layout a);
          (match sample with
          | x :: y :: _ ->
              let a1 = Array1.create kind layout 5 in
              Array1.fill a1 (of_string "0");
              let a2 = Array1.create kind layout 5 in
              Array1.fill a2 (of_string "0");
              (* We expect a difference here *)
              if kind_to_string kind <> kind_to_string Int
                 && kind_to_string kind <> kind_to_string Nativeint
              then Printf.printf "Size in bits: %d\n" (Array1.size_in_bytes a1);
              print_a a1;
              if Array1.get a1 2 <> x
              then
                Printf.printf
                  "ERR: %s <> %s\n"
                  (to_string x)
                  (to_string (Array1.get a1 2));
              Array1.set a1 2 y;
              if Array1.get a1 2 <> y
              then
                Printf.printf
                  "ERR: %s <> %s\n"
                  (to_string y)
                  (to_string (Array1.get a1 2));
              print_a a1;
              print_a a2;
              Printf.printf "blit2\n";
              Array1.blit a1 a2;
              print_a a1;
              print_a a2;
              Array1.fill a1 x;
              print_a a1;
              print_a a2
          | _ :: _ | [] -> ());
          Printf.printf "\n"
        with e -> Printf.printf "Error: %s\n" (Printexc.to_string e));
    [%expect
      {xxx|
      Layout: C_layout
      Kind: Float32
      Size in bits: 20
      {| "0." "0." "0." "0." "0." |}
      ERR: nan <> 0.
      {| "0." "0." "inf" "0." "0." |}
      {| "0." "0." "0." "0." "0." |}
      blit2
      {| "0." "0." "inf" "0." "0." |}
      {| "0." "0." "inf" "0." "0." |}
      {| "nan" "nan" "nan" "nan" "nan" |}
      {| "0." "0." "inf" "0." "0." |}

      Layout: Fortran_layout
      Kind: Float32
      Size in bits: 20
      {| "0." "0." "0." "0." "0." |}
      ERR: nan <> 0.
      {| "0." "inf" "0." "0." "0." |}
      {| "0." "0." "0." "0." "0." |}
      blit2
      {| "0." "inf" "0." "0." "0." |}
      {| "0." "inf" "0." "0." "0." |}
      {| "nan" "nan" "nan" "nan" "nan" |}
      {| "0." "inf" "0." "0." "0." |}

      Layout: C_layout
      Kind: Float64
      Size in bits: 40
      {| "0." "0." "0." "0." "0." |}
      ERR: nan <> 0.
      {| "0." "0." "inf" "0." "0." |}
      {| "0." "0." "0." "0." "0." |}
      blit2
      {| "0." "0." "inf" "0." "0." |}
      {| "0." "0." "inf" "0." "0." |}
      {| "nan" "nan" "nan" "nan" "nan" |}
      {| "0." "0." "inf" "0." "0." |}

      Layout: Fortran_layout
      Kind: Float64
      Size in bits: 40
      {| "0." "0." "0." "0." "0." |}
      ERR: nan <> 0.
      {| "0." "inf" "0." "0." "0." |}
      {| "0." "0." "0." "0." "0." |}
      blit2
      {| "0." "inf" "0." "0." "0." |}
      {| "0." "inf" "0." "0." "0." |}
      {| "nan" "nan" "nan" "nan" "nan" |}
      {| "0." "inf" "0." "0." "0." |}

      Layout: C_layout
      Kind: Int8_signed
      Size in bits: 5
      {| "0" "0" "0" "0" "0" |}
      ERR: 128 <> -128
      {| "0" "0" "-128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "0" "-128" "0" "0" |}
      {| "0" "0" "-128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "-128" "0" "0" |}

      Layout: Fortran_layout
      Kind: Int8_signed
      Size in bits: 5
      {| "0" "0" "0" "0" "0" |}
      ERR: 128 <> -128
      {| "0" "-128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "-128" "0" "0" "0" |}
      {| "0" "-128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "-128" "0" "0" "0" |}

      Layout: C_layout
      Kind: Int8_unsigned
      Size in bits: 5
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}

      Layout: Fortran_layout
      Kind: Int8_unsigned
      Size in bits: 5
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "128" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}

      Layout: C_layout
      Kind: Int16_signed
      Size in bits: 10
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}

      Layout: Fortran_layout
      Kind: Int16_signed
      Size in bits: 10
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "128" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}

      Layout: C_layout
      Kind: Int16_unsigned
      Size in bits: 10
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}

      Layout: Fortran_layout
      Kind: Int16_unsigned
      Size in bits: 10
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "128" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}

      Layout: C_layout
      Kind: Int32
      Size in bits: 20
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}

      Layout: Fortran_layout
      Kind: Int32
      Size in bits: 20
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "128" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}

      Layout: C_layout
      Kind: Int64
      Size in bits: 40
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}

      Layout: Fortran_layout
      Kind: Int64
      Size in bits: 40
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "128" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}

      Layout: C_layout
      Kind: Int
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}

      Layout: Fortran_layout
      Kind: Int
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "128" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}

      Layout: C_layout
      Kind: Nativeint
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "128" "0" "0" |}

      Layout: Fortran_layout
      Kind: Nativeint
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "128" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "128" "0" "0" "0" |}

      Layout: C_layout
      Kind: Complex32

      Layout: Fortran_layout
      Kind: Complex32

      Layout: C_layout
      Kind: Complex64

      Layout: Fortran_layout
      Kind: Complex64

      Layout: C_layout
      Kind: Char
      Size in bits: 5
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "20" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "0" "20" "0" "0" |}
      {| "0" "0" "20" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "0" "20" "0" "0" |}

      Layout: Fortran_layout
      Kind: Char
      Size in bits: 5
      {| "0" "0" "0" "0" "0" |}
      {| "0" "20" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      blit2
      {| "0" "20" "0" "0" "0" |}
      {| "0" "20" "0" "0" "0" |}
      {| "0" "0" "0" "0" "0" |}
      {| "0" "20" "0" "0" "0" |} |xxx}]

  module Caml_ba_ref_1 = struct end

  module Caml_ba_set_1 = struct end

  module Caml_ba_unsafe_ref_1 = struct end

  module Caml_ba_unsafe_set_1 = struct end

  module Caml_ba_dim_1 = struct end

  module Caml_ba_kind = struct end

  module Caml_ba_layout = struct end

  module Caml_ba_change_layout = struct end

  module Caml_ba_sub = struct end

  module Caml_ba_blit = struct end

  module Caml_ba_fill = struct end
end

module Array2 = struct
  let%expect_test "array" =
    List.iter all ~f:(fun (Pack {layout; kind; to_string; of_string; sample}) ->
        Printf.printf "Layout: %s\n%!" (layout_to_string layout);
        Printf.printf "Kind: %s\n%!" (kind_to_string kind);
        try
          let print_a a =
            let offset =
              match Pack_layout layout with
              | Pack_layout Fortran_layout -> 1
              | Pack_layout C_layout -> 0
            in
            Printf.printf "{| ";
            for i = 0 to Array2.dim1 a - 1 do
              if i <> 0 then Printf.printf "   ";
              Printf.printf "{| ";
              for j = 0 to Array2.dim2 a - 1 do
                Printf.printf "%S " (to_string (Array2.get a (i + offset) (j + offset)))
              done;
              Printf.printf "|}\n"
            done;
            Printf.printf "|}\n"
          in
          let a = Array2.create kind layout 5 8 in
          Array2.fill a (of_string "0");
          assert (kind = Array2.kind a);
          assert (layout = Array2.layout a);
          (match sample with
          | x :: y :: _ ->
              let a1 = Array2.create kind layout 5 10 in
              Array2.fill a1 (of_string "0");
              let a2 = Array2.create kind layout 5 10 in
              Array2.fill a2 (of_string "0");
              (* We expect a difference here *)
              if kind_to_string kind <> kind_to_string Int
                 && kind_to_string kind <> kind_to_string Nativeint
              then Printf.printf "Size in bits: %d\n" (Array2.size_in_bytes a1);
              print_a a1;
              if Array2.get a1 2 3 <> x
              then
                Printf.printf
                  "ERR: %s <> %s\n"
                  (to_string x)
                  (to_string (Array2.get a1 2 3));
              Array2.set a1 2 3 y;
              if Array2.get a1 2 3 <> y
              then
                Printf.printf
                  "ERR: %s <> %s\n"
                  (to_string y)
                  (to_string (Array2.get a1 2 3));
              print_a a1;
              print_a a2;
              Printf.printf "blit\n";
              Array2.blit a1 a2;
              print_a a1;
              print_a a2;
              Array2.fill a1 x;
              print_a a1;
              print_a a2
          | _ :: _ | [] -> ());
          Printf.printf "\n"
        with e -> Printf.printf "Error: %s\n" (Printexc.to_string e));
    [%expect
      {xxx|
      Layout: C_layout
      Kind: Float32
      Size in bits: 200
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      ERR: nan <> 0.
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "inf" "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      blit
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "inf" "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "inf" "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      {| {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
      |}
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "inf" "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}

      Layout: Fortran_layout
      Kind: Float32
      Size in bits: 200
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      ERR: nan <> 0.
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "inf" "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      blit
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "inf" "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "inf" "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      {| {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
      |}
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "inf" "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}

      Layout: C_layout
      Kind: Float64
      Size in bits: 400
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      ERR: nan <> 0.
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "inf" "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      blit
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "inf" "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "inf" "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      {| {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
      |}
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "inf" "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}

      Layout: Fortran_layout
      Kind: Float64
      Size in bits: 400
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      ERR: nan <> 0.
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "inf" "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      blit
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "inf" "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "inf" "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}
      {| {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
         {| "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" "nan" |}
      |}
      {| {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "inf" "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
         {| "0." "0." "0." "0." "0." "0." "0." "0." "0." "0." |}
      |}

      Layout: C_layout
      Kind: Int8_signed
      Size in bits: 50
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      ERR: 128 <> -128
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "-128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "-128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "-128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "-128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: Fortran_layout
      Kind: Int8_signed
      Size in bits: 50
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      ERR: 128 <> -128
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "-128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "-128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "-128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "-128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: C_layout
      Kind: Int8_unsigned
      Size in bits: 50
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: Fortran_layout
      Kind: Int8_unsigned
      Size in bits: 50
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: C_layout
      Kind: Int16_signed
      Size in bits: 100
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: Fortran_layout
      Kind: Int16_signed
      Size in bits: 100
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: C_layout
      Kind: Int16_unsigned
      Size in bits: 100
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: Fortran_layout
      Kind: Int16_unsigned
      Size in bits: 100
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: C_layout
      Kind: Int32
      Size in bits: 200
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: Fortran_layout
      Kind: Int32
      Size in bits: 200
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: C_layout
      Kind: Int64
      Size in bits: 400
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: Fortran_layout
      Kind: Int64
      Size in bits: 400
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: C_layout
      Kind: Int
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: Fortran_layout
      Kind: Int
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: C_layout
      Kind: Nativeint
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "128" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: Fortran_layout
      Kind: Nativeint
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "128" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: C_layout
      Kind: Complex32

      Layout: Fortran_layout
      Kind: Complex32

      Layout: C_layout
      Kind: Complex64

      Layout: Fortran_layout
      Kind: Complex64

      Layout: C_layout
      Kind: Char
      Size in bits: 50
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "20" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "20" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "20" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "20" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}

      Layout: Fortran_layout
      Kind: Char
      Size in bits: 50
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "20" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      blit
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "20" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "20" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |}
      {| {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "20" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
         {| "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" |}
      |} |xxx}]

  module Caml_ba_ref_2 = struct end

  module Caml_ba_set_2 = struct end

  module Caml_ba_unsafe_ref_2 = struct end

  module Caml_ba_unsafe_set_2 = struct end

  module Caml_ba_dim_1 = struct end

  module Caml_ba_dim_2 = struct end

  module Caml_ba_kind = struct end

  module Caml_ba_layout = struct end

  module Caml_ba_change_layout = struct end

  module Caml_ba_sub = struct end

  module Caml_ba_blit = struct end

  module Caml_ba_fill = struct end
end

module Array3 = struct
  let%expect_test "array" =
    List.iter all ~f:(fun (Pack {layout; kind; to_string; of_string; sample}) ->
        Printf.printf "Layout: %s\n" (layout_to_string layout);
        Printf.printf "Kind: %s\n" (kind_to_string kind);
        try
          let print_a a =
            let offset =
              match Pack_layout layout with
              | Pack_layout Fortran_layout -> 1
              | Pack_layout C_layout -> 0
            in
            Printf.printf "{| ";
            for i = 0 to Array3.dim1 a - 1 do
              if i <> 0 then Printf.printf "   ";
              Printf.printf "{| ";
              for j = 0 to Array3.dim2 a - 1 do
                if j <> 0 then Printf.printf "      ";
                Printf.printf "{| ";
                for k = 0 to Array3.dim3 a - 1 do
                  Printf.printf
                    "%S "
                    (to_string (Array3.get a (i + offset) (j + offset) (k + offset)))
                done;
                Printf.printf "|}\n"
              done;
              Printf.printf "   |}\n"
            done;
            Printf.printf "|}\n"
          in
          let a = Array3.create kind layout 5 8 13 in
          Array3.fill a (of_string "0");
          assert (kind = Array3.kind a);
          assert (layout = Array3.layout a);
          (match sample with
          | x :: y :: _ -> (
              let a1 = Array3.create kind layout 5 10 3 in
              Array3.fill a1 (of_string "0");
              let a2 = Array3.create kind layout 5 10 3 in
              Array3.fill a2 (of_string "0");
              Array3.fill a1 (of_string "0");
              Array3.fill a2 (of_string "0");
              (* We expect a difference here *)
              if kind_to_string kind <> kind_to_string Int
                 && kind_to_string kind <> kind_to_string Nativeint
              then Printf.printf "Size in bits: %d\n" (Array3.size_in_bytes a1);
              print_a a1;
              if Array3.get a1 2 3 1 <> x
              then
                Printf.printf
                  "ERR: %s <> %s\n"
                  (to_string x)
                  (to_string (Array3.get a1 2 3 1));
              Array3.set a1 2 3 1 y;
              if Array3.get a1 2 3 1 <> y
              then
                Printf.printf
                  "ERR: %s <> %s\n"
                  (to_string y)
                  (to_string (Array3.get a1 2 3 1));
              print_a a1;
              print_a a2;
              Printf.printf "blit\n";
              Array3.blit a1 a2;
              print_a a1;
              print_a a2;
              Array3.fill a1 x;
              print_a a1;
              print_a a2;
              match Array3.layout a1 with
              | C_layout ->
                  print_endline "sub-c";
                  for i = 0 to Array3.dim1 a1 - 1 do
                    for j = 0 to Array3.dim2 a1 - 1 do
                      for k = 0 to Array3.dim3 a1 - 1 do
                        Array3.set
                          a1
                          i
                          j
                          k
                          (string_of_int (((i + (2 * j) + (4 * k)) mod 11) + 1)
                          |> of_string)
                      done
                    done
                  done;
                  let a3 = Array3.sub_left a1 1 3 in
                  print_a a3
              | Fortran_layout ->
                  let a3 = Array3.sub_right a1 2 2 in
                  print_endline "sub-fortran";
                  Array3.set a3 1 1 1 (of_string "2");
                  Array3.set a3 1 2 1 (of_string "3");
                  print_a a1)
          | _ :: _ | [] -> ());
          Printf.printf "\n"
        with e -> Printf.printf "Error: %s\n" (Printexc.to_string e));
    [%expect
      {xxx|
      Layout: C_layout
      Kind: Float32
      Size in bits: 600
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      ERR: nan <> 0.
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "inf" "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      blit
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "inf" "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "inf" "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      {| {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
      |}
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "inf" "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      sub-c
      {| {| {| "2." "6." "10." |}
            {| "4." "8." "1." |}
            {| "6." "10." "3." |}
            {| "8." "1." "5." |}
            {| "10." "3." "7." |}
            {| "1." "5." "9." |}
            {| "3." "7." "11." |}
            {| "5." "9." "2." |}
            {| "7." "11." "4." |}
            {| "9." "2." "6." |}
         |}
         {| {| "3." "7." "11." |}
            {| "5." "9." "2." |}
            {| "7." "11." "4." |}
            {| "9." "2." "6." |}
            {| "11." "4." "8." |}
            {| "2." "6." "10." |}
            {| "4." "8." "1." |}
            {| "6." "10." "3." |}
            {| "8." "1." "5." |}
            {| "10." "3." "7." |}
         |}
         {| {| "4." "8." "1." |}
            {| "6." "10." "3." |}
            {| "8." "1." "5." |}
            {| "10." "3." "7." |}
            {| "1." "5." "9." |}
            {| "3." "7." "11." |}
            {| "5." "9." "2." |}
            {| "7." "11." "4." |}
            {| "9." "2." "6." |}
            {| "11." "4." "8." |}
         |}
      |}

      Layout: Fortran_layout
      Kind: Float32
      Size in bits: 600
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      ERR: nan <> 0.
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "inf" "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      blit
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "inf" "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "inf" "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      {| {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
      |}
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "inf" "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      sub-fortran
      {| {| {| "nan" "2." "nan" |}
            {| "nan" "3." "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
      |}

      Layout: C_layout
      Kind: Float64
      Size in bits: 1200
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      ERR: nan <> 0.
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "inf" "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      blit
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "inf" "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "inf" "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      {| {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
      |}
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "inf" "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      sub-c
      {| {| {| "2." "6." "10." |}
            {| "4." "8." "1." |}
            {| "6." "10." "3." |}
            {| "8." "1." "5." |}
            {| "10." "3." "7." |}
            {| "1." "5." "9." |}
            {| "3." "7." "11." |}
            {| "5." "9." "2." |}
            {| "7." "11." "4." |}
            {| "9." "2." "6." |}
         |}
         {| {| "3." "7." "11." |}
            {| "5." "9." "2." |}
            {| "7." "11." "4." |}
            {| "9." "2." "6." |}
            {| "11." "4." "8." |}
            {| "2." "6." "10." |}
            {| "4." "8." "1." |}
            {| "6." "10." "3." |}
            {| "8." "1." "5." |}
            {| "10." "3." "7." |}
         |}
         {| {| "4." "8." "1." |}
            {| "6." "10." "3." |}
            {| "8." "1." "5." |}
            {| "10." "3." "7." |}
            {| "1." "5." "9." |}
            {| "3." "7." "11." |}
            {| "5." "9." "2." |}
            {| "7." "11." "4." |}
            {| "9." "2." "6." |}
            {| "11." "4." "8." |}
         |}
      |}

      Layout: Fortran_layout
      Kind: Float64
      Size in bits: 1200
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      ERR: nan <> 0.
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "inf" "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      blit
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "inf" "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "inf" "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      {| {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
      |}
      {| {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "inf" "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
         {| {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
            {| "0." "0." "0." |}
         |}
      |}
      sub-fortran
      {| {| {| "nan" "2." "nan" |}
            {| "nan" "3." "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
         {| {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
            {| "nan" "nan" "nan" |}
         |}
      |}

      Layout: C_layout
      Kind: Int8_signed
      Size in bits: 150
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      ERR: 128 <> -128
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "-128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "-128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "-128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "-128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-c
      {| {| {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
         |}
         {| {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
            {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
         |}
         {| {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
         |}
      |}

      Layout: Fortran_layout
      Kind: Int8_signed
      Size in bits: 150
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      ERR: 128 <> -128
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "-128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "-128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "-128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "-128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-fortran
      {| {| {| "0" "2" "0" |}
            {| "0" "3" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}

      Layout: C_layout
      Kind: Int8_unsigned
      Size in bits: 150
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-c
      {| {| {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
         |}
         {| {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
            {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
         |}
         {| {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
         |}
      |}

      Layout: Fortran_layout
      Kind: Int8_unsigned
      Size in bits: 150
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-fortran
      {| {| {| "0" "2" "0" |}
            {| "0" "3" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}

      Layout: C_layout
      Kind: Int16_signed
      Size in bits: 300
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-c
      {| {| {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
         |}
         {| {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
            {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
         |}
         {| {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
         |}
      |}

      Layout: Fortran_layout
      Kind: Int16_signed
      Size in bits: 300
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-fortran
      {| {| {| "0" "2" "0" |}
            {| "0" "3" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}

      Layout: C_layout
      Kind: Int16_unsigned
      Size in bits: 300
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-c
      {| {| {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
         |}
         {| {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
            {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
         |}
         {| {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
         |}
      |}

      Layout: Fortran_layout
      Kind: Int16_unsigned
      Size in bits: 300
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-fortran
      {| {| {| "0" "2" "0" |}
            {| "0" "3" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}

      Layout: C_layout
      Kind: Int32
      Size in bits: 600
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-c
      {| {| {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
         |}
         {| {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
            {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
         |}
         {| {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
         |}
      |}

      Layout: Fortran_layout
      Kind: Int32
      Size in bits: 600
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-fortran
      {| {| {| "0" "2" "0" |}
            {| "0" "3" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}

      Layout: C_layout
      Kind: Int64
      Size in bits: 1200
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-c
      {| {| {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
         |}
         {| {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
            {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
         |}
         {| {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
         |}
      |}

      Layout: Fortran_layout
      Kind: Int64
      Size in bits: 1200
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-fortran
      {| {| {| "0" "2" "0" |}
            {| "0" "3" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}

      Layout: C_layout
      Kind: Int
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-c
      {| {| {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
         |}
         {| {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
            {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
         |}
         {| {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
         |}
      |}

      Layout: Fortran_layout
      Kind: Int
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-fortran
      {| {| {| "0" "2" "0" |}
            {| "0" "3" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}

      Layout: C_layout
      Kind: Nativeint
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "128" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-c
      {| {| {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
         |}
         {| {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
            {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
         |}
         {| {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
         |}
      |}

      Layout: Fortran_layout
      Kind: Nativeint
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "128" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-fortran
      {| {| {| "0" "2" "0" |}
            {| "0" "3" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}

      Layout: C_layout
      Kind: Complex32

      Layout: Fortran_layout
      Kind: Complex32

      Layout: C_layout
      Kind: Complex64

      Layout: Fortran_layout
      Kind: Complex64

      Layout: C_layout
      Kind: Char
      Size in bits: 150
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "20" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "20" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "20" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "20" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-c
      {| {| {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
         |}
         {| {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
            {| "2" "6" "10" |}
            {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
         |}
         {| {| "4" "8" "1" |}
            {| "6" "10" "3" |}
            {| "8" "1" "5" |}
            {| "10" "3" "7" |}
            {| "1" "5" "9" |}
            {| "3" "7" "11" |}
            {| "5" "9" "2" |}
            {| "7" "11" "4" |}
            {| "9" "2" "6" |}
            {| "11" "4" "8" |}
         |}
      |}

      Layout: Fortran_layout
      Kind: Char
      Size in bits: 150
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "20" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      blit
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "20" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "20" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      {| {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "20" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |}
      sub-fortran
      {| {| {| "0" "2" "0" |}
            {| "0" "3" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
         {| {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
            {| "0" "0" "0" |}
         |}
      |} |xxx}]

  module Caml_ba_ref_3 = struct end

  module Caml_ba_set_3 = struct end

  module Caml_ba_unsafe_ref_3 = struct end

  module Caml_ba_unsafe_set_3 = struct end

  module Caml_ba_dim_1 = struct end

  module Caml_ba_dim_2 = struct end

  module Caml_ba_dim_3 = struct end

  module Caml_ba_kind = struct end

  module Caml_ba_layout = struct end

  module Caml_ba_change_layout = struct end

  module Caml_ba_sub = struct end

  module Caml_ba_blit = struct end

  module Caml_ba_fill = struct end
end

module Caml_ba_reshape = struct end
