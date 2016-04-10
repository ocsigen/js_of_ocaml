open Deriving_Json

let test name v =
  Printf.printf "%s = %s\n%!" name v

let test' _to _from name  v =
  let b = Buffer.create 17 in
  let () = _to b v in
  let str = Buffer.contents b in
  Printf.printf "%s = %s\n%!" name str

let () = test "char" (Json_char.to_string '4')

let () = test "bool" (Json_bool.to_string true)

let () = test "bool" (Json_bool.to_string false)

let () = test "unit" (Json_unit.to_string ())

let () = test "int" (Json_int.to_string 42)

let () = test "int32" (Json_int32.to_string 42l)

let () = test "int64" (Json_int64.to_string 42L)

(* let () = test "nativeint" (Json_nativeint.to_string 42n);; *)

let () = test "float" (Json_float.to_string  42.42)

let () = test "string" (Json_string.to_string  "42")

module Int_list   = Json_list(Json_int)

module Int_ref    = Json_ref(Json_int)

module Int_option = Json_option(Json_int)

module Int_array  = Json_array(Json_int)

let () = test "int_list" (Int_list.to_string  [4;2;42])

let () = test "int_ref" (Int_ref.to_string (ref 42))

let () = test "int_option" (Int_option.to_string (Some 42))

let () = test "int_option" (Int_option.to_string None)

let () = test "int_array" (Int_array.to_string [|4;2;42;24|])

type tuple1 = (int * string) [@@deriving json]

type variant1 = A | B | C | D of variant1 [@@deriving json]

type variant2 = D of string | E of variant1 [@@deriving json]

type record1 = { f : variant1; g : variant2; h : record1 option } [@@deriving json]

type poly1 = [`A | `B of string] [@@deriving json]

type poly2 = [poly1 | `C of int] [@@deriving json];;

let () = test' tuple1_to_json tuple1_of_json "tuple1" ((42,"42"))

let () = test' variant1_to_json variant1_of_json "variant1 A" (A)

let () = test' variant1_to_json variant1_of_json "variant1 B" (B)

let () = test' variant1_to_json variant1_of_json "variant1 C" (C)

let () = test' variant2_to_json variant2_of_json "variant2 D 'hello'" ((D "hello"))

let () = test' variant2_to_json variant2_of_json "variant2 E A" ((E A))

let () = test' record1_to_json record1_of_json "record1" ({f = A; g = D "d"; h = None })

let () = test' record1_to_json record1_of_json "record1"
           ({f = A;
             g = D "d";
             h = Some {f = B;
                       g = E (D B);
                       h = None }
            })

let () = test' poly1_to_json poly1_of_json "poly1 `A" (`A)

let () = test' poly1_to_json poly1_of_json "poly1 `B str" ((`B "str"))

let () = test' poly2_to_json poly2_of_json "poly2 `A" (`A)

let () = test' poly2_to_json poly2_of_json "poly2 `B str" ((`B "str"))

let () = test' poly2_to_json poly2_of_json "poly2 `C 42" ((`C 42))
;;

type inline_record = | I of {name: string; age : int} | J of {empty : unit} [@@deriving json]
let () = test' inline_record_to_json inline_record_of_json "inline_record 1"
           (I {name="bob"; age = 0})
let () = test' inline_record_to_json inline_record_of_json "inline_record 2"
           (J {empty = ()})
;;
