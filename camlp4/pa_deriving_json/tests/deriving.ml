open Deriving_Json

let test name v = Printf.printf "%s = %s\n%!" name v

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

type tuple1 = (int * string) deriving (Json)

type variant1 = A | B | C | D of variant1 deriving (Json)

type variant2 = D of string | E of variant1 deriving (Json)

type record1 = { f : variant1; g : variant2; h : record1 option } deriving (Json)

type poly1 = [`A | `B of string] deriving (Json)

type poly2 = [poly1 | `C of int] deriving (Json);;

(* Weird camlp4 issue. *)
();;let () = ()

let () = test "tuple1" (Json_tuple1.to_string (42,"42"))

let () = test "variant1 A" (Json_variant1.to_string A)

let () = test "variant1 B" (Json_variant1.to_string B)

let () = test "variant1 C" (Json_variant1.to_string C)

let () = test "variant2 D 'hello'" (Json_variant2.to_string (D "hello"))

let () = test "variant2 E A" (Json_variant2.to_string (E A))

let () = test "record1" (Json_record1.to_string {f = A; g = D "d"; h = None })

let () = test "record1"
           (Json_record1.to_string
              {f = A;
               g = D "d";
               h = Some {f = B;
                         g = E (D B);
                         h = None }
              })

let () = test "poly1 `A" (Json_poly1.to_string `A)

let () = test "poly1 `B str" (Json_poly1.to_string (`B "str"))

let () = test "poly2 `A" (Json_poly2.to_string `A)

let () = test "poly2 `B str" (Json_poly2.to_string (`B "str"))

let () = test "poly2 `C 42" (Json_poly2.to_string (`C 42));;
