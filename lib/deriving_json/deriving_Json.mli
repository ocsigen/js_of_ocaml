(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright Grégoire Henry 2010.
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

(** Typesafe IO (based on the {i deriving} library).
    @see <https://github.com/ocsigen/deriving> the source code of {i deriving}
    @see <http://code.google.com/p/deriving/>
      the documentation of the original {i deriving} library by Jeremy Yallop. *)

type 'a t
(** The type of JSON parser/printer for value of type ['a]. *)

val make : (Buffer.t -> 'a -> unit) -> (Deriving_Json_lexer.lexbuf -> 'a) -> 'a t

val write : 'a t -> Buffer.t -> 'a -> unit

val read : 'a t -> Deriving_Json_lexer.lexbuf -> 'a

val to_string : 'a t -> 'a -> string
(** [to_string Json.t<ty> v] marshal the [v] of type [ty] to a JSON string.*)

val from_string : 'a t -> string -> 'a
(** [from_string Json.t<ty> s] safely unmarshal the JSON [s] into an OCaml value of type
    [ty]. Throws [Failure] if the received value isn't the javascript representation of a
    value of type [ty]. *)

(** The signature of the JSON class. *)
module type Json = sig
  type a

  val t : a t

  val write : Buffer.t -> a -> unit

  val read : Deriving_Json_lexer.lexbuf -> a

  val to_string : a -> string

  val from_string : string -> a

  (**/**)

  val match_variant : [ `Cst of int | `NCst of int ] -> bool

  val read_variant : Deriving_Json_lexer.lexbuf -> [ `Cst of int | `NCst of int ] -> a
end

(**/**)

(** {2 Conversion} *)

val convert : 'a t -> ('a -> 'b) -> ('b -> 'a) -> 'b t
(** [convert (t : 'a t) (from_ : 'a -> 'b) (to_ : 'b -> 'a)] generate a JSON
    parser/printer for value of type ['b] using the parser/printer of type ['a] provided
    by [t] *)

(** The signature of the Converter class. *)
module type Json_converter = sig
  type a

  type b

  val t : a t

  val from_ : a -> b

  val to_ : b -> a
end

(** Generate a JSON class from a Converter *)
module Convert (J : Json_converter) : Json with type a = J.b

(** {3 Examples} *)

(** Parse and serialize a map as if it was an array of tuple.
    {[
      (* My map module *)
      module StringMap = Map.Make(String)

      (* Use deriving_json syntax to generate the JSON class for the array of tuple *)
      type 'a t = (string * 'a) array deriving (Json)

      (* generate the JSON class for StringMap *)
      module Json_string_map_t(A : Deriving_Json.Json) : Deriving_Json.Json with type a = A.a StringMap.t = struct
        module S = Json_t(A)
        include Deriving_Json.Convert(struct
            type a = A.a t
            type b = A.a StringMap.t
            let t = S.t
            let to_ : b -> A.a t = fun a -> Array.of_list (StringMap.bindings a)
            let from_ : A.a t -> b = fun l ->
              Array.fold_left
                (fun map (x,v) -> StringMap.add x v map)
                StringMap.empty
                l
          end)
      end
    ]}

    You can then ask the syntax extension to use the JSON class [Json_string_map_t] for
    [StringMap.t] by registering an alias
    {[
      Pa_deriving_Json.register_predefs [ "StringMap"; "t" ] [ "string_map_t" ]
    ]} *)

(**/**)

(** Deriver *)

module type Json_min = sig
  type a

  val write : Buffer.t -> a -> unit

  val read : Deriving_Json_lexer.lexbuf -> a
end

module type Json_min' = sig
  type a

  val write : Buffer.t -> a -> unit

  val read : Deriving_Json_lexer.lexbuf -> a

  val match_variant : [ `Cst of int | `NCst of int ] -> bool

  val read_variant : Deriving_Json_lexer.lexbuf -> [ `Cst of int | `NCst of int ] -> a
end

module type Json_min'' = sig
  type a

  val t : a t
end

module Defaults (J : Json_min) : Json with type a = J.a

module Defaults' (J : Json_min') : Json with type a = J.a

module Defaults'' (J : Json_min'') : Json with type a = J.a

module Json_char : Json with type a = char

module Json_bool : Json with type a = bool

module Json_unit : Json with type a = unit

module Json_int : Json with type a = int

module Json_int32 : Json with type a = int32

module Json_int64 : Json with type a = int64

module Json_nativeint : Json with type a = nativeint

(* module Json_num       : Json with type a = Num.num *)
module Json_float : Json with type a = float

module Json_string : Json with type a = string

module Json_list (A : Json) : Json with type a = A.a list

module Json_ref (A : Json) : Json with type a = A.a ref

module Json_option (A : Json) : Json with type a = A.a option

module Json_array (A : Json) : Json with type a = A.a array

val read_list :
  (Deriving_Json_lexer.lexbuf -> 'a) -> Deriving_Json_lexer.lexbuf -> 'a list

val write_list : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a list -> unit

val read_ref : (Deriving_Json_lexer.lexbuf -> 'a) -> Deriving_Json_lexer.lexbuf -> 'a ref

val write_ref : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a ref -> unit

val read_option :
  (Deriving_Json_lexer.lexbuf -> 'a) -> Deriving_Json_lexer.lexbuf -> 'a option

val write_option : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a option -> unit

val read_array :
  (Deriving_Json_lexer.lexbuf -> 'a) -> Deriving_Json_lexer.lexbuf -> 'a array

val write_array : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a array -> unit
