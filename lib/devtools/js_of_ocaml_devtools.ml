(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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

(* The formatter only exists when the compiler ships [Introspect] (Introcaml);
   elsewhere [register_formatters] does nothing. *)

let register_formatters () = () [@@if not introspect]

module Formatter = struct
  module Js = Jsoo_runtime.Js
  module Dyn = Introspect.Dyn

  (* Untyped JavaScript values, through the runtime primitives only *)

  let any (x : 'a) : Js.t = Obj.magic x

  let str s = Js.string s

  let undefined : Js.t = Js.pure_js_expr "undefined"

  let null : Js.t = Js.pure_js_expr "null"

  let global : Js.t = Js.pure_js_expr "globalThis"

  let is_undefined v = Js.strict_equals v undefined

  let is_null v = Js.strict_equals v null

  (* [null] or [undefined] *)
  let is_nullish v = Js.equals v null

  let typeof v = Js.to_string (Js.typeof v)

  let get v key = Js.get v (Js.string key)

  (* JsonML, the format expected by DevTools: [[tag, attributes, children...]],
     where the "object" tag embeds a reference to be formatted in turn. *)

  let jsonml tag ?(attrs = []) children =
    let attrs = Js.obj (Array.of_list attrs) in
    Js.array (Array.of_list (str tag :: attrs :: children))

  let span ?style s =
    let attrs =
      match style with
      | None -> []
      | Some style -> [ "style", str style ]
    in
    jsonml "span" ~attrs [ str s ]

  let label_style = "color: rgb(136, 19, 145)"

  let body_style = "list-style: none; padding-left: 12px; margin: 0"

  (* Values nested in a body are handed back to DevTools as "object" nodes; the
     configuration tells [header] the object comes from us (it may lack a
     descriptor of its own, e.g. a block built by the runtime) and carries the
     approximation known from the enclosing type. *)

  let config_of_dyn ?(offset = 0) d =
    Js.obj
      [| "jsoo", Js.bool true
       ; "approx", any (Obj.repr (Dyn.get_approx d))
       ; "offset", any offset
      |]

  (* The config DevTools passes along: ours, another formatter's, or none *)
  type config = Js.t

  let our_config (config : config) =
    if is_nullish config || is_undefined (get config "jsoo") then None else Some config

  let is_our_config config = Option.is_some (our_config config)

  (* Arrays are expanded by chunks: the entry for the rest of an array is the
     array itself, with the offset of the chunk in its config. *)
  let config_offset config =
    match our_config config with
    | Some config ->
        let offset = get config "offset" in
        if is_undefined offset then 0 else (Obj.magic offset : int)
    | None -> 0

  let lift (obj : Js.t) config : Dyn.t =
    let approx =
      Option.map
        (fun config -> (Obj.magic (get config "approx") : Introspect.Desc.approx))
        (our_config config)
    in
    Dyn.lift ?approx (Obj.repr obj)

  (* Values with a representation of their own under js_of_ocaml, which
     [Introspect] cannot know about: [bytes] are [MlBytes] objects while
     strings are JavaScript strings (unless strings are MlBytes too, in which
     case the two cannot be told apart, as in native code), boxed integers and
     bigarrays are custom objects. *)

  let js_strings = String.equal (typeof (any (Sys.opaque_identity ""))) "string"

  let is_bytes (raw : Obj.t) =
    js_strings
    && Obj.is_block raw
    && Obj.tag raw = Obj.string_tag
    && not (String.equal (typeof (any raw)) "string")

  let custom_name (raw : Obj.t) =
    let name = get (any raw) "caml_custom" in
    if is_undefined name then None else Some (Js.to_string name)

  let is_int64 raw =
    match custom_name raw with
    | Some "_j" -> true
    | _ -> false

  let is_bigarray raw =
    match custom_name raw with
    | Some "_bigarr02" -> true
    | _ -> false

  let custom_repr (raw : Obj.t) =
    match custom_name raw with
    | Some "_j" -> Int64.to_string (Obj.obj raw) ^ "L"
    | Some "_bigarr02" ->
        let dims : int array = Js.to_array (get (any raw) "dims") in
        Printf.sprintf
          "<bigarray %s>"
          (String.concat "x" (List.map string_of_int (Array.to_list dims)))
    | _ -> "<custom>"

  (* JavaScript values embedded in OCaml ones (DOM nodes, promises, [Js.t]
     objects, [null]...), which [Introspect] cannot know about either: the
     console renders them itself when they are handed back to it as "object"
     nodes. *)

  let is_js_object (raw : Obj.t) =
    let v = any raw in
    String.equal (typeof v) "object"
    && (not (is_null v))
    && not (Js.to_bool (Js.meth_call (get global "Array") "isArray" [| v |]))

  (* [Introspect] sizes every block before looking at it, which under
     js_of_ocaml reads [.length]: [null] and [undefined] would throw. *)
  let view (d : Dyn.t) : Dyn.view =
    if is_nullish (any (Dyn.get_obj d)) then Unknown else Dyn.view d

  let constructor_name v =
    try
      let ctor = get v "constructor" in
      if is_undefined ctor
      then "Object"
      else
        let name = get ctor "name" in
        if
          String.equal (typeof name) "string" && not (String.equal (Js.to_string name) "")
        then Js.to_string name
        else "Object"
    with _ -> "Object"

  let unknown_repr (raw : Obj.t) =
    let v = any raw in
    match typeof v with
    | "undefined" -> "undefined"
    | "boolean" -> if Js.to_bool v then "true" else "false"
    | "object" when is_null v -> "null"
    | "object" when is_js_object raw -> "<" ^ constructor_name v ^ ">"
    | "object" when Obj.tag raw = Obj.object_tag -> "<object>"
    | "object" -> "<unknown>"
    | t -> "<" ^ t ^ ">"

  let string_repr (raw : Obj.t) s =
    if is_bytes raw
    then Printf.sprintf "Bytes.of_string %S" (Bytes.to_string (Obj.obj raw))
    else Printf.sprintf "%S" s

  (* Float printing from typing/oprint.ml, as in [Introspect] *)
  let float_repres f =
    match classify_float f with
    | FP_nan -> "nan"
    | FP_infinite -> if f < 0.0 then "neg_infinity" else "infinity"
    | _ ->
        let s =
          let s1 = Printf.sprintf "%.12g" f in
          if Float.equal f (float_of_string s1)
          then s1
          else
            let s2 = Printf.sprintf "%.15g" f in
            if Float.equal f (float_of_string s2) then s2 else Printf.sprintf "%.18g" f
        in
        let l = String.length s in
        let rec loop i =
          if i >= l
          then s ^ "."
          else
            match s.[i] with
            | '0' .. '9' | '-' -> loop (i + 1)
            | _ -> s
        in
        loop 0

  (* One-line printing in OCaml syntax, after [Introspect.Dyn.pp], but aware
     of the representations above and bounded by a character budget rather
     than a step budget, so that huge values cost their visible prefix only. *)

  (* Lists are expanded by chunks: past this many elements, the rest of the
     list is one entry, expandable in turn; the one-line header shows the same
     elements. *)
  let max_list_elements = 10

  module H = Hashtbl.Make (struct
    type t = Obj.t

    let equal = ( == )

    let hash = Hashtbl.hash
  end)

  exception Budget

  type printer =
    { buf : Buffer.t
    ; max_chars : int
    ; seen : unit H.t
    }

  let add p s =
    if Buffer.length p.buf >= p.max_chars then raise Budget;
    Buffer.add_string p.buf s

  let is_ref fields =
    Dyn.field_count fields = 1 && String.equal (fst (Dyn.field_get fields 0)) "contents"

  let rec pp ?(offset = 0) p depth fragile (d : Dyn.t) =
    if depth <= 0
    then add p "..."
    else
      let raw = Dyn.get_obj d in
      let protect = Obj.is_block raw && Obj.tag raw < Obj.no_scan_tag in
      if protect && H.mem p.seen raw
      then add p "<cycle>"
      else (
        if protect then H.add p.seen raw ();
        pp_view p depth fragile ~offset raw (view d);
        if protect then H.remove p.seen raw)

  and pp_view p depth fragile ~offset raw (view : Dyn.view) =
    let self fragile d = pp p (depth - 1) fragile d in
    let paren cond f =
      if cond then add p "(";
      f ();
      if cond then add p ")"
    in
    let fields sep f fs =
      for i = 0 to Dyn.field_count fs - 1 do
        if i > 0 then add p sep;
        f (Dyn.field_get fs i)
      done
    in
    let tuple_field (k, v) =
      if not (String.equal k "") then add p ("~" ^ k ^ ":");
      self false v
    in
    let record_field (k, v) =
      add p (k ^ " = ");
      self false v
    in
    match view with
    | String s -> add p (string_repr raw s)
    | Float f -> add p (float_repres f)
    | Char c -> add p (Printf.sprintf "%C" c)
    (* Floats are unboxed numbers, which [Introspect] takes for integers:
       print those that cannot be integers as floats. Never reached in native
       code, where the formatter is not installed. *)
    | Int_or_constant (i, []) when not (Float.is_integer (Obj.magic i : float)) ->
        add p (float_repres (Obj.magic i : float))
    | Int_or_constant (i, keys) ->
        paren
          (fragile
          &&
          match keys with
          | [] -> false
          | _ :: _ -> true)
          (fun () ->
            add p (string_of_int i);
            List.iter (fun k -> add p (" or `" ^ k)) keys)
    | Constant [] -> add p "<invalid constant>"
    | Constant [ name ] -> add p name
    | Constant (name :: names) ->
        paren fragile (fun () ->
            add p name;
            List.iter (fun n -> add p (" or " ^ n)) names)
    | Array fs ->
        let n = Dyn.field_count fs in
        let last = min n (offset + max_list_elements) in
        add p "[|";
        for i = offset to last - 1 do
          if i > offset then add p "; ";
          self false (Dyn.field_get fs i)
        done;
        if last < n then add p (if last > offset then "; ..." else "...");
        add p "|]"
    | Tuple { name = "::"; fields = fs } when Dyn.field_count fs = 2 ->
        add p "[";
        list_elements p depth 0 fs;
        add p "]"
    | Tuple { name = ""; fields = fs } ->
        add p "(";
        fields ", " tuple_field fs;
        add p ")"
    | Tuple { name; fields = fs }
      when Dyn.field_count fs = 1
           && (not fragile)
           && String.equal (fst (Dyn.field_get fs 0)) "" ->
        add p (name ^ " ");
        self true (snd (Dyn.field_get fs 0))
    | Tuple { name; fields = fs } ->
        paren fragile (fun () ->
            if not (String.equal name "") then add p (name ^ " ");
            add p "(";
            fields ", " tuple_field fs;
            add p ")")
    (* A record with the single field [contents] is a ['a ref] (or a record
       defined exactly like it): print it as it is built *)
    | Record { name = ""; fields = fs } when is_ref fs ->
        paren fragile (fun () ->
            add p "ref ";
            self true (snd (Dyn.field_get fs 0)))
    | Record { name; fields = fs } ->
        paren
          (fragile && not (String.equal name ""))
          (fun () ->
            if not (String.equal name "") then add p (name ^ " ");
            add p "{";
            fields "; " record_field fs;
            add p "}")
    | Extension (name, uid, fs) ->
        add p (name ^ "/" ^ string_of_int uid);
        if Dyn.field_count fs > 0
        then (
          add p " (";
          fields ", " (self false) fs;
          add p ")")
    | Polymorphic_variant (name, payload) ->
        paren fragile (fun () ->
            add p ("`" ^ name ^ " ");
            self true payload)
    | Closure -> add p "<closure>"
    | Abstract -> add p "<abstract>"
    | Custom -> add p (custom_repr raw)
    | Unknown -> add p (unknown_repr raw)
    | Lazy_unforced -> add p "<lazy>"
    | Lazy_forcing -> add p "<lazy (forcing)>"
    | Lazy_forward d ->
        paren fragile (fun () ->
            add p "lazy ";
            self true d)

  (* Elements of a list, the cells not counting towards the depth; as many
     as the body lists, then "..." *)
  and list_elements p depth i fs =
    let label, car = Dyn.field_get fs 0 in
    if not (String.equal label "") then add p ("~" ^ label ^ ":");
    pp p (depth - 1) false car;
    let _, cdr = Dyn.field_get fs 1 in
    match view cdr with
    | Constant [ "[]" ] | Int_or_constant (0, _) -> ()
    | Tuple { name = "::"; fields = fs } when Dyn.field_count fs = 2 ->
        add p "; ";
        if i + 1 >= max_list_elements
        then add p "..."
        else
          let raw = Dyn.get_obj cdr in
          if H.mem p.seen raw
          then add p "<cycle>"
          else (
            H.add p.seen raw ();
            list_elements p depth (i + 1) fs;
            H.remove p.seen raw)
    | _ -> add p "<malformed list>"

  let to_string ?(depth = 6) ?(max_chars = 160) ?offset d =
    let p = { buf = Buffer.create 64; max_chars; seen = H.create 7 } in
    match pp ?offset p depth false d with
    | () -> Buffer.contents p.buf
    | exception Budget -> Buffer.contents p.buf ^ "..."

  (* Claim OCaml blocks carrying a descriptor, blocks recognized from their
     shape alone (extensions such as exceptions and lazy values, which the
     runtime allocates without descriptor, bytes and boxed int64), and
     anything we referenced from a body. *)
  let claims (obj : Js.t) (config : config) =
    is_our_config config
    ||
    let obj = Obj.repr obj in
    Obj.is_block obj
    && (Obj.get_reserved obj <> 0
       ||
       match view (Dyn.lift obj) with
       | Extension _ | Lazy_forward _ | Lazy_unforced | Lazy_forcing -> true
       | String _ -> is_bytes obj
       | Custom -> is_int64 obj
       | Float _
       | Char _
       | Int_or_constant _
       | Constant _
       | Array _
       | Tuple _
       | Record _
       | Polymorphic_variant _
       | Closure
       | Abstract
       | Unknown -> false)

  (* The entries listed when a value is expanded. *)

  type child =
    { label : string
    ; value : Dyn.t
    ; offset : int (* of the chunk shown, for the rest of an array *)
    }

  let child label value = { label; value; offset = 0 }

  let indexed fields =
    List.init (Dyn.field_count fields) (fun i ->
        child (string_of_int i) (Dyn.field_get fields i))

  (* A chunk of an array, then the array again from the next chunk on *)
  let array_elements d ~offset fields =
    let n = Dyn.field_count fields in
    let last = min n (offset + max_list_elements) in
    let elements =
      List.init (last - offset) (fun i ->
          child (string_of_int (offset + i)) (Dyn.field_get fields (offset + i)))
    in
    if last < n
    then elements @ [ { label = "..."; value = d; offset = last } ]
    else elements

  (* The single unnamed argument of a constructor is not numbered. *)
  let labelled fields =
    match Dyn.field_count fields with
    | 1 when String.equal (fst (Dyn.field_get fields 0)) "" ->
        [ child "" (snd (Dyn.field_get fields 0)) ]
    | n ->
        List.init n (fun i ->
            let label, value = Dyn.field_get fields i in
            child (if String.equal label "" then string_of_int i else label) value)

  (* Lists are shown flat, elements numbered; past [max_list_elements] (or on
     a malformed list) the tail is shown as a single entry. *)
  let list_elements fields =
    let rec loop acc i fields =
      let _, car = Dyn.field_get fields 0 in
      let _, cdr = Dyn.field_get fields 1 in
      let acc = child (string_of_int i) car :: acc in
      let rest () = List.rev (child "..." cdr :: acc) in
      if i + 1 >= max_list_elements
      then rest ()
      else
        match view cdr with
        | Tuple { name = "::"; fields } when Dyn.field_count fields = 2 ->
            loop acc (i + 1) fields
        | Constant _ | Int_or_constant _ -> List.rev acc
        | _ -> rest ()
    in
    loop [] 0 fields

  let children ?(offset = 0) (d : Dyn.t) =
    match view d with
    | Array fields -> array_elements d ~offset fields
    | Tuple { name = "::"; fields } when Dyn.field_count fields = 2 ->
        list_elements fields
    | Record { name = ""; fields } when is_ref fields ->
        [ child "" (snd (Dyn.field_get fields 0)) ]
    | Tuple { fields; _ } | Record { fields; _ } -> labelled fields
    | Extension (_, _, fields) -> indexed fields
    | Polymorphic_variant (_, value) | Lazy_forward value -> [ child "" value ]
    | String _
    | Float _
    | Char _
    | Int_or_constant _
    | Constant _
    | Closure
    | Abstract
    | Custom
    | Unknown
    | Lazy_unforced
    | Lazy_forcing -> []

  (* Strings longer than this are handed to the console rather than cut *)
  let inline_string_length = 60

  let render_value ~offset d =
    match view d with
    | (Array _ | Tuple _ | Record _ | Polymorphic_variant _ | Lazy_forward _ | Extension _)
      when Obj.is_block (Dyn.get_obj d) ->
        jsonml
          "object"
          ~attrs:
            [ "object", any (Dyn.get_obj d); "config", any (config_of_dyn ~offset d) ]
          []
    | Unknown when is_js_object (Dyn.get_obj d) ->
        (* No config: the console formats the value itself *)
        jsonml "object" ~attrs:[ "object", any (Dyn.get_obj d) ] []
    | Closure when String.equal (typeof (any (Dyn.get_obj d))) "function" ->
        (* Likewise: the console shows the function with a link to its source *)
        jsonml "object" ~attrs:[ "object", any (Dyn.get_obj d) ] []
    | Custom when is_bigarray (Dyn.get_obj d) ->
        (* Likewise: the console shows the underlying typed array *)
        jsonml "object" ~attrs:[ "object", any (Dyn.get_obj d) ] []
    | String s
      when String.length s > inline_string_length && not (is_bytes (Dyn.get_obj d)) ->
        (* Long strings in full, with copy, rather than cut *)
        jsonml "object" ~attrs:[ "object", any (Dyn.get_obj d) ] []
    | _ -> span (to_string ~depth:3 ~max_chars:80 d)

  let render_child { label; value; offset } =
    let value = render_value ~offset value in
    jsonml
      "li"
      (if String.equal label ""
       then [ value ]
       else [ span ~style:label_style (label ^ ": "); value ])

  (* A throwing formatter breaks the console, so never let an exception out. *)
  let guard default f obj config = try f obj config with _ -> default

  let header =
    guard null (fun obj config ->
        if claims obj config
        then
          span
            ~style:"font-family: monospace"
            (to_string ~offset:(config_offset config) (lift obj config))
        else null)

  let has_body =
    guard (Js.bool false) (fun obj config ->
        Js.bool
          (claims obj config
          &&
          match children ~offset:(config_offset config) (lift obj config) with
          | [] -> false
          | _ :: _ -> true))

  let body =
    guard null (fun obj config ->
        jsonml
          "ol"
          ~attrs:[ "style", str body_style ]
          (List.map
             render_child
             (children ~offset:(config_offset config) (lift obj config))))

  let formatter () =
    Js.obj
      [| "header", any (Js.callback_with_arity 2 header)
       ; "hasBody", any (Js.callback_with_arity 2 has_body)
       ; "body", any (Js.callback_with_arity 2 body)
      |]

  let registered = ref false

  let register_formatters () =
    match Sys.backend_type with
    | Other "js_of_ocaml" when not !registered ->
        registered := true;
        let key = "devtoolsFormatters" in
        let formatters =
          let formatters = get global key in
          if is_nullish formatters
          then (
            let formatters = Js.array [||] in
            Js.set global (Js.string key) formatters;
            formatters)
          else formatters
        in
        ignore (Js.meth_call formatters "push" [| formatter () |] : Js.t)
    | _ -> ()
end
[@@if introspect]

let register_formatters = Formatter.register_formatters [@@if introspect]
