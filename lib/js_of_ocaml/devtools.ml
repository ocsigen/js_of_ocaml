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
  module Dyn = Introspect.Dyn

  (* JsonML, the format expected by DevTools: [[tag, attributes, children...]],
     where the "object" tag embeds a reference to be formatted in turn. *)

  let any = Js.Unsafe.inject

  let str s = any (Js.string s)

  let jsonml tag ?(attrs = []) children =
    let attrs = Js.Unsafe.obj (Array.of_list attrs) in
    any (Js.array (Array.of_list (str tag :: any attrs :: children)))

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

  let config_of_dyn d =
    Js.Unsafe.obj [| "jsoo", any Js._true; "approx", any (Obj.repr (Dyn.get_approx d)) |]

  type config = Js.Unsafe.any Js.opt

  let our_config (config : config) =
    match Js.Opt.to_option config with
    | Some config when Js.Optdef.test (Js.Unsafe.get config (Js.string "jsoo")) ->
        Some config
    | _ -> None

  let is_our_config config = Option.is_some (our_config config)

  let lift (obj : Js.Unsafe.any) config : Dyn.t =
    let approx =
      Option.map
        (fun config -> Obj.obj (Js.Unsafe.get config (Js.string "approx")))
        (our_config config)
    in
    Dyn.lift ?approx (Obj.repr obj)

  (* Values with a representation of their own under js_of_ocaml, which
     [Introspect] cannot know about: [bytes] are [MlBytes] objects while
     strings are JavaScript strings (unless strings are MlBytes too, in which
     case the two cannot be told apart, as in native code), boxed integers and
     bigarrays are custom objects. *)

  let js_strings =
    String.equal
      (Js.to_string (Js.typeof (Js.Unsafe.inject (Sys.opaque_identity ""))))
      "string"

  let is_bytes (raw : Obj.t) =
    js_strings
    && Obj.is_block raw
    && Obj.tag raw = Obj.string_tag
    && not (String.equal (Js.to_string (Js.typeof (Js.Unsafe.inject raw))) "string")

  let custom_name (raw : Obj.t) =
    let name : Js.js_string Js.t Js.optdef =
      Js.Unsafe.get raw (Js.string "caml_custom")
    in
    Option.map Js.to_string (Js.Optdef.to_option name)

  let is_int64 raw =
    match custom_name raw with
    | Some "_j" -> true
    | _ -> false

  let custom_repr (raw : Obj.t) =
    match custom_name raw with
    | Some "_j" -> Int64.to_string (Obj.obj raw) ^ "L"
    | Some "_bigarr02" ->
        let dims : int Js.js_array Js.t = Js.Unsafe.get raw (Js.string "dims") in
        Printf.sprintf
          "<bigarray %s>"
          (String.concat "x" (List.map string_of_int (Array.to_list (Js.to_array dims))))
    | _ -> "<custom>"

  (* JavaScript values embedded in OCaml ones (DOM nodes, promises, [Js.t]
     objects, [Js.null]...), which [Introspect] cannot know about either:
     the console renders them itself when they are handed back to it as
     "object" nodes. *)

  let is_null (v : Js.Unsafe.any) = not (Js.Opt.test (Obj.magic v : Js.Unsafe.any Js.opt))

  let is_js_object (raw : Obj.t) =
    let v = Js.Unsafe.inject raw in
    String.equal (Js.to_string (Js.typeof v)) "object"
    && (not (is_null v))
    && not (Js.to_bool (Js.Unsafe.global##._Array##isArray v))

  (* [Introspect] sizes every block before looking at it, which under
     js_of_ocaml reads [.length]: [null] and [undefined] would throw. *)
  let is_nullish (raw : Obj.t) =
    let v = Js.Unsafe.inject raw in
    (not (Js.Optdef.test (Obj.magic v : Js.Unsafe.any Js.optdef))) || is_null v

  let view (d : Dyn.t) : Dyn.view =
    if is_nullish (Dyn.get_obj d) then Unknown else Dyn.view d

  let constructor_name v =
    try
      let ctor : < name : Js.js_string Js.t Js.readonly_prop > Js.t Js.optdef =
        Js.Unsafe.get v (Js.string "constructor")
      in
      match Js.Optdef.to_option ctor with
      | Some ctor when not (String.equal (Js.to_string ctor##.name) "") ->
          Js.to_string ctor##.name
      | _ -> "Object"
    with _ -> "Object"

  let unknown_repr (raw : Obj.t) =
    let v = Js.Unsafe.inject raw in
    match Js.to_string (Js.typeof v) with
    | "undefined" -> "undefined"
    | "boolean" -> if Js.to_bool (Js.Unsafe.coerce v) then "true" else "false"
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

  let rec pp p depth fragile (d : Dyn.t) =
    if depth <= 0
    then add p "..."
    else
      let raw = Dyn.get_obj d in
      let protect = Obj.is_block raw && Obj.tag raw < Obj.no_scan_tag in
      if protect && H.mem p.seen raw
      then add p "<cycle>"
      else (
        if protect then H.add p.seen raw ();
        pp_view p depth fragile raw (view d);
        if protect then H.remove p.seen raw)

  and pp_view p depth fragile raw (view : Dyn.view) =
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
        add p "[|";
        fields "; " (self false) fs;
        add p "|]"
    | Tuple { name = "::"; fields = fs } when Dyn.field_count fs = 2 ->
        add p "[";
        list_elements p depth fs;
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

  (* Elements of a list, the cells not counting towards the depth *)
  and list_elements p depth fs =
    let label, car = Dyn.field_get fs 0 in
    if not (String.equal label "") then add p ("~" ^ label ^ ":");
    pp p (depth - 1) false car;
    let _, cdr = Dyn.field_get fs 1 in
    match view cdr with
    | Constant [ "[]" ] | Int_or_constant (0, _) -> ()
    | Tuple { name = "::"; fields = fs } when Dyn.field_count fs = 2 ->
        add p "; ";
        let raw = Dyn.get_obj cdr in
        if H.mem p.seen raw
        then add p "<cycle>"
        else (
          H.add p.seen raw ();
          list_elements p depth fs;
          H.remove p.seen raw)
    | _ -> add p "<malformed list>"

  let to_string ?(depth = 6) ?(max_chars = 160) d =
    let p = { buf = Buffer.create 64; max_chars; seen = H.create 7 } in
    match pp p depth false d with
    | () -> Buffer.contents p.buf
    | exception Budget -> Buffer.contents p.buf ^ "..."

  (* Claim OCaml blocks carrying a descriptor, blocks recognized from their
     shape alone (extensions such as exceptions and lazy values, which the
     runtime allocates without descriptor, bytes and boxed int64), and
     anything we referenced from a body. *)
  let claims (obj : Js.Unsafe.any) (config : config) =
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
    }

  let max_list_elements = 100

  let indexed fields =
    List.init (Dyn.field_count fields) (fun i ->
        { label = string_of_int i; value = Dyn.field_get fields i })

  (* The single unnamed argument of a constructor is not numbered. *)
  let labelled fields =
    match Dyn.field_count fields with
    | 1 when String.equal (fst (Dyn.field_get fields 0)) "" ->
        [ { label = ""; value = snd (Dyn.field_get fields 0) } ]
    | n ->
        List.init n (fun i ->
            let label, value = Dyn.field_get fields i in
            { label = (if String.equal label "" then string_of_int i else label); value })

  (* Lists are shown flat, elements numbered; past [max_list_elements] (or on
     a malformed list) the tail is shown as a single value. *)
  let list_elements fields =
    let rec loop acc i fields =
      let _, car = Dyn.field_get fields 0 in
      let _, cdr = Dyn.field_get fields 1 in
      let acc = { label = string_of_int i; value = car } :: acc in
      let rest () = List.rev ({ label = "..."; value = cdr } :: acc) in
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

  let children (view : Dyn.view) =
    match view with
    | Array fields -> indexed fields
    | Tuple { name = "::"; fields } when Dyn.field_count fields = 2 ->
        list_elements fields
    | Tuple { fields; _ } | Record { fields; _ } -> labelled fields
    | Extension (_, _, fields) -> indexed fields
    | Polymorphic_variant (_, value) | Lazy_forward value -> [ { label = ""; value } ]
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

  let render_value d =
    match view d with
    | (Array _ | Tuple _ | Record _ | Polymorphic_variant _ | Lazy_forward _ | Extension _)
      when Obj.is_block (Dyn.get_obj d) ->
        jsonml
          "object"
          ~attrs:[ "object", any (Dyn.get_obj d); "config", any (config_of_dyn d) ]
          []
    | Unknown when is_js_object (Dyn.get_obj d) ->
        (* No config: the console formats the value itself *)
        jsonml "object" ~attrs:[ "object", any (Dyn.get_obj d) ] []
    | _ -> span (to_string ~depth:3 ~max_chars:80 d)

  let render_child { label; value } =
    let value = render_value value in
    jsonml
      "li"
      (if String.equal label ""
       then [ value ]
       else [ span ~style:label_style (label ^ ": "); value ])

  (* A throwing formatter breaks the console, so never let an exception out. *)
  let guard default f obj config = try f obj config with _ -> default

  let header =
    guard Js.null (fun obj config ->
        if claims obj config
        then Js.some (span ~style:"font-family: monospace" (to_string (lift obj config)))
        else Js.null)

  let has_body =
    guard Js._false (fun obj config ->
        Js.bool
          (claims obj config
          &&
          match children (view (lift obj config)) with
          | [] -> false
          | _ :: _ -> true))

  let body =
    guard (any Js.null) (fun obj config ->
        jsonml
          "ol"
          ~attrs:[ "style", str body_style ]
          (List.map render_child (children (view (lift obj config)))))

  let formatter () =
    Js.Unsafe.obj
      [| "header", any (Js.Unsafe.callback_with_arity 2 header)
       ; "hasBody", any (Js.Unsafe.callback_with_arity 2 has_body)
       ; "body", any (Js.Unsafe.callback_with_arity 2 body)
      |]

  let registered = ref false

  let register_formatters () =
    match Sys.backend_type with
    | Other "js_of_ocaml" when not !registered ->
        registered := true;
        let global = Js.Unsafe.global in
        let key = Js.string "devtoolsFormatters" in
        let formatters : Js.Unsafe.any Js.js_array Js.t =
          match Js.Opt.to_option (Js.Unsafe.get global key) with
          | Some formatters -> formatters
          | None ->
              let formatters = new%js Js.array_empty in
              Js.Unsafe.set global key formatters;
              formatters
        in
        ignore (formatters##push (any (formatter ())) : int)
    | _ -> ()
end
[@@if introspect]

let register_formatters = Formatter.register_formatters [@@if introspect]
