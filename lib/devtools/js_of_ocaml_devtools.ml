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

let inspect v = Obj.magic v [@@if not introspect]

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

  (* Under Wasm_of_ocaml, OCaml values are Wasm GC objects, opaque to
     JavaScript: see [Wasm] below. *)
  let wasm =
    match Sys.backend_type with
    | Other "wasm_of_ocaml" -> true
    | Native | Bytecode | Other _ -> false

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
    if wasm
    then Some (Js.custom_identifier raw)
    else
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
    (* Boxed in Wasm only *)
    | Some "_i" -> Int32.to_string (Obj.obj raw) ^ "l"
    | Some "_n" -> Nativeint.to_string (Obj.obj raw) ^ "n"
    | Some "_bigarr02" when wasm ->
        let dims =
          Bigarray.Genarray.dims
            (Obj.obj raw : (int, int, Bigarray.c_layout) Bigarray.Genarray.t)
        in
        Printf.sprintf
          "<bigarray %s>"
          (String.concat "x" (List.map string_of_int (Array.to_list dims)))
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

  (* Under Wasm, JavaScript values are wrapped in a struct, which has no tag
     of its own *)
  let is_js_value (raw : Obj.t) =
    wasm && Obj.is_block raw && Obj.tag raw = Obj.abstract_tag

  let is_js_object (raw : Obj.t) =
    let v = any raw in
    if wasm
    then
      is_js_value raw
      && (match typeof v with
        | "object" | "function" -> true
        | _ -> false)
      && not (is_null v)
    else
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

  (* Under Wasm_of_ocaml, the debugger shows the values of a paused Wasm frame
     as proxies: V8 wraps each value as a [{type; value}] object without
     prototype, [type] being its Wasm type, with immediates as numbers and
     arrays and structs as proxies, whose elements and fields ([$field0]...
     without a name section) are wrapped in turn. A proxy cannot be turned
     back into the value it shows, so we copy this value into a real one, as
     far as printing needs, and recognize the representations of OCaml values
     from their shape: a block is an array starting with its header (an i31
     holding its tag and descriptor), a string an i8 array, a float array an
     f64 array, a boxed float a struct with an f64 field, a custom block a
     struct starting with its operations (whose first field is the
     identifier), a closure a struct starting with a function and a
     JavaScript value a struct holding it. Values handed to the console as
     ["object"] nodes are the proxies they were copied from, so that they are
     copied again when expanded. *)
  module Wasm = struct
    let object_ = get global "Object"

    (* OCaml values handed to the console, which cannot see into them *)
    let wrapper : Js.t = Js.pure_js_expr "(function OCaml(value) { this.value = value; })"

    let wrap (raw : Obj.t) = Js.new_obj wrapper [| any raw |]

    let is_proxy (o : Js.t) =
      String.equal (typeof o) "object"
      && (not (is_null o))
      && is_null (Js.meth_call object_ "getPrototypeOf" [| o |])
      && String.equal (typeof (get o "type")) "string"

    let wasm_type w = Js.to_string (get w "type")

    let value w = get w "value"

    let to_int v = int_of_float (Js.float_of_number v)

    let index (a : Js.t) i = Js.get a (any i)

    let length a = to_int (get a "length")

    let keys s : Js.t array = Js.to_array (Js.meth_call object_ "keys" [| s |])

    let field s k = Js.get s k

    let is_array_proxy v = String.equal (typeof (get v "length")) "number"

    (* References to JavaScript values (anyref, externref...) *)
    let is_host_type t =
      let contains sub =
        let n = String.length sub in
        let rec loop i =
          i + n <= String.length t && (String.equal (String.sub t i n) sub || loop (i + 1))
        in
        loop 0
      in
      contains "any" || contains "extern"

    (* The objects of the current copy: copied blocks and placeholders with
       the proxy they come from, values printed as a text (such as custom
       blocks which cannot be copied) and truncated strings *)
    type copy =
      { proxies : Js.t H.t
      ; placeholders : unit H.t
      ; texts : string H.t
      ; truncated : unit H.t
      ; unknown : unit H.t (* structs we do not recognize *)
      }

    let empty () =
      { proxies = H.create 16
      ; placeholders = H.create 16
      ; texts = H.create 16
      ; truncated = H.create 16
      ; unknown = H.create 16
      }

    let current = ref (empty ())

    (* A physically distinct value *)
    let fresh () = Obj.repr (ref ())

    (* Fields left out of a copy *)
    let far = fresh ()

    let text raw = if raw == far then Some "..." else H.find_opt !current.texts raw

    let is_placeholder raw = H.mem !current.placeholders raw

    let is_truncated raw = H.mem !current.truncated raw

    (* Placeholders and values printed as a text *)
    let elided raw = Option.is_some (text raw) || is_placeholder raw

    let elision raw = Option.value (text raw) ~default:"..."

    let proxy raw = H.find_opt !current.proxies raw

    (* Values left to the console *)
    let is_unknown raw = H.mem !current.unknown raw

    (* A copy does not cost much more than printing: the fields of a block are
       copied breadth-first, at most [max_fields] of them, from the offset
       shown for the value itself, and once [max_nodes] blocks are copied,
       the others are placeholders, printed as "...". *)
    let max_nodes = 1000

    let max_fields = 100

    let max_string = 1024

    let dummy_closure () =
      let r = ref () in
      Obj.repr (fun () -> !r)

    let bytes a n =
      Bytes.init n (fun i -> Char.unsafe_chr (to_int (value (index a i)) land 255))

    let is_string_proxy v =
      is_array_proxy v && length v > 0 && String.equal (wasm_type (index v 0)) "i8"

    let copy ~offset (w : Js.t) =
      let st = empty () in
      let blocks = ref [] in
      let budget = ref max_nodes in
      let queue = Queue.create () in
      let as_text s =
        let v = fresh () in
        H.replace st.texts v s;
        v
      in
      let rec node ~top w =
        let v = value w in
        match typeof v with
        | "number" ->
            let t = wasm_type w in
            if String.equal t "f64" || String.equal t "f32"
            then Obj.repr (Js.float_of_number v)
            else Obj.repr (to_int v)
        | "bigint" ->
            Obj.repr (Int64.of_string (Js.to_string (Js.meth_call v "toString" [||])))
        | "function" -> dummy_closure ()
        | "object" when (not (is_null v)) && not (is_host_type (wasm_type w)) ->
            if is_array_proxy v then array ~top w v else struct_ w v
        | _ -> (* A JavaScript value *) Obj.repr v
      and array ~top w a =
        let n = length a in
        let first = if top then offset else 0 in
        if n = 0
        then (* No block is empty: an empty string (or float array) *)
          Obj.repr ""
        else
          match wasm_type (index a 0) with
          | "i8" ->
              let s = Bytes.unsafe_to_string (bytes a (min n max_string)) in
              let s = Obj.repr (Sys.opaque_identity s) in
              if n > max_string then H.replace st.truncated s ();
              budget := !budget - (min n max_string / 64);
              s
          | "f64" ->
              let fa = Array.make n 0. in
              for i = first to min n (first + max_fields) - 1 do
                fa.(i) <- Js.float_of_number (value (index a i))
              done;
              Obj.repr fa
          | _ ->
              let hd = to_int (value (index a 0)) in
              let size = n - 1 in
              let b = Obj.new_block (hd land 0xff) size in
              if hd lsr 8 <> 0 then ignore (Obj.set_reserved b (hd lsr 8) : bool);
              blocks := (b, w) :: !blocks;
              for i = 0 to size - 1 do
                if i >= first && i < first + max_fields
                then Queue.add (b, i, index a (i + 1)) queue
                else Obj.set_field b i far
              done;
              b
      and struct_ w s =
        let ks = keys s in
        let unknown () =
          let v = as_text "<abstract>" in
          H.replace st.unknown v ();
          H.replace st.proxies v w;
          v
        in
        let first () = value (field s ks.(0)) in
        match ks with
        | [| k |] when String.equal (wasm_type (field s k)) "f64" ->
            Obj.repr (Js.float_of_number (value (field s k)))
        | [| k |] when is_host_type (wasm_type (field s k)) ->
            Obj.repr (value (field s k))
        | [||] -> unknown ()
        | _ when String.equal (typeof (first ())) "function" ->
            let c = dummy_closure () in
            blocks := (c, w) :: !blocks;
            c
        | _ when Array.length ks >= 2 && is_operations (first ()) -> (
            let ops = first () in
            let id =
              Bytes.to_string
                (let a = value (field ops (keys ops).(0)) in
                 bytes a (length a))
            in
            let payload = value (field s ks.(1)) in
            match id with
            | "_j" ->
                Obj.repr
                  (Int64.of_string (Js.to_string (Js.meth_call payload "toString" [||])))
            | "_i" -> Obj.repr (Int32.of_float (Js.float_of_number payload))
            | "_n" -> Obj.repr (Nativeint.of_float (Js.float_of_number payload))
            | "_bigarr02" -> as_text "<bigarray>"
            | id -> as_text ("<custom " ^ id ^ ">"))
        | _ -> unknown ()
      (* Custom operations start with the identifier *)
      and is_operations v =
        String.equal (typeof v) "object"
        && (not (is_null v))
        && (not (is_array_proxy v))
        &&
        let ks = keys v in
        Array.length ks > 0 && is_string_proxy (value (field v ks.(0)))
      in
      let top = node ~top:true w in
      while not (Queue.is_empty queue) do
        let b, i, fw = Queue.pop queue in
        let x =
          if String.equal (typeof (value fw)) "number"
          then node ~top:false fw
          else if !budget <= 0
          then (
            let p = fresh () in
            H.replace st.placeholders p ();
            H.replace st.proxies p fw;
            p)
          else (
            decr budget;
            node ~top:false fw)
        in
        Obj.set_field b i x
      done;
      (* Once their contents is final, as they are hashed *)
      List.iter (fun (b, w) -> H.replace st.proxies b w) !blocks;
      st, top

    (* The formatter is asked for the header of a value, then whether it has
       a body: copy it once *)
    let last = ref None

    let copy ~offset w =
      match !last with
      | Some (w', offset', st, v) when Js.strict_equals w w' && offset = offset' ->
          current := st;
          v
      | Some _ | None ->
          let st, v = copy ~offset w in
          last := Some (w, offset, st, v);
          current := st;
          v
  end

  exception Budget

  (* Under Wasm, blocks without descriptor are displayed too, such as arrays
     built by the runtime: the large ones are expanded by chunks, as arrays *)
  let chunked_tuple raw fields =
    wasm && Obj.get_reserved raw = 0 && Dyn.field_count fields > max_list_elements

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
      if wasm && Wasm.elided raw
      then add p (Wasm.elision raw)
      else if protect && H.mem p.seen raw
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
    | String s ->
        add p (string_repr raw s);
        if wasm && Wasm.is_truncated raw then add p "..."
    | Float f -> add p (float_repres f)
    | Char c -> add p (Printf.sprintf "%C" c)
    (* Floats are unboxed numbers, which [Introspect] takes for integers:
       print those that cannot be integers as floats. Never reached in native
       code, where the formatter is not installed. *)
    | Int_or_constant (i, [])
      when (not wasm) && not (Float.is_integer (Obj.magic i : float)) ->
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
    | Tuple { name = ""; fields = fs } when chunked_tuple raw fs ->
        let n = Dyn.field_count fs in
        let last = min n (offset + max_list_elements) in
        add p (if offset > 0 then "(..., " else "(");
        for i = offset to last - 1 do
          if i > offset then add p ", ";
          tuple_field (Dyn.field_get fs i)
        done;
        if last < n then add p ", ...";
        add p ")"
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
    | Abstract when is_js_value raw -> add p (unknown_repr raw)
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
    | _ when wasm && Wasm.elided (Dyn.get_obj cdr) -> add p "; ..."
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

  (* Likewise, for a large block without descriptor *)
  let tuple_elements d ~offset fields =
    let n = Dyn.field_count fields in
    let last = min n (offset + max_list_elements) in
    let elements =
      List.init (last - offset) (fun i ->
          child (string_of_int (offset + i)) (snd (Dyn.field_get fields (offset + i))))
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
    | Tuple { name = ""; fields } when chunked_tuple (Dyn.get_obj d) fields ->
        tuple_elements d ~offset fields
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

  (* An OCaml value as an "object" node: under Wasm, the proxy a copy comes
     from, or a wrapper *)
  let handle (raw : Obj.t) =
    if wasm
    then
      match Wasm.proxy raw with
      | Some w -> w
      | None -> Wasm.wrap raw
    else any raw

  let render_value ~offset d =
    match view d with
    | _ when wasm && Wasm.is_unknown (Dyn.get_obj d) ->
        (* No config: the console shows the proxy itself *)
        jsonml "object" ~attrs:[ "object", handle (Dyn.get_obj d) ] []
    | _ when wasm && Option.is_some (Wasm.text (Dyn.get_obj d)) ->
        span (Wasm.elision (Dyn.get_obj d))
    | (Array _ | Tuple _ | Record _ | Polymorphic_variant _ | Lazy_forward _ | Extension _)
      when Obj.is_block (Dyn.get_obj d) ->
        jsonml
          "object"
          ~attrs:
            [ "object", handle (Dyn.get_obj d); "config", any (config_of_dyn ~offset d) ]
          []
    | (Unknown | Abstract) when is_js_object (Dyn.get_obj d) ->
        (* No config: the console formats the value itself *)
        jsonml "object" ~attrs:[ "object", any (Dyn.get_obj d) ] []
    | Closure when (not wasm) && String.equal (typeof (any (Dyn.get_obj d))) "function" ->
        (* Likewise: the console shows the function with a link to its source *)
        jsonml "object" ~attrs:[ "object", any (Dyn.get_obj d) ] []
    | Closure when wasm && Option.is_some (Wasm.proxy (Dyn.get_obj d)) ->
        (* Likewise, from the proxy, which we do not format *)
        jsonml "object" ~attrs:[ "object", handle (Dyn.get_obj d) ] []
    | Custom when (not wasm) && is_bigarray (Dyn.get_obj d) ->
        (* Likewise: the console shows the underlying typed array *)
        jsonml "object" ~attrs:[ "object", any (Dyn.get_obj d) ] []
    | String s
      when String.length s > inline_string_length && not (is_bytes (Dyn.get_obj d)) ->
        (* Long strings in full, with copy, rather than cut *)
        jsonml
          "object"
          ~attrs:[ ("object", if wasm then Js.string s else any (Dyn.get_obj d)) ]
          []
    | _ -> span (to_string ~depth:3 ~max_chars:80 d)

  let render_child { label; value; offset } =
    let value = render_value ~offset value in
    jsonml
      "li"
      (if String.equal label ""
       then [ value ]
       else [ span ~style:label_style (label ^ ": "); value ])

  (* The value [obj] shows, if we display it: under Wasm, a wrapper or a
     proxy, whose copy we display unless it is a closure, a JavaScript value
     or a struct we do not recognize, which the console shows better *)
  let resolve (obj : Js.t) (config : config) =
    if not wasm
    then if claims obj config then Some (lift obj config) else None
    else if Js.instanceof obj Wasm.wrapper
    then (
      Wasm.current := Wasm.empty ();
      Some (lift (get obj "value") config))
    else if Wasm.is_proxy obj
    then
      let raw = Wasm.copy ~offset:(config_offset config) obj in
      if
        Wasm.is_unknown raw
        || (Obj.is_block raw && (Obj.tag raw = Obj.closure_tag || is_js_value raw))
      then None
      else Some (lift (any raw) config)
    else None

  (* A throwing formatter breaks the console, so never let an exception out. *)
  let guard default f obj config = try f obj config with _ -> default

  let header =
    guard null (fun obj config ->
        match resolve obj config with
        | Some d ->
            span
              ~style:"font-family: monospace"
              (to_string ~offset:(config_offset config) d)
        | None -> null)

  let has_body =
    guard (Js.bool false) (fun obj config ->
        Js.bool
          (match resolve obj config with
          | Some d -> (
              match children ~offset:(config_offset config) d with
              | [] -> false
              | _ :: _ -> true)
          | None -> false))

  let body =
    guard null (fun obj config ->
        match resolve obj config with
        | Some d ->
            jsonml
              "ol"
              ~attrs:[ "style", str body_style ]
              (List.map render_child (children ~offset:(config_offset config) d))
        | None -> null)

  (* Under Wasm, a trap is not an OCaml exception *)
  let js_guard : Js.t =
    Js.pure_js_expr
      "(function (f, d) { return function (o, c) { try { return f(o, c); } catch (e) { \
       return d; } }; })"

  let callback f default =
    let f = any (Js.callback_with_arity 2 f) in
    if wasm then Js.fun_call js_guard [| f; default |] else f

  let formatter () =
    Js.obj
      [| "header", callback header null
       ; "hasBody", callback has_body (Js.bool false)
       ; "body", callback body null
      |]

  let inspect v = if wasm then Wasm.wrap (Obj.repr v) else any v

  let registered = ref false

  let register_formatters () =
    match Sys.backend_type with
    | Other ("js_of_ocaml" | "wasm_of_ocaml") when not !registered ->
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

let inspect = Formatter.inspect [@@if introspect]
