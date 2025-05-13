(* Wasm_of_ocaml compiler
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

open Stdlib

type heaptype =
  | Func
  | Nofunc
  | Extern
  | Noextern
  | Any
  | Eq
  | I31
  | Struct
  | Array
  | None_
  | Type of int

type reftype =
  { nullable : bool
  ; typ : heaptype
  }

type valtype =
  | I32
  | I64
  | F32
  | F64
  | V128
  | Ref of reftype

type packedtype =
  | I8
  | I16

type storagetype =
  | Val of valtype
  | Packed of packedtype

type 'ty mut =
  { mut : bool
  ; typ : 'ty
  }

type fieldtype = storagetype mut

type comptype =
  | Func of
      { params : valtype array
      ; results : valtype array
      }
  | Struct of fieldtype array
  | Array of fieldtype

type subtype =
  { final : bool
  ; supertype : int option
  ; typ : comptype
  }

type rectype = subtype array

type limits =
  { min : int
  ; max : int option
  ; shared : bool
  ; index_type : [ `I32 | `I64 ]
  }

type tabletype =
  { limits : limits
  ; typ : reftype
  }

type importdesc =
  | Func of int
  | Table of tabletype
  | Mem of limits
  | Global of valtype mut
  | Tag of int

type import =
  { module_ : string
  ; name : string
  ; desc : importdesc
  }

type exportable =
  | Func
  | Table
  | Mem
  | Global
  | Tag

let heaptype_eq t1 t2 =
  Stdlib.phys_equal t1 t2
  ||
  match t1, t2 with
  | Type i1, Type i2 -> i1 = i2
  | _ -> false

let reftype_eq { nullable = n1; typ = t1 } { nullable = n2; typ = t2 } =
  Bool.(n1 = n2) && heaptype_eq t1 t2

let valtype_eq t1 t2 =
  Stdlib.phys_equal t1 t2
  ||
  match t1, t2 with
  | Ref t1, Ref t2 -> reftype_eq t1 t2
  | _ -> false

let rec output_uint ch i =
  if i < 128
  then output_byte ch i
  else (
    output_byte ch (128 + (i land 127));
    output_uint ch (i lsr 7))

module Write = struct
  type st = { mutable type_index_count : int }

  let byte ch b = Buffer.add_char ch (Char.chr b)

  let string ch s = Buffer.add_string ch s

  let rec sint ch i =
    if i >= -64 && i < 64
    then byte ch (i land 127)
    else (
      byte ch (128 + (i land 127));
      sint ch (i asr 7))

  let rec uint ch i =
    if i < 128
    then byte ch i
    else (
      byte ch (128 + (i land 127));
      uint ch (i lsr 7))

  let vec f ch l =
    uint ch (Array.length l);
    Array.iter ~f:(fun x -> f ch x) l

  let name ch name =
    uint ch (String.length name);
    string ch name

  let typeidx st idx = if idx < 0 then lnot idx + st.type_index_count else idx

  let heaptype st ch typ =
    match (typ : heaptype) with
    | Nofunc -> byte ch 0x73
    | Noextern -> byte ch 0x72
    | None_ -> byte ch 0x71
    | Func -> byte ch 0x70
    | Extern -> byte ch 0x6F
    | Any -> byte ch 0x6E
    | Eq -> byte ch 0x6D
    | I31 -> byte ch 0x6C
    | Struct -> byte ch 0x6B
    | Array -> byte ch 0x6A
    | Type idx -> sint ch (typeidx st idx)

  let reftype st ch { nullable; typ } =
    (match nullable, typ with
    | false, _ -> byte ch 0x64
    | true, Type _ -> byte ch 0x63
    | _ -> ());
    heaptype st ch typ

  let valtype st ch (typ : valtype) =
    match typ with
    | I32 -> byte ch 0x7F
    | I64 -> byte ch 0x7E
    | F32 -> byte ch 0x7D
    | F64 -> byte ch 0x7C
    | V128 -> byte ch 0x7B
    | Ref typ -> reftype st ch typ

  let mutability ch mut = byte ch (if mut then 0x01 else 0x00)

  let fieldtype st ch { mut; typ } =
    (match typ with
    | Val typ -> valtype st ch typ
    | Packed typ -> (
        match typ with
        | I8 -> byte ch 0x78
        | I16 -> byte ch 0x77));
    mutability ch mut

  let functype st ch params results =
    byte ch 0x60;
    vec (valtype st) ch params;
    vec (valtype st) ch results

  let subtype st ch { final; supertype; typ } =
    (match supertype, final with
    | None, true -> ()
    | None, false ->
        byte ch 0x50;
        byte ch 0
    | Some supertype, _ ->
        byte ch (if final then 0X4F else 0x50);
        byte ch 1;
        uint ch (typeidx st supertype));
    match typ with
    | Array field_type ->
        byte ch 0x5E;
        fieldtype st ch field_type
    | Struct l ->
        byte ch 0x5F;
        vec (fieldtype st) ch l
    | Func { params; results } -> functype st ch params results

  let rectype st ch l =
    let len = Array.length l in
    if len > 1
    then (
      byte ch 0x4E;
      uint ch len);
    Array.iter ~f:(subtype st ch) l;
    st.type_index_count <- st.type_index_count + len

  let types ch l =
    let st = { type_index_count = 0 } in
    vec (rectype st) ch l;
    st

  let limits ch { min; max; shared; index_type } =
    let kind =
      (if Option.is_none max then 0 else 1)
      + (if shared then 2 else 0)
      +
      match index_type with
      | `I64 -> 4
      | `I32 -> 0
    in
    byte ch kind;
    uint ch min;
    Option.iter ~f:(uint ch) max

  let globaltype st ch mut typ =
    valtype st ch typ;
    mutability ch mut

  let tabletype st ch { limits = l; typ } =
    reftype st ch typ;
    limits ch l

  let imports st ch imports =
    vec
      (fun ch { module_; name = nm; desc } ->
        name ch module_;
        name ch nm;
        match desc with
        | Func typ ->
            byte ch 0x00;
            uint ch typ
        | Table typ ->
            byte ch 0x01;
            tabletype st ch typ
        | Mem l ->
            byte ch 0x03;
            limits ch l
        | Global { mut; typ } ->
            byte ch 0x03;
            globaltype st ch mut typ
        | Tag typ ->
            byte ch 0x04;
            byte ch 0x00;
            uint ch typ)
      ch
      imports

  let functions = vec uint

  let memtype = limits

  let memories = vec memtype

  let export ch kind nm idx =
    name ch nm;
    byte
      ch
      (match kind with
      | Func -> 0
      | Table -> 1
      | Mem -> 2
      | Global -> 3
      | Tag -> 4);
    uint ch idx

  let start = uint

  let tag ch tag =
    byte ch 0;
    uint ch tag

  let tags = vec tag

  let data_count = uint

  let nameassoc ch idx nm =
    uint ch idx;
    name ch nm

  let namemap = vec (fun ch (idx, name) -> nameassoc ch idx name)
end

type 'a exportable_info =
  { mutable func : 'a
  ; mutable table : 'a
  ; mutable mem : 'a
  ; mutable global : 'a
  ; mutable tag : 'a
  }

let iter_exportable_info f { func; table; mem; global; tag } =
  f Func func;
  f Table table;
  f Mem mem;
  f Global global;
  f Tag tag

let map_exportable_info f { func; table; mem; global; tag } =
  { func = f Func func
  ; table = f Table table
  ; mem = f Mem mem
  ; global = f Global global
  ; tag = f Tag tag
  }

let fold_exportable_info f acc { func; table; mem; global; tag } =
  acc |> f Func func |> f Table table |> f Mem mem |> f Global global |> f Tag tag

let init_exportable_info f =
  { func = f (); table = f (); mem = f (); global = f (); tag = f () }

let make_exportable_info v = init_exportable_info (fun _ -> v)

let exportable_kind d =
  match d with
  | 0 -> Func
  | 1 -> Table
  | 2 -> Mem
  | 3 -> Global
  | 4 -> Tag
  | _ -> assert false

let get_exportable_info info kind =
  match kind with
  | Func -> info.func
  | Table -> info.table
  | Mem -> info.mem
  | Global -> info.global
  | Tag -> info.tag

let set_exportable_info info kind v =
  match kind with
  | Func -> info.func <- v
  | Table -> info.table <- v
  | Mem -> info.mem <- v
  | Global -> info.global <- v
  | Tag -> info.tag <- v

module Read = struct
  let header = "\000asm\001\000\000\000"

  let check_header file contents =
    if
      String.length contents < 8
      || not (String.equal header (String.sub contents ~pos:0 ~len:8))
    then failwith (file ^ " is not a Wasm binary file (bad magic)")

  type ch =
    { buf : string
    ; mutable pos : int
    ; limit : int
    }

  let pos_in ch = ch.pos

  let seek_in ch pos = ch.pos <- pos

  let input_byte ch =
    let pos = ch.pos in
    ch.pos <- pos + 1;
    Char.code ch.buf.[pos]

  let peek_byte ch = Char.code ch.buf.[ch.pos]

  let really_input_string ch len =
    let pos = ch.pos in
    ch.pos <- pos + len;
    String.sub ch.buf ~pos ~len

  let rec uint ?(n = 5) ch =
    let i = input_byte ch in
    if n = 1 then assert (i < 16);
    if i < 128 then i else i - 128 + (uint ~n:(n - 1) ch lsl 7)

  let rec sint ?(n = 5) ch =
    let i = input_byte ch in
    if n = 1 then assert (i < 8 || (i > 120 && i < 128));
    if i < 64 then i else if i < 128 then i - 128 else i - 128 + (sint ~n:(n - 1) ch lsl 7)

  let repeat n f ch = Array.init n ~f:(fun _ -> f ch)

  let vec f ch = repeat (uint ch) f ch

  let repeat' n f ch =
    for _ = 1 to n do
      f ch
    done

  let vec' f ch = repeat' (uint ch) f ch

  let name ch = really_input_string ch (uint ch)

  type section =
    { id : int
    ; pos : int
    ; size : int
    }

  type index =
    { sections : section Int.Hashtbl.t
    ; custom_sections : section String.Hashtbl.t
    }

  let next_section ch =
    if pos_in ch = ch.limit
    then None
    else
      let id = input_byte ch in
      let size = uint ch in
      Some { id; pos = pos_in ch; size }

  let skip_section ch { pos; size; _ } = seek_in ch (pos + size)

  let index ch =
    let index =
      { sections = Int.Hashtbl.create 16; custom_sections = String.Hashtbl.create 16 }
    in
    let rec loop () =
      match next_section ch with
      | None -> index
      | Some sect ->
          if sect.id = 0
          then String.Hashtbl.add index.custom_sections (name ch) sect
          else Int.Hashtbl.add index.sections sect.id sect;
          skip_section ch sect;
          loop ()
    in
    loop ()

  type t =
    { ch : ch
    ; mutable type_mapping : int array
    ; mutable type_index_count : int
    ; index : index
    }

  let open_in f buf =
    check_header f buf;
    let ch = { buf; pos = 8; limit = String.length buf } in
    { ch; type_mapping = [||]; type_index_count = 0; index = index ch }

  let find_section contents n =
    match Int.Hashtbl.find contents.index.sections n with
    | { pos; _ } ->
        seek_in contents.ch pos;
        true
    | exception Not_found -> false

  let get_custom_section contents name =
    String.Hashtbl.find_opt contents.index.custom_sections name

  let focus_on_custom_section contents section =
    let pos, limit =
      match get_custom_section contents section with
      | Some { pos; size; _ } -> pos, pos + size
      | None -> 0, 0
    in
    let ch = { buf = contents.ch.buf; pos; limit } in
    if limit > 0 then ignore (name ch);
    { contents with index = index ch }

  module RecTypeTbl = Hashtbl.Make (struct
    type t = rectype

    let hash t =
      (* We have large structs, that tend to hash to the same value *)
      Hashtbl.hash_param 15 100 t

    let storagetype_eq t1 t2 =
      match t1, t2 with
      | Val v1, Val v2 -> valtype_eq v1 v2
      | Packed p1, Packed p2 -> Stdlib.phys_equal p1 p2
      | _ -> false

    let fieldtype_eq { mut = m1; typ = t1 } { mut = m2; typ = t2 } =
      Bool.(m1 = m2) && storagetype_eq t1 t2

    (* Does not allocate and return false on length mismatch *)
    let array_for_all2 p a1 a2 =
      let n1 = Array.length a1 and n2 = Array.length a2 in
      n1 = n2
      &&
      let rec loop p a1 a2 n1 i =
        i = n1 || (p a1.(i) a2.(i) && loop p a1 a2 n1 (succ i))
      in
      loop p a1 a2 n1 0

    let comptype_eq (t1 : comptype) (t2 : comptype) =
      match t1, t2 with
      | Func { params = p1; results = r1 }, Func { params = p2; results = r2 } ->
          array_for_all2 valtype_eq p1 p2 && array_for_all2 valtype_eq r1 r2
      | Struct l1, Struct l2 -> array_for_all2 fieldtype_eq l1 l2
      | Array f1, Array f2 -> fieldtype_eq f1 f2
      | _ -> false

    let subtype_eq
        { final = f1; supertype = s1; typ = t1 }
        { final = f2; supertype = s2; typ = t2 } =
      Bool.(f1 = f2)
      && (match s1, s2 with
         | Some _, None | None, Some _ -> false
         | None, None -> true
         | Some i1, Some i2 -> i1 = i2)
      && comptype_eq t1 t2

    let equal t1 t2 =
      match t1, t2 with
      | [| t1 |], [| t2 |] -> subtype_eq t1 t2
      | _ -> array_for_all2 subtype_eq t1 t2
  end)

  type types =
    { types : int RecTypeTbl.t
    ; mutable last_index : int
    ; mutable rev_list : rectype list
    }

  let create_types () = { types = RecTypeTbl.create 2000; last_index = 0; rev_list = [] }

  let add_rectype types typ =
    try RecTypeTbl.find types.types typ
    with Not_found ->
      let index = types.last_index in
      RecTypeTbl.add types.types typ index;
      types.last_index <- Array.length typ + index;
      types.rev_list <- typ :: types.rev_list;
      index

  let heaptype st ch =
    let i = sint ch in
    match i + 128 with
    | 0X73 -> Nofunc
    | 0x72 -> Noextern
    | 0x71 -> None_
    | 0x70 -> Func
    | 0x6F -> Extern
    | 0x6E -> Any
    | 0x6D -> Eq
    | 0x6C -> I31
    | 0x6B -> Struct
    | 0x6A -> Array
    | _ ->
        if i < 0 then failwith (Printf.sprintf "Unknown heaptype %x@." i);
        let i =
          if i >= st.type_index_count
          then lnot (i - st.type_index_count)
          else st.type_mapping.(i)
        in
        Type i

  let nullable typ = { nullable = true; typ }

  let ref_eq = { nullable = false; typ = Eq }

  let ref_i31 = { nullable = false; typ = I31 }

  let reftype' st i ch =
    match i with
    | 0X73 -> nullable Nofunc
    | 0x72 -> nullable Noextern
    | 0x71 -> nullable None_
    | 0x70 -> nullable Func
    | 0x6F -> nullable Extern
    | 0x6E -> nullable Any
    | 0x6D -> nullable Eq
    | 0x6C -> nullable I31
    | 0x6B -> nullable Struct
    | 0x6A -> nullable Array
    | 0x63 -> nullable (heaptype st ch)
    | 0x64 -> { nullable = false; typ = heaptype st ch }
    | _ -> failwith (Printf.sprintf "Unknown reftype %x@." i)

  let reftype st ch = reftype' st (input_byte ch) ch

  let ref_i31 = Ref ref_i31

  let ref_eq = Ref ref_eq

  let valtype' st i ch =
    match i with
    | 0x7B -> V128
    | 0x7C -> F64
    | 0x7D -> F32
    | 0x7E -> I64
    | 0x7F -> I32
    | 0x64 -> (
        match peek_byte ch with
        | 0x6C ->
            ignore (input_byte ch);
            ref_i31
        | 0x6D ->
            ignore (input_byte ch);
            ref_eq
        | _ -> Ref { nullable = false; typ = heaptype st ch })
    | _ -> Ref (reftype' st i ch)

  let valtype st ch =
    let i = uint ch in
    valtype' st i ch

  let storagetype st ch =
    let i = uint ch in
    match i with
    | 0x78 -> Packed I8
    | 0x77 -> Packed I16
    | _ -> Val (valtype' st i ch)

  let fieldtype st ch =
    let typ = storagetype st ch in
    let mut = input_byte ch <> 0 in
    { mut; typ }

  let comptype st i ch =
    match i with
    | 0x5E -> Array (fieldtype st ch)
    | 0x5F -> Struct (vec (fieldtype st) ch)
    | 0x60 ->
        let params = vec (valtype st) ch in
        let results = vec (valtype st) ch in
        Func { params; results }
    | c -> failwith (Printf.sprintf "Unknown comptype %d" c)

  let supertype st ch =
    match input_byte ch with
    | 0 -> None
    | 1 ->
        let t = uint ch in
        Some
          (if t >= st.type_index_count
           then lnot (t - st.type_index_count)
           else st.type_mapping.(t))
    | _ -> assert false

  let subtype st i ch =
    match i with
    | 0x50 ->
        let supertype = supertype st ch in
        { final = false; supertype; typ = comptype st (input_byte ch) ch }
    | 0x4F ->
        let supertype = supertype st ch in
        { final = true; supertype; typ = comptype st (input_byte ch) ch }
    | _ -> { final = true; supertype = None; typ = comptype st i ch }

  let rectype st ch =
    match input_byte ch with
    | 0x4E -> vec (fun ch -> subtype st (input_byte ch) ch) ch
    | i -> [| subtype st i ch |]

  let type_section st types ch =
    let n = uint ch in
    st.type_mapping <- Array.make n 0;
    st.type_index_count <- 0;
    repeat'
      n
      (fun ch ->
        let ty = rectype st ch in
        let pos = st.type_index_count in
        let pos' = add_rectype types ty in
        let count = Array.length ty in
        let len = Array.length st.type_mapping in
        if pos + count > len
        then (
          let m = Array.make (len + (len / 5) + count) 0 in
          Array.blit ~src:st.type_mapping ~src_pos:0 ~dst:m ~dst_pos:0 ~len;
          st.type_mapping <- m);
        for i = 0 to count - 1 do
          st.type_mapping.(pos + i) <- pos' + i
        done;
        st.type_index_count <- pos + count)
      ch

  let limits ch =
    let kind = input_byte ch in
    assert (kind < 8);
    let shared = kind land 2 <> 0 in
    let index_type = if kind land 4 = 0 then `I32 else `I64 in
    let min = uint ch in
    let max = if kind land 1 = 0 then None else Some (uint ch) in
    { min; max; shared; index_type }

  let memtype = limits

  let tabletype st ch =
    let typ = reftype st ch in
    let limits = limits ch in
    { limits; typ }

  let typeidx st ch = st.type_mapping.(uint ch)

  let globaltype st ch =
    let typ = valtype st ch in
    let mut = input_byte ch in
    assert (mut < 2);
    { mut = mut <> 0; typ }

  let import tbl st ch =
    let module_ = name ch in
    let name = name ch in
    let d = uint ch in
    if d > 4 then failwith (Printf.sprintf "Unknown import %x@." d);
    let importdesc : importdesc =
      match d with
      | 0 -> Func st.type_mapping.(uint ch)
      | 1 -> Table (tabletype st ch)
      | 2 -> Mem (memtype ch)
      | 3 -> Global (globaltype st ch)
      | 4 ->
          let b = uint ch in
          assert (b = 0);
          Tag st.type_mapping.(uint ch)
      | _ -> assert false
    in
    let entry = { module_; name; desc = importdesc } in
    let kind = exportable_kind d in
    set_exportable_info tbl kind (entry :: get_exportable_info tbl kind)

  let export tbl ch =
    let name = name ch in
    let d = uint ch in
    if d > 4 then failwith (Printf.sprintf "Unknown export %x@." d);
    let idx = uint ch in
    let entry = name, idx in
    let kind = exportable_kind d in
    set_exportable_info tbl kind (entry :: get_exportable_info tbl kind)

  type interface =
    { imports : import array exportable_info
    ; exports : (string * int) list exportable_info
    }

  let type_section types contents =
    if find_section contents 1 then type_section contents types contents.ch

  let interface contents =
    let imports =
      if find_section contents 2
      then (
        let tbl = make_exportable_info [] in
        vec' (import tbl contents) contents.ch;
        map_exportable_info (fun _ l -> Array.of_list (List.rev l)) tbl)
      else make_exportable_info [||]
    in
    let exports =
      let tbl = make_exportable_info [] in
      if find_section contents 7 then vec' (export tbl) contents.ch;
      tbl
    in
    { imports; exports }

  let functions contents =
    if find_section contents 3
    then vec (fun ch -> typeidx contents ch) contents.ch
    else [||]

  let memories contents = if find_section contents 5 then vec memtype contents.ch else [||]

  let tag contents ch =
    let b = input_byte ch in
    assert (b = 0);
    typeidx contents ch

  let tags contents =
    if find_section contents 13 then vec (tag contents) contents.ch else [||]

  let data_count contents =
    if find_section contents 12
    then uint contents.ch
    else if find_section contents 11
    then uint contents.ch
    else 0

  let start contents = if find_section contents 8 then Some (uint contents.ch) else None

  let nameassoc ch =
    let idx = uint ch in
    let name = name ch in
    idx, name

  let namemap contents = vec nameassoc contents.ch
end

module Scan = struct
  let debug = false

  type maps =
    { typ : int array
    ; func : int array
    ; table : int array
    ; mem : int array
    ; global : int array
    ; elem : int array
    ; data : int array
    ; tag : int array
    }

  let default_maps =
    { typ = [||]
    ; func = [||]
    ; table = [||]
    ; mem = [||]
    ; global = [||]
    ; elem = [||]
    ; data = [||]
    ; tag = [||]
    }

  type resize_data = Wasm_source_map.resize_data =
    { mutable i : int
    ; mutable pos : int array
    ; mutable delta : int array
    }

  let push_resize resize_data pos delta =
    let p = resize_data.pos in
    let i = resize_data.i in
    let p =
      if i = Array.length p
      then (
        let p = Array.make (2 * i) 0 in
        let d = Array.make (2 * i) 0 in
        Array.blit ~src:resize_data.pos ~src_pos:0 ~dst:p ~dst_pos:0 ~len:i;
        Array.blit ~src:resize_data.delta ~src_pos:0 ~dst:d ~dst_pos:0 ~len:i;
        resize_data.pos <- p;
        resize_data.delta <- d;
        p)
      else p
    in
    p.(i) <- pos;
    resize_data.delta.(i) <- delta;
    resize_data.i <- i + 1

  let create_resize_data () =
    { i = 0; pos = Array.make 1024 0; delta = Array.make 1024 0 }

  let clear_resize_data resize_data = resize_data.i <- 0

  type position_data =
    { mutable i : int
    ; mutable pos : int array
    }

  let create_position_data () = { i = 0; pos = Array.make 100 0 }

  let clear_position_data position_data = position_data.i <- 0

  let push_position position_data pos =
    let p = position_data.pos in
    let i = position_data.i in
    let p =
      if i = Array.length p
      then (
        let p = Array.make (2 * i) 0 in
        Array.blit ~src:position_data.pos ~src_pos:0 ~dst:p ~dst_pos:0 ~len:i;
        position_data.pos <- p;
        p)
      else p
    in
    p.(i) <- pos;
    position_data.i <- i + 1

  let scanner report mark maps buf code =
    let rec output_uint buf i =
      if i < 128
      then Buffer.add_char buf (Char.chr i)
      else (
        Buffer.add_char buf (Char.chr (128 + (i land 127)));
        output_uint buf (i lsr 7))
    in
    let rec output_sint buf i =
      if i >= -64 && i < 64
      then Buffer.add_char buf (Char.chr (i land 127))
      else (
        Buffer.add_char buf (Char.chr (128 + (i land 127)));
        output_sint buf (i asr 7))
    in
    let start = ref 0 in
    let get pos = Char.code (String.get code pos) in
    let rec int pos = if get pos >= 128 then int (pos + 1) else pos + 1 in
    let rec uint32 pos =
      let i = get pos in
      if i < 128
      then pos + 1, i
      else
        let pos, i' = pos + 1 |> uint32 in
        pos, (i' lsl 7) + (i land 0x7f)
    in
    let rec sint32 pos =
      let i = get pos in
      if i < 64
      then pos + 1, i
      else if i < 128
      then pos + 1, i - 128
      else
        let pos, i' = pos + 1 |> sint32 in
        pos, i - 128 + (i' lsl 7)
    in
    let rec repeat n f pos = if n = 0 then pos else repeat (n - 1) f (f pos) in
    let vector f pos =
      let pos, i =
        let i = get pos in
        if i < 128 then pos + 1, i else uint32 pos
      in
      repeat i f pos
    in
    let name pos =
      let pos', i =
        let i = get pos in
        if i < 128 then pos + 1, i else uint32 pos
      in
      pos' + i
    in
    let flush' pos pos' =
      if !start < pos then Buffer.add_substring buf code !start (pos - !start);
      start := pos'
    in
    let flush pos = flush' pos pos in
    let rewrite map pos =
      let pos', idx =
        let i = get pos in
        if i < 128
        then pos + 1, i
        else
          let i' = get (pos + 1) in
          if i' < 128 then pos + 2, (i' lsl 7) + (i land 0x7f) else uint32 pos
      in
      let idx' = map idx in
      if idx <> idx'
      then (
        flush' pos pos';
        let p = Buffer.length buf in
        output_uint buf idx';
        let p' = Buffer.length buf in
        let dp = p' - p in
        let dpos = pos' - pos in
        if dp <> dpos then report pos' (dp - dpos));
      pos'
    in
    let rewrite_signed map pos =
      let pos', idx =
        let i = get pos in
        if i < 64 then pos + 1, i else if i < 128 then pos + 1, i - 128 else sint32 pos
      in
      let idx' = map idx in
      if idx <> idx'
      then (
        flush' pos pos';
        let p = Buffer.length buf in
        output_sint buf idx';
        let p' = Buffer.length buf in
        let dp = p' - p in
        let dpos = pos' - pos in
        if dp <> dpos then report pos (dp - dpos));
      pos'
    in
    let typ_map idx = maps.typ.(idx) in
    let typeidx pos = rewrite typ_map pos in
    let signed_typeidx pos = rewrite_signed typ_map pos in
    let func_map idx = maps.func.(idx) in
    let funcidx pos = rewrite func_map pos in
    let table_map idx = maps.table.(idx) in
    let tableidx pos = rewrite table_map pos in
    let mem_map idx = maps.mem.(idx) in
    let memidx pos = rewrite mem_map pos in
    let global_map idx = maps.global.(idx) in
    let globalidx pos = rewrite global_map pos in
    let elem_map idx = maps.elem.(idx) in
    let elemidx pos = rewrite elem_map pos in
    let data_map idx = maps.data.(idx) in
    let dataidx pos = rewrite data_map pos in
    let tag_map idx = maps.tag.(idx) in
    let tagidx pos = rewrite tag_map pos in
    let labelidx = int in
    let localidx = int in
    let laneidx pos = pos + 1 in
    let heaptype pos =
      let c = get pos in
      if c >= 64 && c < 128 then (* absheaptype *) pos + 1 else signed_typeidx pos
    in
    let absheaptype pos =
      match get pos with
      | 0X73 (* nofunc *)
      | 0x72 (* noextern *)
      | 0x71 (* none *)
      | 0x70 (* func *)
      | 0x6F (* extern *)
      | 0x6E (* any *)
      | 0x6D (* eq *)
      | 0x6C (* i31 *)
      | 0x6B (* struct *)
      | 0x6A (* array *) -> pos + 1
      | c -> failwith (Printf.sprintf "Bad heap type 0x%02X@." c)
    in
    let reftype pos =
      match get pos with
      | 0x63 | 0x64 -> pos + 1 |> heaptype
      | _ -> pos |> absheaptype
    in
    let valtype pos =
      let c = get pos in
      match c with
      | 0x63 (* ref null ht *) | 0x64 (* ref ht *) -> pos + 1 |> heaptype
      | _ -> pos + 1
    in
    let blocktype pos =
      let c = get pos in
      if c >= 64 && c < 128 then pos |> valtype else pos |> signed_typeidx
    in
    let memarg pos =
      let pos', c = uint32 pos in
      if c < 64
      then (
        if mem_map 0 <> 0
        then (
          flush' pos pos';
          let p = Buffer.length buf in
          output_uint buf (c + 64);
          output_uint buf (mem_map 0);
          let p' = Buffer.length buf in
          let dp = p' - p in
          let dpos = pos' - pos in
          if dp <> dpos then report pos (dp - dpos));
        pos' |> int)
      else pos' |> memidx |> int
    in
    let rec instructions pos =
      if debug then Format.eprintf "0x%02X (@%d)@." (get pos) pos;
      match get pos with
      (* Control instruction *)
      | 0x00 (* unreachable *) | 0x01 (* nop *) | 0x0F (* return *) ->
          pos + 1 |> instructions
      | 0x02 (* block *) | 0x03 (* loop *) ->
          pos + 1 |> blocktype |> instructions |> block_end |> instructions
      | 0x04 (* if *) -> pos + 1 |> blocktype |> instructions |> opt_else |> instructions
      | 0x0C (* br *)
      | 0x0D (* br_if *)
      | 0xD5 (* br_on_null *)
      | 0xD6 (* br_on_non_null *) -> pos + 1 |> labelidx |> instructions
      | 0x0E (* br_table *) -> pos + 1 |> vector labelidx |> labelidx |> instructions
      | 0x10 (* call *) | 0x12 (* return_call *) -> pos + 1 |> funcidx |> instructions
      | 0x11 (* call_indirect *) | 0x13 (* return_call_indirect *) ->
          pos + 1 |> typeidx |> tableidx |> instructions
      | 0x14 (* call_ref *) | 0x15 (* return_call_ref *) ->
          pos + 1 |> typeidx |> instructions
      (* Exceptions *)
      | 0x06 (* try *) -> pos + 1 |> blocktype |> instructions |> opt_catch
      | 0x08 (* throw *) -> pos + 1 |> tagidx |> instructions
      | 0x09 (* rethrow *) -> pos + 1 |> int |> instructions
      | 0x0A (* throw_ref *) -> pos + 1 |> instructions
      (* Parametric instructions *)
      | 0x1A (* drop *) | 0x1B (* select *) -> pos + 1 |> instructions
      | 0x1C (* select *) -> pos + 1 |> vector valtype |> instructions
      | 0x1F (* try_table *) ->
          pos + 1
          |> blocktype
          |> vector catch
          |> instructions
          |> block_end
          |> instructions
      (* Variable instructions *)
      | 0x20 (* local.get *) | 0x21 (* local.set *) | 0x22 (* local.tee *) ->
          pos + 1 |> localidx |> instructions
      | 0x23 (* global.get *) | 0x24 (* global.set *) ->
          pos + 1 |> globalidx |> instructions
      (* Table instructions *)
      | 0x25 (* table.get *) | 0x26 (* table.set *) -> pos + 1 |> tableidx |> instructions
      (* Memory instructions *)
      | 0x28
      | 0x29
      | 0x2A
      | 0x2B
      | 0x2C
      | 0x2D
      | 0x2E
      | 0x2F
      | 0x30
      | 0x31
      | 0x32
      | 0x33
      | 0x34
      | 0x35 (* load *)
      | 0x36 | 0x37 | 0x38 | 0x39 | 0x3A | 0x3B | 0x3C | 0x3D | 0x3E (* store *) ->
          pos + 1 |> memarg |> instructions
      | 0x3F | 0x40 -> pos + 1 |> memidx |> instructions
      (* Numeric instructions *)
      | 0x41 (* i32.const *) | 0x42 (* i64.const *) -> pos + 1 |> int |> instructions
      | 0x43 (* f32.const *) -> pos + 5 |> instructions
      | 0x44 (* f64.const *) -> pos + 9 |> instructions
      | 0x45
      | 0x46
      | 0x47
      | 0x48
      | 0x49
      | 0x4A
      | 0x4B
      | 0x4C
      | 0x4D
      | 0x4E
      | 0x4F
      | 0x50
      | 0x51
      | 0x52
      | 0x53
      | 0x54
      | 0x55
      | 0x56
      | 0x57
      | 0x58
      | 0x59
      | 0x5A
      | 0x5B
      | 0x5C
      | 0x5D
      | 0x5E
      | 0x5F
      | 0x60
      | 0x61
      | 0x62
      | 0x63
      | 0x64
      | 0x65
      | 0x66
      | 0x67
      | 0x68
      | 0x69
      | 0x6A
      | 0x6B
      | 0x6C
      | 0x6D
      | 0x6E
      | 0x6F
      | 0x70
      | 0x71
      | 0x72
      | 0x73
      | 0x74
      | 0x75
      | 0x76
      | 0x77
      | 0x78
      | 0x79
      | 0x7A
      | 0x7B
      | 0x7C
      | 0x7D
      | 0x7E
      | 0x7F
      | 0x80
      | 0x81
      | 0x82
      | 0x83
      | 0x84
      | 0x85
      | 0x86
      | 0x87
      | 0x88
      | 0x89
      | 0x8A
      | 0x8B
      | 0x8C
      | 0x8D
      | 0x8E
      | 0x8F
      | 0x90
      | 0x91
      | 0x92
      | 0x93
      | 0x94
      | 0x95
      | 0x96
      | 0x97
      | 0x98
      | 0x99
      | 0x9A
      | 0x9B
      | 0x9C
      | 0x9D
      | 0x9E
      | 0x9F
      | 0xA0
      | 0xA1
      | 0xA2
      | 0xA3
      | 0xA4
      | 0xA5
      | 0xA6
      | 0xA7
      | 0xA8
      | 0xA9
      | 0xAA
      | 0xAB
      | 0xAC
      | 0xAD
      | 0xAE
      | 0xAF
      | 0xB0
      | 0xB1
      | 0xB2
      | 0xB3
      | 0xB4
      | 0xB5
      | 0xB6
      | 0xB7
      | 0xB8
      | 0xB9
      | 0xBA
      | 0xBB
      | 0xBC
      | 0xBD
      | 0xBE
      | 0xBF
      | 0xC0
      | 0xC1
      | 0xC2
      | 0xC3
      | 0xC4 -> pos + 1 |> instructions
      (* Reference instructions *)
      | 0xD0 (* ref.null *) -> pos + 1 |> heaptype |> instructions
      | 0xD1 (* ref.is_null *) | 0xD3 (* ref.eq *) | 0xD4 (* ref.as_non_null *) ->
          pos + 1 |> instructions
      | 0xD2 (* ref.func *) -> pos + 1 |> funcidx |> instructions
      | 0xFB -> pos + 1 |> gc_instruction
      | 0xFC -> (
          if debug then Format.eprintf "  %d@." (get (pos + 1));
          match get (pos + 1) with
          | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 (* xx.trunc_sat_xxx_x *) ->
              pos + 2 |> instructions
          | 8 (* memory.init *) -> pos + 2 |> dataidx |> memidx |> instructions
          | 9 (* data.drop *) -> pos + 2 |> dataidx |> instructions
          | 10 (* memory.copy *) -> pos + 2 |> memidx |> memidx |> instructions
          | 11 (* memory.fill *) -> pos + 2 |> memidx |> instructions
          | 12 (* table.init *) -> pos + 2 |> elemidx |> tableidx |> instructions
          | 13 (* elem.drop *) -> pos + 2 |> elemidx |> instructions
          | 14 (* table.copy *) -> pos + 2 |> tableidx |> tableidx |> instructions
          | 15 (* table.grow *) | 16 (* table.size *) | 17 (* table.fill *) ->
              pos + 2 |> tableidx |> instructions
          | c -> failwith (Printf.sprintf "Bad instruction 0xFC 0x%02X" c))
      | 0xFD -> pos + 1 |> vector_instruction
      | 0xFE -> pos + 1 |> atomic_instruction
      | _ -> pos
    and gc_instruction pos =
      if debug then Format.eprintf "  %d@." (get pos);
      match get pos with
      | 0 (* struct.new *)
      | 1 (* struct.new_default *)
      | 6 (* array.new *)
      | 7 (* array.new_default *)
      | 11 (* array.get *)
      | 12 (* array.get_s *)
      | 13 (* array.get_u *)
      | 14 (* array.set *)
      | 16 (* array.fill *) -> pos + 1 |> typeidx |> instructions
      | 2 (* struct.get *)
      | 3 (* struct.get_s *)
      | 4 (* struct.get_u *)
      | 5 (* struct.set *)
      | 8 (* array.new_fixed *) -> pos + 1 |> typeidx |> int |> instructions
      | 9 (* array.new_data *) | 18 (* array.init_data *) ->
          pos + 1 |> typeidx |> dataidx |> instructions
      | 10 (* array.new_elem *) | 19 (* array.init_elem *) ->
          pos + 1 |> typeidx |> elemidx |> instructions
      | 15 (* array.len *)
      | 26 (* any.convert_extern *)
      | 27 (* extern.convert_any *)
      | 28 (* ref.i31 *)
      | 29 (* i31.get_s *)
      | 30 (* i31.get_u *) -> pos + 1 |> instructions
      | 17 (* array.copy *) -> pos + 1 |> typeidx |> typeidx |> instructions
      | 20 | 21 (* ref_test *) | 22 | 23 (* ref.cast*) ->
          pos + 1 |> heaptype |> instructions
      | 24 (* br_on_cast *) | 25 (* br_on_cast_fail *) ->
          pos + 2 |> labelidx |> heaptype |> heaptype |> instructions
      | c -> failwith (Printf.sprintf "Bad instruction 0xFB 0x%02X" c)
    and vector_instruction pos =
      if debug then Format.eprintf "  %d@." (get pos);
      let pos, i = uint32 pos in
      match i with
      | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 92 | 93 (* v128.load / store *)
        -> pos + 1 |> memarg |> instructions
      | 84 | 85 | 86 | 87 | 88 | 89 | 90 | 91 (* v128.load/store_lane *) ->
          pos + 1 |> memarg |> laneidx |> instructions
      | 12 (* v128.const *) | 13 (* v128.shuffle *) -> pos + 17 |> instructions
      | 21
      | 22
      | 23
      | 24
      | 25
      | 26
      | 27
      | 28
      | 29
      | 30
      | 31
      | 32
      | 33
      | 34 (* xx.extract/replace_lane *) -> pos + 1 |> laneidx |> instructions
      | ( 162
        | 165
        | 166
        | 175
        | 176
        | 178
        | 179
        | 180
        | 187
        | 194
        | 197
        | 198
        | 207
        | 208
        | 210
        | 211
        | 212
        | 226
        | 238 ) as c -> failwith (Printf.sprintf "Bad instruction 0xFD 0x%02X" c)
      | c ->
          if c <= 275
          then pos + 1 |> instructions
          else failwith (Printf.sprintf "Bad instruction 0xFD 0x%02X" c)
    and atomic_instruction pos =
      if debug then Format.eprintf "  %d@." (get pos);
      match get pos with
      | 0 (* memory.atomic.notify *)
      | 1 | 2 (* memory.atomic.waitxx *)
      | 16 | 17 | 18 | 19 | 20 | 21 | 22 (* xx.atomic.load *)
      | 23 | 24 | 25 | 26 | 27 | 28 | 29 (* xx.atomic.store *)
      | 30 | 31 | 32 | 33 | 34 | 35 | 36 (* xx.atomic.rmw.add *)
      | 37 | 38 | 39 | 40 | 41 | 42 | 43 (* xx.atomic.rmw.sub *)
      | 44 | 45 | 46 | 47 | 48 | 49 | 50 (* xx.atomic.rmw.and *)
      | 51 | 52 | 53 | 54 | 55 | 56 | 57 (* xx.atomic.rmw.or *)
      | 58 | 59 | 60 | 61 | 62 | 63 | 64 (* xx.atomic.rmw.xor *)
      | 65 | 66 | 67 | 68 | 69 | 70 | 71 (* xx.atomic.rmw.xchg *)
      | 72 | 73 | 74 | 75 | 76 | 77 | 78 (* xx.atomic.rmw.cmpxchg *) ->
          pos + 1 |> memarg |> instructions
      | 3 (* memory.fence *) ->
          let c = get pos + 1 in
          assert (c = 0);
          pos + 2 |> instructions
      | c -> failwith (Printf.sprintf "Bad instruction 0xFE 0x%02X" c)
    and opt_else pos =
      if debug then Format.eprintf "0x%02X (@%d) else@." (get pos) pos;
      match get pos with
      | 0x05 (* else *) -> pos + 1 |> instructions |> block_end |> instructions
      | _ -> pos |> block_end |> instructions
    and opt_catch pos =
      if debug then Format.eprintf "0x%02X (@%d) catch@." (get pos) pos;
      match get pos with
      | 0x07 (* catch *) -> pos + 1 |> tagidx |> instructions |> opt_catch
      | 0x05 (* catch_all *) -> pos + 1 |> instructions |> block_end |> instructions
      | _ -> pos |> block_end |> instructions
    and catch pos =
      match get pos with
      | 0 (* catch *) | 1 (* catch_ref *) -> pos + 1 |> tagidx |> labelidx
      | 2 (* catch_all *) | 3 (* catch_all_ref *) -> pos + 1 |> labelidx
      | c -> failwith (Printf.sprintf "bad catch 0x02%d@." c)
    and block_end pos =
      if debug then Format.eprintf "0x%02X (@%d) block end@." (get pos) pos;
      match get pos with
      | 0x0B -> pos + 1
      | c -> failwith (Printf.sprintf "Bad instruction 0x%02X" c)
    in
    let locals pos = pos |> int |> valtype in
    let expr pos = pos |> instructions |> block_end in
    let func pos =
      start := pos;
      pos |> vector locals |> expr |> flush
    in
    let mut pos = pos + 1 in
    let limits pos =
      let c = get pos in
      assert (c < 8);
      if c land 1 = 0 then pos + 1 |> int else pos + 1 |> int |> int
    in
    let tabletype pos =
      mark pos;
      pos |> reftype |> limits
    in
    let table pos =
      match get pos with
      | 0x40 ->
          assert (get (pos + 1) = 0);
          pos + 2 |> tabletype |> expr
      | _ -> pos |> tabletype
    in
    let table_section ~count pos =
      start := pos;
      pos |> repeat count table |> flush
    in
    let globaltype pos =
      mark pos;
      pos |> valtype |> mut
    in
    let global pos = pos |> globaltype |> expr in
    let global_section ~count pos =
      start := pos;
      pos |> repeat count global |> flush
    in
    let elemkind pos =
      assert (get pos = 0);
      pos + 1
    in
    let elem pos =
      match get pos with
      | 0 -> pos + 1 |> expr |> vector funcidx
      | 1 -> pos + 1 |> elemkind |> vector funcidx
      | 2 -> pos + 1 |> tableidx |> expr |> elemkind |> vector funcidx
      | 3 -> pos + 1 |> elemkind |> vector funcidx
      | 4 -> pos + 1 |> expr |> vector expr
      | 5 -> pos + 1 |> reftype |> vector expr
      | 6 -> pos + 1 |> tableidx |> expr |> reftype |> vector expr
      | 7 -> pos + 1 |> reftype |> vector expr
      | c -> failwith (Printf.sprintf "Bad element 0x%02X" c)
    in
    let bytes pos =
      let pos, len = uint32 pos in
      pos + len
    in
    let data pos =
      match get pos with
      | 0 -> pos + 1 |> expr |> bytes
      | 1 -> pos + 1 |> bytes
      | 2 -> pos + 1 |> memidx |> expr |> bytes
      | c -> failwith (Printf.sprintf "Bad data segment 0x%02X" c)
    in
    let elem_section ~count pos =
      start := pos;
      !start |> repeat count elem |> flush
    in
    let data_section ~count pos =
      start := pos;
      !start |> repeat count data |> flush
    in
    let local_nameassoc pos = pos |> localidx |> name in
    let local_namemap pos =
      start := pos;
      pos |> vector local_nameassoc |> flush
    in
    table_section, global_section, elem_section, data_section, func, local_namemap

  let table_section positions maps buf s =
    let table_section, _, _, _, _, _ =
      scanner (fun _ _ -> ()) (fun pos -> push_position positions pos) maps buf s
    in
    table_section

  let global_section positions maps buf s =
    let _, global_section, _, _, _, _ =
      scanner (fun _ _ -> ()) (fun pos -> push_position positions pos) maps buf s
    in
    global_section

  let elem_section maps buf s =
    let _, _, elem_section, _, _, _ = scanner (fun _ _ -> ()) (fun _ -> ()) maps buf s in
    elem_section

  let data_section maps buf s =
    let _, _, _, data_section, _, _ = scanner (fun _ _ -> ()) (fun _ -> ()) maps buf s in
    data_section

  let func resize_data maps buf s =
    let _, _, _, _, func, _ =
      scanner
        (fun pos delta -> push_resize resize_data pos delta)
        (fun _ -> ())
        maps
        buf
        s
    in
    func

  let local_namemap buf s =
    let _, _, _, _, _, local_namemap =
      scanner (fun _ _ -> ()) (fun _ -> ()) default_maps buf s
    in
    local_namemap
end

let interface types contents =
  Read.type_section types contents;
  Read.interface contents

type t =
  { module_name : string
  ; file : string
  ; contents : Read.t
  ; source_map_contents : Source_map.Standard.t option
  }

type import_status =
  | Resolved of int * int
  | Unresolved of int

let check_limits export import =
  export.min >= import.min
  &&
  match export.max, import.max with
  | _, None -> true
  | None, Some _ -> false
  | Some e, Some i -> e <= i

let rec subtype subtyping_info (i : int) i' =
  i = i'
  ||
  match subtyping_info.(i).supertype with
  | None -> false
  | Some s -> subtype subtyping_info s i'

let heap_subtype (subtyping_info : subtype array) (ty : heaptype) (ty' : heaptype) =
  match ty, ty' with
  | (Func | Nofunc), Func
  | Nofunc, Nofunc
  | (Extern | Noextern), Extern
  | (Any | Eq | I31 | Struct | Array | None_ | Type _), Any
  | (Eq | I31 | Struct | Array | None_ | Type _), Eq
  | (I31 | None_), I31
  | (Struct | None_), Struct
  | (Array | None_), Array
  | None_, None_ -> true
  | Type i, Struct -> (
      match subtyping_info.(i).typ with
      | Struct _ -> true
      | Array _ | Func _ -> false)
  | Type i, Array -> (
      match subtyping_info.(i).typ with
      | Array _ -> true
      | Struct _ | Func _ -> false)
  | Type i, Func -> (
      match subtyping_info.(i).typ with
      | Func _ -> true
      | Struct _ | Array _ -> false)
  | Type i, Type i' -> subtype subtyping_info i i'
  | _ -> false

let ref_subtype subtyping_info { nullable; typ } { nullable = nullable'; typ = typ' } =
  ((not nullable) || nullable') && heap_subtype subtyping_info typ typ'

let val_subtype subtyping_info ty ty' =
  match ty, ty' with
  | Ref t, Ref t' -> ref_subtype subtyping_info t t'
  | _ -> Stdlib.phys_equal ty ty'

let check_export_import_types ~subtyping_info ~files i (desc : importdesc) i' import =
  let ok =
    match desc, import.desc with
    | Func t, Func t' -> subtype subtyping_info t t'
    | Table { limits; typ }, Table { limits = limits'; typ = typ' } ->
        check_limits limits limits' && reftype_eq typ typ'
    | Mem limits, Mem limits' -> check_limits limits limits'
    | Global { mut; typ }, Global { mut = mut'; typ = typ' } ->
        Bool.(mut = mut')
        && if mut then valtype_eq typ typ' else val_subtype subtyping_info typ typ'
    | Tag t, Tag t' -> t = t'
    | _ -> false
  in
  if not ok
  then
    failwith
      (Printf.sprintf
         "In module %s, the import %s / %s refers to an export in module %s of an \
          incompatible type"
         files.(i').file
         import.module_
         import.name
         files.(i).file)

let build_mappings resolved_imports unresolved_imports kind counts =
  let current_offset = ref (get_exportable_info unresolved_imports kind) in
  let mappings =
    Array.mapi
      ~f:(fun i count ->
        let imports = get_exportable_info resolved_imports.(i) kind in
        let import_count = Array.length imports in
        let offset = !current_offset - import_count in
        current_offset := !current_offset + count;
        Array.init
          (Array.length imports + count)
          ~f:(fun i ->
            if i < import_count
            then
              match imports.(i) with
              | Unresolved i -> i
              | Resolved _ -> -1
            else i + offset))
      counts
  in
  Array.iteri
    ~f:(fun i map ->
      let imports = get_exportable_info resolved_imports.(i) kind in
      for i = 0 to Array.length imports - 1 do
        match imports.(i) with
        | Unresolved _ -> ()
        | Resolved (j, k) -> map.(i) <- mappings.(j).(k)
      done)
    mappings;
  mappings

let build_simple_mappings ~counts =
  let current_offset = ref 0 in
  Array.map
    ~f:(fun count ->
      let offset = !current_offset in
      current_offset := !current_offset + count;
      Array.init count ~f:(fun j -> j + offset))
    counts

let add_section out_ch ~id ?count buf =
  match count with
  | Some 0 -> Buffer.clear buf
  | _ ->
      let buf' = Buffer.create 5 in
      Option.iter ~f:(fun c -> Write.uint buf' c) count;
      output_byte out_ch id;
      output_uint out_ch (Buffer.length buf' + Buffer.length buf);
      Buffer.output_buffer out_ch buf';
      Buffer.output_buffer out_ch buf;
      Buffer.clear buf

let add_subsection buf ~id ?count buf' =
  match count with
  | Some 0 -> Buffer.clear buf'
  | _ ->
      let buf'' = Buffer.create 5 in
      Option.iter ~f:(fun c -> Write.uint buf'' c) count;
      Buffer.add_char buf (Char.chr id);
      Write.uint buf (Buffer.length buf'' + Buffer.length buf');
      Buffer.add_buffer buf buf'';
      Buffer.add_buffer buf buf';
      Buffer.clear buf'

let check_exports_against_imports
    ~intfs
    ~subtyping_info
    ~resolved_imports
    ~files
    ~kind
    ~to_desc =
  Array.iteri
    ~f:(fun i intf ->
      let imports = get_exportable_info intf.Read.imports kind in
      let statuses = get_exportable_info resolved_imports.(i) kind in
      Array.iter2
        ~f:(fun import status ->
          match status with
          | Unresolved _ -> ()
          | Resolved (i', idx') -> (
              match to_desc i' idx' with
              | None -> ()
              | Some desc ->
                  check_export_import_types ~subtyping_info ~files i' desc i import))
        imports
        statuses)
    intfs

let read_desc_from_file ~intfs ~files ~positions ~read i j =
  let offset = Array.length (get_exportable_info intfs.(i).Read.imports Table) in
  if j < offset
  then None
  else
    let { contents; _ } = files.(i) in
    Read.seek_in contents.ch positions.(i).Scan.pos.(j - offset);
    Some (read contents)

let index_in_output ~unresolved_imports ~mappings ~kind ~get i' idx' =
  let offset = get_exportable_info unresolved_imports kind in
  let idx'' = mappings.(i').(idx') - offset in
  if idx'' >= 0 then Some (get idx'') else None

let write_simple_section
    ~intfs
    ~subtyping_info
    ~resolved_imports
    ~unresolved_imports
    ~files
    ~out_ch
    ~buf
    ~kind
    ~id
    ~read
    ~to_type
    ~write =
  let data = Array.map ~f:(fun f -> read f.contents) files in
  let entries = Array.concat (Array.to_list data) in
  if Array.length entries <> 0
  then (
    write buf entries;
    add_section out_ch ~id buf);
  let counts = Array.map ~f:Array.length data in
  let mappings = build_mappings resolved_imports unresolved_imports kind counts in
  check_exports_against_imports
    ~intfs
    ~subtyping_info
    ~resolved_imports
    ~files
    ~kind
    ~to_desc:
      (index_in_output ~unresolved_imports ~mappings ~kind ~get:(fun idx ->
           to_type entries.(idx)));
  mappings

let write_section_with_scan ~files ~out_ch ~buf ~id ~scan =
  let counts =
    Array.mapi
      ~f:(fun i { contents; _ } ->
        if Read.find_section contents id
        then (
          let count = Read.uint contents.ch in
          scan
            i
            { Scan.default_maps with typ = contents.type_mapping }
            buf
            contents.ch.buf
            ~count
            contents.ch.pos;
          count)
        else 0)
      files
  in
  add_section out_ch ~id ~count:(Array.fold_left ~f:( + ) ~init:0 counts) buf;
  counts

let write_simple_namemap ~name_sections ~name_section_buffer ~buf ~section_id ~mappings =
  let count = ref 0 in
  Array.iter2
    ~f:(fun name_section mapping ->
      if Read.find_section name_section section_id
      then (
        let map = Read.namemap name_section in
        Array.iter ~f:(fun (idx, name) -> Write.nameassoc buf mapping.(idx) name) map;
        count := !count + Array.length map))
    name_sections
    mappings;
  add_subsection name_section_buffer ~id:section_id ~count:!count buf

let write_namemap
    ~resolved_imports
    ~unresolved_imports
    ~name_sections
    ~name_section_buffer
    ~buf
    ~kind
    ~section_id
    ~mappings =
  let import_names = Array.make (get_exportable_info unresolved_imports kind) None in
  Array.iteri
    ~f:(fun i name_section ->
      if Read.find_section name_section section_id
      then
        let imports = get_exportable_info resolved_imports.(i) kind in
        let import_count = Array.length imports in
        let n = Read.uint name_section.ch in
        let rec loop j =
          if j < n
          then
            let idx = Read.uint name_section.ch in
            let name = Read.name name_section.ch in
            if idx < import_count
            then (
              let idx' =
                match imports.(idx) with
                | Unresolved idx' -> idx'
                | Resolved (i', idx') -> mappings.(i').(idx')
              in
              if idx' < Array.length import_names && Option.is_none import_names.(idx')
              then import_names.(idx') <- Some name;
              loop (j + 1))
        in
        loop 0)
    name_sections;
  let count = ref 0 in
  Array.iteri
    ~f:(fun idx name ->
      match name with
      | None -> ()
      | Some name ->
          incr count;
          Write.nameassoc buf idx name)
    import_names;
  Array.iteri
    ~f:(fun i name_section ->
      if Read.find_section name_section section_id
      then
        let mapping = mappings.(i) in
        let imports = get_exportable_info resolved_imports.(i) kind in
        let import_count = Array.length imports in
        let n = Read.uint name_section.ch in
        let ch = name_section.ch in
        for _ = 1 to n do
          let idx = Read.uint ch in
          let len = Read.uint ch in
          if idx >= import_count
          then (
            incr count;
            Write.uint buf mapping.(idx);
            Write.uint buf len;
            Buffer.add_substring buf ch.buf ch.pos len);
          ch.pos <- ch.pos + len
        done)
    name_sections;
  add_subsection name_section_buffer ~id:section_id ~count:!count buf

let write_indirectnamemap ~name_sections ~name_section_buffer ~buf ~section_id ~mappings =
  let count = ref 0 in
  Array.iter2
    ~f:(fun name_section mapping ->
      if Read.find_section name_section section_id
      then (
        let n = Read.uint name_section.ch in
        let scan_map = Scan.local_namemap buf name_section.ch.buf in
        for _ = 1 to n do
          let idx = mapping.(Read.uint name_section.ch) in
          Write.uint buf idx;
          let p = Buffer.length buf in
          scan_map name_section.ch.pos;
          name_section.ch.pos <- name_section.ch.pos + Buffer.length buf - p
        done;
        count := !count + n))
    name_sections
    mappings;
  add_subsection name_section_buffer ~id:section_id ~count:!count buf

let rec resolve
    depth
    ~files
    ~intfs
    ~subtyping_info
    ~exports
    ~kind
    i
    ({ module_; name; _ } as import) =
  let i', index = Poly.Hashtbl.find exports (module_, name) in
  let imports = get_exportable_info intfs.(i').Read.imports kind in
  if index < Array.length imports
  then (
    if depth > 100 then failwith (Printf.sprintf "Import loop on %s %s" module_ name);
    let entry = imports.(index) in
    check_export_import_types ~subtyping_info ~files i' entry.desc i import;
    try resolve (depth + 1) ~files ~intfs ~subtyping_info ~exports ~kind i' entry
    with Not_found -> i', index)
  else i', index

type input =
  { module_name : string
  ; file : string
  ; code : string option
  ; opt_source_map : Source_map.Standard.t option
  }

let f files ~output_file =
  let files =
    Array.map
      ~f:(fun { module_name; file; code; opt_source_map } ->
        let data =
          match code with
          | None -> Fs.read_file file
          | Some data -> data
        in
        let contents = Read.open_in file data in
        { module_name; file; contents; source_map_contents = opt_source_map })
      (Array.of_list files)
  in

  let out_ch = open_out_bin output_file in
  output_string out_ch Read.header;
  let buf = Buffer.create 100000 in

  (* 1: type *)
  let types = Read.create_types () in
  let intfs = Array.map ~f:(fun f -> interface types f.contents) files in
  let type_list = List.rev types.rev_list in
  let subtyping_info = Array.concat type_list in
  let st = Write.types buf (Array.of_list type_list) in
  add_section out_ch ~id:1 buf;

  (* 2: import *)
  let exports = init_exportable_info (fun _ -> Poly.Hashtbl.create 128) in
  Array.iteri
    ~f:(fun i intf ->
      iter_exportable_info
        (fun kind lst ->
          let h = get_exportable_info exports kind in
          List.iter
            ~f:(fun (name, index) ->
              Poly.Hashtbl.add h (files.(i).module_name, name) (i, index))
            lst)
        intf.Read.exports)
    intfs;
  let import_list = ref [] in
  let unresolved_imports = make_exportable_info 0 in
  let resolved_imports =
    let tbl = Poly.Hashtbl.create 128 in
    Array.mapi
      ~f:(fun i intf ->
        map_exportable_info
          (fun kind imports ->
            let exports = get_exportable_info exports kind in
            Array.map
              ~f:(fun (import : import) ->
                match resolve 0 ~files ~intfs ~subtyping_info ~exports ~kind i import with
                | i', idx -> Resolved (i', idx)
                | exception Not_found -> (
                    match Poly.Hashtbl.find tbl import with
                    | status -> status
                    | exception Not_found ->
                        let idx = get_exportable_info unresolved_imports kind in
                        let status = Unresolved idx in
                        Poly.Hashtbl.replace tbl import status;
                        set_exportable_info unresolved_imports kind (1 + idx);
                        import_list := import :: !import_list;
                        status))
              imports)
          intf.Read.imports)
      intfs
  in
  Write.imports st buf (Array.of_list (List.rev !import_list));
  add_section out_ch ~id:2 buf;

  let start_count =
    Array.fold_left
      ~f:(fun count f ->
        match Read.start f.contents with
        | None -> count
        | Some _ -> count + 1)
      ~init:0
      files
  in

  (* 3: function *)
  let functions = Array.map ~f:(fun f -> Read.functions f.contents) files in
  let func_types =
    let l = Array.to_list functions in
    let l =
      if start_count > 1
      then
        let ty =
          let typ : comptype = Func { params = [||]; results = [||] } in
          Read.add_rectype types [| { final = true; supertype = None; typ } |]
        in
        l @ [ [| ty |] ]
      else l
    in
    Array.concat l
  in
  Write.functions buf func_types;
  add_section out_ch ~id:3 buf;
  let func_counts = Array.map ~f:Array.length functions in
  let func_mappings =
    build_mappings resolved_imports unresolved_imports Func func_counts
  in
  let func_count =
    Array.fold_left ~f:( + ) ~init:(if start_count > 1 then 1 else 0) func_counts
  in
  check_exports_against_imports
    ~intfs
    ~subtyping_info
    ~resolved_imports
    ~files
    ~kind:Func
    ~to_desc:
      (index_in_output
         ~unresolved_imports
         ~mappings:func_mappings
         ~kind:Func
         ~get:(fun idx : importdesc -> Func func_types.(idx)));

  (* 4: table *)
  let positions =
    Array.init (Array.length files) ~f:(fun _ -> Scan.create_position_data ())
  in
  let table_counts =
    write_section_with_scan ~files ~out_ch ~buf ~id:4 ~scan:(fun i maps ->
        Scan.table_section positions.(i) { maps with func = func_mappings.(i) })
  in
  let table_mappings =
    build_mappings resolved_imports unresolved_imports Table table_counts
  in
  check_exports_against_imports
    ~intfs
    ~subtyping_info
    ~resolved_imports
    ~files
    ~kind:Table
    ~to_desc:
      (read_desc_from_file ~intfs ~files ~positions ~read:(fun contents : importdesc ->
           Table (Read.tabletype contents contents.ch)));
  Array.iter ~f:Scan.clear_position_data positions;

  (* 5: memory *)
  let mem_mappings =
    write_simple_section
      ~intfs
      ~subtyping_info
      ~resolved_imports
      ~unresolved_imports
      ~out_ch
      ~buf
      ~kind:Mem
      ~id:5
      ~read:Read.memories
      ~to_type:(fun limits -> Mem limits)
      ~write:Write.memories
      ~files
  in

  (* 13: tag *)
  let tag_mappings =
    write_simple_section
      ~intfs
      ~subtyping_info
      ~resolved_imports
      ~unresolved_imports
      ~out_ch
      ~buf
      ~kind:Tag
      ~id:13
      ~read:Read.tags
      ~to_type:(fun ty -> Tag ty)
      ~write:Write.tags
      ~files
  in

  (* 6: global *)
  let global_mappings = Array.make (Array.length files) [||] in
  let global_counts =
    let current_offset = ref (get_exportable_info unresolved_imports Global) in
    Array.mapi
      ~f:(fun i { file; contents; _ } ->
        let imports = get_exportable_info resolved_imports.(i) Global in
        let import_count = Array.length imports in
        let offset = !current_offset - import_count in
        let build_map count =
          let map =
            Array.init
              (Array.length imports + count)
              ~f:(fun j ->
                if j < import_count
                then (
                  match imports.(j) with
                  | Unresolved j' -> j'
                  | Resolved (i', j') ->
                      (if i' > i
                       then
                         let import =
                           (get_exportable_info intfs.(i).imports Global).(j)
                         in
                         failwith
                           (Printf.sprintf
                              "In module %s, the import %s / %s refers to an export in a \
                               later module %s"
                              file
                              import.module_
                              import.name
                              files.(i').file));
                      global_mappings.(i').(j'))
                else j + offset)
          in
          global_mappings.(i) <- map;
          map
        in
        let count =
          if Read.find_section contents 6
          then (
            let count = Read.uint contents.ch in
            let map = build_map count in
            Scan.global_section
              positions.(i)
              { Scan.default_maps with
                typ = contents.type_mapping
              ; func = func_mappings.(i)
              ; global = map
              }
              buf
              contents.ch.buf
              contents.ch.pos
              ~count;
            count)
          else (
            ignore (build_map 0);
            0)
        in
        current_offset := !current_offset + count;
        count)
      files
  in
  add_section out_ch ~id:6 ~count:(Array.fold_left ~f:( + ) ~init:0 global_counts) buf;
  check_exports_against_imports
    ~intfs
    ~subtyping_info
    ~resolved_imports
    ~files
    ~kind:Global
    ~to_desc:(fun i j : importdesc option ->
      let offset = Array.length (get_exportable_info intfs.(i).imports Global) in
      if j < offset
      then None
      else
        let { contents; _ } = files.(i) in
        Read.seek_in contents.ch positions.(i).pos.(j - offset);
        Some (Global (Read.globaltype contents contents.ch)));
  Array.iter ~f:Scan.clear_position_data positions;

  (* 7: export *)
  let export_count =
    Array.fold_left
      ~f:(fun count intf ->
        fold_exportable_info
          (fun _ exports count -> List.length exports + count)
          count
          intf.Read.exports)
      ~init:0
      intfs
  in
  Write.uint buf export_count;
  let exports = String.Hashtbl.create 128 in
  Array.iteri
    ~f:(fun i intf ->
      iter_exportable_info
        (fun kind lst ->
          let map =
            match kind with
            | Func -> func_mappings.(i)
            | Table -> table_mappings.(i)
            | Mem -> mem_mappings.(i)
            | Global -> global_mappings.(i)
            | Tag -> tag_mappings.(i)
          in
          List.iter
            ~f:(fun (name, idx) ->
              match String.Hashtbl.find exports name with
              | i' ->
                  failwith
                    (Printf.sprintf
                       "Duplicated export %s from %s and %s"
                       name
                       files.(i').file
                       files.(i).file)
              | exception Not_found ->
                  String.Hashtbl.add exports name i;
                  Write.export buf kind name map.(idx))
            lst)
        intf.Read.exports)
    intfs;
  add_section out_ch ~id:7 buf;

  (* 8: start *)
  let starts =
    Array.mapi
      ~f:(fun i f ->
        Read.start f.contents |> Option.map ~f:(fun idx -> func_mappings.(i).(idx)))
      files
    |> Array.to_list
    |> List.filter_map ~f:(fun x -> x)
  in
  (match starts with
  | [] -> ()
  | [ start ] ->
      Write.start buf start;
      add_section out_ch ~id:8 buf
  | _ :: _ :: _ ->
      Write.start buf (func_count - 1);
      add_section out_ch ~id:8 buf);

  (* 9: elements *)
  let elem_counts =
    write_section_with_scan ~files ~out_ch ~buf ~id:9 ~scan:(fun i maps ->
        Scan.elem_section
          { maps with func = func_mappings.(i); global = global_mappings.(i) })
  in
  let elem_mappings = build_simple_mappings ~counts:elem_counts in

  (* 12: data count *)
  let data_mappings, data_count =
    let data_counts = Array.map ~f:(fun f -> Read.data_count f.contents) files in
    let data_count = Array.fold_left ~f:( + ) ~init:0 data_counts in
    let data_mappings = build_simple_mappings ~counts:data_counts in
    data_mappings, data_count
  in
  if data_count > 0
  then (
    Write.data_count buf data_count;
    add_section out_ch ~id:12 buf);

  (* 10: code *)
  let code_pieces = Buffer.create 100000 in
  let resize_data = Scan.create_resize_data () in
  let source_maps = ref [] in
  Write.uint code_pieces func_count;
  Array.iteri
    ~f:(fun i { contents; source_map_contents; _ } ->
      if Read.find_section contents 10
      then (
        let pos = Buffer.length code_pieces in
        let scan_func =
          Scan.func
            resize_data
            { typ = contents.type_mapping
            ; func = func_mappings.(i)
            ; table = table_mappings.(i)
            ; mem = mem_mappings.(i)
            ; global = global_mappings.(i)
            ; elem = elem_mappings.(i)
            ; data = data_mappings.(i)
            ; tag = tag_mappings.(i)
            }
            buf
            contents.ch.buf
        in
        let code (ch : Read.ch) =
          let pos = ch.pos in
          let i = resize_data.i in
          let size = Read.uint ch in
          let pos' = ch.pos in
          Scan.push_resize resize_data pos' 0;
          scan_func ch.pos;
          ch.pos <- ch.pos + size;
          let p = Buffer.length code_pieces in
          Write.uint code_pieces (Buffer.length buf);
          let p' = Buffer.length code_pieces in
          let delta = p' - p - pos' + pos in
          resize_data.delta.(i) <- delta;
          Buffer.add_buffer code_pieces buf;
          Buffer.clear buf
        in
        let count = Read.uint contents.ch in
        Scan.clear_resize_data resize_data;
        Scan.push_resize resize_data 0 (-Read.pos_in contents.ch);
        Read.repeat' count code contents.ch;
        Option.iter
          ~f:(fun sm ->
            if not (Wasm_source_map.is_empty sm)
            then
              source_maps := (pos, Wasm_source_map.resize resize_data sm) :: !source_maps)
          source_map_contents))
    files;
  if start_count > 1
  then (
    (* no local *)
    Buffer.add_char buf (Char.chr 0);
    List.iter
      ~f:(fun idx ->
        (* call idx *)
        Buffer.add_char buf (Char.chr 0x10);
        Write.uint buf idx)
      starts;
    Buffer.add_buffer code_pieces buf;
    Buffer.clear buf);
  let code_section_offset =
    let b = Buffer.create 5 in
    Write.uint b (Buffer.length code_pieces);
    pos_out out_ch + 1 + Buffer.length b
  in
  add_section out_ch ~id:10 code_pieces;
  let source_map =
    Wasm_source_map.concatenate
      (List.map
         ~f:(fun (pos, sm) -> pos + code_section_offset, sm)
         (List.rev !source_maps))
  in

  (* 11: data *)
  ignore
    (write_section_with_scan ~files ~out_ch ~buf ~id:11 ~scan:(fun i maps ->
         Scan.data_section { maps with global = global_mappings.(i) }));

  (* Custom section: name *)
  let name_sections =
    Array.map
      ~f:(fun { contents; _ } -> Read.focus_on_custom_section contents "name")
      files
  in
  let name_section_buffer = Buffer.create 100000 in
  Write.name name_section_buffer "name";

  (* 1: functions *)
  write_namemap
    ~resolved_imports
    ~unresolved_imports
    ~name_sections
    ~name_section_buffer
    ~buf
    ~kind:Func
    ~section_id:1
    ~mappings:func_mappings;
  (* 2: locals *)
  write_indirectnamemap
    ~name_sections
    ~name_section_buffer
    ~buf
    ~section_id:2
    ~mappings:func_mappings;
  (* 3: labels *)
  write_indirectnamemap
    ~name_sections
    ~name_section_buffer
    ~buf
    ~section_id:3
    ~mappings:func_mappings;

  (* 4: types *)
  let type_names = Array.make types.last_index None in
  Array.iter2
    ~f:(fun { contents; _ } name_section ->
      if Read.find_section name_section 4
      then
        let map = Read.namemap name_section in
        Array.iter
          ~f:(fun (idx, name) ->
            let idx = contents.type_mapping.(idx) in
            if Option.is_none type_names.(idx) then type_names.(idx) <- Some (idx, name))
          map)
    files
    name_sections;
  Write.namemap
    buf
    (Array.of_list (List.filter_map ~f:(fun x -> x) (Array.to_list type_names)));
  add_subsection name_section_buffer ~id:4 buf;

  (* 5: tables *)
  write_namemap
    ~resolved_imports
    ~unresolved_imports
    ~name_sections
    ~name_section_buffer
    ~buf
    ~kind:Table
    ~section_id:5
    ~mappings:table_mappings;
  (* 6: memories *)
  write_namemap
    ~resolved_imports
    ~unresolved_imports
    ~name_sections
    ~name_section_buffer
    ~buf
    ~kind:Mem
    ~section_id:6
    ~mappings:mem_mappings;
  (* 7: globals *)
  write_namemap
    ~resolved_imports
    ~unresolved_imports
    ~name_sections
    ~name_section_buffer
    ~buf
    ~kind:Global
    ~section_id:7
    ~mappings:global_mappings;
  (* 8: elems *)
  write_simple_namemap
    ~name_sections
    ~name_section_buffer
    ~buf
    ~section_id:8
    ~mappings:elem_mappings;
  (* 9: data segments *)
  write_simple_namemap
    ~name_sections
    ~name_section_buffer
    ~buf
    ~section_id:9
    ~mappings:data_mappings;

  (* 10: field names *)
  let type_field_names = Array.make types.last_index None in
  Array.iter2
    ~f:(fun { contents; _ } name_section ->
      if Read.find_section name_section 10
      then
        let n = Read.uint name_section.ch in
        let scan_map = Scan.local_namemap buf name_section.ch.buf in
        for _ = 1 to n do
          let idx = contents.type_mapping.(Read.uint name_section.ch) in
          scan_map name_section.ch.pos;
          name_section.ch.pos <- name_section.ch.pos + Buffer.length buf;
          if Option.is_none type_field_names.(idx)
          then type_field_names.(idx) <- Some (idx, Buffer.contents buf);
          Buffer.clear buf
        done)
    files
    name_sections;
  let type_field_names =
    Array.of_list (List.filter_map ~f:(fun x -> x) (Array.to_list type_field_names))
  in
  Write.uint buf (Array.length type_field_names);
  for i = 0 to Array.length type_field_names - 1 do
    let idx, map = type_field_names.(i) in
    Write.uint buf idx;
    Buffer.add_string buf map
  done;
  add_subsection name_section_buffer ~id:10 buf;

  (* 11: tags *)
  write_namemap
    ~resolved_imports
    ~unresolved_imports
    ~name_sections
    ~name_section_buffer
    ~buf
    ~kind:Tag
    ~section_id:11
    ~mappings:tag_mappings;

  add_section out_ch ~id:0 name_section_buffer;

  close_out out_ch;

  source_map

(*
LATER
- testsuite : import/export matching, source maps, multiple start functions, ...
- missing instructions ==> typed continuations (?)
- check features?

MAYBE
- topologic sort of globals?
  => easy: just look at the import/export dependencies between modules
- reorder types/globals/functions to generate a smaller binary
*)
