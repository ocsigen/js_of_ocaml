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
  | Exn
  | Noexn
  | Cont
  | Nocont
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
  | Cont of int

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
  type st =
    { mutable type_index_count : int
    ; type_map : int array
    }

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

  let typeidx st idx =
    if idx < 0 then lnot idx + st.type_index_count else st.type_map.(idx)

  let heaptype st ch typ =
    match (typ : heaptype) with
    | Nocont -> byte ch 0x75
    | Noexn -> byte ch 0x74
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
    | Exn -> byte ch 0x69
    | Cont -> byte ch 0x68
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
    | Cont idx ->
        byte ch 0x5D;
        sint ch (typeidx st idx)
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

  let types ch ~type_map l =
    let st = { type_index_count = 0; type_map } in
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
            uint ch st.type_map.(typ)
        | Table typ ->
            byte ch 0x01;
            tabletype st ch typ
        | Mem l ->
            byte ch 0x02;
            limits ch l
        | Global { mut; typ } ->
            byte ch 0x03;
            globaltype st ch mut typ
        | Tag typ ->
            byte ch 0x04;
            byte ch 0x00;
            uint ch st.type_map.(typ))
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
      | Cont i1, Cont i2 -> i1 = i2
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
    ; mutable rev_subtyping_info : subtype array list
          (* Same as [rev_list], but with absolute supertype indices *)
    }

  let create_types () =
    { types = RecTypeTbl.create 2000
    ; last_index = 0
    ; rev_list = []
    ; rev_subtyping_info = []
    }

  let add_rectype types typ =
    try RecTypeTbl.find types.types typ
    with Not_found ->
      let index = types.last_index in
      RecTypeTbl.add types.types typ index;
      types.last_index <- Array.length typ + index;
      types.rev_list <- typ :: types.rev_list;
      (* Supertypes in the same recursive group are given relative to
         the start of the group *)
      types.rev_subtyping_info <-
        Array.map
          ~f:(fun (t : subtype) ->
            match t.supertype with
            | Some s when s < 0 -> { t with supertype = Some (index + lnot s) }
            | _ -> t)
          typ
        :: types.rev_subtyping_info;
      index

  let heaptype st ch =
    let i = sint ch in
    match i + 128 with
    | 0x75 -> Nocont
    | 0x74 -> Noexn
    | 0x73 -> Nofunc
    | 0x72 -> Noextern
    | 0x71 -> None_
    | 0x70 -> Func
    | 0x6F -> Extern
    | 0x6E -> Any
    | 0x6D -> Eq
    | 0x6C -> I31
    | 0x6B -> Struct
    | 0x6A -> Array
    | 0x69 -> Exn
    | 0x68 -> Cont
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
    | 0x75 -> nullable Nocont
    | 0x74 -> nullable Noexn
    | 0x73 -> nullable Nofunc
    | 0x72 -> nullable Noextern
    | 0x71 -> nullable None_
    | 0x70 -> nullable Func
    | 0x6F -> nullable Extern
    | 0x6E -> nullable Any
    | 0x6D -> nullable Eq
    | 0x6C -> nullable I31
    | 0x6B -> nullable Struct
    | 0x6A -> nullable Array
    | 0x69 -> nullable Exn
    | 0x68 -> nullable Cont
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
    | 0x5D ->
        let i = sint ch in
        let i =
          if i >= st.type_index_count
          then lnot (i - st.type_index_count)
          else st.type_mapping.(i)
        in
        Cont i
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
      map_exportable_info (fun _ l -> List.rev l) tbl
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

  (* A live entry refers to an entry which was found dead: the liveness
     analysis missed a reference *)
  let dead_reference ~file kind idx =
    failwith
      (Printf.sprintf
         "Wasm linker: in module %s, reference to removed %s %d"
         file
         kind
         idx)

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

  type ref_kind =
    [ `Func
    | `Global
    | `Tag
    | `Elem
    | `Data
    | `Type
    | `Ref_func (* Function referenced by [ref.func] *)
    ]

  type scanner =
    { table_section : count:int -> int -> unit
    ; elem_section : keep:(int -> bool) -> count:int -> int -> unit
    ; data_section : keep:(int -> bool) -> count:int -> int -> unit
    ; func : int -> unit
    ; local_namemap : int -> unit
    ; table : int -> int
    ; global : int -> int
    ; global_entry : int -> int
    ; elem : int -> int
    ; data : int -> int
    }

  (* In [analysis] mode, nothing is written: the scanner only reports
     the references to functions, globals, tags and element segments
     through [visit]. *)
  let scanner
      ?(analysis = false)
      ?(visit = fun (_ : ref_kind) (_ : int) -> ())
      ?(file = "")
      report
      mark
      maps
      buf
      code =
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
    (* Set while going through an entry that is dropped: indices are not
       rewritten, since the entry may refer to dropped entries. *)
    let skipping = ref analysis in
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
      if (not analysis) && !start < pos
      then Buffer.add_substring buf code !start (pos - !start);
      start := pos'
    in
    let flush pos = flush' pos pos in
    let rewrite kind visit map pos =
      let pos', idx =
        let i = get pos in
        if i < 128
        then pos + 1, i
        else
          let i' = get (pos + 1) in
          if i' < 128 then pos + 2, (i' lsl 7) + (i land 0x7f) else uint32 pos
      in
      visit idx;
      if !skipping
      then pos'
      else
        let idx' = map idx in
        if idx' < 0 then dead_reference ~file kind idx;
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
    let rewrite_signed kind visit map pos =
      let pos', idx =
        let i = get pos in
        if i < 64 then pos + 1, i else if i < 128 then pos + 1, i - 128 else sint32 pos
      in
      visit idx;
      if !skipping
      then pos'
      else
        let idx' = map idx in
        if idx' < 0 then dead_reference ~file kind idx;
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
    let no_visit _ = () in
    let visit_func idx = visit `Func idx in
    let visit_ref_func idx = visit `Ref_func idx in
    let visit_global idx = visit `Global idx in
    let visit_elem idx = visit `Elem idx in
    let visit_tag idx = visit `Tag idx in
    let visit_data idx = visit `Data idx in
    let visit_type idx = visit `Type idx in
    let typ_map idx = maps.typ.(idx) in
    let typeidx pos = rewrite "type" visit_type typ_map pos in
    let signed_typeidx pos = rewrite_signed "type" visit_type typ_map pos in
    let func_map idx = maps.func.(idx) in
    let funcidx pos = rewrite "function" visit_func func_map pos in
    let table_map idx = maps.table.(idx) in
    let tableidx pos = rewrite "table" no_visit table_map pos in
    let mem_map idx = maps.mem.(idx) in
    let memidx pos = rewrite "memory" no_visit mem_map pos in
    let global_map idx = maps.global.(idx) in
    let globalidx pos = rewrite "global" visit_global global_map pos in
    let elem_map idx = maps.elem.(idx) in
    let elemidx pos = rewrite "element segment" visit_elem elem_map pos in
    let data_map idx = maps.data.(idx) in
    let dataidx pos = rewrite "data segment" visit_data data_map pos in
    let tag_map idx = maps.tag.(idx) in
    let tagidx pos = rewrite "tag" visit_tag tag_map pos in
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
      | 0x6A (* array *)
      | 0x69 (* exn *)
      | 0x74 (* noexn *)
      | 0x68 (* cont *)
      | 0x75 (* nocont *) -> pos + 1
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
        if (not !skipping) && mem_map 0 <> 0
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
      | 0xD2 (* ref.func *) ->
          pos + 1 |> rewrite "function" visit_ref_func func_map |> instructions
      | 0xE0 (* cont.new *) -> pos + 1 |> typeidx |> instructions
      | 0xE1 (* cont.bind *) -> pos + 1 |> typeidx |> typeidx |> instructions
      | 0xE2 (* suspend *) -> pos + 1 |> tagidx |> instructions
      | 0xE3 (* resume *) -> pos + 1 |> typeidx |> vector on_clause |> instructions
      | 0xE4 (* resume_throw *) ->
          pos + 1 |> typeidx |> tagidx |> vector on_clause |> instructions
      | 0xE5 (* resume_throw_ref *) ->
          pos + 1 |> typeidx |> vector on_clause |> instructions
      | 0xE6 (* switch *) -> pos + 1 |> typeidx |> tagidx |> instructions
      | 0xFB -> pos + 1 |> gc_instruction
      | 0xFC -> (
          if debug then Format.eprintf "  %d@." (get (pos + 1));
          match get (pos + 1) with
          | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 (* xx.trunc_sat_xxx_x *)
          | 19 (* add128 *)
          | 20 (* sub128 *)
          | 21 | 22 (* mul_wide *) -> pos + 2 |> instructions
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
      | 16 (* array.fill *)
      | 32 (* struct.new_desc *)
      | 33 (* struct.new_default_desc *)
      | 34 (* ref.get_desc *) -> pos + 1 |> typeidx |> instructions
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
      | 20 | 21 (* ref_test *) | 22 | 23 (* ref.cast*) | 35 | 36 (* ref.cast_desc_eq *) ->
          pos + 1 |> heaptype |> instructions
      | 24 (* br_on_cast *)
      | 25 (* br_on_cast_fail *)
      | 37 (* br_on_cast_desc_eq *)
      | 38 (* br_on_cast_desc_eq_fail *) ->
          pos + 2 |> labelidx |> heaptype |> heaptype |> instructions
      | c -> failwith (Printf.sprintf "Bad instruction 0xFB 0x%02X" c)
    and vector_instruction pos =
      if debug then Format.eprintf "  %d@." (get pos);
      (* [uint32] consumes the sub-opcode: [pos] is now at the immediates *)
      let pos, i = uint32 pos in
      match i with
      | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 92 | 93 (* v128.load / store *)
        -> pos |> memarg |> instructions
      | 84 | 85 | 86 | 87 | 88 | 89 | 90 | 91 (* v128.load/store_lane *) ->
          pos |> memarg |> laneidx |> instructions
      | 12 (* v128.const *) | 13 (* v128.shuffle *) -> pos + 16 |> instructions
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
      | 34 (* xx.extract/replace_lane *) -> pos |> laneidx |> instructions
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
          then pos |> instructions
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
          let c = get (pos + 1) in
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
      | 0x19 (* catch_all *) -> pos + 1 |> instructions |> block_end |> instructions
      | 0x18 (* delegate *) -> pos + 1 |> labelidx |> instructions
      | _ -> pos |> block_end |> instructions
    and catch pos =
      match get pos with
      | 0 (* catch *) | 1 (* catch_ref *) -> pos + 1 |> tagidx |> labelidx
      | 2 (* catch_all *) | 3 (* catch_all_ref *) -> pos + 1 |> labelidx
      | c -> failwith (Printf.sprintf "bad catch 0x%02x@." c)
    and on_clause pos =
      match get pos with
      | 0 (* on *) -> pos + 1 |> tagidx |> labelidx
      | 1 (* on .. switch *) -> pos + 1 |> tagidx
      | c -> failwith (Printf.sprintf "bad on clause 0x%02x@." c)
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
    (* Go through [count] entries, dropping the ones not satisfying [keep] *)
    let filtered_entries entry ~keep ~count pos =
      let rec loop j pos =
        if j = count
        then pos
        else if keep j
        then loop (j + 1) (entry j pos)
        else (
          flush pos;
          skipping := true;
          let pos' = entry j pos in
          skipping := analysis;
          start := pos';
          loop (j + 1) pos')
      in
      loop 0 pos
    in
    let global_entry pos =
      start := pos;
      let pos' = global pos in
      flush pos';
      pos'
    in
    let elemkind pos =
      assert (get pos = 0);
      pos + 1
    in
    (* An active segment with an implicit table or memory (element kinds
       0 and 4, data kind 0) refers to index 0. When this table or
       memory gets another index in the output, the segment is rewritten
       to its explicit form (element kinds 2 and 6, data kind 2): the
       [flag] is changed, the index is inserted after it, and for element
       segments, the element kind or type [mid] is inserted after the
       offset expression. *)
    let active_segment ~map ~flag ?mid rest pos =
      if !skipping || map 0 = 0
      then pos + 1 |> expr |> rest
      else (
        flush' pos (pos + 1);
        Buffer.add_char buf flag;
        output_uint buf (map 0);
        let pos' = pos + 1 |> expr in
        Option.iter mid ~f:(fun mid ->
            flush' pos' pos';
            Buffer.add_char buf mid);
        pos' |> rest)
    in
    let elem pos =
      match get pos with
      | 0 ->
          pos |> active_segment ~map:table_map ~flag:'\x02' ~mid:'\x00' (vector funcidx)
      | 1 -> pos + 1 |> elemkind |> vector funcidx
      | 2 -> pos + 1 |> tableidx |> expr |> elemkind |> vector funcidx
      | 3 -> pos + 1 |> elemkind |> vector funcidx
      | 4 -> pos |> active_segment ~map:table_map ~flag:'\x06' ~mid:'\x70' (vector expr)
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
      | 0 -> pos |> active_segment ~map:mem_map ~flag:'\x02' bytes
      | 1 -> pos + 1 |> bytes
      | 2 -> pos + 1 |> memidx |> expr |> bytes
      | c -> failwith (Printf.sprintf "Bad data segment 0x%02X" c)
    in
    (* A segment that is not kept is a declarative segment, or a passive
       segment which is not used: it only matters as a declaration of
       the functions it mentions, so we only keep the functions which are
       live. A segment of expressions (flags 5 and 7) becomes a segment of
       function indices (flags 1 and 3): its type and its other
       expressions may refer to removed entries. *)
    let filtered_elem pos =
      let flag = get pos in
      flush pos;
      let item pos =
        match flag with
        | 1 | 3 ->
            let pos', idx = uint32 pos in
            pos', Some idx
        | _ ->
            if get pos = 0xD2 && get (int (pos + 1)) = 0x0B
            then
              let pos', idx = uint32 (pos + 1) in
              pos' + 1, Some idx
            else expr pos, None
      in
      let rec collect n pos acc =
        if n = 0
        then pos, List.rev acc
        else
          let pos', idx = item pos in
          let acc =
            match idx with
            | Some idx when func_map idx >= 0 -> func_map idx :: acc
            | Some _ | None -> acc
          in
          collect (n - 1) pos' acc
      in
      skipping := true;
      let pos' =
        match flag with
        | 1 | 3 -> pos + 1 |> elemkind
        | 5 | 7 -> pos + 1 |> reftype
        | c -> failwith (Printf.sprintf "Bad element 0x%02X" c)
      in
      let pos', n = uint32 pos' in
      let pos', l = collect n pos' [] in
      skipping := analysis;
      Buffer.add_char buf (Char.chr (if flag = 1 || flag = 5 then 1 else 3));
      Buffer.add_char buf (Char.chr 0);
      output_uint buf (List.length l);
      List.iter ~f:(fun idx -> output_uint buf idx) l;
      start := pos';
      pos'
    in
    let elem_section ~keep ~count pos =
      start := pos;
      let rec loop j pos =
        if j = count
        then pos
        else loop (j + 1) (if keep j then elem pos else filtered_elem pos)
      in
      pos |> loop 0 |> flush
    in
    let data_section ~keep ~count pos =
      start := pos;
      pos |> filtered_entries (fun _ pos -> data pos) ~keep ~count |> flush
    in
    let local_nameassoc pos = pos |> localidx |> name in
    let local_namemap pos =
      start := pos;
      pos |> vector local_nameassoc |> flush
    in
    { table_section
    ; elem_section
    ; data_section
    ; func
    ; local_namemap
    ; table
    ; global
    ; global_entry
    ; elem
    ; data
    }

  let table_section ~file positions maps buf s =
    (scanner ~file (fun _ _ -> ()) (fun pos -> push_position positions pos) maps buf s)
      .table_section

  let global_entry ~file maps buf s =
    (scanner ~file (fun _ _ -> ()) (fun _ -> ()) maps buf s).global_entry

  let elem_section ~file maps buf s =
    (scanner ~file (fun _ _ -> ()) (fun _ -> ()) maps buf s).elem_section

  let data_section ~file maps buf s =
    (scanner ~file (fun _ _ -> ()) (fun _ -> ()) maps buf s).data_section

  let func ~file resize_data maps buf s =
    (scanner
       ~file
       (fun pos delta -> push_resize resize_data pos delta)
       (fun _ -> ())
       maps
       buf
       s)
      .func

  let local_namemap buf s =
    (scanner (fun _ _ -> ()) (fun _ -> ()) default_maps buf s).local_namemap

  let analysis ~visit s =
    scanner
      ~analysis:true
      ~visit
      (fun _ _ -> ())
      (fun _ -> ())
      default_maps
      (Buffer.create 0)
      s
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
  Bool.equal export.shared import.shared
  && Poly.equal export.index_type import.index_type
  && export.min >= import.min
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
  | Noextern, Noextern
  | (Exn | Noexn), Exn
  | Noexn, Noexn
  | (Cont | Nocont), Cont
  | Nocont, Nocont
  | (Any | Eq | I31 | Struct | Array | None_), Any
  | (Eq | I31 | Struct | Array | None_), Eq
  | (I31 | None_), I31
  | (Struct | None_), Struct
  | (Array | None_), Array
  | None_, None_ -> true
  | Type i, (Any | Eq) -> (
      match subtyping_info.(i).typ with
      | Struct _ | Array _ -> true
      | Func _ | Cont _ -> false)
  | Type i, Struct -> (
      match subtyping_info.(i).typ with
      | Struct _ -> true
      | Array _ | Func _ | Cont _ -> false)
  | Type i, Array -> (
      match subtyping_info.(i).typ with
      | Array _ -> true
      | Struct _ | Func _ | Cont _ -> false)
  | Type i, Func -> (
      match subtyping_info.(i).typ with
      | Func _ -> true
      | Struct _ | Array _ | Cont _ -> false)
  | Type i, Cont -> (
      match subtyping_info.(i).typ with
      | Cont _ -> true
      | Struct _ | Array _ | Func _ -> false)
  | None_, Type i -> (
      match subtyping_info.(i).typ with
      | Struct _ | Array _ -> true
      | Func _ | Cont _ -> false)
  | Nofunc, Type i -> (
      match subtyping_info.(i).typ with
      | Func _ -> true
      | Struct _ | Array _ | Cont _ -> false)
  | Nocont, Type i -> (
      match subtyping_info.(i).typ with
      | Cont _ -> true
      | Struct _ | Array _ | Func _ -> false)
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

(* Dead entries are mapped to -1 *)
let build_mappings ~live resolved_imports unresolved_imports kind counts =
  let current_offset = ref (get_exportable_info unresolved_imports kind) in
  let mappings =
    Array.mapi
      ~f:(fun i count ->
        let imports = get_exportable_info resolved_imports.(i) kind in
        let import_count = Array.length imports in
        let live = get_exportable_info live.(i) kind in
        Array.init
          (Array.length imports + count)
          ~f:(fun i ->
            if i < import_count
            then
              match imports.(i) with
              | Unresolved i -> i
              | Resolved _ -> -1
            else if live.(i)
            then (
              let idx = !current_offset in
              incr current_offset;
              idx)
            else -1))
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

(* The type of the [j]-th entity of module [i], [get i k] returning the
   type of the [k]-th entity defined by module [i]; [None] for an import.
   This does not depend on the output layout, so an import is still
   checked against an export whose target is removed. *)
let defined_entity ~intfs ~kind ~get i j =
  let offset = Array.length (get_exportable_info intfs.(i).Read.imports kind) in
  if j < offset then None else Some (get i (j - offset))

(* The live entries of [data]. The [live] flags also cover the imports,
   which come first. *)
let filter_live ~live data =
  let offset = Array.length live - Array.length data in
  let l = ref [] in
  for j = Array.length data - 1 downto 0 do
    if live.(j + offset) then l := data.(j) :: !l
  done;
  Array.of_list !l

let write_simple_section
    ~live
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
  let entries =
    Array.concat
      (Array.to_list
         (Array.mapi
            ~f:(fun i data -> filter_live ~live:(get_exportable_info live.(i) kind) data)
            data))
  in
  if Array.length entries <> 0
  then (
    write buf entries;
    add_section out_ch ~id buf);
  let counts = Array.map ~f:Array.length data in
  let mappings = build_mappings ~live resolved_imports unresolved_imports kind counts in
  check_exports_against_imports
    ~intfs
    ~subtyping_info
    ~resolved_imports
    ~files
    ~kind
    ~to_desc:(defined_entity ~intfs ~kind ~get:(fun i k -> to_type data.(i).(k)));
  mappings

let write_section_with_scan
    ?(written = fun _ count -> count)
    ?(extra = fun _ -> 0)
    ~files
    ~type_maps
    ~out_ch
    ~buf
    ~id
    ~scan
    () =
  let counts =
    Array.mapi
      ~f:(fun i { contents; _ } ->
        if Read.find_section contents id
        then (
          let count = Read.uint contents.ch in
          scan
            i
            { Scan.default_maps with typ = type_maps.(i) }
            buf
            contents.ch.buf
            ~count
            contents.ch.pos;
          count)
        else 0)
      files
  in
  let extra_count = extra buf in
  add_section
    out_ch
    ~id
    ~count:(extra_count + Array.fold_left ~f:( + ) ~init:0 (Array.mapi ~f:written counts))
    buf;
  counts

let write_simple_namemap ~name_sections ~name_section_buffer ~buf ~section_id ~mappings =
  let count = ref 0 in
  Array.iter2
    ~f:(fun name_section mapping ->
      if Read.find_section name_section section_id
      then
        let map = Read.namemap name_section in
        Array.iter
          ~f:(fun (idx, name) ->
            let idx = mapping.(idx) in
            if idx >= 0
            then (
              Write.nameassoc buf idx name;
              incr count))
          map)
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
              if
                idx' >= 0
                && idx' < Array.length import_names
                && Option.is_none import_names.(idx')
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
  let write_entry idx s pos len =
    incr count;
    Write.uint buf idx;
    Write.uint buf len;
    Buffer.add_substring buf s pos len
  in
  (* Entries must be sorted by index. Only globals are reordered: the
     other entries are already in the right order. *)
  let reordered =
    match kind with
    | Global -> true
    | Func | Table | Mem | Tag -> false
  in
  let entries = ref [] in
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
          if idx >= import_count && mapping.(idx) >= 0
          then
            if reordered
            then entries := (mapping.(idx), ch.buf, ch.pos, len) :: !entries
            else write_entry mapping.(idx) ch.buf ch.pos len;
          ch.pos <- ch.pos + len
        done)
    name_sections;
  List.iter
    ~f:(fun (idx, s, pos, len) -> write_entry idx s pos len)
    (List.sort ~cmp:(fun (i, _, _, _) (i', _, _, _) -> compare i i') !entries);
  add_subsection name_section_buffer ~id:section_id ~count:!count buf

let write_indirectnamemap ~name_sections ~name_section_buffer ~buf ~section_id ~mappings =
  let count = ref 0 in
  Array.iter2
    ~f:(fun name_section mapping ->
      if Read.find_section name_section section_id
      then
        let n = Read.uint name_section.ch in
        let scan_map = Scan.local_namemap buf name_section.ch.buf in
        for _ = 1 to n do
          let idx = mapping.(Read.uint name_section.ch) in
          let p0 = Buffer.length buf in
          Write.uint buf (max idx 0);
          let p = Buffer.length buf in
          scan_map name_section.ch.pos;
          name_section.ch.pos <- name_section.ch.pos + Buffer.length buf - p;
          if idx >= 0 then incr count else Buffer.truncate buf p0
        done)
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

type dependency =
  { name : string
  ; export : string option
  ; import : (string * string) option
  ; reaches : string list
  ; root : bool
  }

let parse_dependencies s =
  let open Yojson.Basic.Util in
  List.map
    ~f:(fun node : dependency ->
      let opt f = function
        | `Null -> None
        | v -> Some (f v)
      in
      { name = node |> member "name" |> to_string
      ; export = node |> member "export" |> opt to_string
      ; import =
          node
          |> member "import"
          |> opt (fun v ->
              match to_list v with
              | [ m; n ] -> to_string m, to_string n
              | _ -> failwith "bad import in dependency graph")
      ; reaches =
          node
          |> member "reaches"
          |> opt (fun l -> List.map ~f:to_string (to_list l))
          |> Option.value ~default:[]
      ; root = node |> member "root" |> opt to_bool |> Option.value ~default:false
      })
    (to_list (Yojson.Basic.from_string s))

type item =
  | Entity of int * exportable * int
  | Segment of int * int
  | Data of int * int

(* Positions of the entries of a section, given a function that
   skips one entry *)
let section_entries (contents : Read.t) id skip =
  if Read.find_section contents id
  then
    let count = Read.uint contents.ch in
    let pos = ref contents.ch.pos in
    Array.init count ~f:(fun _ ->
        let p = !pos in
        pos := skip p;
        p)
  else [||]

let code_entries (contents : Read.t) =
  if Read.find_section contents 10
  then
    let ch = contents.ch in
    Read.repeat
      (Read.uint ch)
      (fun ch ->
        let size = Read.uint ch in
        let p = ch.pos in
        ch.pos <- p + size;
        p)
      ch
  else [||]

let iter_valtype_types f (t : valtype) =
  match t with
  | Ref { typ = Type i; _ } -> f i
  | _ -> ()

let iter_subtype_types f ({ supertype; typ; _ } : subtype) =
  let field ({ typ; _ } : fieldtype) =
    match typ with
    | Val t -> iter_valtype_types f t
    | Packed _ -> ()
  in
  Option.iter ~f supertype;
  match (typ : comptype) with
  | Func { params; results } ->
      Array.iter ~f:(iter_valtype_types f) params;
      Array.iter ~f:(iter_valtype_types f) results
  | Struct l -> Array.iter ~f:field l
  | Array t -> field t
  | Cont i -> f i

let iter_importdesc_types f (desc : importdesc) =
  match desc with
  | Func t | Tag t -> f t
  | Table { typ; _ } -> iter_valtype_types f (Ref typ)
  | Global { typ; _ } -> iter_valtype_types f typ
  | Mem _ -> ()

(* Order the nodes [0 .. n - 1] so that each node comes after the
   nodes it depends on, choosing the node with the highest priority
   whenever there is a choice (then the lowest index). *)
(* Raised by [priority_topological_sort] when the dependencies form a cycle,
   with the nodes that could not be ordered. *)
exception Cycle of int list

let priority_topological_sort ~n ~deps ~priority =
  let module S = Set.Make (struct
    type t = int * int

    let compare (p, i) (p', i') =
      match compare p' p with
      | 0 -> compare i i'
      | c -> c
  end) in
  let pending = Array.make n 0 in
  let successors = Array.make n [] in
  for i = 0 to n - 1 do
    List.iter
      ~f:(fun j ->
        if j <> i
        then (
          pending.(i) <- pending.(i) + 1;
          successors.(j) <- i :: successors.(j)))
      (deps i)
  done;
  let ready = ref S.empty in
  for i = 0 to n - 1 do
    if pending.(i) = 0 then ready := S.add (priority i, i) !ready
  done;
  let order = Array.make n 0 in
  for k = 0 to n - 1 do
    if S.is_empty !ready
    then
      raise
        (Cycle (List.filter ~f:(fun i -> pending.(i) > 0) (List.init ~len:n ~f:Fun.id)));
    let ((_, i) as elt) = S.min_elt !ready in
    ready := S.remove elt !ready;
    order.(k) <- i;
    List.iter
      ~f:(fun j ->
        pending.(j) <- pending.(j) - 1;
        if pending.(j) = 0 then ready := S.add (priority j, j) !ready)
      successors.(i)
  done;
  order

type ordering =
  { type_groups : int array  (** Start index of the live type groups, in order *)
  ; globals : (int * int) array
        (** Live global definitions (module, local index), in order *)
  ; global_positions : int array array
        (** Position of each global definition in the input modules *)
  }

type liveness =
  { live : bool array exportable_info array
        (** Per input module, for each local index (imports included) *)
  ; segments : bool array array
  ; data : bool array array
  ; unresolved : bool array exportable_info
  ; keep_export : string -> bool
  ; ordering : ordering
        (** How to order types and globals so that the most used ones get
            the smallest indices *)
  ; undeclared_functions : (int * int) list
        (** Functions referenced by [ref.func] in function bodies which would
            not be declared anymore in the output, since the global
            initializers or exports which declared them have been removed *)
  }

(* The functions in [candidates] which are not declared in [declared] or
   by a kept export, each listed once *)
let undeclared_functions ~resolved_imports ~intfs ~keep_export ~declared candidates =
  let key i j =
    let imports = get_exportable_info resolved_imports.(i) Func in
    if j < Array.length imports
    then
      match imports.(j) with
      | Resolved (i', j') -> i', j'
      | Unresolved u -> -1, u
    else i, j
  in
  let declared_keys = Poly.Hashtbl.create 128 in
  let declare (i, j) = Poly.Hashtbl.replace declared_keys (key i j) () in
  List.iter ~f:declare declared;
  Array.iteri
    ~f:(fun i intf ->
      List.iter
        ~f:(fun (name, idx) -> if keep_export name then declare (i, idx))
        intf.Read.exports.func)
    intfs;
  List.filter
    ~f:(fun (i, j) ->
      let k = key i j in
      if Poly.Hashtbl.mem declared_keys k
      then false
      else (
        Poly.Hashtbl.replace declared_keys k ();
        true))
    candidates

(* Order the live global definitions so that the initializer of a global
   only refers to earlier globals, choosing the most used globals
   first. *)
let order_globals ~(files : t array) ~resolved_imports ~live ~global_counts ~global_deps =
  let global_import_count i =
    Array.length (get_exportable_info resolved_imports.(i) Global)
  in
  let definition i j =
    if j < global_import_count i
    then
      match (get_exportable_info resolved_imports.(i) Global).(j) with
      | Resolved (i', j') when j' >= global_import_count i' -> Some (i', j')
      | Resolved _ | Unresolved _ -> None
    else Some (i, j)
  in
  let global_ids = Array.map ~f:(fun l -> Array.make (Array.length l.global) (-1)) live in
  let nodes = ref [] in
  let n = ref 0 in
  Array.iteri
    ~f:(fun i l ->
      Array.iteri
        ~f:(fun j is_live ->
          if is_live && j >= global_import_count i
          then (
            global_ids.(i).(j) <- !n;
            incr n;
            nodes := (i, j) :: !nodes))
        l.global)
    live;
  let nodes = Array.of_list (List.rev !nodes) in
  let node_id i j =
    match definition i j with
    | Some (i', j') -> global_ids.(i').(j')
    | None -> -1
  in
  let priorities = Array.make (Array.length nodes) 0 in
  Array.iteri
    ~f:(fun i counts ->
      Array.iteri
        ~f:(fun j c ->
          let id = node_id i j in
          if id >= 0 then priorities.(id) <- priorities.(id) + c)
        counts)
    global_counts;
  let global_order =
    try
      priority_topological_sort
        ~n:(Array.length nodes)
        ~deps:(fun id ->
          let i, j = nodes.(id) in
          List.filter
            ~f:(fun id -> id >= 0)
            (List.map ~f:(fun j' -> node_id i j') global_deps.(i).(j)))
        ~priority:(fun id -> priorities.(id))
    with Cycle l ->
      failwith
        (Printf.sprintf
           "The initializers of some globals of %s read each other in a cycle: they \
            cannot be ordered"
           (String.concat
              ~sep:", "
              (List.map
                 ~f:(fun i -> files.(i).file)
                 (List.sort_uniq ~cmp:compare (List.map ~f:(fun id -> fst nodes.(id)) l)))))
  in
  Array.map ~f:(fun id -> nodes.(id)) global_order

(* For each type index, the start index of its recursive group and the
   group itself *)
let type_groups (types : Read.types) =
  let groups = Array.make types.last_index (0, [||]) in
  let _ =
    List.fold_left
      ~f:(fun idx rectype ->
        Array.iteri ~f:(fun j _ -> groups.(idx + j) <- idx, rectype) rectype;
        idx + Array.length rectype)
      ~init:0
      (List.rev types.rev_list)
  in
  groups

(* Compute which entries are reachable from the roots: the start
   functions, the exports that are kept, tables, active data and
   element segments. If [dependencies] is provided, the exports that
   are kept are the ones reachable from its root nodes; an import node
   is reached when the corresponding import is live. Without
   [dependencies], everything is live. *)
let compute_liveness
    ~files
    ~types
    ~groups
    ~resolved_imports
    ~(import_list : import array exportable_info)
    ~unresolved_imports
    ~functions
    ~start_type
    ~intfs
    ~filter_export
    ~dependencies =
  let import_count i kind =
    Array.length (get_exportable_info resolved_imports.(i) kind)
  in
  let dce = Option.is_some dependencies in
  let section_size (contents : Read.t) id =
    if Read.find_section contents id then Read.uint contents.ch else 0
  in
  let tags = Array.map ~f:(fun { contents; _ } -> Read.tags contents) files in
  let live =
    Array.mapi
      ~f:(fun i { contents; _ } ->
        { func = Array.make (import_count i Func + Array.length functions.(i)) (not dce)
        ; table = Array.make (import_count i Table + section_size contents 4) true
        ; mem = Array.make (import_count i Mem + section_size contents 5) true
        ; global = Array.make (import_count i Global + section_size contents 6) (not dce)
        ; tag = Array.make (import_count i Tag + Array.length tags.(i)) (not dce)
        })
      files
  in
  let unresolved =
    map_exportable_info
      (fun kind n ->
        match kind with
        | Table | Mem -> Array.make n true
        | Func | Global | Tag -> Array.make n (not dce))
      unresolved_imports
  in
  let segments =
    Array.map
      ~f:(fun { contents; _ } -> Array.make (section_size contents 9) (not dce))
      files
  in
  let data =
    Array.map
      ~f:(fun { contents; _ } -> Array.make (Read.data_count contents) (not dce))
      files
  in
  let type_live = Array.make types.Read.last_index (not dce) in
  if not dce
  then
    (* Keep the order of the input, except for globals whose
       initializer refers to a global defined later *)
    let global_deps =
      Array.map ~f:(fun l -> Array.make (Array.length l.global) []) live
    in
    let global_positions =
      Array.mapi
        ~f:(fun i { contents; _ } ->
          let current = ref 0 in
          let scanner =
            Scan.analysis
              ~visit:(fun kind idx ->
                match kind with
                | `Global ->
                    global_deps.(i).(!current) <- idx :: global_deps.(i).(!current)
                | `Func | `Ref_func | `Tag | `Elem | `Data | `Type -> ())
              contents.ch.buf
          in
          let offset = import_count i Global in
          let k = ref 0 in
          section_entries contents 6 (fun pos ->
              current := offset + !k;
              incr k;
              scanner.global pos))
        files
    in
    let global_counts =
      Array.map ~f:(fun l -> Array.make (Array.length l.global) 0) live
    in
    let undeclared_functions =
      (* Everything is kept but the exports filtered out. Rather than
         scanning the function bodies for [ref.func] instructions, we
         declare all the functions which were only declared by such an
         export. *)
      let candidates =
        List.concat
          (Array.to_list
             (Array.mapi
                ~f:(fun i intf ->
                  List.filter_map
                    ~f:(fun (name, idx) ->
                      if filter_export name then None else Some (i, idx))
                    intf.Read.exports.func)
                intfs))
      in
      match candidates with
      | [] -> []
      | _ :: _ ->
          let declared = ref [] in
          Array.iteri
            ~f:(fun i { contents; _ } ->
              let scanner =
                Scan.analysis
                  ~visit:(fun kind idx ->
                    match kind with
                    | `Func | `Ref_func -> declared := (i, idx) :: !declared
                    | `Global | `Tag | `Elem | `Data | `Type -> ())
                  contents.ch.buf
              in
              ignore (section_entries contents 4 scanner.table);
              ignore (section_entries contents 6 scanner.global);
              ignore (section_entries contents 9 scanner.elem))
            files;
          undeclared_functions
            ~resolved_imports
            ~intfs
            ~keep_export:filter_export
            ~declared:!declared
            candidates
    in
    { live
    ; segments
    ; data
    ; unresolved
    ; keep_export = filter_export
    ; ordering =
        { type_groups =
            (let l = ref [] in
             Array.iteri ~f:(fun t (idx, _) -> if idx = t then l := t :: !l) groups;
             Array.of_list (List.rev !l))
        ; globals =
            order_globals ~files ~resolved_imports ~live ~global_counts ~global_deps
        ; global_positions
        }
    ; undeclared_functions
    }
  else
    let stack = Stack.create () in
    (* Types: a type is live with its whole recursive group *)
    (* Number of references to each type and each global *)
    let type_counts = Array.make types.Read.last_index 0 in
    let global_counts =
      Array.map ~f:(fun live -> Array.make (Array.length live.global) 0) live
    in
    (* The globals referenced by the initializer of each global *)
    let global_deps =
      Array.map ~f:(fun live -> Array.make (Array.length live.global) []) live
    in
    let current_global = ref None in
    (* Functions referenced by [ref.func] in function bodies, and
       functions declared in global initializers, table initializers and
       live element segments *)
    let ref_funcs = ref [] in
    let declared = ref [] in
    let in_function = ref false in
    let rec mark_type t =
      type_counts.(t) <- type_counts.(t) + 1;
      if not type_live.(t)
      then (
        let idx, rectype = groups.(t) in
        Array.iteri ~f:(fun j _ -> type_live.(idx + j) <- true) rectype;
        Array.iter ~f:(iter_subtype_types (fun t -> if t >= 0 then mark_type t)) rectype)
    in
    Option.iter ~f:mark_type start_type;
    let mark i kind j =
      let l = get_exportable_info live.(i) kind in
      if not l.(j)
      then (
        l.(j) <- true;
        Stack.push (Entity (i, kind, j)) stack)
    in
    let mark_segment i j =
      if not segments.(i).(j)
      then (
        segments.(i).(j) <- true;
        Stack.push (Segment (i, j)) stack)
    in
    let mark_data i j =
      if not data.(i).(j)
      then (
        data.(i).(j) <- true;
        Stack.push (Data (i, j)) stack)
    in
    (* Exports *)
    let exports = String.Hashtbl.create 128 in
    Array.iteri
      ~f:(fun i intf ->
        iter_exportable_info
          (fun kind lst ->
            List.iter
              ~f:(fun (name, idx) ->
                if filter_export name then String.Hashtbl.add exports name (i, kind, idx))
              lst)
          intf.Read.exports)
      intfs;
    let kept_exports = String.Hashtbl.create 16 in
    let keep_export name =
      if String.Hashtbl.mem exports name && not (String.Hashtbl.mem kept_exports name)
      then (
        String.Hashtbl.replace kept_exports name ();
        List.iter
          ~f:(fun (i, kind, idx) -> mark i kind idx)
          (String.Hashtbl.find_all exports name))
    in
    (* Dependency graph *)
    let dependencies = Option.value ~default:[] dependencies in
    let nodes = String.Hashtbl.create 128 in
    let import_nodes = Poly.Hashtbl.create 128 in
    List.iter
      ~f:(fun (node : dependency) ->
        String.Hashtbl.replace nodes node.name node;
        Option.iter
          ~f:(fun import -> Poly.Hashtbl.add import_nodes import node)
          node.import)
      dependencies;
    let reached = String.Hashtbl.create 128 in
    let rec reach (node : dependency) =
      if not (String.Hashtbl.mem reached node.name)
      then (
        String.Hashtbl.replace reached node.name ();
        Option.iter ~f:keep_export node.export;
        List.iter
          ~f:(fun name ->
            match String.Hashtbl.find nodes name with
            | node -> reach node
            | exception Not_found -> ())
          node.reaches)
    in
    let mark_unresolved kind u =
      let l = get_exportable_info unresolved kind in
      if not l.(u)
      then (
        l.(u) <- true;
        let { module_; name; desc } = (get_exportable_info import_list kind).(u) in
        iter_importdesc_types mark_type desc;
        List.iter ~f:reach (Poly.Hashtbl.find_all import_nodes (module_, name)))
    in
    List.iter ~f:(fun (node : dependency) -> if node.root then reach node) dependencies;
    (* Imported tables and memories are always kept: they are used *)
    Array.iter
      ~f:(fun { desc; _ } -> iter_importdesc_types mark_type desc)
      import_list.table;
    Array.iter
      ~f:(fun { module_; name; _ } ->
        List.iter ~f:reach (Poly.Hashtbl.find_all import_nodes (module_, name)))
      (Array.append import_list.table import_list.mem);
    (* Other roots *)
    let scanners =
      Array.mapi
        ~f:(fun i { contents; _ } ->
          let type_mapping = contents.type_mapping in
          Scan.analysis
            ~visit:(fun kind idx ->
              match kind with
              | `Func ->
                  (* Outside function bodies, only element segments refer
                     to functions this way *)
                  if not !in_function then declared := (i, idx) :: !declared;
                  mark i Func idx
              | `Ref_func ->
                  if !in_function
                  then ref_funcs := (i, idx) :: !ref_funcs
                  else declared := (i, idx) :: !declared;
                  mark i Func idx
              | `Global ->
                  global_counts.(i).(idx) <- global_counts.(i).(idx) + 1;
                  (match !current_global with
                  | Some j -> global_deps.(i).(j) <- idx :: global_deps.(i).(j)
                  | None -> ());
                  mark i Global idx
              | `Tag -> mark i Tag idx
              | `Elem -> mark_segment i idx
              | `Data -> mark_data i idx
              | `Type -> mark_type type_mapping.(idx))
            contents.ch.buf)
        files
    in
    let positions id entry =
      Array.map
        ~f:(fun { contents; _ } ->
          section_entries
            contents
            id
            (entry (Scan.analysis ~visit:(fun _ _ -> ()) contents.ch.buf)))
        files
    in
    let global_positions = positions 6 (fun scanner -> scanner.Scan.global) in
    let segment_positions = positions 9 (fun scanner -> scanner.Scan.elem) in
    let data_positions = positions 11 (fun scanner -> scanner.Scan.data) in
    let code_positions =
      Array.map ~f:(fun { contents; _ } -> code_entries contents) files
    in
    Array.iteri
      ~f:(fun i { contents; _ } ->
        Option.iter ~f:(fun idx -> mark i Func idx) (Read.start contents);
        (* Declarative segments and passive segments which are not used
           do not make the functions they mention live *)
        Array.iteri
          ~f:(fun j pos ->
            match Char.code contents.ch.buf.[pos] with
            | 1 | 3 | 5 | 7 -> ()
            | _ -> mark_segment i j)
          segment_positions.(i);
        (* Passive data segments are only live if used *)
        Array.iteri
          ~f:(fun j pos ->
            match Char.code contents.ch.buf.[pos] with
            | 1 -> ()
            | _ -> mark_data i j)
          data_positions.(i);
        ignore (section_entries contents 4 scanners.(i).table))
      files;
    (* Propagate *)
    while not (Stack.is_empty stack) do
      match Stack.pop stack with
      | Entity (i, kind, j) -> (
          let imports = get_exportable_info resolved_imports.(i) kind in
          if j < Array.length imports
          then
            match imports.(j) with
            | Resolved (i', j') -> mark i' kind j'
            | Unresolved u -> mark_unresolved kind u
          else
            let k = j - Array.length imports in
            match kind with
            | Func ->
                mark_type functions.(i).(k);
                in_function := true;
                scanners.(i).func code_positions.(i).(k);
                in_function := false
            | Global ->
                current_global := Some j;
                ignore (scanners.(i).global global_positions.(i).(k));
                current_global := None
            | Tag -> mark_type tags.(i).(k)
            | Table | Mem -> ())
      | Segment (i, j) -> ignore (scanners.(i).elem segment_positions.(i).(j))
      | Data (i, j) -> ignore (scanners.(i).data data_positions.(i).(j))
    done;
    (* Order the type groups: a group can only refer to earlier groups *)
    let group_starts =
      let l = ref [] in
      Array.iteri
        ~f:(fun t (idx, _) -> if idx = t && type_live.(t) then l := t :: !l)
        groups;
      Array.of_list (List.rev !l)
    in
    let group_ids = Array.make types.Read.last_index (-1) in
    Array.iteri ~f:(fun n idx -> group_ids.(idx) <- n) group_starts;
    let type_order =
      priority_topological_sort
        ~n:(Array.length group_starts)
        ~deps:(fun n ->
          let l = ref [] in
          Array.iter
            ~f:
              (iter_subtype_types (fun t ->
                   if t >= 0 then l := group_ids.(fst groups.(t)) :: !l))
            (snd groups.(group_starts.(n)));
          !l)
        ~priority:(fun n ->
          let idx = group_starts.(n) in
          let count = ref 0 in
          Array.iteri
            ~f:(fun j _ -> count := !count + type_counts.(idx + j))
            (snd groups.(idx));
          !count)
    in
    let global_order =
      order_globals ~files ~resolved_imports ~live ~global_counts ~global_deps
    in
    (* A function referenced by [ref.func] in a function body must be
       declared: in an element segment (all are kept, with their live
       functions), in a table initializer (tables are kept), in an
       export, or in a global initializer. *)
    let undeclared_functions =
      (* The segments which are not live are kept as declarations of
         their live functions *)
      Array.iteri
        ~f:(fun i { contents; _ } ->
          let scanner =
            Scan.analysis
              ~visit:(fun kind idx ->
                match kind with
                | `Func | `Ref_func -> declared := (i, idx) :: !declared
                | `Global | `Tag | `Elem | `Data | `Type -> ())
              contents.ch.buf
          in
          Array.iteri
            ~f:(fun j pos -> if not segments.(i).(j) then ignore (scanner.elem pos))
            segment_positions.(i))
        files;
      undeclared_functions
        ~resolved_imports
        ~intfs
        ~keep_export:(fun name -> String.Hashtbl.mem kept_exports name)
        ~declared:!declared
        !ref_funcs
    in
    { live
    ; segments
    ; data
    ; unresolved
    ; keep_export = (fun name -> String.Hashtbl.mem kept_exports name)
    ; ordering =
        { type_groups = Array.map ~f:(fun n -> group_starts.(n)) type_order
        ; globals = global_order
        ; global_positions
        }
    ; undeclared_functions
    }

(* Output indices of the globals, in the order given by [ordering]. Dead
   globals are mapped to -1. *)
let global_mappings ~files ~resolved_imports ~unresolved_imports ordering =
  let imports i = get_exportable_info resolved_imports.(i) Global in
  let global_mappings =
    Array.mapi
      ~f:(fun i _ ->
        Array.make
          (Array.length (imports i) + Array.length ordering.global_positions.(i))
          (-1))
      files
  in
  let offset = get_exportable_info unresolved_imports Global in
  Array.iteri ~f:(fun n (i, j) -> global_mappings.(i).(j) <- offset + n) ordering.globals;
  (* Imports resolve to definitions or to unresolved imports *)
  Array.iteri
    ~f:(fun i _ ->
      Array.iteri
        ~f:(fun j status ->
          match status with
          | Unresolved u -> global_mappings.(i).(j) <- u
          | Resolved _ -> ())
        (imports i))
    files;
  Array.iteri
    ~f:(fun i _ ->
      Array.iteri
        ~f:(fun j status ->
          match status with
          | Resolved (i', j') -> global_mappings.(i).(j) <- global_mappings.(i').(j')
          | Unresolved _ -> ())
        (imports i))
    files;
  global_mappings

(* Write the global definitions in the order given by [ordering] *)
let write_globals
    ~files
    ~resolved_imports
    ~type_maps
    ~func_mappings
    ~global_mappings
    ~(positions : Scan.position_data array)
    ~buf
    ordering =
  let imports i = get_exportable_info resolved_imports.(i) Global in
  let scanners =
    Array.mapi
      ~f:(fun i { contents; file; _ } ->
        (* Copy, so that [positions] does not alias the ordering *)
        let p = Array.copy ordering.global_positions.(i) in
        positions.(i).pos <- p;
        positions.(i).i <- Array.length p;
        Scan.global_entry
          ~file
          { Scan.default_maps with
            typ = type_maps.(i)
          ; func = func_mappings.(i)
          ; global = global_mappings.(i)
          }
          buf
          contents.ch.buf)
      files
  in
  Array.iter
    ~f:(fun (i, j) ->
      ignore (scanners.(i) ordering.global_positions.(i).(j - Array.length (imports i))))
    ordering.globals;
  Array.length ordering.globals

type output =
  { source_map : Source_map.t
  ; imports : (string * string) list
  }

let f ?(filter_export = fun _ -> true) ?dependencies ?(names = true) files ~output_file =
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

  let types = Read.create_types () in
  let intfs = Array.map ~f:(fun f -> interface types f.contents) files in
  let start_count =
    Array.fold_left
      ~f:(fun count f ->
        match Read.start f.contents with
        | None -> count
        | Some _ -> count + 1)
      ~init:0
      files
  in
  (* Type of the function calling all start functions *)
  let start_type =
    if start_count > 1
    then
      let typ : comptype = Func { params = [||]; results = [||] } in
      Some (Read.add_rectype types [| { final = true; supertype = None; typ } |])
    else None
  in
  let groups = type_groups types in
  let subtyping_info = Array.concat (List.rev types.rev_subtyping_info) in

  (* Import resolution *)
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
  let import_list = make_exportable_info [] in
  let unresolved_imports = make_exportable_info 0 in
  let resolved_imports =
    let tbl = Poly.Hashtbl.create 128 in
    Array.mapi
      ~f:(fun i intf ->
        map_exportable_info
          (fun kind imports ->
            let exports = get_exportable_info exports kind in
            let unresolved import =
              match Poly.Hashtbl.find tbl import with
              | status -> status
              | exception Not_found ->
                  let idx = get_exportable_info unresolved_imports kind in
                  let status = Unresolved idx in
                  Poly.Hashtbl.replace tbl import status;
                  set_exportable_info unresolved_imports kind (1 + idx);
                  set_exportable_info
                    import_list
                    kind
                    (import :: get_exportable_info import_list kind);
                  status
            in
            Array.map
              ~f:(fun (import : import) ->
                match resolve 0 ~files ~intfs ~subtyping_info ~exports ~kind i import with
                | i', idx ->
                    (* An export of an import of another module which
                       remains unresolved: we use this import directly,
                       so that [Resolved] always refers to a definition *)
                    let imports' = get_exportable_info intfs.(i').Read.imports kind in
                    if idx < Array.length imports'
                    then unresolved imports'.(idx)
                    else Resolved (i', idx)
                | exception Not_found -> unresolved import)
              imports)
          intf.Read.imports)
      intfs
  in
  let import_list =
    map_exportable_info (fun _ l -> Array.of_list (List.rev l)) import_list
  in

  (* Dead code elimination *)
  let functions = Array.map ~f:(fun f -> Read.functions f.contents) files in
  let liveness =
    compute_liveness
      ~files
      ~types
      ~groups
      ~resolved_imports
      ~import_list
      ~unresolved_imports
      ~functions
      ~start_type
      ~intfs
      ~filter_export
      ~dependencies
  in
  let live = liveness.live in
  (* Number the live entries consecutively, across all the arrays. Dead
     entries are mapped to -1. Also return the number of live entries of
     each array. *)
  let compact live =
    let n = ref 0 in
    let counts = Array.make (Array.length live) 0 in
    let mappings =
      Array.mapi
        ~f:(fun i live ->
          let first = !n in
          let mapping =
            Array.map
              ~f:(fun l ->
                if l
                then (
                  let idx = !n in
                  incr n;
                  idx)
                else -1)
              live
          in
          counts.(i) <- !n - first;
          mapping)
        live
    in
    mappings, counts
  in
  let rectype idx = snd groups.(idx) in
  let group_order = liveness.ordering.type_groups in
  let type_map = Array.make types.last_index (-1) in
  let _ =
    Array.fold_left
      ~f:(fun n idx ->
        Array.iteri ~f:(fun j _ -> type_map.(idx + j) <- n + j) (rectype idx);
        n + Array.length (rectype idx))
      ~init:0
      group_order
  in
  let type_maps =
    Array.map
      ~f:(fun { contents; _ } ->
        Array.map ~f:(fun t -> type_map.(t)) contents.Read.type_mapping)
      files
  in
  (* Renumber the imports which are kept *)
  let unresolved_mappings =
    map_exportable_info
      (fun kind l ->
        let mappings, counts = compact [| l |] in
        set_exportable_info unresolved_imports kind counts.(0);
        mappings.(0))
      liveness.unresolved
  in
  Array.iter
    ~f:(fun statuses ->
      iter_exportable_info
        (fun kind statuses ->
          let map = get_exportable_info unresolved_mappings kind in
          Array.iteri
            ~f:(fun j status ->
              match status with
              | Unresolved u -> statuses.(j) <- Unresolved map.(u)
              | Resolved _ -> ())
            statuses)
        statuses)
    resolved_imports;

  let out_ch = open_out_bin output_file in
  output_string out_ch Read.header;
  let buf = Buffer.create 100000 in

  (* 1: type *)
  let st = Write.types buf ~type_map (Array.map ~f:rectype group_order) in
  add_section out_ch ~id:1 buf;

  (* 2: import *)
  let imports = ref [] in
  iter_exportable_info
    (fun kind import_list ->
      let map = get_exportable_info unresolved_mappings kind in
      Array.iteri
        ~f:(fun idx import -> if map.(idx) >= 0 then imports := import :: !imports)
        import_list)
    import_list;
  let imports = Array.of_list (List.rev !imports) in
  if Array.length imports > 0
  then (
    Write.imports st buf imports;
    add_section out_ch ~id:2 buf);

  (* 3: function *)
  let func_types =
    let l =
      Array.to_list
        (Array.mapi ~f:(fun i types -> filter_live ~live:live.(i).func types) functions)
    in
    let l =
      match start_type with
      | Some ty -> l @ [ [| ty |] ]
      | None -> l
    in
    Array.concat l
  in
  Write.functions buf (Array.map ~f:(fun t -> type_map.(t)) func_types);
  add_section out_ch ~id:3 buf;
  let func_counts = Array.map ~f:Array.length functions in
  let func_mappings =
    build_mappings ~live resolved_imports unresolved_imports Func func_counts
  in
  let func_count = Array.length func_types in
  check_exports_against_imports
    ~intfs
    ~subtyping_info
    ~resolved_imports
    ~files
    ~kind:Func
    ~to_desc:
      (defined_entity ~intfs ~kind:Func ~get:(fun i k : importdesc ->
           Func functions.(i).(k)));

  let global_mappings =
    global_mappings ~files ~resolved_imports ~unresolved_imports liveness.ordering
  in

  (* 4: table *)
  (* The table section comes before the global section: a table
     initializer can only refer to imported globals *)
  let imported_globals = get_exportable_info unresolved_imports Global in
  Array.iteri
    ~f:(fun i { contents; _ } ->
      let scanner =
        Scan.analysis
          ~visit:(fun kind idx ->
            match kind with
            | `Global ->
                if global_mappings.(i).(idx) >= imported_globals
                then
                  failwith
                    (Printf.sprintf
                       "In module %s, a table initializer refers to a global which is \
                        not imported in the linked module"
                       files.(i).file)
            | `Func | `Ref_func | `Tag | `Elem | `Data | `Type -> ())
          contents.ch.buf
      in
      ignore (section_entries contents 4 scanner.table))
    files;
  let positions =
    Array.init (Array.length files) ~f:(fun _ -> Scan.create_position_data ())
  in
  let table_counts =
    write_section_with_scan
      ~files
      ~type_maps
      ~out_ch
      ~buf
      ~id:4
      ~scan:(fun i maps ->
        Scan.table_section
          ~file:files.(i).file
          positions.(i)
          { maps with func = func_mappings.(i); global = global_mappings.(i) })
      ()
  in
  let table_mappings =
    build_mappings ~live resolved_imports unresolved_imports Table table_counts
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
      ~live
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
      ~live
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
      ~write:(fun buf l -> Write.tags buf (Array.map ~f:(fun t -> type_map.(t)) l))
      ~files
  in

  (* 6: global *)
  let global_count =
    write_globals
      ~files
      ~resolved_imports
      ~type_maps
      ~func_mappings
      ~global_mappings
      ~positions
      ~buf
      liveness.ordering
  in
  add_section out_ch ~id:6 ~count:global_count buf;
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
  let exports =
    Array.map
      ~f:(fun intf ->
        map_exportable_info
          (fun _ exports ->
            List.filter ~f:(fun (nm, _) -> liveness.keep_export nm) exports)
          intf.Read.exports)
      intfs
  in
  let export_count =
    Array.fold_left
      ~f:(fun count exports ->
        fold_exportable_info
          (fun _ exports count -> List.length exports + count)
          count
          exports)
      ~init:0
      exports
  in
  Write.uint buf export_count;
  let export_tbl = String.Hashtbl.create 128 in
  Array.iteri
    ~f:(fun i exports ->
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
              match String.Hashtbl.find export_tbl name with
              | i' ->
                  failwith
                    (Printf.sprintf
                       "Duplicated export %s from %s and %s"
                       name
                       files.(i').file
                       files.(i).file)
              | exception Not_found ->
                  String.Hashtbl.add export_tbl name i;
                  Write.export buf kind name map.(idx))
            lst)
        exports)
    exports;
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
      Write.start buf (get_exportable_info unresolved_imports Func + func_count - 1);
      add_section out_ch ~id:8 buf);

  (* 9: elements *)
  let elem_counts =
    write_section_with_scan
      ~files
      ~type_maps
      ~out_ch
      ~buf
      ~id:9
      ~scan:(fun i maps buf s ->
        Scan.elem_section
          ~file:files.(i).file
          { maps with
            func = func_mappings.(i)
          ; global = global_mappings.(i)
          ; table = table_mappings.(i)
          }
          buf
          s
          ~keep:(fun j -> liveness.segments.(i).(j)))
      ~extra:(fun buf ->
        match liveness.undeclared_functions with
        | [] -> 0
        | l ->
            (* A declarative segment *)
            Buffer.add_char buf '\x03';
            Buffer.add_char buf '\x00';
            Write.uint buf (List.length l);
            List.iter ~f:(fun (i, j) -> Write.uint buf func_mappings.(i).(j)) l;
            1)
      ()
  in
  let elem_mappings = build_simple_mappings ~counts:elem_counts in

  (* 12: data count *)
  let data_mappings, data_counts = compact liveness.data in
  let data_count = Array.fold_left ~f:( + ) ~init:0 data_counts in
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
    ~f:(fun i { contents; source_map_contents; file; _ } ->
      if Read.find_section contents 10
      then (
        let pos = Buffer.length code_pieces in
        let scan_func =
          Scan.func
            ~file
            resize_data
            { typ = type_maps.(i)
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
        let live = live.(i).func in
        let offset = Array.length live - Array.length functions.(i) in
        let j = ref offset in
        let dead_ranges = ref [] in
        let code (ch : Read.ch) =
          let pos = ch.pos in
          let i = resize_data.i in
          let size = Read.uint ch in
          let pos' = ch.pos in
          let is_live = live.(!j) in
          incr j;
          if not is_live
          then (
            (* Drop the function, and the corresponding mappings *)
            ch.pos <- ch.pos + size;
            dead_ranges := (pos, ch.pos) :: !dead_ranges;
            Scan.push_resize resize_data ch.pos (pos - ch.pos))
          else (
            Scan.push_resize resize_data pos' 0;
            scan_func ch.pos;
            ch.pos <- ch.pos + size;
            let p = Buffer.length code_pieces in
            Write.uint code_pieces (Buffer.length buf);
            let p' = Buffer.length code_pieces in
            let delta = p' - p - pos' + pos in
            resize_data.delta.(i) <- delta;
            Buffer.add_buffer code_pieces buf;
            Buffer.clear buf)
        in
        let count = Read.uint contents.ch in
        Scan.clear_resize_data resize_data;
        Scan.push_resize resize_data 0 (-Read.pos_in contents.ch);
        Read.repeat' count code contents.ch;
        Option.iter
          ~f:(fun sm ->
            if not (Wasm_source_map.is_empty sm)
            then
              source_maps :=
                ( pos
                , Wasm_source_map.resize
                    ~dead_ranges:(List.rev !dead_ranges)
                    resize_data
                    sm )
                :: !source_maps)
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
    (* end *)
    Buffer.add_char buf (Char.chr 0x0B);
    Write.uint code_pieces (Buffer.length buf);
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
    (write_section_with_scan
       ~files
       ~type_maps
       ~out_ch
       ~buf
       ~id:11
       ~scan:(fun i maps buf s ->
         Scan.data_section
           ~file:files.(i).file
           { maps with mem = mem_mappings.(i); global = global_mappings.(i) }
           buf
           s
           ~keep:(fun j -> liveness.data.(i).(j)))
       ~written:(fun i _ -> data_counts.(i))
       ());

  (* Custom section: name *)
  if names
  then (
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
      ~f:(fun type_map name_section ->
        if Read.find_section name_section 4
        then
          let map = Read.namemap name_section in
          Array.iter
            ~f:(fun (idx, name) ->
              let idx = type_map.(idx) in
              if idx >= 0 && Option.is_none type_names.(idx)
              then type_names.(idx) <- Some (idx, name))
            map)
      type_maps
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
      ~f:(fun type_map name_section ->
        if Read.find_section name_section 10
        then
          let n = Read.uint name_section.ch in
          let scan_map = Scan.local_namemap buf name_section.ch.buf in
          for _ = 1 to n do
            let idx = type_map.(Read.uint name_section.ch) in
            scan_map name_section.ch.pos;
            name_section.ch.pos <- name_section.ch.pos + Buffer.length buf;
            if idx >= 0 && Option.is_none type_field_names.(idx)
            then type_field_names.(idx) <- Some (idx, Buffer.contents buf);
            Buffer.clear buf
          done)
      type_maps
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

    add_section out_ch ~id:0 name_section_buffer);

  close_out out_ch;

  { source_map
  ; imports =
      Array.to_list (Array.map ~f:(fun { module_; name; _ } -> module_, name) imports)
  }

(*
LATER
- testsuite : import/export matching, source maps, multiple start functions, ...
- check features?

MAYBE
- topologic sort of globals?
  => easy: just look at the import/export dependencies between modules
- reorder types/globals/functions to generate a smaller binary
*)
