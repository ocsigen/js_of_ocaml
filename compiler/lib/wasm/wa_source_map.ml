open Stdlib

type resize_data =
  { mutable i : int
  ; mutable pos : int array
  ; mutable delta : int array
  }

type t = Yojson.Raw.t

type u =
  { mappings : string
  ; mutable pos : int
  }

module Vlq = struct
  let code = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

  let code_rev =
    let a = Array.make 255 (-1) in
    for i = 0 to String.length code - 1 do
      a.(Char.code code.[i]) <- i
    done;
    a

  let vlq_base_shift = 5

  let vlq_base = 1 lsl vlq_base_shift

  let vlq_base_mask = vlq_base - 1

  let vlq_continuation_bit = vlq_base

  let rec decode' src s pos offset i =
    let digit = Array.unsafe_get code_rev (Char.code s.[pos]) in
    if digit = -1 then invalid_arg "Vql64.decode'";
    let i = i + ((digit land vlq_base_mask) lsl offset) in
    if digit >= vlq_continuation_bit
    then decode' src s (pos + 1) (offset + vlq_base_shift) i
    else (
      src.pos <- pos + 1;
      i)

  let fromVLQSigned v =
    let is_neg = v land 1 = 1 in
    let shift = v lsr 1 in
    if is_neg then -shift else shift

  let toVLQSigned v = if v < 0 then (-v lsl 1) + 1 else v lsl 1

  let decode src = fromVLQSigned (decode' src src.mappings src.pos 0 0)

  let rec encode' buf i =
    let digit = i land vlq_base_mask in
    let i = i lsr vlq_base_shift in
    if i = 0
    then Buffer.add_char buf (String.unsafe_get code digit)
    else (
      Buffer.add_char buf (String.unsafe_get code (digit lor vlq_continuation_bit));
      encode' buf i)

  let encode buf i = encode' buf (toVLQSigned i)
end

let rec next' src mappings pos =
  match mappings.[pos] with
  | '"' ->
      src.pos <- pos + 1;
      false
  | ',' ->
      src.pos <- pos + 1;
      true
  | _ -> next' src mappings (pos + 1)

let next src = next' src src.mappings src.pos

let flush buf src start pos =
  if start < pos then Buffer.add_substring buf src.mappings start (pos - start)

let rec resize_rec buf start src resize_data i col0 delta0 col =
  let pos = src.pos in
  let delta = Vlq.decode src in
  let col = col + delta in
  if col < col0
  then
    if next src
    then resize_rec buf start src resize_data i col0 delta0 col
    else flush buf src start (String.length src.mappings)
  else
    let delta = delta + delta0 in
    adjust buf start src resize_data i col delta pos

and adjust buf start src (resize_data : resize_data) i col delta pos =
  assert (delta > 0);
  if i < resize_data.i
  then
    let col0 = resize_data.pos.(i) in
    let delta0 = resize_data.delta.(i) in
    if col < col0
    then (
      flush buf src start pos;
      Vlq.encode buf delta;
      let start = src.pos in
      if next src
      then resize_rec buf start src resize_data (i + 1) col0 delta0 col
      else flush buf src start (String.length src.mappings))
    else
      let delta = delta + delta0 in
      adjust buf start src resize_data (i + 1) col delta pos
  else (
    flush buf src start pos;
    Vlq.encode buf delta;
    let start = src.pos in
    flush buf src start (String.length src.mappings))

let resize_mappings (resize_data : resize_data) mappings =
  if String.equal mappings "\"\"" || resize_data.i = 0
  then mappings
  else
    let col0 = resize_data.pos.(0) in
    let delta0 = resize_data.delta.(0) in
    let buf = Buffer.create (String.length mappings) in
    resize_rec buf 0 { mappings; pos = 1 } resize_data 1 col0 delta0 0;
    Buffer.contents buf

let to_raw_string v =
  match v with
  | `Stringlit s -> s
  | _ -> assert false

let replace_member assoc m v =
  `Assoc ((m, v) :: List.remove_assoc m (Yojson.Raw.Util.to_assoc assoc))

let resize resize_data sm =
  let open Yojson.Raw.Util in
  let mappings = to_raw_string (member "mappings" sm) in
  let mappings = resize_mappings resize_data mappings in
  replace_member sm "mappings" (`Stringlit mappings)

let is_empty sm =
  let open Yojson.Raw.Util in
  match member "mappings" sm with
  | `Stringlit "\"\"" -> true
  | _ -> false

let concatenate l =
  `Assoc
    [ "version", `Intlit "3"
    ; ( "sections"
      , `List
          (List.map
             ~f:(fun (ofs, sm) ->
               `Assoc
                 [ ( "offset"
                   , `Assoc [ "line", `Intlit "0"; "column", `Intlit (string_of_int ofs) ]
                   )
                 ; "map", sm
                 ])
             l) )
    ]

let parse ?tmp_buf s = Yojson.Raw.from_string ?buf:tmp_buf s

let load ?tmp_buf name = parse ?tmp_buf (Fs.read_file name)

let write name sm = Yojson.Raw.to_file name sm

let string_from_raw_string s = Yojson.Basic.Util.to_string (Yojson.Basic.from_string s)

let raw_string_from_string s = Yojson.Basic.to_string (`String s)

let iter_sources' sm i f =
  let open Yojson.Raw.Util in
  let l = sm |> member "sources" |> to_option to_list |> Option.value ~default:[] in
  let single = List.length l = 1 in
  List.iteri
    ~f:(fun j nm ->
      f i (if single then None else Some j) (string_from_raw_string (to_raw_string nm)))
    l

let iter_sources sm f =
  let open Yojson.Raw.Util in
  match to_option to_list (member "sections" sm) with
  | None -> iter_sources' sm None f
  | Some l ->
      let single_map = List.length l = 1 in
      List.iteri
        ~f:(fun i entry ->
          iter_sources' (member "map" entry) (if single_map then None else Some i) f)
        l

let insert_source_contents' ~rewrite_path sm i f =
  let rewrite_path path =
    raw_string_from_string (rewrite_path (string_from_raw_string path))
  in
  let open Yojson.Raw.Util in
  let l = sm |> member "sources" |> to_option to_list |> Option.value ~default:[] in
  let single = List.length l = 1 in
  let contents =
    List.mapi
      ~f:(fun j nm ->
        match
          f
            i
            (if single then None else Some j)
            (string_from_raw_string (to_raw_string nm))
        with
        | Some c -> `Stringlit c
        | None -> `Null)
      l
  in
  let sm = replace_member sm "sourcesContent" (`List contents) in
  let sm =
    replace_member
      sm
      "sources"
      (match member "sources" sm with
      | `Null -> `Null
      | `List l ->
          `List (List.map ~f:(fun s -> `Stringlit (rewrite_path (to_raw_string s))) l)
      | _ -> assert false)
  in
  sm

let insert_source_contents ~rewrite_path sm f =
  let open Yojson.Raw.Util in
  match to_option to_list (member "sections" sm) with
  | None -> insert_source_contents' ~rewrite_path sm None f
  | Some l ->
      let single_map = List.length l = 1 in
      let sections =
        List.mapi
          ~f:(fun i entry ->
            replace_member
              entry
              "map"
              (insert_source_contents'
                 ~rewrite_path
                 (member "map" entry)
                 (if single_map then None else Some i)
                 f))
          l
      in
      replace_member sm "sections" (`List sections)
