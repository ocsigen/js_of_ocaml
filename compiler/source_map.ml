type map = {
  gen_line : int;
  gen_col : int;
  ori_source : int;
  ori_line : int;
  ori_col : int;
  ori_name : int option
}

type mapping = map list

type t = {
  version : int;
  file : string;
  sourceroot : string option;
  mutable sources : string list;
  mutable sources_content : string option list;
  mutable names : string list;
  mutable mappings : mapping ;
}

let string_of_mapping mapping =
  let a = Array.of_list mapping in
  let len = Array.length a in
  Array.fast_sort (fun t1 t2 ->
    match compare t1.gen_line t2.gen_line with
      | 0 -> compare t1.gen_col t2.gen_col
      | n -> n) a;
  let buf = Buffer.create 1024 in
  let rec loop prev i =
    if i >= len
    then ()
    else
      let c = a.(i) in
      let prev =
        if prev.gen_line <> c.gen_line
        then begin
          assert (prev.gen_line < c.gen_line);
          for j = prev.gen_line to c.gen_line - 1 do
            Buffer.add_char buf ';';
          done;
          {prev with gen_col = 0; gen_line = c.gen_line}
        end
        else begin
          if i > 0 then Buffer.add_char buf ',';
          prev
        end in
      begin
        let diff_name,prev_name = match c.ori_name, prev.ori_name with
          | None,None -> None,None
          | Some o,Some p -> Some (o - p), Some o
          | Some o,None -> Some o, Some o
          | None, Some p -> None, Some p in

        let l = [c.gen_col - prev.gen_col;
                 c.ori_source - prev.ori_source;
                 c.ori_line - prev.ori_line;
                 c.ori_col - prev.ori_col ] in
        let l = match diff_name with
          | None -> l
          | Some d -> l@[d] in

        Vlq64.encode_l buf l;
        loop {c with ori_name = prev_name} (succ i)
      end
  in
  loop {gen_line=0;gen_col=0;ori_source=0;ori_line=0;ori_col=0;ori_name=None} 0;
  Buffer.contents buf

let statements t =
  let open Javascript in
  [Statement (
    Expression_statement (
      EObj [
        PNS "version", ENum (float_of_int t.version);
        PNS "file", EStr (t.file,`Bytes);
        PNS "sourceRoot", EStr ((match t.sourceroot with None -> "" | Some s -> s),`Bytes);
        PNS "sources", EArr (List.map (fun s -> Some (EStr (s,`Bytes))) t.sources);
        PNS "sources_content", EArr (List.map (function
          | None -> Some (EVar (S {name="null";var=None}))
          | Some s -> Some (EStr (s,`Bytes))) t. sources_content);
        PNS "names", EArr (List.map (fun s -> Some (EStr (s,`Bytes))) t.names);
        PNS "mappings", EStr (string_of_mapping t.mappings,`Bytes)
      ],
      None)
  )]
