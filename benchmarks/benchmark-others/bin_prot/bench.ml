open Bin_prot.Std

type element =
  { a : string
  ; b : string
  ; c : string
  ; d : bool
  ; e : bool
  ; f : bool
  ; g : string option
  ; h : bool
  ; i : bool
  ; j : bool
  ; k : bool
  ; l : bool
  ; m : bool
  ; n : bool
  ; o : string option
  ; p : bool
  ; q : bool
  ; r : int
  ; s : int
  ; t : int
  ; u : int
  ; v : string list (* these are small - 1-5 *)
  }
[@@deriving bin_io]

let s = "abcdefabcdefabcdef"

let v = [ s; s; s; s ]

let x =
  { a = s
  ; b = s
  ; c = s
  ; d = true
  ; e = true
  ; f = true
  ; g = Some s
  ; h = true
  ; i = true
  ; j = true
  ; k = true
  ; l = true
  ; m = true
  ; n = true
  ; o = Some s
  ; p = true
  ; q = true
  ; r = 65537
  ; s = 65537
  ; t = 65537
  ; u = 65537
  ; v
  }

type t = element list [@@deriving bin_io]

let rec f acc n = if n = 0 then acc else f (x :: acc) (n - 1)

let () =
  if Array.length Sys.argv > 1
  then (
    let count = int_of_string Sys.argv.(1) in
    let l = f [] count in
    let len = [%bin_size: t] l in
    let b = Bin_prot.Common.create_buf len in
    ignore ([%bin_write: t] b ~pos:0 l : int);
    let s = Bytes.create len in
    Bin_prot.Common.blit_buf_string ~src_pos:0 b ~dst_pos:0 s ~len;
    Out_channel.with_open_bin "data" @@ fun ch -> Out_channel.output_bytes ch s)
  else
    let s = In_channel.with_open_bin "data" @@ In_channel.input_all in
    let len = String.length s in
    let b = Bin_prot.Common.create_buf len in
    Bin_prot.Common.blit_string_buf ~src_pos:0 s ~dst_pos:0 b ~len;
    let t = Unix.gettimeofday () in
    for _ = 0 to 4 do
      ignore ([%bin_read: t] b ~pos_ref:(ref 0) : t)
    done;
    let t' = Unix.gettimeofday () in
    Format.printf "%.2f@." (t' -. t)
