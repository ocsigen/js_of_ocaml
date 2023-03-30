open! Stdlib

module PP : sig
  type t

  val empty : t

  val ( ^^ ) : t -> t -> t

  val string : string -> t

  val line : t -> t

  val indent : t -> t

  val concat_map : ('a -> t) -> 'a list -> t

  val separate_map : t -> ('a -> t) -> 'a list -> t

  val to_channel : out_channel -> t -> unit

  (*  val to_buffer : Buffer.t -> t -> unit *)
end = struct
  let spaces = "\t" ^ String.make 80 ' '

  type st =
    { mutable indent : int
    ; output : string -> int -> int -> unit
    }

  type t = st -> unit

  let empty _ = ()

  let string s st = st.output s 0 (String.length s)

  let ( ^^ ) s s' st =
    s st;
    s' st

  let line l st =
    st.output spaces 0 (min (String.length spaces) st.indent);
    l st;
    st.output "\n" 0 1

  let indent x st =
    st.indent <- st.indent + 1;
    x st;
    st.indent <- st.indent - 1

  let concat_map f l st = List.iter ~f:(fun x -> f x st) l

  let separate_map sep f l st =
    List.iteri
      ~f:(fun i x ->
        if i > 0 then sep st;
        f x st)
      l

  let to_channel ch doc = doc { indent = 0; output = output_substring ch }

  (*
  let to_buffer b doc =
    doc { indent = 0; output = (fun s i l -> Buffer.add_substring b s i l) }
  *)
end

open PP
open Wa_ast

let value_type (t : value_type) =
  string
    (match t with
    | I32 -> "i32"
    | I64 -> "i64"
    | F64 -> "f64")

let func_type { params; result } =
  assert (List.length result <= 1);
  string "("
  ^^ separate_map (string ", ") value_type params
  ^^ string ") -> ("
  ^^ separate_map (string ", ") value_type result
  ^^ string ")"

let block_type ty =
  match ty with
  | { params = []; result = [] } -> empty
  | { params = []; result = [ res ] } -> string " " ^^ value_type res
  | _ -> assert false

let type_prefix op =
  match op with
  | I32 _ -> string "i32."
  | I64 _ -> string "i64."
  | F64 _ -> string "f64."

let int_un_op op =
  match op with
  | Clz -> "clz"
  | Ctz -> "ctz"
  | Popcnt -> "popcnt"
  | Eqz -> "eqz"

let signage op (s : Wa_ast.signage) =
  op
  ^
  match s with
  | S -> "_s"
  | U -> "_u"

let int_bin_op (op : int_bin_op) =
  match op with
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div s -> signage "div" s
  | Rem s -> signage "rem" s
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Shl -> "shl"
  | Shr s -> signage "shr" s
  | Rotl -> "rotl"
  | Rotr -> "rotr"
  | Eq -> "eq"
  | Ne -> "ne"
  | Lt s -> signage "lt" s
  | Gt s -> signage "gt" s
  | Le s -> signage "le" s
  | Ge s -> signage "ge" s

let float_un_op op =
  match op with
  | Neg -> "neg"
  | Abs -> "abs"
  | Ceil -> "ceil"
  | Floor -> "floor"
  | Trunc -> "trunc"
  | Nearest -> "nearest"
  | Sqrt -> "sqrt"

let float_bin_op op =
  match op with
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Min -> "min"
  | Max -> "max"
  | CopySign -> "copysign"
  | Eq -> "eq"
  | Ne -> "ne"
  | Lt -> "lt"
  | Gt -> "gt"
  | Le -> "le"
  | Ge -> "ge"

let select i32 i64 f64 op =
  match op with
  | I32 x -> i32 x
  | I64 x -> i64 x
  | F64 x -> f64 x

let integer i = string (string_of_int i)

let integer32 i =
  string
    (if Poly.(i > -10000l && i < 10000l)
     then Int32.to_string i
     else Printf.sprintf "0x%lx" i)

let integer64 i =
  string
    (if Poly.(i > -10000L && i < 10000L)
     then Int64.to_string i
     else Printf.sprintf "0x%Lx" i)

let symbol name offset =
  string
    (match name with
    | V name -> Code.Var.to_string name
    | S name -> name)
  ^^
  if offset = 0
  then empty
  else (if offset < 0 then empty else string "+") ^^ integer offset

let rec expression e =
  match e with
  | Const op ->
      line
        (type_prefix op
        ^^ string "const "
        ^^ select integer32 integer64 (fun f -> string (string_of_float f (*ZZZ*))) op)
  | ConstSym (name, offset) ->
      line (type_prefix (I32 ()) ^^ string "const " ^^ symbol name offset)
  | UnOp (op, e') ->
      expression e'
      ^^ line (type_prefix op ^^ string (select int_un_op int_un_op float_un_op op))
  | BinOp (op, e1, e2) ->
      expression e1
      ^^ expression e2
      ^^ line (type_prefix op ^^ string (select int_bin_op int_bin_op float_bin_op op))
  | Load (offset, e') ->
      expression e'
      ^^ line
           (type_prefix offset
           ^^ string "load "
           ^^ string (select Int32.to_string Int32.to_string Int32.to_string offset))
  | Load8 (s, offset, e') ->
      expression e'
      ^^ line
           (type_prefix offset
           ^^ string (signage "load8" s)
           ^^ string " "
           ^^ string (select Int32.to_string Int32.to_string Int32.to_string offset))
  | LocalGet i -> line (string "local.get " ^^ integer i)
  | LocalTee (i, e') -> expression e' ^^ line (string "local.tee " ^^ integer i)
  | GlobalGet nm -> line (string "global.get " ^^ symbol nm 0)
  | Call_indirect (typ, f, l) ->
      concat_map expression l
      ^^ expression f
      ^^ line (string "call_indirect " ^^ func_type typ)
  | Call (x, l) -> concat_map expression l ^^ line (string "call " ^^ symbol x 0)
  | MemoryGrow (mem, e) -> expression e ^^ line (string "memory.grow " ^^ integer mem)
  | Seq (l, e') -> concat_map instruction l ^^ expression e'
  | Pop _ -> empty

and instruction i =
  match i with
  | Drop e -> expression e ^^ line (string "drop")
  | Store (offset, e, e') ->
      expression e
      ^^ expression e'
      ^^ line
           (type_prefix offset
           ^^ string "store "
           ^^ string (select Int32.to_string Int32.to_string Int32.to_string offset))
  | Store8 (s, offset, e, e') ->
      expression e
      ^^ expression e'
      ^^ line
           (type_prefix offset
           ^^ string (signage "store8" s)
           ^^ string " "
           ^^ string (select Int32.to_string Int32.to_string Int32.to_string offset))
  | LocalSet (i, e) -> expression e ^^ line (string "local.set " ^^ integer i)
  | GlobalSet (nm, e) -> expression e ^^ line (string "global.set " ^^ symbol nm 0)
  | Loop (ty, l) ->
      line (string "loop" ^^ block_type ty)
      ^^ indent (concat_map instruction l)
      ^^ line (string "end_loop")
  | Block (ty, l) ->
      line (string "block" ^^ block_type ty)
      ^^ indent (concat_map instruction l)
      ^^ line (string "end_block")
  | If (ty, e, l1, l2) ->
      expression e
      ^^ line (string "if" ^^ block_type ty)
      ^^ indent (concat_map instruction l1)
      ^^ line (string "else")
      ^^ indent (concat_map instruction l2)
      ^^ line (string "end_if")
  | Br_table (e, l, i) ->
      expression e
      ^^ line
           (string "br_table {"
           ^^ separate_map (string ", ") integer (l @ [ i ])
           ^^ string "}")
  | Br (i, Some e) -> expression e ^^ instruction (Br (i, None))
  | Br (i, None) -> line (string "br " ^^ integer i)
  | Return (Some e) -> expression e ^^ instruction (Return None)
  | Return None -> line (string "return")
  | CallInstr (x, l) -> concat_map expression l ^^ line (string "call " ^^ symbol x 0)
  | Nop -> empty
  | Push e -> expression e

let escape_string s =
  let b = Buffer.create (String.length s + 2) in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if Poly.(c >= ' ' && c <= '~' && c <> '"' && c <> '\\')
    then Buffer.add_char b c
    else Printf.bprintf b "\\x%02x" (Char.code c)
  done;
  Buffer.contents b

let section_header kind name =
  line
    (string ".section ." ^^ string kind ^^ string "." ^^ string name ^^ string ",\"\",@")

let f fields =
  List.iter
    ~f:(fun f ->
      match f with
      | Import { name; _ } -> Var_printer.add_reserved name
      | Function _ | Data _ | Global _ -> ())
    fields;
  to_channel stdout
  @@
  let types =
    List.filter_map
      ~f:(fun f ->
        match f with
        | Function { name; typ; _ } -> Some (Code.Var.to_string name, typ)
        | Import { name; desc = Fun typ } -> Some (name, typ)
        | Data _ | Global _ -> None)
      fields
  in
  let globals =
    List.filter_map
      ~f:(fun f ->
        match f with
        | Function _ | Import _ | Data _ -> None
        | Global { name; typ; init } ->
            assert (Poly.equal init (Const (I32 0l)));
            Some (name, typ))
      fields
  in
  let define_symbol name =
    line (string ".hidden " ^^ string name) ^^ line (string ".globl " ^^ string name)
  in
  let declare_global name { mut; typ } =
    line
      (string ".globaltype "
      ^^ symbol name 0
      ^^ string ", "
      ^^ value_type typ
      ^^ if mut then empty else string ", immutable")
  in
  let declare_func_type name typ =
    line (string ".functype " ^^ string name ^^ string " " ^^ func_type typ)
  in
  let data_sections =
    concat_map
      (fun f ->
        match f with
        | Function _ | Import _ -> empty
        | Data { name; read_only; active; contents } ->
            assert active;
            (* Not supported *)
            let name = Code.Var.to_string name in
            let size =
              List.fold_left
                ~init:0
                ~f:(fun s d ->
                  s
                  +
                  match d with
                  | DataI8 _ -> 1
                  | DataI32 _ | DataSym _ -> 4
                  | DataI64 _ -> 8
                  | DataBytes b -> String.length b
                  | DataSpace n -> n)
                contents
            in
            indent
              (section_header (if read_only then "rodata" else "data") name
              ^^ define_symbol name
              ^^ line (string ".p2align 2")
              ^^ line (string ".size " ^^ string name ^^ string ", " ^^ integer size))
            ^^ line (string name ^^ string ":")
            ^^ indent
                 (concat_map
                    (fun d ->
                      line
                        (match d with
                        | DataI8 i -> string ".int8 " ^^ integer i
                        | DataI32 i -> string ".int32 " ^^ integer32 i
                        | DataI64 i -> string ".int64 " ^^ integer64 i
                        | DataBytes b ->
                            string ".ascii \"" ^^ string (escape_string b) ^^ string "\""
                        | DataSym (name, offset) -> string ".int32 " ^^ symbol name offset
                        | DataSpace n -> string ".space " ^^ integer n))
                    contents)
        | Global { name; _ } ->
            let name =
              match name with
              | V name -> Code.Var.to_string name
              | S name -> name
            in
            indent (section_header "data" name ^^ define_symbol name)
            ^^ line (string name ^^ string ":"))
      fields
  in
  let function_section =
    concat_map
      (fun f ->
        match f with
        | Function { name; exported_name; typ; locals; body } ->
            let name = Code.Var.to_string name in
            indent
              (section_header "text" name
              ^^ define_symbol name
              ^^
              match exported_name with
              | None -> empty
              | Some exported_name ->
                  line
                    (string ".export_name "
                    ^^ string name
                    ^^ string ","
                    ^^ string exported_name))
            ^^ line (string name ^^ string ":")
            ^^ indent
                 (declare_func_type name typ
                 ^^ (if List.is_empty locals
                     then empty
                     else
                       line
                         (string ".local " ^^ separate_map (string ", ") value_type locals))
                 ^^ concat_map instruction body
                 ^^ line (string "end_function"))
        | Import _ | Data _ | Global _ -> empty)
      fields
  in
  indent
    (concat_map (fun (name, typ) -> declare_global name typ) globals
    ^^ concat_map (fun (name, typ) -> declare_func_type name typ) types)
  ^^ function_section
  ^^ data_sections
