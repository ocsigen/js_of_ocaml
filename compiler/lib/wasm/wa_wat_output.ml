open! Stdlib
open Wa_ast

let target = `Binaryen (*`Reference*)

type sexp =
  | Atom of string
  | List of sexp list

let rec format_sexp f s =
  match s with
  | Atom s -> Format.fprintf f "%s" s
  | List l ->
      Format.fprintf f "@[<2>(";
      Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f "@ ") format_sexp f l;
      Format.fprintf f ")@]"

let index x = Atom ("$" ^ Code.Var.to_string x)

let symbol name =
  match name with
  | V name -> index name
  | S name -> Atom ("$" ^ name)

let heap_type (ty : heap_type) =
  match ty with
  | Func -> Atom "func"
  | Extern -> Atom "extern"
  | Eq -> Atom "eq"
  | I31 -> Atom "i31"
  | Type t -> index t

let ref_type { nullable; typ } =
  let r = [ heap_type typ ] in
  List (Atom "ref" :: (if nullable then Atom "null" :: r else r))

let value_type (t : value_type) =
  match t with
  | I32 -> Atom "i32"
  | I64 -> Atom "i64"
  | F32 -> Atom "f32"
  | F64 -> Atom "f64"
  | Ref ty -> ref_type ty

let packed_type t =
  match t with
  | I8 -> Atom "i8"
  | I16 -> Atom "i16"

let list ?(always = false) name f l =
  if (not always) && List.is_empty l then [] else [ List (Atom name :: f l) ]

let value_type_list name tl = list name (fun tl -> List.map ~f:value_type tl) tl

let func_type { params; result } =
  value_type_list "param" params @ value_type_list "result" result

let storage_type typ =
  match typ with
  | Value typ -> value_type typ
  | Packed typ -> packed_type typ

let mut_type f { mut; typ } = if mut then List [ Atom "mut"; f typ ] else f typ

let field_type typ = mut_type storage_type typ

let global_type typ = mut_type value_type typ

let str_type typ =
  match typ with
  | Func ty -> List (Atom "func" :: func_type ty)
  | Struct l ->
      List (Atom "struct" :: List.map ~f:(fun f -> List [ Atom "field"; field_type f ]) l)
  | Array ty -> List [ Atom "array"; field_type ty ]

let block_type = func_type

let quoted_name name = Atom ("\"" ^ name ^ "\"")

let export name =
  match name with
  | None -> []
  | Some name -> [ List [ Atom "export"; quoted_name name ] ]

let type_prefix op nm =
  (match op with
  | I32 _ -> "i32."
  | I64 _ -> "i64."
  | F32 _ -> "f32."
  | F64 _ -> "f64.")
  ^ nm

let signage op (s : Wa_ast.signage) =
  op
  ^
  match s with
  | S -> "_s"
  | U -> "_u"

let int_un_op sz op =
  match op with
  | Clz -> "clz"
  | Ctz -> "ctz"
  | Popcnt -> "popcnt"
  | Eqz -> "eqz"
  | TruncSatF64 s -> signage "trunc_sat_f64" s
  | ReinterpretF -> "reinterpret_f" ^ sz

let int_bin_op _ (op : int_bin_op) =
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

let float_un_op sz op =
  match op with
  | Neg -> "neg"
  | Abs -> "abs"
  | Ceil -> "ceil"
  | Floor -> "floor"
  | Trunc -> "trunc"
  | Nearest -> "nearest"
  | Sqrt -> "sqrt"
  | Convert (`I32, s) -> signage "convert_i32" s
  | Convert (`I64, s) -> signage "convert_i64" s
  | ReinterpretI -> "reinterpret_i" ^ sz

let float_bin_op _ op =
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

let select i32 i64 f32 f64 op =
  match op with
  | I32 x -> i32 "32" x
  | I64 x -> i64 "64" x
  | F32 x -> f32 "32" x
  | F64 x -> f64 "64" x

type ctx =
  { addresses : int Code.Var.Map.t
  ; mutable functions : int Code.Var.Map.t
  ; mutable function_refs : Code.Var.Set.t
  ; mutable function_count : int
  }

let reference_function ctx f = ctx.function_refs <- Code.Var.Set.add f ctx.function_refs

let lookup_symbol ctx (x : symbol) =
  match x with
  | S _ -> assert false
  | V x -> (
      try Code.Var.Map.find x ctx.addresses
      with Not_found -> (
        try Code.Var.Map.find x ctx.functions
        with Not_found ->
          let i = ctx.function_count in
          ctx.functions <- Code.Var.Map.add x i ctx.functions;
          ctx.function_count <- ctx.function_count + 1;
          i))

let remove_nops l = List.filter ~f:(fun i -> not (Poly.equal i Nop)) l

let float64 _ f = Printf.sprintf "%h" f (*ZZZ*)

let float32 _ f = Printf.sprintf "%h" f (*ZZZ*)

let expression_or_instructions ctx in_function =
  let rec expression e =
    match e with
    | Const op ->
        [ List
            [ Atom (type_prefix op "const")
            ; Atom
                (select
                   (fun _ i -> Int32.to_string i)
                   (fun _ i -> Int64.to_string i)
                   float64
                   float32
                   op)
            ]
        ]
    | ConstSym (symb, ofs) ->
        let i = lookup_symbol ctx symb in
        [ List [ Atom "i32.const"; Atom (string_of_int (i + ofs)) ] ]
    | UnOp (op, e') ->
        [ List
            (Atom (type_prefix op (select int_un_op int_un_op float_un_op float_un_op op))
            :: expression e')
        ]
    | BinOp (op, e1, e2) ->
        [ List
            (Atom
               (type_prefix
                  op
                  (select int_bin_op int_bin_op float_bin_op float_bin_op op))
            :: (expression e1 @ expression e2))
        ]
    | I32WrapI64 e -> [ List (Atom "i32.wrap_i64" :: expression e) ]
    | I64ExtendI32 (s, e) -> [ List (Atom (signage "i64.extend_i32" s) :: expression e) ]
    | F32DemoteF64 e -> [ List (Atom "f32.demote_f64" :: expression e) ]
    | F64PromoteF32 e -> [ List (Atom "f64.promote_f64" :: expression e) ]
    | Load (offset, e') ->
        let offs _ i =
          if Int32.equal i 0l then [] else [ Atom (Printf.sprintf "offset=%ld" i) ]
        in
        [ List
            ((Atom (type_prefix offset "load") :: select offs offs offs offs offset)
            @ expression e')
        ]
    | Load8 (s, offset, e') ->
        let offs _ i =
          if Int32.equal i 0l then [] else [ Atom (Printf.sprintf "offset=%ld" i) ]
        in
        [ List
            (Atom (type_prefix offset (signage "load" s))
             :: select offs offs offs offs offset
            @ expression e')
        ]
    | LocalGet i -> [ List [ Atom "local.get"; Atom (string_of_int i) ] ]
    | LocalTee (i, e') ->
        [ List (Atom "local.tee" :: Atom (string_of_int i) :: expression e') ]
    | GlobalGet nm -> [ List [ Atom "global.get"; symbol nm ] ]
    | BlockExpr (ty, l) -> [ List (Atom "block" :: (block_type ty @ instructions l)) ]
    | Call_indirect (typ, e, l) ->
        [ List
            ((Atom "call_indirect" :: func_type typ)
            @ List.concat (List.map ~f:expression (l @ [ e ])))
        ]
    | Call (f, l) ->
        [ List (Atom "call" :: index f :: List.concat (List.map ~f:expression l)) ]
    | MemoryGrow (_, e) -> [ List (Atom "memory.grow" :: expression e) ]
    | Seq (l, e) -> instructions l @ expression e
    | Pop ty -> (
        match target with
        | `Binaryen -> [ List [ Atom "pop"; value_type ty ] ]
        | `Reference -> [])
    | RefFunc symb ->
        if in_function then reference_function ctx symb;
        [ List [ Atom "ref.func"; index symb ] ]
    | Call_ref (f, e, l) ->
        [ List
            (Atom "call_ref"
            :: index f
            :: List.concat (List.map ~f:expression (l @ [ e ])))
        ]
    | I31New e -> [ List (Atom "i31.new" :: expression e) ]
    | I31Get (s, e) -> [ List (Atom (signage "i31.get" s) :: expression e) ]
    | ArrayNew (typ, e, e') ->
        [ List (Atom "array.new" :: index typ :: (expression e @ expression e')) ]
    | ArrayNewFixed (typ, l) ->
        [ List
            (Atom "array.new_fixed"
            :: index typ
            :: Atom (string_of_int (List.length l))
            :: List.concat (List.map ~f:expression l))
        ]
    | ArrayNewData (typ, data, e, e') ->
        [ List
            (Atom "array.new_data"
            :: index typ
            :: index data
            :: (expression e @ expression e'))
        ]
    | ArrayGet (None, typ, e, e') ->
        [ List (Atom "array.get" :: index typ :: (expression e @ expression e')) ]
    | ArrayGet (Some s, typ, e, e') ->
        [ List
            (Atom (signage "array.get" s) :: index typ :: (expression e @ expression e'))
        ]
    | ArrayLen e -> [ List (Atom "array.len" :: expression e) ]
    | StructNew (typ, l) ->
        [ List (Atom "struct.new" :: index typ :: List.concat (List.map ~f:expression l))
        ]
    | StructGet (None, typ, i, e) ->
        [ List (Atom "struct.get" :: index typ :: Atom (string_of_int i) :: expression e)
        ]
    | StructGet (Some s, typ, i, e) ->
        [ List
            (Atom (signage "struct.get" s)
            :: index typ
            :: Atom (string_of_int i)
            :: expression e)
        ]
    | RefCast (ty, e) -> [ List (Atom "ref.cast" :: ref_type ty :: expression e) ]
    | RefTest (ty, e) -> [ List (Atom "ref.test" :: ref_type ty :: expression e) ]
    | RefEq (e, e') -> [ List (Atom "ref.eq" :: (expression e @ expression e')) ]
    | RefNull ty -> [ List [ Atom "ref.null"; heap_type ty ] ]
    | Br_on_cast (i, ty, ty', e) ->
        [ List
            (Atom "br_on_cast"
            :: Atom (string_of_int i)
            :: ref_type ty
            :: ref_type ty'
            :: expression e)
        ]
    | Br_on_cast_fail (i, ty, ty', e) ->
        [ List
            (Atom "br_on_cast_fail"
            :: Atom (string_of_int i)
            :: ref_type ty
            :: ref_type ty'
            :: expression e)
        ]
    | ExternInternalize e -> [ List (Atom "extern.internalize" :: expression e) ]
    | ExternExternalize e -> [ List (Atom "extern.externalize" :: expression e) ]
  and instruction i =
    match i with
    | Drop e -> [ List (Atom "drop" :: expression e) ]
    | Store (offset, e1, e2) ->
        let offs _ i =
          if Int32.equal i 0l then [] else [ Atom (Printf.sprintf "offset=%ld" i) ]
        in
        [ List
            (Atom (type_prefix offset "store")
            :: (select offs offs offs offs offset @ expression e1 @ expression e2))
        ]
    | Store8 (offset, e1, e2) ->
        let offs _ i =
          if Int32.equal i 0l then [] else [ Atom (Printf.sprintf "offset=%ld" i) ]
        in
        [ List
            (Atom (type_prefix offset "store8")
            :: (select offs offs offs offs offset @ expression e1 @ expression e2))
        ]
    | LocalSet (i, Seq (l, e)) -> instructions (l @ [ LocalSet (i, e) ])
    | LocalSet (i, e) ->
        [ List (Atom "local.set" :: Atom (string_of_int i) :: expression e) ]
    | GlobalSet (nm, e) -> [ List (Atom "global.set" :: symbol nm :: expression e) ]
    | Loop (ty, l) -> [ List (Atom "loop" :: (block_type ty @ instructions l)) ]
    | Block (ty, l) -> [ List (Atom "block" :: (block_type ty @ instructions l)) ]
    | If (ty, e, l1, l2) ->
        [ List
            (Atom "if"
            :: (block_type ty
               @ expression e
               @ list ~always:true "then" instructions (remove_nops l1)
               @ list "else" instructions (remove_nops l2)))
        ]
    | Try (ty, body, catches, catch_all) ->
        [ List
            (Atom "try"
            :: (block_type ty
               @ List (Atom "do" :: instructions body)
                 :: (List.map
                       ~f:(fun (tag, l) ->
                         List (Atom "catch" :: index tag :: instructions l))
                       catches
                    @
                    match catch_all with
                    | None -> []
                    | Some l -> [ List (Atom "catch_all" :: instructions l) ])))
        ]
    | Br_table (e, l, i) ->
        [ List
            (Atom "br_table"
            :: (List.map ~f:(fun i -> Atom (string_of_int i)) (l @ [ i ]) @ expression e)
            )
        ]
    | Br (i, e) ->
        [ List
            (Atom "br"
            :: Atom (string_of_int i)
            ::
            (match e with
            | None -> []
            | Some e -> expression e))
        ]
    | Return e ->
        [ List
            (Atom "return"
            ::
            (match e with
            | None -> []
            | Some e -> expression e))
        ]
    | Throw (tag, e) -> [ List (Atom "throw" :: index tag :: expression e) ]
    | Rethrow i -> [ List [ Atom "rethrow"; Atom (string_of_int i) ] ]
    | CallInstr (f, l) ->
        [ List (Atom "call" :: index f :: List.concat (List.map ~f:expression l)) ]
    | Nop -> []
    | Push e -> expression e
    | ArraySet (typ, e, e', e'') ->
        [ List
            (Atom "array.set"
            :: index typ
            :: (expression e @ expression e' @ expression e''))
        ]
    | StructSet (typ, i, e, e') ->
        [ List
            (Atom "struct.set"
            :: index typ
            :: Atom (string_of_int i)
            :: (expression e @ expression e'))
        ]
    | Return_call_indirect (typ, e, l) ->
        [ List
            ((Atom "return_call_indirect" :: func_type typ)
            @ List.concat (List.map ~f:expression (l @ [ e ])))
        ]
    | Return_call (f, l) ->
        [ List (Atom "return_call" :: index f :: List.concat (List.map ~f:expression l)) ]
    | Return_call_ref (typ, e, l) ->
        [ List
            (Atom "return_call_ref"
            :: index typ
            :: List.concat (List.map ~f:expression (l @ [ e ])))
        ]
  and instructions l = List.concat (List.map ~f:instruction l) in
  expression, instructions

let expression ctx = fst (expression_or_instructions ctx false)

let instructions ctx = snd (expression_or_instructions ctx true)

let funct ctx name exported_name typ locals body =
  List
    ((Atom "func" :: index name :: export exported_name)
    @ func_type typ
    @ value_type_list "local" locals
    @ instructions ctx body)

let import f =
  match f with
  | Function _ | Global _ | Data _ | Tag _ | Type _ -> []
  | Import { import_module; import_name; name; desc } ->
      [ List
          [ Atom "import"
          ; quoted_name import_module
          ; quoted_name import_name
          ; List
              (match desc with
              | Fun typ -> Atom "func" :: index name :: func_type typ
              | Global ty -> [ Atom "global"; index name; global_type ty ]
              | Tag ty -> [ Atom "tag"; index name; List [ Atom "param"; value_type ty ] ])
          ]
      ]

let escape_string s =
  let b = Buffer.create (String.length s + 2) in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if Poly.(c >= ' ' && c <= '~' && c <> '"' && c <> '\\')
    then Buffer.add_char b c
    else Printf.bprintf b "\\%02x" (Char.code c)
  done;
  Buffer.contents b

let data_contents ctx contents =
  let b = Buffer.create 16 in
  List.iter
    ~f:(fun d ->
      match d with
      | DataI8 c -> Buffer.add_uint8 b c
      | DataI32 i -> Buffer.add_int32_le b i
      | DataI64 i -> Buffer.add_int64_le b i
      | DataBytes s -> Buffer.add_string b s
      | DataSym (symb, ofs) ->
          Buffer.add_int32_le b (Int32.of_int (lookup_symbol ctx symb + ofs))
      | DataSpace n -> Buffer.add_string b (String.make n '\000'))
    contents;
  escape_string (Buffer.contents b)

let type_field { name; typ; supertype; final } =
  if final && Option.is_none supertype
  then List [ Atom "type"; index name; str_type typ ]
  else
    List
      [ Atom "type"
      ; index name
      ; List
          (Atom "sub"
          :: ((if final then [ Atom "final" ] else [])
             @ (match supertype with
               | Some supertype -> [ index supertype ]
               | None -> [])
             @ [ str_type typ ]))
      ]

let field ctx f =
  match f with
  | Function { name; exported_name; typ; locals; body } ->
      [ funct ctx name exported_name typ locals body ]
  | Global { name; typ; init } ->
      [ List (Atom "global" :: symbol name :: global_type typ :: expression ctx init) ]
  | Tag { name; typ } ->
      [ List [ Atom "tag"; index name; List [ Atom "param"; value_type typ ] ] ]
  | Import _ -> []
  | Data { name; active; contents; _ } ->
      [ List
          (Atom "data"
          :: index name
          :: ((if active
               then
                 expression ctx (Const (I32 (Int32.of_int (lookup_symbol ctx (V name)))))
               else [])
             @ [ Atom ("\"" ^ data_contents ctx contents ^ "\"") ]))
      ]
  | Type [ t ] -> [ type_field t ]
  | Type l -> [ List (Atom "rec" :: List.map ~f:type_field l) ]

let data_size contents =
  List.fold_left
    ~f:(fun sz d ->
      sz
      +
      match d with
      | DataI8 _ -> 1
      | DataI32 _ -> 4
      | DataI64 _ -> 8
      | DataBytes s -> String.length s
      | DataSym _ -> 4
      | DataSpace n -> n)
    ~init:0
    contents

let data_offsets fields =
  List.fold_left
    ~f:(fun (i, addresses) f ->
      match f with
      | Data { name; contents; active = true; _ } ->
          i + data_size contents, Code.Var.Map.add name i addresses
      | Function _ | Global _ | Tag _ | Import _ | Data { active = false; _ } | Type _ ->
          i, addresses)
    ~init:(0, Code.Var.Map.empty)
    fields

let f ch fields =
  let heap_base, addresses = data_offsets fields in
  let ctx =
    { addresses
    ; functions = Code.Var.Map.empty
    ; function_refs = Code.Var.Set.empty
    ; function_count = 0
    }
  in
  let other_fields = List.concat (List.map ~f:(fun f -> field ctx f) fields) in
  let funct_table =
    let functions =
      List.map
        ~f:fst
        (List.sort
           ~cmp:(fun (_, i) (_, j) -> compare i j)
           (Code.Var.Map.bindings ctx.functions))
    in
    if List.is_empty functions
    then []
    else
      [ List
          [ Atom "table"
          ; Atom "funcref"
          ; List (Atom "elem" :: List.map ~f:index functions)
          ]
      ]
  in
  let funct_decl =
    let functions =
      Code.Var.Set.elements
        (Code.Var.Set.filter
           (fun f -> not (Code.Var.Map.mem f ctx.functions))
           ctx.function_refs)
    in
    if List.is_empty functions
    then []
    else
      [ List (Atom "elem" :: Atom "declare" :: Atom "func" :: List.map ~f:index functions)
      ]
  in
  Format.fprintf
    (Format.formatter_of_out_channel ch)
    "%a@."
    format_sexp
    (List
       (Atom "module"
       :: (List.concat (List.map ~f:import fields)
          @ [ List
                [ Atom "memory"; Atom (string_of_int ((heap_base + 0xffff) / 0x10000)) ]
            ]
          @ funct_table
          @ funct_decl
          @ other_fields)))
