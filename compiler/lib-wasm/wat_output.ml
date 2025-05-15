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

open! Stdlib
open Wasm_ast

let assign_names ?(reversed = true) (f : Code.Var.t -> string option) names =
  let used = ref StringSet.empty in
  let counts = String.Hashtbl.create 101 in
  let rec find_available_name used name =
    let i =
      try String.Hashtbl.find counts name
      with Not_found ->
        let i = ref 0 in
        String.Hashtbl.replace counts name i;
        i
    in
    incr i;
    let nm = Printf.sprintf "%s$%d" name !i in
    if StringSet.mem nm used then find_available_name used name else nm
  in
  let names = if reversed then List.rev names else names in
  let names =
    List.map
      ~f:(fun x ->
        match f x with
        | None -> x, None
        | Some nm ->
            let nm =
              if StringSet.mem nm !used then find_available_name !used nm else nm
            in
            used := StringSet.add nm !used;
            x, Some nm)
      names
  in
  let printer = Var_printer.create Var_printer.Alphabet.javascript in
  let i = ref 0 in
  let rec first_available_name () =
    let nm = Var_printer.to_string printer !i in
    incr i;
    if StringSet.mem nm !used then first_available_name () else nm
  in
  let tbl = Code.Var.Hashtbl.create 16 in
  List.iter
    ~f:(fun (x, nm) ->
      Code.Var.Hashtbl.add
        tbl
        x
        (match nm with
        | Some nm -> nm
        | None -> first_available_name ()))
    names;
  tbl

type st =
  { type_names : string Code.Var.Hashtbl.t
  ; func_names : string Code.Var.Hashtbl.t
  ; global_names : string Code.Var.Hashtbl.t
  ; data_names : string Code.Var.Hashtbl.t
  ; tag_names : string Code.Var.Hashtbl.t
  ; local_names : string Code.Var.Hashtbl.t
  }

let build_name_tables fields =
  let type_names = ref [] in
  let func_names = ref [] in
  let data_names = ref [] in
  let global_names = ref [] in
  let tag_names = ref [] in
  let push l v = l := v :: !l in
  List.iter
    ~f:(fun field ->
      match field with
      | Function { name; _ } -> push func_names name
      | Type l -> List.iter ~f:(fun { name; _ } -> push type_names name) l
      | Data { name; _ } -> push data_names name
      | Global { name; _ } -> push global_names name
      | Tag { name; _ } -> push tag_names name
      | Import { name; desc; _ } -> (
          match desc with
          | Fun _ -> push func_names name
          | Global _ -> push global_names name
          | Tag _ -> push tag_names name))
    fields;
  let index = Code.Var.get_name in
  { type_names = assign_names index !type_names
  ; func_names = assign_names index !func_names
  ; global_names = assign_names index !global_names
  ; data_names = assign_names index !data_names
  ; tag_names = assign_names index !tag_names
  ; local_names = Code.Var.Hashtbl.create 1
  }

type sexp =
  | Atom of string
  | List of sexp list
  | Comment of string
      (** Line comment. String [s] is rendered as [;;s], on its own line,
                          without space after the double semicolon. *)

let rec format_sexp f s =
  match s with
  | Atom s -> Format.pp_print_string f s
  | List l ->
      let has_comment =
        List.exists l ~f:(function
          | Comment _ -> true
          | _ -> false)
      in
      if has_comment
      then (* Ensure comments are on their own line *)
        Format.pp_open_vbox f 2
      else Format.pp_open_box f 2;
      Format.pp_print_string f "(";
      Format.pp_print_list
        ~pp_sep:(fun f () -> Format.pp_print_space f ())
        format_sexp
        f
        l;
      if
        has_comment
        &&
        match List.last l with
        | Some (Comment _) -> true
        | Some _ | None -> false
      then
        (* Make sure there is a newline when a comment is at the very end. *)
        Format.pp_print_space f ();
      Format.pp_print_string f ")";
      Format.pp_close_box f ()
  | Comment s ->
      Format.pp_print_string f ";;";
      Format.pp_print_string f s

let index tbl x = Atom ("$" ^ Code.Var.Hashtbl.find tbl x)

let heap_type st (ty : heap_type) =
  match ty with
  | Func -> Atom "func"
  | Extern -> Atom "extern"
  | Any -> Atom "any"
  | Eq -> Atom "eq"
  | Struct -> Atom "struct"
  | Array -> Atom "array"
  | I31 -> Atom "i31"
  | None_ -> Atom "none"
  | Type t -> index st.type_names t

let ref_type st { nullable; typ } =
  let r = [ heap_type st typ ] in
  List (Atom "ref" :: (if nullable then Atom "null" :: r else r))

let value_type st (t : value_type) =
  match t with
  | I32 -> Atom "i32"
  | I64 -> Atom "i64"
  | F32 -> Atom "f32"
  | F64 -> Atom "f64"
  | Ref ty -> ref_type st ty

let packed_type t =
  match t with
  | I8 -> Atom "i8"
  | I16 -> Atom "i16"

let list ?(always = false) name f l =
  if (not always) && List.is_empty l then [] else [ List (Atom name :: f l) ]

let value_type_list st name tl =
  list name (fun tl -> List.map ~f:(fun t -> value_type st t) tl) tl

let func_type st ?param_names { params; result } =
  (match param_names with
  | None -> value_type_list st "param" params
  | Some names ->
      List.map2
        ~f:(fun i typ -> List [ Atom "param"; index st.local_names i; value_type st typ ])
        names
        params)
  @ value_type_list st "result" result

let storage_type st typ =
  match typ with
  | Value typ -> value_type st typ
  | Packed typ -> packed_type typ

let mut_type f { mut; typ } = if mut then List [ Atom "mut"; f typ ] else f typ

let field_type st typ = mut_type (fun t -> storage_type st t) typ

let global_type st typ = mut_type (fun t -> value_type st t) typ

let str_type st typ =
  match typ with
  | Func ty -> List (Atom "func" :: func_type st ty)
  | Struct l ->
      List
        (Atom "struct" :: List.map ~f:(fun f -> List [ Atom "field"; field_type st f ]) l)
  | Array ty -> List [ Atom "array"; field_type st ty ]

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

let signage op (s : Wasm_ast.signage) =
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

type ctx = { mutable function_refs : Code.Var.Set.t }

let reference_function ctx f = ctx.function_refs <- Code.Var.Set.add f ctx.function_refs

let remove_nops l =
  List.filter
    ~f:(function
      | Nop -> false
      | _ -> true)
    l

let float64 _ f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero -> Printf.sprintf "%h" f
  | FP_nan ->
      Printf.sprintf
        "nan:0x%Lx"
        Int64.(logand (bits_of_float f) (of_int ((1 lsl 52) - 1)))
  | FP_infinite -> if Float.(f > 0.) then "inf" else "-inf"

let float32 _ f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero -> Printf.sprintf "%h" f
  | FP_nan ->
      Printf.sprintf
        "nan:0x%lx"
        Int32.(logand (bits_of_float f) (of_int ((1 lsl 23) - 1)))
  | FP_infinite -> if Float.(f > 0.) then "inf" else "-inf"

let expression_or_instructions ctx st in_function =
  let rec expression e =
    match e with
    | Const op ->
        [ List
            [ Atom (type_prefix op "const")
            ; Atom
                (select
                   (fun _ i -> Int32.to_string i)
                   (fun _ i -> Int64.to_string i)
                   float32
                   float64
                   op)
            ]
        ]
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
    | F64PromoteF32 e -> [ List (Atom "f64.promote_f32" :: expression e) ]
    | LocalGet i -> [ List [ Atom "local.get"; index st.local_names i ] ]
    | LocalTee (i, e') ->
        [ List (Atom "local.tee" :: index st.local_names i :: expression e') ]
    | GlobalGet nm -> [ List [ Atom "global.get"; index st.global_names nm ] ]
    | BlockExpr (ty, l) -> [ List (Atom "block" :: (block_type st ty @ instructions l)) ]
    | Call (f, l) ->
        [ List
            (Atom "call"
            :: index st.func_names f
            :: List.concat (List.map ~f:expression l))
        ]
    | Seq (l, e) -> instructions l @ expression e
    | Pop _ -> []
    | RefFunc symb ->
        if in_function then reference_function ctx symb;
        [ List [ Atom "ref.func"; index st.func_names symb ] ]
    | Call_ref (f, e, l) ->
        [ List
            (Atom "call_ref"
            :: index st.type_names f
            :: List.concat (List.map ~f:expression (l @ [ e ])))
        ]
    | RefI31 e -> [ List (Atom "ref.i31" :: expression e) ]
    | I31Get (s, e) -> [ List (Atom (signage "i31.get" s) :: expression e) ]
    | ArrayNew (typ, e, e') ->
        [ List
            (Atom "array.new" :: index st.type_names typ :: (expression e @ expression e'))
        ]
    | ArrayNewFixed (typ, l) ->
        [ List
            (Atom "array.new_fixed"
            :: index st.type_names typ
            :: Atom (string_of_int (List.length l))
            :: List.concat (List.map ~f:expression l))
        ]
    | ArrayNewData (typ, data, e, e') ->
        [ List
            (Atom "array.new_data"
            :: index st.type_names typ
            :: index st.data_names data
            :: (expression e @ expression e'))
        ]
    | ArrayGet (None, typ, e, e') ->
        [ List
            (Atom "array.get" :: index st.type_names typ :: (expression e @ expression e'))
        ]
    | ArrayGet (Some s, typ, e, e') ->
        [ List
            (Atom (signage "array.get" s)
            :: index st.type_names typ
            :: (expression e @ expression e'))
        ]
    | ArrayLen e -> [ List (Atom "array.len" :: expression e) ]
    | StructNew (typ, l) ->
        [ List
            (Atom "struct.new"
            :: index st.type_names typ
            :: List.concat (List.map ~f:expression l))
        ]
    | StructGet (None, typ, i, e) ->
        [ List
            (Atom "struct.get"
            :: index st.type_names typ
            :: Atom (string_of_int i)
            :: expression e)
        ]
    | StructGet (Some s, typ, i, e) ->
        [ List
            (Atom (signage "struct.get" s)
            :: index st.type_names typ
            :: Atom (string_of_int i)
            :: expression e)
        ]
    | RefCast (ty, e) -> [ List (Atom "ref.cast" :: ref_type st ty :: expression e) ]
    | RefTest (ty, e) -> [ List (Atom "ref.test" :: ref_type st ty :: expression e) ]
    | RefEq (e, e') -> [ List (Atom "ref.eq" :: (expression e @ expression e')) ]
    | RefNull ty -> [ List [ Atom "ref.null"; heap_type st ty ] ]
    | Br_on_cast (i, ty, ty', e) ->
        [ List
            (Atom "br_on_cast"
            :: Atom (string_of_int i)
            :: ref_type st ty
            :: ref_type st ty'
            :: expression e)
        ]
    | Br_on_cast_fail (i, ty, ty', e) ->
        [ List
            (Atom "br_on_cast_fail"
            :: Atom (string_of_int i)
            :: ref_type st ty
            :: ref_type st ty'
            :: expression e)
        ]
    | Br_on_null (i, e) ->
        [ List (Atom "br_on_null" :: Atom (string_of_int i) :: expression e) ]
    | IfExpr (ty, cond, ift, iff) ->
        [ List
            ((Atom "if" :: block_type st { params = []; result = [ ty ] })
            @ expression cond
            @ [ List (Atom "then" :: expression ift) ]
            @ [ List (Atom "else" :: expression iff) ])
        ]
    | Try (ty, body, catches) ->
        [ List
            (Atom "try"
            :: (block_type st ty
               @ List (Atom "do" :: instructions body)
                 :: List.map
                      ~f:(fun (tag, i, ty) ->
                        List
                          (Atom "catch"
                          :: index st.tag_names tag
                          :: (instruction (Wasm_ast.Event Code_generation.hidden_location)
                             @ instruction (Wasm_ast.Br (i + 1, Some (Pop ty))))))
                      catches))
        ]
    | ExternConvertAny e' -> [ List (Atom "extern.convert_any" :: expression e') ]
  and instruction i =
    match i with
    | Drop e -> [ List (Atom "drop" :: expression e) ]
    | LocalSet (i, Seq (l, e)) -> instructions (l @ [ LocalSet (i, e) ])
    | LocalSet (i, e) ->
        [ List (Atom "local.set" :: index st.local_names i :: expression e) ]
    | GlobalSet (nm, e) ->
        [ List (Atom "global.set" :: index st.global_names nm :: expression e) ]
    | Loop (ty, l) -> [ List (Atom "loop" :: (block_type st ty @ instructions l)) ]
    | Block (ty, l) -> [ List (Atom "block" :: (block_type st ty @ instructions l)) ]
    | If (ty, e, l1, l2) ->
        [ List
            (Atom "if"
            :: (block_type st ty
               @ expression e
               @ list ~always:true "then" instructions (remove_nops l1)
               @ list "else" instructions (remove_nops l2)))
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
    | Br_if (i, e) -> [ List (Atom "br_if" :: Atom (string_of_int i) :: expression e) ]
    | Return e ->
        [ List
            (Atom "return"
            ::
            (match e with
            | None -> []
            | Some e -> expression e))
        ]
    | Throw (tag, e) -> [ List (Atom "throw" :: index st.tag_names tag :: expression e) ]
    | Rethrow i -> [ List [ Atom "rethrow"; Atom (string_of_int i) ] ]
    | CallInstr (f, l) ->
        [ List
            (Atom "call"
            :: index st.func_names f
            :: List.concat (List.map ~f:expression l))
        ]
    | Nop -> []
    | Push e -> expression e
    | ArraySet (typ, e, e', e'') ->
        [ List
            (Atom "array.set"
            :: index st.type_names typ
            :: (expression e @ expression e' @ expression e''))
        ]
    | StructSet (typ, i, e, e') ->
        [ List
            (Atom "struct.set"
            :: index st.type_names typ
            :: Atom (string_of_int i)
            :: (expression e @ expression e'))
        ]
    | Return_call (f, l) ->
        [ List
            (Atom "return_call"
            :: index st.func_names f
            :: List.concat (List.map ~f:expression l))
        ]
    | Return_call_ref (typ, e, l) ->
        [ List
            (Atom "return_call_ref"
            :: index st.type_names typ
            :: List.concat (List.map ~f:expression (l @ [ e ])))
        ]
    | Unreachable -> [ List [ Atom "unreachable" ] ]
    | Event Parse_info.{ src = None | Some ""; _ } -> [ Comment "@" ]
    | Event Parse_info.{ src = Some src; col; line; _ } ->
        let loc = Format.sprintf "%s:%d:%d" src line col in
        [ Comment ("@ " ^ loc) ]
  and instructions l = List.concat (List.map ~f:instruction l) in
  expression, instructions

let expression ctx st = fst (expression_or_instructions ctx st false)

let instructions ctx st = snd (expression_or_instructions ctx st true)

let funct ctx st name exported_name typ signature param_names locals body =
  let st =
    { st with
      local_names =
        assign_names
          ~reversed:false
          Code.Var.get_name
          (param_names @ List.map ~f:fst locals)
    }
  in
  List
    ((Atom "func" :: index st.func_names name :: export exported_name)
    @ (match typ with
      | None -> []
      | Some typ -> [ List [ Atom "type"; index st.type_names typ ] ])
    @ func_type st ~param_names signature
    @ List.map
        ~f:(fun (i, t) -> List [ Atom "local"; index st.local_names i; value_type st t ])
        locals
    @ instructions ctx st body)

let import st f =
  match f with
  | Function _ | Global _ | Data _ | Tag _ | Type _ -> []
  | Import { import_module; import_name; name; desc } ->
      [ List
          [ Atom "import"
          ; quoted_name import_module
          ; quoted_name import_name
          ; List
              (match desc with
              | Fun typ -> Atom "func" :: index st.func_names name :: func_type st typ
              | Global ty ->
                  [ Atom "global"; index st.global_names name; global_type st ty ]
              | Tag ty ->
                  [ Atom "tag"
                  ; index st.tag_names name
                  ; List [ Atom "param"; value_type st ty ]
                  ])
          ]
      ]

let escape_string s =
  let b = Buffer.create (String.length s + 2) in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if Char.(c >= ' ' && c <= '~' && c <> '"' && c <> '\\')
    then Buffer.add_char b c
    else Printf.bprintf b "\\%02x" (Char.code c)
  done;
  Buffer.contents b

let type_field st { name; typ; supertype; final } =
  if final && Option.is_none supertype
  then List [ Atom "type"; index st.type_names name; str_type st typ ]
  else
    List
      [ Atom "type"
      ; index st.type_names name
      ; List
          (Atom "sub"
          :: ((if final then [ Atom "final" ] else [])
             @ (match supertype with
               | Some supertype -> [ index st.type_names supertype ]
               | None -> [])
             @ [ str_type st typ ]))
      ]

let field ctx st f =
  match f with
  | Function { name; exported_name; typ; signature; param_names; locals; body } ->
      [ funct ctx st name exported_name typ signature param_names locals body ]
  | Global { name; exported_name; typ; init } ->
      [ List
          (Atom "global"
          :: index st.global_names name
          :: (export exported_name @ (global_type st typ :: expression ctx st init)))
      ]
  | Tag { name; typ } ->
      [ List
          [ Atom "tag"
          ; index st.tag_names name
          ; List [ Atom "param"; value_type st typ ]
          ]
      ]
  | Import _ -> []
  | Data { name; contents } ->
      [ List
          [ Atom "data"
          ; index st.data_names name
          ; Atom ("\"" ^ escape_string contents ^ "\"")
          ]
      ]
  | Type [ t ] -> [ type_field st t ]
  | Type l -> [ List (Atom "rec" :: List.map ~f:(type_field st) l) ]

let f ch fields =
  let st = build_name_tables fields in
  let ctx = { function_refs = Code.Var.Set.empty } in
  let other_fields = List.concat (List.map ~f:(fun f -> field ctx st f) fields) in
  let funct_decl =
    let functions = Code.Var.Set.elements ctx.function_refs in
    if List.is_empty functions
    then []
    else
      [ List
          (Atom "elem"
          :: Atom "declare"
          :: Atom "func"
          :: List.map ~f:(index st.func_names) functions)
      ]
  in
  Format.fprintf
    (Format.formatter_of_out_channel ch)
    "%a@."
    format_sexp
    (List
       (Atom "module"
       :: (List.concat (List.map ~f:(fun i -> import st i) fields)
          @ funct_decl
          @ other_fields)))
