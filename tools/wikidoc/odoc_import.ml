let map_option f o = match o with
  | None -> None
  | Some v -> Some (f v)

let default_margin = 75

let new_fmt () =
  let open Format in
  let buf = Buffer.create 512 in
  let fmt = formatter_of_buffer buf in
  let flush () =
    pp_print_flush fmt ();
    let s = Buffer.contents buf in
    Buffer.reset buf ;
    s
  in
  (fmt, flush, pp_set_margin fmt)

let (type_fmt, flush_type_fmt, type_set_margin) = new_fmt ()
let (modtype_fmt, flush_modtype_fmt, modtype_set_margin) = new_fmt ()

let string_of_type_expr ?(margin = default_margin) t =
  type_set_margin margin;
  Printtyp.mark_loops t;
  Printtyp.type_scheme_max ~b_reset_names: false type_fmt t;
  flush_type_fmt ()

let raw_string_of_type_list ?(margin = default_margin) sep type_list =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  Format.pp_set_margin fmt margin;
  let rec need_parent t =
    match t.Types.desc with
      Types.Tarrow _ | Types.Ttuple _ -> true
    | Types.Tlink t2 | Types.Tsubst t2 -> need_parent t2
    | Types.Tconstr _ ->
        false
    (* | Types.Tvar | Types.Tunivar | Types.Tobject _ | Types.Tpoly _ *)
    (* | Types.Tfield _ | Types.Tnil | Types.Tvariant _ | Types.Tpackage _ *)
    (* Text for ocamlduce *)
    | _ -> false
  in
  let print_one_type variance t =
    Printtyp.mark_loops t;
    if need_parent t then
      (
       Format.fprintf fmt "(%s" variance;
       Printtyp.type_scheme_max ~b_reset_names: false fmt t;
       Format.fprintf fmt ")"
      )
    else
      (
       Format.fprintf fmt "%s" variance;
       Printtyp.type_scheme_max ~b_reset_names: false fmt t
      )
  in
  begin match type_list with
    [] -> ()
  | [(variance, ty)] -> print_one_type variance ty
  | (variance, ty) :: tyl ->
      Format.fprintf fmt "@[<hov 2>";
      print_one_type variance ty;
      List.iter
        (fun (variance, t) ->
          Format.fprintf fmt "@,%s" sep;
          print_one_type variance t
        )
        tyl;
      Format.fprintf fmt "@]"
  end;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let string_of_type_list ?par ?margin sep type_list =
  let par =
    match par with
    | Some b -> b
    | None ->
        match type_list with
          [] | [_] -> false
        | _ -> true
  in
  Printf.sprintf "%s%s%s"
    (if par then "(" else "")
    (raw_string_of_type_list sep ?margin (List.map (fun t -> ("", t)) type_list))
    (if par then ")" else "")

let string_of_type_param_list ?margin t =
  let par =
    match t.Odoc_type.ty_parameters with
      [] | [_] -> false
    | _ -> true
  in
  Printf.sprintf "%s%s%s"
    (if par then "(" else "")
    (raw_string_of_type_list ?margin ", "
       (List.map
          (fun (typ, co, cn) -> (Odoc_str.string_of_variance t (co, cn), typ))
          t.Odoc_type.ty_parameters
       )
    )
    (if par then ")" else "")

let string_of_class_type_param_list ?margin l =
  let par =
    match l with
      [] | [_] -> false
    | _ -> true
  in
  Printf.sprintf "%s%s%s"
    (if par then "[" else "")
    (raw_string_of_type_list ?margin ", "
       (List.map
          (fun typ -> ("", typ))
          l
       )
    )
    (if par then "]" else "")

let rec is_arrow_type t =
  match t.Types.desc with
    Types.Tarrow _ -> true
  | Types.Tlink t2 | Types.Tsubst t2 -> is_arrow_type t2
  (* | Types.Ttuple _ *)
  (* | Types.Tconstr _ *)
  (* | Types.Tvar | Types.Tunivar | Types.Tobject _ | Types.Tpoly _ *)
  (* | Types.Tfield _ | Types.Tnil | Types.Tvariant _ | Types.Tpackage _ *)
  (* Text for ocamlduce *)
  | _  -> false

let string_of_class_params ?(margin = default_margin) c =
  let b = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer b in
  Format.pp_set_margin fmt margin;
  let rec iter = function
      Types.Cty_arrow (label, t, ctype) ->
        let parent = is_arrow_type t in
        let ty =
          if Odoc_misc.is_optional label then
            Odoc_misc.remove_option t
          else
            t in
        Printtyp.mark_loops t;
        Format.fprintf fmt "@[<hov 2>%s%s%a%s@] ->@ "
          (
           match label with
            | Asttypes.Nolabel -> ""
            | Asttypes.Labelled s -> s^":"
            | Asttypes.Optional s -> "?"^s^":"
          )
          (if parent then "(" else "") (* TODO open_box ?*)
          (Printtyp.type_scheme_max ~b_reset_names:false) ty
          (if parent then ")" else "");
        iter ctype
    | Types.Cty_signature _
    | Types.Cty_constr _ -> ()
  in
  iter c.Odoc_class.cl_type;
  Format.pp_print_flush fmt ();
  Buffer.contents b


exception Use_code of string

let simpl_module_type ?code t =
  let rec iter t =
    match t with
      Types.Mty_ident p -> t
    | Types.Mty_signature _ ->
        (
         match code with
           None -> Types.Mty_signature []
         | Some s -> raise (Use_code s)
        )
    | Types.Mty_functor (id, mt1, mt2) ->
        Types.Mty_functor (id, map_option iter mt1, iter mt2)
    | Types.Mty_alias _ -> t
  in

  iter t

let string_of_module_type ?code ?(complete=false) ?(margin = default_margin) t =
  try
    modtype_set_margin margin;
    let t2 = if complete then t else simpl_module_type ?code t in
    Printtyp.modtype modtype_fmt t2;
    flush_modtype_fmt ()
  with
    Use_code s -> s
