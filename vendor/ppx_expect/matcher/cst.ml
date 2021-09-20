open! Base
open Import

let for_all_string s ~f =
  let b = ref true in
  for i = 0 to String.length s - 1 do
    b := !b && f s.[i]
  done;
  !b
;;

let is_blank = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let is_space = function
  | ' ' | '\t' | '\n' -> true
  | _ -> false
;;

let is_blanks s = for_all_string s ~f:is_blank
let is_spaces s = for_all_string s ~f:is_space
let no_nl s = for_all_string s ~f:(fun c -> Char.( <> ) c '\n')
let has_nl s = not (no_nl s)

module Line = struct
  type 'a not_blank =
    { trailing_blanks : string
    ; orig : string
    ; data : 'a
    }
  [@@deriving_inline sexp_of, compare, equal]

  let _ = fun (_ : 'a not_blank) -> ()

  let sexp_of_not_blank :
    'a. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a not_blank -> Ppx_sexp_conv_lib.Sexp.t
    =
    fun _of_a -> function
      | { trailing_blanks = v_trailing_blanks; orig = v_orig; data = v_data } ->
        let bnds = [] in
        let bnds =
          let arg = _of_a v_data in
          Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "data"; arg ] :: bnds
        in
        let bnds =
          let arg = sexp_of_string v_orig in
          Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "orig"; arg ] :: bnds
        in
        let bnds =
          let arg = sexp_of_string v_trailing_blanks in
          Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "trailing_blanks"; arg ]
          :: bnds
        in
        Ppx_sexp_conv_lib.Sexp.List bnds
  ;;

  let _ = sexp_of_not_blank

  let compare_not_blank : 'a. ('a -> 'a -> int) -> 'a not_blank -> 'a not_blank -> int =
    fun _cmp__a a__001_ b__002_ ->
    if Ppx_compare_lib.phys_equal a__001_ b__002_
    then 0
    else (
      match compare_string a__001_.trailing_blanks b__002_.trailing_blanks with
      | 0 ->
        (match compare_string a__001_.orig b__002_.orig with
         | 0 -> _cmp__a a__001_.data b__002_.data
         | n -> n)
      | n -> n)
  ;;

  let _ = compare_not_blank

  let equal_not_blank : 'a. ('a -> 'a -> bool) -> 'a not_blank -> 'a not_blank -> bool =
    fun _cmp__a a__003_ b__004_ ->
    if Ppx_compare_lib.phys_equal a__003_ b__004_
    then true
    else
      Ppx_compare_lib.( && )
        (equal_string a__003_.trailing_blanks b__004_.trailing_blanks)
        (Ppx_compare_lib.( && )
           (equal_string a__003_.orig b__004_.orig)
           (_cmp__a a__003_.data b__004_.data))
  ;;

  let _ = equal_not_blank

  [@@@end]

  type 'a t =
    | Blank of string
    | Not_blank of 'a not_blank
  [@@deriving_inline sexp_of, compare, equal]

  let _ = fun (_ : 'a t) -> ()

  let sexp_of_t
    : type a. (a -> Ppx_sexp_conv_lib.Sexp.t) -> a t -> Ppx_sexp_conv_lib.Sexp.t
    =
    fun _of_a -> function
      | Blank v0 ->
        let v0 = sexp_of_string v0 in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Blank"; v0 ]
      | Not_blank v0 ->
        let v0 = sexp_of_not_blank _of_a v0 in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Not_blank"; v0 ]
  ;;

  let _ = sexp_of_t

  let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
    fun _cmp__a a__005_ b__006_ ->
    if Ppx_compare_lib.phys_equal a__005_ b__006_
    then 0
    else (
      match a__005_, b__006_ with
      | Blank _a__007_, Blank _b__008_ -> compare_string _a__007_ _b__008_
      | Blank _, _ -> -1
      | _, Blank _ -> 1
      | Not_blank _a__009_, Not_blank _b__010_ ->
        compare_not_blank _cmp__a _a__009_ _b__010_)
  ;;

  let _ = compare

  let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
    fun _cmp__a a__013_ b__014_ ->
    if Ppx_compare_lib.phys_equal a__013_ b__014_
    then true
    else (
      match a__013_, b__014_ with
      | Blank _a__015_, Blank _b__016_ -> equal_string _a__015_ _b__016_
      | Blank _, _ -> false
      | _, Blank _ -> false
      | Not_blank _a__017_, Not_blank _b__018_ ->
        equal_not_blank _cmp__a _a__017_ _b__018_)
  ;;

  let _ = equal

  [@@@end]

  let map t ~f =
    match t with
    | Blank b -> Blank b
    | Not_blank n -> Not_blank { n with data = f n.orig n.data }
  ;;

  let strip = function
    | Blank _ -> Blank ""
    | Not_blank n -> Not_blank { n with trailing_blanks = "" }
  ;;

  let invariant inv = function
    | Blank s -> assert (is_blanks s)
    | Not_blank n ->
      assert (is_blanks n.trailing_blanks);
      inv n.data;
      assert (no_nl n.orig);
      let len = String.length n.orig in
      assert (len > 0 && not (is_blank n.orig.[len - 1]))
  ;;

  let data t ~blank =
    match t with
    | Blank _ -> blank
    | Not_blank n -> n.data
  ;;

  let orig = function
    | Blank _ -> ""
    | Not_blank n -> n.orig
  ;;
end

type 'a single_line =
  { leading_blanks : string
  ; trailing_spaces : string
  ; orig : string
  ; data : 'a
  }
[@@deriving_inline sexp_of, compare, equal]

let _ = fun (_ : 'a single_line) -> ()

let sexp_of_single_line :
  'a. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a single_line -> Ppx_sexp_conv_lib.Sexp.t
  =
  fun _of_a -> function
    | { leading_blanks = v_leading_blanks
      ; trailing_spaces = v_trailing_spaces
      ; orig = v_orig
      ; data = v_data
      } ->
      let bnds = [] in
      let bnds =
        let arg = _of_a v_data in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "data"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_string v_orig in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "orig"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_string v_trailing_spaces in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "trailing_spaces"; arg ]
        :: bnds
      in
      let bnds =
        let arg = sexp_of_string v_leading_blanks in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "leading_blanks"; arg ]
        :: bnds
      in
      Ppx_sexp_conv_lib.Sexp.List bnds
;;

let _ = sexp_of_single_line

let compare_single_line :
  'a. ('a -> 'a -> int) -> 'a single_line -> 'a single_line -> int
  =
  fun _cmp__a a__021_ b__022_ ->
  if Ppx_compare_lib.phys_equal a__021_ b__022_
  then 0
  else (
    match compare_string a__021_.leading_blanks b__022_.leading_blanks with
    | 0 ->
      (match compare_string a__021_.trailing_spaces b__022_.trailing_spaces with
       | 0 ->
         (match compare_string a__021_.orig b__022_.orig with
          | 0 -> _cmp__a a__021_.data b__022_.data
          | n -> n)
       | n -> n)
    | n -> n)
;;

let _ = compare_single_line

let equal_single_line :
  'a. ('a -> 'a -> bool) -> 'a single_line -> 'a single_line -> bool
  =
  fun _cmp__a a__023_ b__024_ ->
  if Ppx_compare_lib.phys_equal a__023_ b__024_
  then true
  else
    Ppx_compare_lib.( && )
      (equal_string a__023_.leading_blanks b__024_.leading_blanks)
      (Ppx_compare_lib.( && )
         (equal_string a__023_.trailing_spaces b__024_.trailing_spaces)
         (Ppx_compare_lib.( && )
            (equal_string a__023_.orig b__024_.orig)
            (_cmp__a a__023_.data b__024_.data)))
;;

let _ = equal_single_line

[@@@end]

type 'a multi_lines =
  { leading_spaces : string
  ; trailing_spaces : string
  ; indentation : string
  ; lines : 'a Line.t list
  }
[@@deriving_inline sexp_of, compare, equal]

let _ = fun (_ : 'a multi_lines) -> ()

let sexp_of_multi_lines :
  'a. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a multi_lines -> Ppx_sexp_conv_lib.Sexp.t
  =
  fun _of_a -> function
    | { leading_spaces = v_leading_spaces
      ; trailing_spaces = v_trailing_spaces
      ; indentation = v_indentation
      ; lines = v_lines
      } ->
      let bnds = [] in
      let bnds =
        let arg = sexp_of_list (Line.sexp_of_t _of_a) v_lines in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "lines"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_string v_indentation in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "indentation"; arg ]
        :: bnds
      in
      let bnds =
        let arg = sexp_of_string v_trailing_spaces in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "trailing_spaces"; arg ]
        :: bnds
      in
      let bnds =
        let arg = sexp_of_string v_leading_spaces in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "leading_spaces"; arg ]
        :: bnds
      in
      Ppx_sexp_conv_lib.Sexp.List bnds
;;

let _ = sexp_of_multi_lines

let compare_multi_lines :
  'a. ('a -> 'a -> int) -> 'a multi_lines -> 'a multi_lines -> int
  =
  fun _cmp__a a__025_ b__026_ ->
  if Ppx_compare_lib.phys_equal a__025_ b__026_
  then 0
  else (
    match compare_string a__025_.leading_spaces b__026_.leading_spaces with
    | 0 ->
      (match compare_string a__025_.trailing_spaces b__026_.trailing_spaces with
       | 0 ->
         (match compare_string a__025_.indentation b__026_.indentation with
          | 0 ->
            compare_list
              (fun a__027_ b__028_ -> Line.compare _cmp__a a__027_ b__028_)
              a__025_.lines
              b__026_.lines
          | n -> n)
       | n -> n)
    | n -> n)
;;

let _ = compare_multi_lines

let equal_multi_lines :
  'a. ('a -> 'a -> bool) -> 'a multi_lines -> 'a multi_lines -> bool
  =
  fun _cmp__a a__031_ b__032_ ->
  if Ppx_compare_lib.phys_equal a__031_ b__032_
  then true
  else
    Ppx_compare_lib.( && )
      (equal_string a__031_.leading_spaces b__032_.leading_spaces)
      (Ppx_compare_lib.( && )
         (equal_string a__031_.trailing_spaces b__032_.trailing_spaces)
         (Ppx_compare_lib.( && )
            (equal_string a__031_.indentation b__032_.indentation)
            (equal_list
               (fun a__033_ b__034_ -> Line.equal _cmp__a a__033_ b__034_)
               a__031_.lines
               b__032_.lines)))
;;

let _ = equal_multi_lines

[@@@end]

type 'a t =
  | Empty of string
  | Single_line of 'a single_line
  | Multi_lines of 'a multi_lines
[@@deriving_inline sexp_of, compare, equal]

let _ = fun (_ : 'a t) -> ()

let sexp_of_t
  : type a. (a -> Ppx_sexp_conv_lib.Sexp.t) -> a t -> Ppx_sexp_conv_lib.Sexp.t
  =
  fun _of_a -> function
    | Empty v0 ->
      let v0 = sexp_of_string v0 in
      Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Empty"; v0 ]
    | Single_line v0 ->
      let v0 = sexp_of_single_line _of_a v0 in
      Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Single_line"; v0 ]
    | Multi_lines v0 ->
      let v0 = sexp_of_multi_lines _of_a v0 in
      Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Multi_lines"; v0 ]
;;

let _ = sexp_of_t

let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
  fun _cmp__a a__037_ b__038_ ->
  if Ppx_compare_lib.phys_equal a__037_ b__038_
  then 0
  else (
    match a__037_, b__038_ with
    | Empty _a__039_, Empty _b__040_ -> compare_string _a__039_ _b__040_
    | Empty _, _ -> -1
    | _, Empty _ -> 1
    | Single_line _a__041_, Single_line _b__042_ ->
      compare_single_line _cmp__a _a__041_ _b__042_
    | Single_line _, _ -> -1
    | _, Single_line _ -> 1
    | Multi_lines _a__045_, Multi_lines _b__046_ ->
      compare_multi_lines _cmp__a _a__045_ _b__046_)
;;

let _ = compare

let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
  fun _cmp__a a__049_ b__050_ ->
  if Ppx_compare_lib.phys_equal a__049_ b__050_
  then true
  else (
    match a__049_, b__050_ with
    | Empty _a__051_, Empty _b__052_ -> equal_string _a__051_ _b__052_
    | Empty _, _ -> false
    | _, Empty _ -> false
    | Single_line _a__053_, Single_line _b__054_ ->
      equal_single_line _cmp__a _a__053_ _b__054_
    | Single_line _, _ -> false
    | _, Single_line _ -> false
    | Multi_lines _a__057_, Multi_lines _b__058_ ->
      equal_multi_lines _cmp__a _a__057_ _b__058_)
;;

let _ = equal

[@@@end]

let invariant inv t =
  match t with
  | Empty s -> assert (is_spaces s)
  | Single_line s ->
    assert (is_blanks s.leading_blanks);
    assert (is_spaces s.trailing_spaces);
    inv s.data;
    assert (no_nl s.orig);
    let len = String.length s.orig in
    assert (len > 0 && (not (is_blank s.orig.[0])) && not (is_blank s.orig.[len - 1]))
  | Multi_lines m ->
    assert (is_spaces m.leading_spaces);
    let ld_len = String.length m.leading_spaces in
    assert (ld_len = 0 || Char.equal m.leading_spaces.[ld_len - 1] '\n');
    let tr_has_nl = has_nl m.trailing_spaces in
    assert (
      is_spaces m.trailing_spaces
      && ((not tr_has_nl) || Char.equal m.trailing_spaces.[0] '\n'));
    assert (is_blanks m.indentation);
    List.iter m.lines ~f:(Line.invariant inv);
    (match m.lines with
     | [] -> assert false
     | Blank _ :: _ -> assert false
     | [ Not_blank n ] ->
       assert (ld_len > 0 && (tr_has_nl || String.is_empty n.trailing_blanks))
     | l ->
       let rec check_last = function
         | [] -> assert false
         | [ Line.Blank _ ] -> assert false
         | [ Line.Not_blank n ] -> assert (tr_has_nl || String.is_empty n.trailing_blanks)
         | _ :: l -> check_last l
       in
       check_last l)
;;

let empty = Empty ""

let map t ~f =
  match t with
  | Empty e -> Empty e
  | Single_line s -> Single_line { s with data = f s.orig s.data }
  | Multi_lines m -> Multi_lines { m with lines = List.map m.lines ~f:(Line.map ~f) }
;;

let data t ~blank =
  match t with
  | Empty _ -> []
  | Single_line s -> [ s.data ]
  | Multi_lines m -> List.map m.lines ~f:(Line.data ~blank)
;;

let stripped_original_lines t =
  match t with
  | Empty _ -> []
  | Single_line s -> [ s.orig ]
  | Multi_lines m -> List.map m.lines ~f:Line.orig
;;

let line_of_single s : _ Line.t =
  Not_blank { trailing_blanks = ""; orig = s.orig; data = s.data }
;;

let to_lines t =
  match t with
  | Empty _ -> []
  | Single_line s -> [ line_of_single s ]
  | Multi_lines m -> m.lines
;;

let strip t =
  match t with
  | Empty _ -> Empty ""
  | Single_line s -> Single_line { s with leading_blanks = ""; trailing_spaces = "" }
  | Multi_lines m ->
    (match m.lines with
     | [] -> Empty ""
     | [ Blank _ ] -> assert false
     | [ Not_blank n ] ->
       Single_line
         { leading_blanks = ""; trailing_spaces = ""; orig = n.orig; data = n.data }
     | lines ->
       Multi_lines
         { leading_spaces = ""
         ; trailing_spaces = ""
         ; indentation = ""
         ; lines = List.map lines ~f:Line.strip
         })
;;

let to_string t =
  match t with
  | Empty s -> s
  | Single_line s -> s.leading_blanks ^ s.orig ^ s.trailing_spaces
  | Multi_lines m ->
    let indent (line : _ Line.t) =
      match line with
      | Blank b -> b
      | Not_blank n -> m.indentation ^ n.orig ^ n.trailing_blanks
    in
    let s = List.map m.lines ~f:indent |> String.concat ~sep:"\n" in
    m.leading_spaces ^ s ^ m.trailing_spaces
;;

let trim_lines lines =
  let rec loop0 : _ Line.t list -> _ = function
    | Blank _ :: l -> loop0 l
    | l -> loop1 l ~acc:[] ~acc_with_trailing_blanks:[]
  and loop1 ~acc ~acc_with_trailing_blanks = function
    | (Blank _ as x) :: l ->
      loop1 l ~acc ~acc_with_trailing_blanks:(x :: acc_with_trailing_blanks)
    | (Not_blank _ as x) :: l ->
      let acc = x :: acc_with_trailing_blanks in
      loop1 l ~acc ~acc_with_trailing_blanks:acc
    | [] -> List.rev acc
  in
  loop0 lines
;;

let not_blank_lines lines =
  List.fold_left lines ~init:[] ~f:(fun acc (l : _ Line.t) ->
    match l with
    | Blank _ -> acc
    | Not_blank n -> n.orig :: acc)
  |> List.rev
;;

let longest_common_prefix a b =
  let len_a = String.length a in
  let len_b = String.length b in
  let len = min len_a len_b in
  let i = ref 0 in
  while !i < len && Char.equal a.[!i] b.[!i] do
    Int.incr i
  done;
  String.sub a ~pos:0 ~len:!i
;;

let indentation s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && is_blank s.[!i] do
    Int.incr i
  done;
  String.sub s ~pos:0 ~len:!i
;;

let extract_indentation lines =
  match not_blank_lines lines with
  | [] -> "", lines
  | first :: rest ->
    let indent =
      List.fold_left rest ~init:(indentation first) ~f:longest_common_prefix
    in
    let indent_len = String.length indent in
    let update_line : 'a Line.t -> 'a Line.t = function
      | Blank b -> Blank b
      | Not_blank n ->
        let orig =
          String.sub n.orig ~pos:indent_len ~len:(String.length n.orig - indent_len)
        in
        Not_blank { n with orig }
    in
    indent, List.map lines ~f:update_line
;;

let break s at = String.prefix s at, String.drop_prefix s at

let reconcile (type a) t ~lines ~default_indentation ~pad_single_line =
  let module M = struct
    type t =
      | Empty
      | Single_line of a Line.not_blank
      | Multi_lines of a Line.t list
  end
  in
  let lines =
    match trim_lines lines |> extract_indentation |> snd with
    | [] -> M.Empty
    | [ Blank _ ] -> assert false
    | [ Not_blank n ] -> M.Single_line n
    | lines -> M.Multi_lines lines
  in
  let padding = if pad_single_line then " " else "" in
  let res =
    match t, lines with
    | Empty _, Empty -> t
    | Single_line s, Single_line n -> Single_line { s with orig = n.orig; data = n.data }
    | Multi_lines m, Multi_lines l -> Multi_lines { m with lines = l }
    | Empty e, Multi_lines l ->
      let ld, tr =
        if has_nl e
        then (
          let ld, tr = break e (String.index_exn e '\n') in
          ld ^ "\n", tr)
        else "\n", padding
      in
      Multi_lines
        { leading_spaces = ld
        ; trailing_spaces = tr
        ; indentation = String.make (default_indentation + 2) ' '
        ; lines = l
        }
    | Single_line m, Multi_lines l ->
      Multi_lines
        { leading_spaces = "\n"
        ; trailing_spaces = m.trailing_spaces
        ; indentation = String.make (default_indentation + 2) ' '
        ; lines = l
        }
    | Single_line _, Empty | Multi_lines _, Empty -> Empty padding
    | Empty _, Single_line n ->
      Single_line
        { orig = n.orig
        ; data = n.data
        ; leading_blanks = padding
        ; trailing_spaces = padding
        }
    | Multi_lines m, Single_line n -> Multi_lines { m with lines = [ Not_blank n ] }
  in
  invariant ignore res;
  res
;;
