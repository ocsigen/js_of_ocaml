(* Ocamldoc: Generation of wikicreole file (Ocsimore)                  *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)

(*            modif for wiki files by Ocsigen team                     *)
(*            (based on odoc_html, keeping the names html_*            *)
(*            to use late inheritance and late binding)                *)

(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)

let print_DEBUG s = print_string s ; print_newline ()

open Odoc_info
open Parameter
open Value
open Type
open Exception
open Extension
open Class
open Module

module Conf = struct
  let colorize_code = Odoc_html.colorize_code
  let out_file = Odoc_global.out_file
  let html_short_functors = Odoc_html.html_short_functors
  let with_parameter_list = Odoc_html.with_parameter_list
  let target_dir = Global.target_dir
  let title = Global.title
  let intro_file = Odoc_info.Global.intro_file
  let index_only = Odoc_html.index_only
end

let remove_spaces s beg endd =
  let rec find_not_space s i step =
    if (i > endd) || (beg > i)
    then i
    else
    if s.[i] = ' '
    then find_not_space s (i+step) step
    else i
  in
  let first = find_not_space s beg 1 in
  let last = find_not_space s endd (-1) in
  if last >= first
  then String.sub s first (1+ last - first)
  else ""

let rec split char s =
  let longueur = String.length s in
  let rec aux deb =
    if deb >= longueur
    then []
    else
      try
        let firstsep = String.index_from s deb char in
        (remove_spaces s deb (firstsep-1))::
          (aux (firstsep+1))
      with Not_found -> [remove_spaces s deb (longueur-1)]
  in
  aux 0


let subproject =
  ref (try Some (Sys.getenv "ODOC_WIKI_SUBPROJECT") with Not_found -> None)
let _ = Odoc_args.add_option ("-subproject",
    Arg.String (fun s -> subproject := Some s),
    "Add the subproject parameter to the generated <<a_api ...>>")
let get_subproject () = match !subproject with
  | None -> ""
  | Some s -> Printf.sprintf " subproject=\"%s\"" s

(** The functions used for naming files and html marks.*)
module Naming =
struct

  include Odoc_html.Naming

  (** Return the wiki ident for the given module or class name.*)
  let wiki_file name =
    let qual =
      try
        let i = String.rindex name '.' in
        match name.[i + 1] with
        | 'A'..'Z' -> ""
        | _ -> "-c"
      with Not_found -> ""
    in
    name ^ qual ^ ".wiki"

  (** Return the target for the given prefix and simple name. *)
  let target pref simple_name = pref^simple_name

  (** Return the link target for the given type. *)
  let type_target t = target mark_type (Name.simple t.ty_name)

  (** Return the link target for the given exception. *)
  let exception_target e = target mark_exception (Name.simple e.ex_name)

  (** Return the link target for the given value. *)
  let value_target v = target mark_value (Name.simple v.val_name)

  (** Return the complete filename for the code of the given value. *)
  let file_code_value_complete_target v =
    let f = code_prefix^mark_value^(subst_infix_symbols v.val_name)^".wiki" in
    f

  (** Return the link target for the given attribute. *)
  let attribute_target a = target mark_attribute (Name.simple a.att_value.val_name)

  (** Return the complete filename for the code of the given attribute. *)
  let file_code_attribute_complete_target a =
    let f = code_prefix^mark_attribute^a.att_value.val_name^".wiki" in
    f

  (** Return the link target for the given method. *)
  let method_target m = target mark_method (Name.simple m.met_value.val_name)

  (** Return the complete filename for the code of the given method. *)
  let file_code_method_complete_target m =
    let f = code_prefix^mark_method^m.met_value.val_name^".wiki" in
    f

  (** Return the link target for the given label section. *)
  let label_target l = target "" l

  (** Return the complete filename for the code of the type of the
      given module or module type name. *)
  let file_type_module_complete_target name =
    let f = type_prefix^name^".wiki" in
    f

  (** Return the complete filename for the code of the
      given module name. *)
  let file_code_module_complete_target name =
    let f = code_prefix^name^".wiki" in
    f

  (** Return the complete filename for the code of the type of the
      given class or class type name. *)
  let file_type_class_complete_target name =
    let f = type_prefix^name^".wiki" in
    f

end

module StringSet = Odoc_html.StringSet

let new_buf () = Buffer.create 1024
let bp = Printf.bprintf
let bs = Buffer.add_string
let bsn n b s = for i = 1 to n do Buffer.add_string b s done
let regexp_escapable = Str.regexp "\\([]<>*/=[{}:\\~|#-^_;:,]\\)"
let regexp_simple_escapable = Str.regexp "\\(#\\)"
let regexp_nonwiki_escapable = Str.regexp_string ">>"
let ul_level = ref 0
let ol_level = ref 0

(** A class with a method to colorize a string which represents OCaml code. *)
class ocaml_code =
  object(self)
    method html_of_code b ?(with_pre=true) code =
      bp b "<<code language=\"ocaml\" | %s >>" code
  end

(** Generation of wiki code from text structures. *)
class virtual text =
  object (self)
    inherit Odoc_html.text
    inherit ocaml_code

    (** Escape the strings which would clash with wiki syntax *)
    method escape s =
      Str.global_replace regexp_escapable "~\\1" s
    method simple_escape s =
      Str.global_replace regexp_simple_escapable "~\\1" s

    (** Same for non wiki parts. *)
    method escape_nonwiki s =
      Str.global_replace regexp_nonwiki_escapable "~>>" s

    method html_of_Target b ~target ~code =
      if String.lowercase_ascii target = "html" then bs b code else ()

    method html_of_Raw b s =
      bs b (self#escape s)

    method html_of_Code b code =
      if !Conf.colorize_code
      then begin
        (* not supported by current version of Ocsimore:
                bs b "<<inlinecode language='ocaml'|";
                bs b (self#escape_code code);
                bs b ">>"
           in the meantime:
        *)
        bp b "<<span class=\"odocwiki_inlinecode\"|";
        bs b (self#escape code);
        bs b ">>"
      end
      else begin
        bs b "<<span class=\"odocwiki_inlinecode\"|";
        bs b (self#escape code);
        bs b ">>"
      end

    method html_of_CodePre =
      let remove_useless_newlines s =
        let len = String.length s in
        let rec iter_first n =
          if n >= len then
            None
          else
            match s.[n] with
            | '\n' -> iter_first (n+1)
            | _ -> Some n
        in
        match iter_first 0 with
          None -> ""
        | Some first ->
          let rec iter_last n =
            if n <= first then
              None
            else
              match s.[n] with
                '\t'  -> iter_last (n-1)
              | _ -> Some n
          in
          match iter_last (len-1) with
            None -> String.sub s first 1
          | Some last -> String.sub s first ((last-first)+1)
      in
      fun b code ->
        if !Conf.colorize_code
        then begin
          bs b "\n<<code language='ocaml'|";
          bs b (self#escape_nonwiki (remove_useless_newlines code));
          bs b ">>\n"
        end
        else begin
          bs b "\n{{{";
          bs b code; (* no escape *)
          bs b "}}}\n"
        end

    method html_of_Verbatim b s =
      bs b "{{{";
      bs b s; (* no escape *)
      bs b "}}}"

    method html_of_Bold b t =
      bs b "**";
      self#html_of_text b t;
      bs b "**"

    method html_of_Italic b t =
      bs b "//" ;
      self#html_of_text b t;
      bs b "//"

    method html_of_Emphasize b t =
      bs b "//" ;
      self#html_of_text b t ;
      bs b "//"

    method html_of_Center b t =
      bs b "\n<<div style=\"text-align: center;\"|";
      self#html_of_text b t;
      bs b ">>\n"

    method html_of_Left b t =
      bs b "\n<<div style=\"text-align: left;\">";
      self#html_of_text b t;
      bs b ">>\n"

    method html_of_Right b t =
      bs b "\n<<div style=\"text-align: right;\">";
      self#html_of_text b t;
      bs b ">>\n"

    method html_of_List b tl =
      incr ul_level;
      List.iter
        (fun t ->
           bs b "\n";
           bsn !ul_level b "*";
           bs b " ";
           self#html_of_text b t;)
        tl;
      decr ul_level;
      bs b "\n"

    method html_of_Enum b tl =
      incr ol_level;
      List.iter
        (fun t ->
           bsn !ol_level b "#";
           bs b " ";
           self#html_of_text b t; bs b "\n")
        tl;
      decr ol_level;
      bs b "\n"

    method html_of_Newline b = bs b "\n\n"

    method html_of_Block b t =
      bs b "\n<< div class=\"odocwiki_blockquote\"|";
      self#html_of_text b t;
      bs b ">>\n"

    method html_of_Title b n label_opt t =
      let label1 = self#create_title_label (n, label_opt, t) in
      bs b "\n";
      if n <= 6 then (
        bsn n b "=";
        bs b "@@id=\"";
        bs b (Naming.label_target label1);
        bs b "\"@@"
      ) else (
        bp b "<<div class=\"h%d\"| " n
      );
      self#html_of_text b t;
      if n <= 6 then
        bsn n b "="
      else
        bs b ">>";
      bs b "\n"

    method html_of_Latex b s = bs b s
    (* Handle latex as wiki. *)

    method html_of_Link b s t =
      bs b "[[";
      bs b s ;
      bs b "|";
      self#html_of_text b t;
      bs b "]]"

    method html_of_Ref b name ref_opt text_opt =
      match ref_opt with
      | None ->
        let text =
          match text_opt with
            None -> [Odoc_info.Code name]
          | Some t -> t
        in
        self#html_of_text b text
      | Some kind ->
        let target =
          match kind with
            Odoc_info.RK_module -> "module " ^ name
          | Odoc_info.RK_module_type -> "module type " ^ name
          | Odoc_info.RK_class -> "class " ^ name
          | Odoc_info.RK_class_type -> "class type " ^ name
          | Odoc_info.RK_value -> "val " ^ name
          | Odoc_info.RK_type -> "type " ^ name
          | Odoc_info.RK_exception -> "exception " ^ name
          | Odoc_info.RK_attribute -> "attribute " ^ name
          | Odoc_info.RK_method -> "method " ^ name
          | Odoc_info.RK_section t -> "section " ^ name
          | Odoc_info.RK_recfield -> "recfield " ^ name
          | Odoc_info.RK_const -> "const " ^ name
          | Odoc_info.RK_extension -> "extension " ^ name
        in
        bp b "<<a_api%s" (get_subproject ());
        begin match text_opt with
          | Some text -> bp b " text=\"%a\"" (self#html_of_text ?with_p:None) text
          | None -> () end;
        bp b " | %s >>" target;

    method html_of_Superscript b t =
      bs b "^^";
      self#html_of_text b t;
      bs b "^^"

    method html_of_Subscript b t =
      bs b ",,";
      self#html_of_text b t;
      bs b ",,"

    method html_of_Module_list b l =
      bs b "\n";
      List.iter
        (fun name ->
           bs b "|";
           (
             try
               let m =
                 List.find (fun m -> m.m_name = name) self#list_modules
               in
               bp b "<<a_api%s | module %s >>|" (get_subproject ()) m.m_name;
               self#html_of_info_first_sentence b m.m_info;
             with
               Not_found ->
               Odoc_global.pwarning (Odoc_messages.cross_module_not_found name);
               bp b "%s|" name
           );
           bs b "|\n"
        )
        l;
      bs b "\n"

    method html_of_Index_list b =
      let index_if_not_empty l url m =
        match l with
        | [] -> ()
        | _ -> bp b "* <<a_api%s text=%S | index %s >>\n" (get_subproject ()) m url
      in
      index_if_not_empty self#list_types "types" Odoc_messages.index_of_types;
      index_if_not_empty self#list_exceptions "exceptions" Odoc_messages.index_of_exceptions;
      index_if_not_empty self#list_values "values" Odoc_messages.index_of_values;
      index_if_not_empty self#list_attributes "attributes" Odoc_messages.index_of_attributes;
      index_if_not_empty self#list_methods "methods" Odoc_messages.index_of_methods;
      index_if_not_empty self#list_classes "classes" Odoc_messages.index_of_classes;
      index_if_not_empty self#list_class_types "class types" Odoc_messages.index_of_class_types;
      index_if_not_empty self#list_modules "modules" Odoc_messages.index_of_modules;
      index_if_not_empty self#list_module_types "module types" Odoc_messages.index_of_module_types


  end

(** A class used to generate wiki code for info structures. *)
class virtual info =
  object (self)

    (** The list of pairs [(tag, f)] where [f] is a function taking
        the [text] associated to [tag] and returning html code.
        Add a pair here to handle a tag.*)
    val mutable tag_functions = ([] : (string * (Odoc_info.text -> string)) list)

    (** The method used to get html code from a [text]. *)
    method virtual html_of_text : ?with_p:bool -> Buffer.t -> Odoc_info.text -> unit

    (** Print html for an author list. *)
    method html_of_author_list b l =
      match l with
        [] -> ()
      | _ ->
        bp b "**%s:** %s\\\\\n" Odoc_messages.authors (String.concat ", " l)

    (** Print html code for the given optional version information.*)
    method html_of_version_opt b v_opt =
      match v_opt with
        None -> ()
      | Some v ->
        bp b "**%s:** %s\\\\\n" Odoc_messages.version v

    (** Print html code for the given optional since information.*)
    method html_of_since_opt b s_opt =
      match s_opt with
        None -> ()
      | Some s ->
        bp b "**%s** %s\\\\\n" Odoc_messages.since s

    (* Print html code for the given "before" information.*)
    (* method html_of_before b l = *)
    (* let f (v, text) = *)
    (* bp b "**%s %s ** " Odoc_messages.before v; *)
    (* self#html_of_text b text; *)
    (* bs b "\\\\\n" *)
    (* in *)
    (* List.iter f l *)

    (** Print html code for the given list of raised exceptions.*)
    method html_of_raised_exceptions b l =
      match l with
        [] -> ()
      | (s, t) :: [] ->
        bp b "**%s** {{{%s}}} "
          Odoc_messages.raises
          s;
        self#html_of_text b t;
        bs b "\\\\\n"
      | _ ->
        bp b "**%s**\n" Odoc_messages.raises;
        List.iter
          (fun (ex, desc) ->
             bp b "* <<div class='code' | {{{%s}}}>> " ex ;
             self#html_of_text b desc;
             bs b "\n"
          )
          l;
        bs b "\n"

    (** Print html code for the given "see also" reference. *)
    method html_of_see b (see_ref, t)  =
      let t_ref =
        match see_ref with
          Odoc_info.See_url s -> [ Odoc_info.Link (s, t) ]
        | Odoc_info.See_file s -> (Odoc_info.Code s) :: (Odoc_info.Raw " ") :: t
        | Odoc_info.See_doc s -> (Odoc_info.Italic [Odoc_info.Raw s]) :: (Odoc_info.Raw " ") :: t
      in
      self#html_of_text b t_ref

    (** Print html code for the given list of "see also" references.*)
    method html_of_sees b l =
      match l with
        [] -> ()
      | see :: [] ->
        bp b "**%s** " Odoc_messages.see_also;
        self#html_of_see b see;
        bs b "\\\\\n"
      | _ ->
        bp b "**%s**\n" Odoc_messages.see_also;
        List.iter
          (fun see ->
             bs b "* " ;
             self#html_of_see b see;
             bs b "\n"
          )
          l;
        bs b "\n"

    (** Print html code for the given optional return information.*)
    method html_of_return_opt b return_opt =
      match return_opt with
        None -> ()
      | Some s ->
        bp b "**%s** " Odoc_messages.returns;
        self#html_of_text b s;
        bs b "\\\\\n"

    (** Print html code for the given list of custom tagged texts. *)
    method html_of_custom b l =
      List.iter
        (fun (tag, text) ->
           try
             let f = List.assoc tag tag_functions in
             Buffer.add_string b (f text)
           with
             Not_found ->
             Odoc_info.warning (Odoc_messages.tag_not_handled tag)
        )
        l

    (** Print html code for a description, except for the [i_params] field.
        @param indent can be specified not to use the style of info comments;
        default is [true].
    *)
    method html_of_info ?(indent=true) b info_opt =
      match info_opt with
        None ->
        ()
      | Some info ->
        let module M = Odoc_info in
        if indent then bs b "<<div class=\"odocwiki_info\"|";
        (
          match info.M.i_deprecated with
            None -> ()
          | Some d ->
            bs b "<<span class=\"odocwiki_warning\"|";
            bs b Odoc_messages.deprecated ;
            bs b ">>" ;
            self#html_of_text b d;
            bs b "\n"
        );
        (
          match info.M.i_desc with
            None -> ()
          | Some d when d = [Odoc_info.Raw ""] -> ()
          | Some d -> self#html_of_text b d; bs b "\n"
        );
        self#html_of_author_list b info.M.i_authors;
        self#html_of_version_opt b info.M.i_version;
        (* self#html_of_before b info.M.i_before; *)
        self#html_of_since_opt b info.M.i_since;
        self#html_of_raised_exceptions b info.M.i_raised_exceptions;
        self#html_of_return_opt b info.M.i_return_value;
        self#html_of_sees b info.M.i_sees;
        self#html_of_custom b info.M.i_custom;
        if indent then bs b ">>\n"

    (** Print html code for the first sentence of a description.
        The titles and lists in this first sentence has been removed.*)
    method html_of_info_first_sentence b info_opt =
      match info_opt with
        None -> ()
      | Some info ->
        let module M = Odoc_info in
        let dep = info.M.i_deprecated <> None in
        bs b "<<div class=\"odocwiki_info\"|";
        if dep then bs b "<<span class=\"odocwiki_deprecated\"|";
        (
          match info.M.i_desc with
            None -> ()
          | Some d when d = [Odoc_info.Raw ""] -> ()
          | Some d ->
            self#html_of_text b
              (Odoc_info.text_no_title_no_list
                 (Odoc_info.first_sentence_of_text d));
            bs b "\n"
        );
        if dep then bs b ">>";
        bs b ">>\n"

  end

let newline_to_indented_br ?(indent_first=true) ?(len = 0) ~indent s =
  let b = Buffer.create (String.length s)  in
  let last_len = ref len in
  if indent_first &&
     (try ignore (String.index s '\n'); true
      with Not_found -> len > Odoc_import.default_margin);
  then (bp b "\\\\%s" indent; last_len := String.length indent);
  for i = 0 to String.length s - 1 do
    match s.[i] with
      '\n' -> bp b "\\\\%s" indent; last_len := String.length indent
    | c -> Buffer.add_char b c; incr last_len
  done;
  Buffer.contents b, !last_len

(* let newline_to_indented_br ?(indent="   ") s = *)
(* let len = String.length s in *)
(* let b = Buffer.create len in *)
(* let skip_space = ref true in *)
(* ( *)
(* try ignore (String.index s '\n'); bp b "\\\\{{{%s}}}" indent *)
(* with Not_found -> () *)
(* ); *)
(* for i = 0 to len - 1 do *)
(* match s.[i] with *)
(* '\n' -> *)
(* skip_space := true; *)
(* bp b "\\\\{{{%s}}}" indent *)
(* | ' ' | '\t' when !skip_space -> () *)
(* | c -> *)
(* skip_space := false; *)
(* Buffer.add_char b c *)
(* done; *)
(* Buffer.contents b *)

module Generator = struct

  (** This class is used to create objects which can generate a simple wiki documentation. *)
  class wiki =
    object (self)
      (* inherit Odoc_html.html *)
      inherit text
      inherit info

      (** The known types names.
          Used to know if we must create a link to a type
          when printing a type. *)
      val mutable known_types_names = StringSet.empty

      (** The known class and class type names.
          Used to know if we must create a link to a class
          or class type or not when printing a type. *)
      val mutable known_classes_names = StringSet.empty

      (** The known modules and module types names.
          Used to know if we must create a link to a type or not
          when printing a module type. *)
      val mutable known_modules_names = StringSet.empty

      method index_prefix =
        if !Conf.out_file = Odoc_messages.default_out_file then
          "index"
        else
          Filename.basename !Conf.out_file

      (** The main file. *)
      method index =
        let p = self#index_prefix in
        Printf.sprintf "%s.wiki" p

      (** The menu file. *)
      method menu =
        Printf.sprintf "menu.wiki"

      (** The file for the index of values. *)
      method index_values = Printf.sprintf "%s_values.wiki" self#index_prefix

      (** The file for the index of types. *)
      method index_types = Printf.sprintf "%s_types.wiki" self#index_prefix

      (** The file for the index of extensions. *)
      method index_extensions = Printf.sprintf "%s_extensions.wiki" self#index_prefix

      (** The file for the index of exceptions. *)
      method index_exceptions = Printf.sprintf "%s_exceptions.wiki" self#index_prefix

      (** The file for the index of attributes. *)
      method index_attributes = Printf.sprintf "%s_attributes.wiki" self#index_prefix

      (** The file for the index of methods. *)
      method index_methods = Printf.sprintf "%s_methods.wiki" self#index_prefix

      (** The file for the index of classes. *)
      method index_classes = Printf.sprintf "%s_classes.wiki" self#index_prefix

      (** The file for the index of class types. *)
      method index_class_types = Printf.sprintf "%s_class_types.wiki" self#index_prefix

      (** The file for the index of modules. *)
      method index_modules = Printf.sprintf "%s_modules.wiki" self#index_prefix

      (** The file for the index of module types. *)
      method index_module_types = Printf.sprintf "%s_module_types.wiki" self#index_prefix


      (** The list of attributes. Filled in the [generate] method. *)
      val mutable list_attributes = []
      method list_attributes = list_attributes

      (** The list of methods. Filled in the [generate] method. *)
      val mutable list_methods = []
      method list_methods = list_methods

      (** The list of values. Filled in the [generate] method. *)
      val mutable list_values = []
      method list_values = list_values

      (** The list of exceptions. Filled in the [generate] method. *)
      val mutable list_exceptions = []
      method list_exceptions = list_exceptions

      (** The list of extension. Filled in the [generate] method. *)
      val mutable list_extensions = []
      method list_extensions = list_extensions

      (** The list of types. Filled in the [generate] method. *)
      val mutable list_types = []
      method list_types = list_types

      (** The list of modules. Filled in the [generate] method. *)
      val mutable list_modules = []
      method list_modules = list_modules

      (** The list of module types. Filled in the [generate] method. *)
      val mutable list_module_types = []
      method list_module_types = list_module_types

      (** The list of classes. Filled in the [generate] method. *)
      val mutable list_classes = []
      method list_classes = list_classes

      (** The list of class types. Filled in the [generate] method. *)
      val mutable list_class_types = []
      method list_class_types = list_class_types


      (** Return html code with the given string in the keyword style.*)
      method keyword s =
        "<<span class=\"ocsforge_color_keyword\"|"^s^">>"
      method delimiter s =
        "<<span class=\"ocsforge_color_delimiter\"|"^s^">>"


      (** Return html code with the given string in the constructor style. *)
      method constructor s = "<<span class=\"ocsforge_color_uid\"|"^s^">>"

      (** Output the given ocaml code to the given file name. *)
      method private output_code in_title file code =
        try
          let chanout = open_out file in
          let b = new_buf () in
          (* bs b in_title; *)
          self#html_of_code b code;
          Buffer.output_buffer chanout b;
          close_out chanout
        with
          Sys_error s ->
          incr Odoc_info.errors ;
          prerr_endline s

      method colorize_fully_qualified_idents name =
        String.concat "<<span class=\"ocsforge_color_delimiter\"| . >>"
          (List.map
             (fun n ->
                if 'A' < n.[0] && n.[0] < 'Z' then
                  "<<span class=\"ocsforge_color_uid\"|"^n^">>"
                else
                  "<<span class=\"ocsforge_color_lid\"|"^n^">>")
             (split '.' name))


      (** Take a string and return the string where fully qualified
          type (or class or class type) idents
          have been replaced by links to the type referenced by the ident.*)
      method create_fully_qualified_idents_links m_name ?(raw = false) s =
        let f str_t =
          let match_s = Str.matched_string str_t in
          let rel = Name.get_relative m_name match_s in
          let s_final = Odoc_info.apply_if_equal
              Odoc_info.use_hidden_modules
              match_s
              rel
          in
          if raw then s_final
          else if StringSet.mem match_s known_types_names then
            "<<a_api"^get_subproject ()^" text=\"" ^ s_final ^ "\" | type " ^ match_s ^ " >>"
          else if StringSet.mem match_s known_classes_names then
            "<<a_api"^get_subproject ()^" text=\"" ^ s_final ^ "\" | class type " ^ match_s ^ " >>"
          else
            self#colorize_fully_qualified_idents s_final
        in
        let s2 = Str.global_substitute
            (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([a-z][a-zA-Z_'0-9]*\\)")
            f
            s
        in
        if raw then s2
        else
          let s3 = Str.global_substitute
              (Str.regexp "\\((\\|)\\|->\\)")
              (fun str_t -> "<<span class=\"ocsforge_color_delimiter\"| "^Str.matched_string str_t^" >>")
              s2
          in
          let s4 = Str.global_substitute
              (Str.regexp "\\??\\([a-z_][a-zA-Z_'0-9]*:\\)")
              (fun str_t -> "<<span class=\"ocsforge_color_label\"| "^Str.matched_string str_t^" >>")
              s3
          in
          s4

      (** Take a string and return the string where fully qualified module idents
          have been replaced by links to the module referenced by the ident.*)
      method create_fully_qualified_module_idents_links m_name s =
        let f str_t =
          let match_s = Str.matched_string str_t in
          let rel = Name.get_relative m_name match_s in
          let s_final = Odoc_info.apply_if_equal
              Odoc_info.use_hidden_modules
              match_s
              rel
          in
          if StringSet.mem match_s known_modules_names then
            "<<a_api"^get_subproject ()^" text=\"" ^ s_final ^ "\" | module " ^ match_s ^ " >>"
          else
            "<<span class=\"ocsforge_color_uid\"|"^s_final^">>"
        in
        let s2 = Str.global_substitute
            (Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([A-Z][a-zA-Z_'0-9]*\\)")
            f
            s
        in
        s2

      (** Print html code to display a [Types.type_expr]. *)
      method html_of_type_expr b ~init_len ~indent m_name t =
        let s =
          Odoc_info.remove_ending_newline
            (Odoc_import.string_of_type_expr
               ~margin:(Odoc_import.default_margin - String.length indent) t) in
        let s = self#simple_escape s in
        let raw = self#create_fully_qualified_idents_links m_name        ~raw:true s in
        let s2, _ = newline_to_indented_br ~len:(init_len + String.length raw) ~indent s in
        let s3 = self#create_fully_qualified_idents_links m_name s2 in
        if s3 <> "" then bp b "<<span class=\"odocwiki_type\"|%s>>" s3

      (** Print html code to display a [Types.type_expr list]. *)
      method html_of_type_expr_list ?par b ~init_len ~indent m_name sep l =
        let s =
          Odoc_import.string_of_type_list
            ~margin:(Odoc_import.default_margin - String.length indent)
            ?par sep l in
        let raw = self#create_fully_qualified_idents_links m_name        ~raw:true s in
        let s2, _ =
          newline_to_indented_br ~len:(init_len + String.length raw) ~indent s in
        let s3 = self#create_fully_qualified_idents_links m_name s2 in
        if s3 <> "" then bp b "<<span class=\"odocwiki_type\"|%s>>" s3

      (** Print html code to display a [Types.type_expr list] as type parameters
          of a class of class type. *)
      method html_of_class_type_param_expr_list b ~init_len ~indent m_name l =
        let s =
          Odoc_import.string_of_class_type_param_list
            ~margin:(Odoc_import.default_margin - String.length indent)
            l in
        let raw = self#create_fully_qualified_idents_links m_name ~raw:true s in
        let s2, len =
          newline_to_indented_br ~len:(init_len + String.length raw) ~indent s in
        let s3 = self#create_fully_qualified_idents_links m_name s2 in
        if s3 <> "" then bp b "<<span class=\"odocwiki_type\"|[%s]>>" s3;
        len

      method html_of_class_parameter_list b ~init_len ~indent father c =
        let s =
          Odoc_import.string_of_class_params
            ~margin:(Odoc_import.default_margin - String.length indent)
            c in
        let s = Odoc_info.remove_ending_newline s in
        let raw = self#create_fully_qualified_idents_links father ~raw:true s in
        let s2, len =
          newline_to_indented_br ~len:(init_len + String.length raw) ~indent s in
        let s3 = self#create_fully_qualified_idents_links father s2 in
        if s3 <> "" then bp b "<<span class=\"odocwiki_type\"|%s>>" s3;
        len

      (** Print html code to display a list of type parameters for the given type.*)
      method html_of_type_expr_param_list b ~init_len ~indent m_name t =
        let s =
          Odoc_import.string_of_type_param_list
            ~margin:(Odoc_import.default_margin - String.length indent)
            t in
        let raw = self#create_fully_qualified_idents_links m_name        ~raw:true s in
        let s2, len =
          newline_to_indented_br ~len:(init_len + String.length raw) ~indent s in
        let s3 = self#create_fully_qualified_idents_links m_name s2 in
        if s3 <> "" then bp b "<<span class=\"odocwiki_type\"|%s>>" s3;
        len

      (** Print html code to display a [Types.module_type]. *)
      method html_of_module_type b ?code ~init_len ~indent m_name t =
        let s =
          Odoc_info.remove_ending_newline
            (Odoc_import.string_of_module_type
               ~margin:(Odoc_import.default_margin - String.length indent)
               ?code t) in
        let s2, _ =
          newline_to_indented_br ~len:(init_len + String.length s) ~indent s in
        if s2 <> "" then bp b "<<span class=\"odocwiki_type\"|%s>>" s2


      (** Print html code to display the given module kind. *)
      method html_of_module_kind b father ~indent ?modu kind  =
        match kind with
          Module_struct eles ->
          bs b (self#keyword "sig");
          (
            match modu with
              None ->
              bs b "<<span class=\"odocwiki_sig_block\"|";
              List.iter (self#html_of_module_element b ~indent:(indent ^ "  ") ~pre:false father) eles;
              bs b ">>"
            | Some m ->
              bp b "<<a_api%s text=\"..\" | module %s >>" (get_subproject ()) m.m_name
          );
          bs b (self#keyword "end")
        | Module_alias a ->
          bs b "<<span class=\"odocwiki_type\"|";
          bs b (self#create_fully_qualified_module_idents_links father a.ma_name);
          bs b ">>"
        | Module_functor (p, k) ->
          if !Conf.html_short_functors then
            bs b " "
          else
            bs b "<<span class=\"odocwiki_sig_block\"|";
          self#html_of_module_parameter ~indent:indent b father p;
          (
            match k with
              Module_functor _ -> ()
            | _ when !Conf.html_short_functors ->
              bs b ": "
            | _ -> ()
          );
          bp b "\\\\%s  " indent;
          self#html_of_module_kind b father ?modu ~indent k;
          if not !Conf.html_short_functors then
            bs b ">>"
        | Module_apply (k1, k2) ->
          (* TODO: l'application n'est pas correcte dans un .mli.
             Que faire ? -> afficher le module_type du typedtree  *)
          self#html_of_module_kind b ~indent father k1;
          bs b (self#delimiter "(");
          self#html_of_module_kind b ~indent father k2;
          bs b (self#delimiter ")")
        | Module_with (k, s) ->
          (* TODO: à modifier quand Module_with sera plus détaillé *)
          let s2 = self#create_fully_qualified_module_idents_links father s in
          let s3, _ = newline_to_indented_br ~indent_first:false ~indent s2 in
          (
            try ignore (String.index s2 '\n'); bp b "\\\\  %s" indent
            with Not_found -> ()
          );
          self#html_of_module_type_kind b father ~indent ?modu k;
          bp b "<<span class=\"odocwiki_type\"|%s %s>>" indent s3
        | Module_constraint (k, tk) ->
          (* TODO: on affiche quoi ? *)
          self#html_of_module_kind b ~indent father ?modu k
        | Module_typeof s ->
          bs b "<<span class=\"odocwiki_type\"|module type of ";
          bs b (self#create_fully_qualified_module_idents_links father s);
          bs b ">>"
        | Module_unpack (code, mta) ->
          bs b "<<span class=\"odocwiki_type\"|";
          begin
            match mta.mta_module with
              None ->
              bs b (self#create_fully_qualified_module_idents_links father (self#escape code))
            | Some mt ->
              bp b "<<a_api%s text=\"%s\" | module %s >>" (get_subproject ()) (self#escape code) mt.mt_name
          end;
          bs b ">>"

      method html_of_module_parameter b ~indent father p =
        let (s_functor,s_arrow) =
          if !Conf.html_short_functors then
            "", ""
          else
            indent^(self#keyword "functor")^" ", (self#delimiter "-~>")^" "
        in
        bp b "%s%s%s %s " s_functor (self#delimiter "(") p.mp_name (self#delimiter "~:");
        self#html_of_module_type_kind b father ~indent:(indent ^ "    ") p.mp_kind;
        bp b "%s %s" (self#delimiter ")") s_arrow

      method html_of_module_element ?(pre = true) ~indent b father ele =
        match ele with
          Element_module m ->
          self#html_of_module b ~indent ~pre ~complete: false m
        | Element_module_type mt ->
          self#html_of_modtype b ~indent ~pre ~complete: false mt
        | Element_included_module im ->
          self#html_of_included_module b ~indent ~pre im
        | Element_class c ->
          self#html_of_class b ~indent ~pre ~complete: false c
        | Element_class_type ct ->
          self#html_of_class_type b ~indent ~pre ~complete: false ct
        | Element_value v ->
          self#html_of_value b ~indent ~pre v
        | Element_exception e ->
          self#html_of_exception b ~indent ~pre e
        | Element_type t ->
          self#html_of_type b ~indent ~pre t
        | Element_module_comment text ->
          self#html_of_module_comment b text
        | Element_type_extension ext ->
          self#html_of_type_extension b ~indent ~pre ext

      (** Print html code to display the given module type kind. *)
      method html_of_module_type_kind b father ~indent ?modu ?mt kind =
        match kind with
          Module_type_struct eles ->
          bs b (self#keyword "sig");
          (
            match mt with
              None ->
              (
                match modu with
                  None ->
                  bs b "<<span class=\"odocwiki_sig_block\"|";
                  List.iter (self#html_of_module_element b ~indent:(indent ^ "  ") ~pre:false father) eles;
                  bs b ">>"
                | Some m ->
                  bp b "<<a_api%s text=\"..\" | module %s >>" (get_subproject ()) m.m_name
              )
            | Some mt ->
              bp b "<<a_api%s text=\"..\" | module type %s >>" (get_subproject ()) mt.mt_name
          );
          bs b (self#keyword "end")
        | Module_type_functor (p, k) ->
          self#html_of_module_parameter b ~indent father p;
          bp b "\\\\%s  " indent;
          self#html_of_module_type_kind b father ~indent ?modu ?mt k;
        | Module_type_alias a ->
          bs b "<<span class=\"odocwiki_type\"|";
          bs b (self#create_fully_qualified_module_idents_links father a.mta_name);
          bs b ">>"
        | Module_type_with (k, s) ->
          (* TODO: à modifier quand Module_with sera plus détaillé *)
          let s2 = self#create_fully_qualified_module_idents_links father s in
          let s3, _ = newline_to_indented_br ~indent:(indent ^ "  ") ~indent_first:false s2 in
          (
            try ignore (String.index s2 '\n'); bp b "\\\\%s  " indent
            with Not_found -> ()
          );
          self#html_of_module_type_kind b father ~indent ?modu ?mt k;
          bp b "<<span class=\"odocwiki_type\"|%s%s>>" indent s3
        | Module_type_typeof s ->
          bs b "<<span class=\"odocwiki_type\"|module type of ";
          bs b (self#create_fully_qualified_module_idents_links father s);
          bs b ">>"

      (** Print html code to display the type of a module parameter.. *)
      method html_of_module_parameter_type ~init_len ~indent b m_name p =
        match p.mp_type with
        | None -> ()
        | Some typ ->
          self#html_of_module_type
            b m_name ~code:p.mp_type_code ~indent ~init_len typ

      (** Generate a file containing the module type in the given file name. *)
      method output_module_type in_title file mtyp =
        let s = Odoc_info.remove_ending_newline (Odoc_info.string_of_module_type ~complete: true mtyp) in
        self#output_code in_title file s

      (** Generate a file containing the class type in the given file name. *)
      method output_class_type in_title file ctyp =
        let s = Odoc_info.remove_ending_newline (Odoc_info.string_of_class_type ~complete: true ctyp) in
        self#output_code in_title file s

      (** Print html code for a value. *)
      method html_of_value b ?(pre = true) ~indent v =
        Odoc_info.reset_type_names ();
        bp b "<<%s id=\"%s\" class=\"ocsforge_color odocwiki_code\"|"
          (if pre then "pre" else "span") (Naming.value_target v);
        bs b indent;
        bs b (self#keyword "val");
        bs b " ";
        (
          match v.val_code with
            None -> bp b "<<span class=\"odocwiki_name\"|%s>>" (self#escape (Name.simple v.val_name))
          | Some c ->
            let file = Naming.file_code_value_complete_target v in
            self#output_code v.val_name (Filename.concat !Conf.target_dir file) c;
            bp b "<<a_api_code%s | value %s >>" (get_subproject ()) v.val_name
        );
        bs b (" "^(self#delimiter "~:")^" ");
        self#html_of_type_expr b
          ~indent:(indent ^ "  ")
          ~init_len:(7+String.length (Name.simple v.val_name))
          (Name.father v.val_name) v.val_type;
        bs b ">>";
        self#html_of_info b v.val_info;
        (
          if !Conf.with_parameter_list then
            self#html_of_parameter_list b (Name.father v.val_name) v.val_parameters
          else
            self#html_of_described_parameter_list b (Name.father v.val_name) v.val_parameters
        )

      (** Print html code for an exception. *)
      method html_of_exception b ?(pre = true) ~indent e =
        Odoc_info.reset_type_names ();
        bp b "<<%s id=\"%s\" class=\"ocsforge_color odocwiki_code\"|"
          (if pre then "pre" else "span") (Naming.exception_target e);
        bs b indent;
        bs b (self#keyword "exception");
        bs b " ";
        bp b "<<span class=\"odocwiki_name\"|%s>>" (Name.simple e.ex_name);
        self#html_of_type_constructor
          b
          ~indent:(indent^"  ")
          ~init_len:(14+String.length (Name.simple e.ex_name))
          ~father:(Name.father e.ex_name)
          e.ex_args ;
        (
          match e.ex_alias with
            None -> ()
          | Some ea ->
            bs b (" "^(self#delimiter "=")^" ");
            (
              match ea.ea_ex with
                None -> bs b ea.ea_name
              | Some e -> bp b "<<a_api%s | exception %s >>" (get_subproject ()) e.ex_name
            )
        );
        bs b ">>";
        self#html_of_info b e.ex_info

      method private html_of_type_constructor b ~indent ~init_len ~father = function
        | Cstr_tuple [] -> ()
        | Cstr_tuple l ->
          bs b (" "^(self#keyword "of")^" ");
          self#html_of_type_expr_list
            ~indent
            ~init_len
            ~par:false b father " * " l
        | Cstr_record l ->
          bs b (" "^(self#keyword "of")^" ");
          self#html_of_record
            ~indent
            ~father b l

      (** Print html code for a type extension. *)
      method html_of_type_extension b ~indent ?(pre = true) t =
        let father = Name.father t.te_type_name in
        Odoc_info.reset_type_names ();
        bp b "<<%s class=\"ocsforge_color odocwiki_code\" id=\"%s\"|"
          (if pre then "pre" else "span")
          (self#create_fully_qualified_idents_links indent t.te_type_name);
        bs b indent;
        bs b ((self#keyword "type")^" ");
        bp b "<<span class=\"odocwiki_name\"|%s>>" (self#escape (Name.simple t.te_type_name));
        let priv = t.te_private = Asttypes.Private in
        bs b (" "^(self#delimiter "~+=")^" ");
        if priv then bs b "private ";
        let print_one constr =
          bs b "<<span class=\"odocwiki_variant\"|";
          bs b "<<span class=\"odocwiki_variant_constr\"|";
          bs b indent;
          bs b ((self#keyword " ~|")^" ");
          bs b (self#constructor constr.xt_name);
          let init_len = 7 + String.length constr.xt_name in
          self#html_of_type_constructor
            b ~indent:(indent^"    ") ~init_len ~father
            constr.xt_args ;
          bs b ">>";
          (
            match constr.xt_text with
              None -> ()
            | Some t ->
              bs b "<<span class=\"odocwiki_comments\"|";
              bp b "<<span class=\"odocwiki_comments_open\"|(*>> <<span|";
              self#html_of_info b (Some t);
              bp b ">><<span class=\"odocwiki_comments_close\"| ~*)>>";
              bs b ">>";
          );
          bs b ">>"
        in
        bs b "<<span class=\"odocwiki_variants\"|";
        Odoc_html.print_concat b "" print_one t.te_constructors;
        bs b ">>";

      method private html_of_record b ~indent ~father l =
        bs b (self#delimiter "~{" ^ "\n");
        let print_one r =
          bs b "<<span class=\"odocwiki_field\"|";
          bs b "<<span class=\"odocwiki_field_descr\"|  ";
          bs b indent;
          if r.rf_mutable then bs b (self#keyword "mutable ") ;
          bp b "<<span class=\"ocsforge_color_label\"|%s:>> " r.rf_name;
          let init_len = 4 + String.length r.rf_name + if r.rf_mutable then 8 else 0 in
          self#html_of_type_expr b ~indent:(indent ^ "    ") ~init_len father r.rf_type;
          bs b (self#delimiter "~;" ^ " >>");
          (
            match r.rf_text with
              None -> ()
            | Some t ->
              bs b "<<span class=\"odocwiki_comments\"|";
              bp b "<<span class=\"odocwiki_comments_open\"|(*>> <<span|";
              self#html_of_info b (Some t);
              bp b ">><<span class=\"odocwiki_comments_close\"| ~*)>>";
              bs b ">>";
          );
          bs b ">>"
        in
        bs b "<<span class=\"odocwiki_record\"|";
        Odoc_html.print_concat b "" print_one l;
        bs b (">>" ^ self#delimiter "~}")

        (** Print html code for a type. *)
      method html_of_type b ~indent ?(pre = true) t =
        Odoc_info.reset_type_names ();
        let father = Name.father t.ty_name in
        bp b "<<%s class=\"ocsforge_color odocwiki_code\" id=\"%s\"|"
          (if pre then "pre" else "span") (Naming.type_target t);
        bs b indent;
        bs b ((self#keyword "type")^" ");
        let init_len =
          self#html_of_type_expr_param_list b
            ~indent:(indent ^ "  ") ~init_len:5 father t in
        let init_len =
          init_len + (match t.ty_parameters with [] -> 0 | _ -> bs b " "; 1) in
        bp b "<<span class=\"odocwiki_name\"|%s>>" (self#escape (Name.simple t.ty_name));
        let priv = t.ty_private = Asttypes.Private in
        (
          match t.ty_manifest with
            None -> ()
          | Some (Other typ)  ->
            bs b (" "^(self#delimiter "~=")^" ");
            let init_len =
              init_len + if priv then (bs b "private "; 11) else 3 in
            self#html_of_type_expr b ~indent:(indent ^ "  ") ~init_len father typ;
            bs b " "
          | Some (Object_type ltyp)  ->
            bs b (" "^(self#delimiter "~=")^" ");
            let init_len =
              init_len + if priv then (bs b "private "; 11) else 3 in
            List.iter (fun t ->
              self#html_of_type_expr
                b ~indent:(indent ^ "  ") ~init_len father t.of_type)
              ltyp ;
            bs b " "
        );
        (match t.ty_kind with
           Type_abstract -> ()
         | Type_open ->
           bs b (" "^(self#delimiter "~=")^" ..")
         | Type_variant l ->
           bs b (" "^(self#delimiter "~=")^" ");
           if priv then bs b "private ";
           let print_one constr =
             bs b "<<span class=\"odocwiki_variant\"|";
             bs b "<<span class=\"odocwiki_variant_constr\"|";
             bs b indent;
             bs b ((self#keyword " ~|")^" ");
             bs b (self#constructor constr.vc_name);
             let init_len = 7 + String.length constr.vc_name in
             self#html_of_type_constructor
               b ~indent:(indent^"    ") ~init_len ~father
               constr.vc_args ;
             bs b ">>";
             (
               match constr.vc_text with
                 None -> ()
               | Some t ->
                 bs b "<<span class=\"odocwiki_comments\"|";
                 bp b "<<span class=\"odocwiki_comments_open\"|(*>> <<span|";
                 self#html_of_info b (Some t);
                 bp b ">><<span class=\"odocwiki_comments_close\"| ~*)>>";
                 bs b ">>";
             );
             bs b ">>"
           in
           bs b "<<span class=\"odocwiki_variants\"|";
           Odoc_html.print_concat b "" print_one l;
           bs b ">>";

         | Type_record l ->
           bs b (" "^(self#delimiter "~=")^" ");
           if priv then bs b "private " ;
           self#html_of_record b indent father l ;
        );
        bs b ">>";
        self#html_of_info b t.ty_info;
        bs b ""

      (** Print html code for a class attribute. *)
      method html_of_attribute b ~indent a =
        let module_name = Name.father (Name.father a.att_value.val_name) in
        bs b "<<pre class=\"ocsforge_color odocwiki_code\"|" ;
        bs b indent;
        bp b "<<span id=\"%s\"|" (Naming.attribute_target a);
        bs b (self#keyword "val");
        bs b " ";
        let init_len = String.length indent + 4 in
        if a.att_virtual then bs b ((self#keyword "virtual")^ " ");
        let init_len = init_len + if a.att_virtual then 8 else 0 in
        if a.att_mutable then bs b ((self#keyword Odoc_messages.mutab)^ " ");
        let init_len = init_len + if a.att_mutable then String.length Odoc_messages.mutab + 1 else 0 in
        (
          match a.att_value.val_code with
          | None ->
            bp b "<<span class=\"odocwiki_name\"|%s>>" (Name.simple a.att_value.val_name);
          | Some c ->
            let file = Naming.file_code_attribute_complete_target a in
            self#output_code a.att_value.val_name (Filename.concat !Conf.target_dir file) c;
            bp b "<<a_api_code%s text=\"%s\" | attribute %s >>"
              (get_subproject ())
              (Name.simple a.att_value.val_name) a.att_value.val_name;
        );
        let init_len = init_len + String.length (Name.simple a.att_value.val_name) + 3 in
        bs b ">>";
        bs b (" "^(self#delimiter "~:")^" ");
        self#html_of_type_expr b module_name ~indent ~init_len a.att_value.val_type;
        bs b ">>";
        self#html_of_info b a.att_value.val_info

      (** Print html code for a class method. *)
      method html_of_method b ~indent m =
        let module_name = Name.father (Name.father m.met_value.val_name) in
        bs b "<<pre class=\"ocsforge_color odocwiki_code\"|";
        (* html mark *)
        bs b indent;
        bp b "<<span id=\"%s\"|" (Naming.method_target m);
        bs b ((self#keyword "method")^" ");
        let init_len = String.length indent + 8 in
        if m.met_private then bs b ((self#keyword "private")^" ");
        let init_len = init_len + if m.met_private then 9 else 0 in
        if m.met_virtual then bs b ((self#keyword "virtual")^" ");
        let init_len = init_len + if m.met_virtual then 8 else 0 in
        (
          match m.met_value.val_code with
            None -> bp b "<<span class=\"odocwiki_name\"|%s>>" (Name.simple m.met_value.val_name)
          | Some c ->
            let file = Naming.file_code_method_complete_target m in
            self#output_code m.met_value.val_name (Filename.concat !Conf.target_dir file) c;
            bp b "<<a_api_code%s text=\"%s\" | method %s >>"
              (get_subproject ())
              (Name.simple m.met_value.val_name) m.met_value.val_name;
        );
        let init_len = init_len + String.length (Name.simple m.met_value.val_name) + 3 in
        bs b ">>";
        bs b (" "^(self#delimiter "~:")^" ");
        self#html_of_type_expr b module_name ~indent ~init_len m.met_value.val_type;
        bs b ">>";
        self#html_of_info b m.met_value.val_info;
        (
          if !Conf.with_parameter_list then
            self#html_of_parameter_list b
              module_name m.met_value.val_parameters
          else
            self#html_of_described_parameter_list b
              module_name m.met_value.val_parameters
        )

      (** Print html code for the description of a function parameter. *)
      method html_of_parameter_description b p =
        match Parameter.names p with
          [] ->
          ()
        | name :: [] ->
          (
            (* Only one name, no need for label for the description. *)
            match Parameter.desc_by_name p name with
              None -> ()
            | Some t -> self#html_of_text b t
          )
        | l ->
          (*  A list of names, we display those with a description. *)
          let l2 = List.filter
              (fun n -> (Parameter.desc_by_name p n) <> None)
              l
          in
          let print_one n =
            match Parameter.desc_by_name p n with
              None -> ()
            | Some t ->
              bs b "{{{";
              bs b n;
              bs b "}}} : ";
              self#html_of_text b t
          in
          Odoc_html.print_concat b "\\\\\n" print_one l2

      (** Print html code for a list of parameters. *)
      method html_of_parameter_list b m_name l =
        match l with
          [] -> ()
        | _ ->
          bs b "<<div class=\"odocwiki_param_info\"|";
          (*          bs b "<table border=\"0\" cellpadding=\"3\" width=\"100%\">\n";
                      bs b "<tr>\n<td align=\"left\" valign=\"top\" width=\"1%\">"; *)
          bs b "|@@class=\"odocwiki_paramlist\"@ @class=\"lefttop one\">"; (* FIXME *)
          bs b "**";
          bs b Odoc_messages.parameters;
          bs b ": **|" ;
          (*VVV Warning: table in table now supported for now 20101201 *)
          bs b "<<div|";
          let tableattr = ref "class=\"odocwiki_paramstable\"@ @" in
          let print_one p =
            bs b "|@@";
            bs b !tableattr;
            tableattr := "";
            bs b "class=\"lefttop fifteen code\"@@"; (* FIXME *)
            bs b
              (
                match Parameter.complete_name p with
                  "" -> "?"
                | s -> s
              );
            bs b "|@@class=\"centertop\"@@:|"; (* FIXME *)
            self#html_of_type_expr b ~indent:"" ~init_len:0 (* FIXME *) m_name (Parameter.typ p);
            bs b "\\\\";
            self#html_of_parameter_description b p;
            bs b "|\n";
          in
          List.iter print_one l;
          bs b ">>|>>\n"

      (** Print html code for the parameters which have a name and description. *)
      method html_of_described_parameter_list b m_name l =
        (* get the params which have a name, and at least one name described. *)
        let l2 = List.filter
            (fun p ->
               List.exists
                 (fun n -> (Parameter.desc_by_name p n) <> None)
                 (Parameter.names p))
            l
        in
        let f p =
          bs b "<<div class=\"odocwiki_param_info\"|<<span class=\"code\"|"; (* FIXME *)
          bs b (Parameter.complete_name p);
          bs b ">> : " ;
          self#html_of_parameter_description b p;
          bs b ">>\n"
        in
        List.iter f l2

      (** Print html code for a list of module parameters. *)
      method html_of_module_parameter_list b m_name l =
        match l with
          [] ->
          ()
        | _ ->
          (*          bs b "<table border=\"0\" cellpadding=\"3\" width=\"100%\">\n";
                      bs b "<tr>\n";
                      bs b "<td align=\"left\" valign=\"top\" width=\"1%%\">**";
          *)
          bs b "<<div class=\"odocwiki_module_param_list\" | ";
          bp b "<<span class=\"odocwiki_param_title\" |%s :>>" Odoc_messages.parameters;
          bs b "<<div|\n";
          let tableattr = ref "class=\"odocwiki_paramstable\"@ @" in
          List.iter
            (fun (p, desc_opt) ->
               bs b "|@@";
               bs b !tableattr;
               tableattr := "";
               bs b "class=\"centertop fifteen code\"@@{{{"; (* FIXME *)
               bs b p.mp_name;
               bs b "}}}" ;
               bs b "|@@class=\"centertop\"@@:|";
               self#html_of_module_parameter_type b ~indent:"" ~init_len:0 (* FIXME *) m_name p;
               (
                 match desc_opt with
                   None -> ()
                 | Some t ->
                   bs b "|@@class=\"startcom lefttop\"@@";
                   self#html_of_text b t;
               );
               bs b "|\n" ;
            )
            l;
          bs b ">>>>\n"

      (** Print html code for a module. *)
      method html_of_module b ?(pre = true) ?(info=true) ?(complete=true) ?(with_link=true) ~indent m =
        let father = Name.father m.m_name in
        bp b "<<%s class=\"ocsforge_color odocwiki_code\"|"
          (if pre then "pre" else "span");
        bs b indent;
        bs b ((self#keyword "module")^" ");
        (
          if with_link then
            bp b "<<a_api%s text=\"%s\" | module %s >>"
              (get_subproject ()) (Name.simple m.m_name) m.m_name
          else
            bp b "<<span class=\"ocsforge_color_uid\"|%s>>" (self#constructor (Name.simple m.m_name))
        );
        (
          match m.m_kind with
            Module_functor _ when !Conf.html_short_functors  ->
            ()
          | _ -> bs b (" "^(self#delimiter "~:")^" ")
        );
        self#html_of_module_kind b father ~indent ~modu: m m.m_kind;
        bs b ">>";
        if info then
          (
            if complete then
              self#html_of_info ~indent: false
            else
              self#html_of_info_first_sentence
          ) b m.m_info
        else
          ()

      (** Print html code for a module type. *)
      method html_of_modtype b ?(pre = true) ?(info=true) ?(complete=true) ?(with_link=true) ~indent mt =
        let father = Name.father mt.mt_name in
        bp b "<<%s class=\"ocsforge_color odocwiki_code\"|"
          (if pre then "pre" else "span");
        bs b indent;
        bs b ((self#keyword "module type")^" ");
        (
          if with_link then
            bp b "<<a_api%s text=\"%s\" | module type %s >>"
              (get_subproject ()) (Name.simple mt.mt_name) mt.mt_name
          else
            bp b "<<span class=\"ocsforge_color_uid\"|%s>>" (Name.simple mt.mt_name)
        );
        (match mt.mt_kind with
           None -> ()
         | Some k ->
           bs b (" "^(self#delimiter "~=")^" ");
           self#html_of_module_type_kind b ~indent father ~mt k
        );
        bs b ">>";
        if info then
          (
            if complete then
              self#html_of_info ~indent: false
            else
              self#html_of_info_first_sentence
          ) b mt.mt_info
        else
          ()

      (** Print html code for an included module. *)
      method html_of_included_module b ?(pre = true) ~indent im =
        bp b "<<%s class=\"ocsforge_color odocwiki_code\"|"
          (if pre then "pre" else "span");
        bs b indent;
        bs b ((self#keyword "include")^" ");
        (
          match im.im_module with
            None ->
            bs b im.im_name
          | Some mmt ->
            match mmt with
              Mod m -> bp b "<<a_api%s | module %s >>" (get_subproject ()) m.m_name
            | Modtype mt ->
              bp b "<<a_api%s | module type %s >>" (get_subproject ()) mt.mt_name
        );
        bs b ">>\n";
        self#html_of_info b im.im_info

      method html_of_class_element b ~indent element =
        match element with
          Class_attribute a ->
          self#html_of_attribute b ~indent a
        | Class_method m ->
          self#html_of_method b ~indent m
        | Class_comment t ->
          self#html_of_class_comment b ~indent t

      method html_of_class_kind b father ~indent ~init_len ?cl kind =
        match kind with
          Class_structure (inh, eles) ->
          bs b (self#keyword "object");
          (
            match cl with
              None ->
              bs b "\n";
              (
                match inh with
                  [] -> ()
                | _ ->
                  self#generate_inheritance_info b ~indent:(indent^"  ") inh
              );
              List.iter (self#html_of_class_element ~indent:(indent^"  ") b) eles;
            | Some cl ->
              bp b "<<a_api%s text=\"..\" | class %s >>" (get_subproject ()) cl.cl_name
          );
          bs b (self#keyword "end")

        | Class_apply capp ->
          (* TODO: afficher le type final à partir du typedtree *)
          self#html_of_text b [Raw "class application not handled yet"]

        | Class_constr cco ->
          (
            match cco.cco_type_parameters with
              [] -> ()
            | l ->
              ignore(self#html_of_class_type_param_expr_list b
                  ~indent:(indent^"  ") ~init_len father l);
              bs b " "
          );
          bs b "<<span class=\"odocwiki_type\"|";
          bs b (self#create_fully_qualified_idents_links father cco.cco_name);
          bs b ">>"

        | Class_constraint (ck, ctk) ->
          bs b (self#delimiter "(");
          self#html_of_class_kind b ~indent:(indent^"  ") ~init_len father ck;
          bs b (self#delimiter "~:");
          self#html_of_class_type_kind b ~indent:(indent^"  ") father ~init_len ctk;
          bs b (self#delimiter ")")

      method html_of_class_type_kind b father ~indent ~init_len ?ct kind =
        match kind with
          Class_type cta ->
          (
            match cta.cta_type_parameters with
              [] -> ()
            | l ->
              ignore (self#html_of_class_type_param_expr_list ~indent:(indent^"  ") ~init_len b father l);
              bs b " "
          );
          bs b "<<span class=\"odocwiki_type\"|";
          bs b (self#create_fully_qualified_idents_links father cta.cta_name);
          bs b ">>"

        | Class_signature (inh, eles) ->
          bs b (self#keyword "object");
          (
            match ct with
              None ->
              bs b "\n";
              (
                match inh with
                  [] -> ()
                | _ -> self#generate_inheritance_info ~indent:(indent^"  ") b inh
              );
              List.iter (self#html_of_class_element ~indent:(indent^"  ") b) eles
            | Some ct ->
              bp b "<<a_api%s text=\"..\" | class type %s >>"
                (get_subproject ()) ct.clt_name
          );
          bs b (self#keyword "end")

      (** Print html code for a class. *)
      method html_of_class b ?(pre = true) ?(complete=true) ?(with_link=true) ~indent c =
        let father = Name.father c.cl_name in
        Odoc_info.reset_type_names ();
        bp b "<<%s class=\"ocsforge_color odocwiki_code\"|"
          (if pre then "pre" else "span");
        bs b indent;
        (* we add a html id, the same as for a type so we can
           go directly here when the class name is used as a type name *)
        let name =
          Naming.type_target
            { ty_name = c.cl_name ;
              ty_info = None ; ty_parameters = [] ;
              ty_kind = Type_abstract ; ty_private = Asttypes.Public; ty_manifest = None ;
              ty_loc = Odoc_info.dummy_loc ;
              ty_code = None ;
            } in
        bp b "<<span name=\"%s\"|" name;
        bs b ((self#keyword "class")^" ");
        if c.cl_virtual then bs b ((self#keyword "virtual")^" ");
        let init_len = 6 + String.length name + if c.cl_virtual then 8 else 0 in
        let init_len =
          match c.cl_type_parameters with
          | [] -> init_len
          | l ->
            let init_len =
              self#html_of_class_type_param_expr_list b ~init_len ~indent father l in
            bs b " ";
            init_len + 1
        in
        (
          if with_link then
            bp b "<<a_api%s text=\"%s\" | class %s >>"
              (get_subproject ()) (Name.simple c.cl_name) c.cl_name
          else
            bp b "<<span class=\"ocsforge_color_name\"|%s>>" (Name.simple c.cl_name)
        );
        let init_len = init_len + String.length (Name.simple c.cl_name) + 3 in
        bs b ">>";
        bs b (" "^(self#delimiter "~:")^" ");
        let init_len = self#html_of_class_parameter_list b ~init_len ~indent father c + 1 in
        bs b " ";
        self#html_of_class_kind b father ~init_len ~indent ~cl: c c.cl_kind;
        bs b ">>" ;
        (
          if complete then
            self#html_of_info ~indent: false
          else
            self#html_of_info_first_sentence
        ) b c.cl_info

      (** Print html code for a class type. *)
      method html_of_class_type b ?(pre = true) ?(complete=true) ?(with_link=true) ~indent ct =
        Odoc_info.reset_type_names ();
        let father = Name.father ct.clt_name in
        bs b "<<pre class=\"ocsforge_color odocwiki_code\"|";
        (* we add a html id, the same as for a type so we can
           go directly here when the class type name is used as a type name *)
        bs b indent;
        bp b "<<span id=\"%s\"|"
          (Naming.type_target
             { ty_name = ct.clt_name ;
               ty_info = None ; ty_parameters = [] ;
               ty_kind = Type_abstract ; ty_private = Asttypes.Public; ty_manifest = None ;
               ty_loc = Odoc_info.dummy_loc ;
               ty_code = None ;
             }
          );
        bs b ((self#keyword "class type")^" ");
        let init_len = String.length indent + 11 in
        if ct.clt_virtual then bs b ((self#keyword "virtual")^" ");
        let init_len = init_len + if ct.clt_virtual then 8 else 0 in
        let init_len =
          match ct.clt_type_parameters with
          | [] -> init_len
          | l ->
            self#html_of_class_type_param_expr_list b ~indent ~init_len father l in
        if with_link then
          bp b "<<a_api%s text=\"%s\" | class type %s >>"
            (get_subproject ()) (Name.simple ct.clt_name) ct.clt_name
        else
          bp b  "<<span class=\"ocsforge_color_name\"|%s>>" (Name.simple ct.clt_name);
        bs b ">>";
        bs b (" "^(self#delimiter "~=")^" ");
        let init_len = init_len + 3 + String.length (Name.simple ct.clt_name) in
        self#html_of_class_type_kind b father ~indent ~init_len ~ct ct.clt_kind;
        bs b ">>";
        (
          if complete then
            self#html_of_info ~indent: false
          else
            self#html_of_info_first_sentence
        ) b ct.clt_info

      (** Return html code to represent a dag, represented as in Odoc_dag2html. *)
      method html_of_dag dag =
        let f n =
          let (name, cct_opt) = n.Odoc_dag2html.valu in
          (* if we have a c_opt = Some class then we take its information
             because we are sure the name is complete. *)
          let new_v =
            match cct_opt with (* FIXME *)
              None -> "<<a_api"^get_subproject ()^" class=\"dag\" | class "^name^" >>"
            | Some (Cl c) -> "<<a_api"^get_subproject ()^" class=\"dag\" | class "^c.cl_name^" >>"
            | Some (Cltype (ct, _)) ->
              "<<a_api"^get_subproject ()^" class=\"dag\" | class type "^ct.clt_name^" >>"
          in
          { n with Odoc_dag2html.valu = new_v }
        in
        let a = Array.map f dag.Odoc_dag2html.dag in
        Odoc_dag2html.html_of_dag { Odoc_dag2html.dag = a }

      (** Print html code for a module comment.*)
      method html_of_module_comment b text =
        bs b "\n";
        self#html_of_text b text;
        bs b "\n"

      (** Print html code for a class comment.*)
      method html_of_class_comment b ?indent text =
        (* Add some style if there is no style for the first part of the text. *)
        let text2 =
          match text with
          | (Odoc_info.Raw s) :: q ->
            (Odoc_info.Title (2, None, [Odoc_info.Raw s])) :: q
          | _ -> text
        in
        self#html_of_text b text2

      (** Generate html code for the given list of inherited classes.*)
      method generate_inheritance_info b ~indent inher_l =
        let f inh =
          match inh.ic_class with
            None -> (* we can't make the link. *)
            (Odoc_info.Code inh.ic_name) ::
              (match inh.ic_text with
                 None -> []
               | Some t -> (Odoc_info.Raw indent) :: t)
          | Some cct ->
            (* we can create the link. *)
            let kind, real_name = (* even if it should be the same *)
              match cct with
                Cl c -> Odoc_info.RK_class, c.cl_name
              | Cltype (ct, _) -> Odoc_info.RK_class, ct.clt_name
            in
            (Odoc_info.Ref (real_name, Some kind, None)) ::
              (match inh.ic_text with
                 None -> []
               | Some t -> (Odoc_info.Raw indent) :: t)      in
        let text = [
          Odoc_info.Bold [Odoc_info.Raw Odoc_messages.inherits] ;
          Odoc_info.List (List.map f inher_l)
        ]
        in
        self#html_of_text b text

      (** Generate html code for the inherited classes of the given class. *)
      method generate_class_inheritance_info b ~indent cl =
        let rec iter_kind k =
          match k with
            Class_structure ([], _) ->
            ()
          | Class_structure (l, _) ->
            self#generate_inheritance_info ~indent b l
          | Class_constraint (k, ct) ->
            iter_kind k
          | Class_apply _
          | Class_constr _ ->
            ()
        in
        iter_kind cl.cl_kind

      (** Generate html code for the inherited classes of the given class type. *)
      method generate_class_type_inheritance_info b ~indent clt =
        match clt.clt_kind with
          Class_signature ([], _) ->
          ()
        | Class_signature (l, _) ->
          self#generate_inheritance_info ~indent b l
        | Class_type _ ->
          ()

      (** A method to create index files. *)
      method generate_elements_index :
        'a.
          string ->
        'a list ->
        ('a -> Odoc_info.Name.t) ->
        ('a -> Odoc_info.info option) ->
        ('a -> string) -> string -> string -> unit =
        fun target_kind elements name info target title simple_file ->
          try
            let chanout = open_out (Filename.concat !Conf.target_dir simple_file) in
            let b = new_buf () in
            bp b "=%s=\n" title;

            let sorted_elements = List.sort
                (fun e1 e2 -> compare (Name.simple (name e1)) (Name.simple (name e2)))
                elements
            in
            let groups = Odoc_info.create_index_lists sorted_elements (fun e -> Name.simple (name e)) in
            let f_ele e =
              let simple_name = Name.simple (name e) in
              let father_name = Name.father (target e) in
              bp b "|@@class=\"odocwiki_index\"@ @@<<a_api%s text=\"%s\"| %s %s >> "
                (get_subproject ()) simple_name target_kind (target e);
              if simple_name <> father_name && father_name <> "" then
                bp b "~[<<a_api%s | module %s >>~]"
                  (get_subproject ()) father_name;
              bs b "|";
              self#html_of_info_first_sentence b (info e);
              bs b "|\n";
            in
            let f_group l =
              match l with
                [] -> ()
              | e :: _ ->
                let s =
                  match (Char.uppercase_ascii (Name.simple (name e)).[0]) with
                    'A'..'Z' as c -> String.make 1 c
                  | _ -> ""
                in
                bs b "|@@ @style=\"tet-align: left;\"@@\\\\";
                bs b s ;
                bs b "|\n" ;
                List.iter f_ele l
            in
            bs b "\n";
            List.iter f_group groups ;
            bs b "\\\\\n" ;
            Buffer.output_buffer chanout b;
            close_out chanout
          with
            Sys_error s ->
            raise (Failure s)

      (** A method to generate a list of module/class files. *)
      method generate_elements :
        'a. ('a option -> 'a option -> 'a -> unit) -> 'a list -> unit =
        fun f_generate l ->
          let rec iter pre_opt = function
              [] -> ()
            | ele :: [] -> f_generate pre_opt None ele
            | ele1 :: ele2 :: q ->
              f_generate pre_opt (Some ele2) ele1 ;
              iter (Some ele1) (ele2 :: q)
          in
          iter None l

      (** Generate the code of the html page for the given class.*)
      method generate_for_class pre post cl =
        Odoc_info.reset_type_names ();
        let wiki_file = Naming.wiki_file cl.cl_name in
        let type_file = Naming.file_type_class_complete_target cl.cl_name in
        try
          let chanout = open_out (Filename.concat !Conf.target_dir wiki_file) in
          let b = new_buf () in
          bs b "=";
          bs b (Odoc_messages.clas^" ");
          if cl.cl_virtual then bs b "virtual " ;
          bp b "<<a_api_type%s | class %s >> "(get_subproject ()) cl.cl_name;
          bs b "=\n";
          self#html_of_class b ~with_link: false ~indent:"" cl;
          (* parameters *)
          self#html_of_described_parameter_list b
            (Name.father cl.cl_name) cl.cl_parameters;
          (* class inheritance *)
          self#generate_class_inheritance_info b ~indent:"" cl;
          (* a horizontal line *)
          bs b "\n----\n";
          (* the various elements *)
          List.iter (self#html_of_class_element ~indent:"" b)
            (Class.class_elements ~trans:false cl);
          Buffer.output_buffer chanout b;
          close_out chanout;

          (* generate the file with the complete class type *)
          self#output_class_type
            cl.cl_name
            (Filename.concat !Conf.target_dir type_file)
            cl.cl_type
        with
          Sys_error s ->
          raise (Failure s)

      (** Generate the code of the html page for the given class type.*)
      method generate_for_class_type pre post clt =
        Odoc_info.reset_type_names ();
        let wiki_file = Naming.wiki_file clt.clt_name in
        let type_file = Naming.file_type_class_complete_target clt.clt_name in
        try
          let chanout = open_out (Filename.concat !Conf.target_dir wiki_file) in
          let b = new_buf () in
          bs b "=";
          bs b (Odoc_messages.class_type^" ");
          if clt.clt_virtual then bs b "virtual ";
          bp b "<<a_api_type%s | class type %s >> "(get_subproject ()) clt.clt_name;
          bs b "=\n";
          self#html_of_class_type b ~with_link: false ~indent:""clt;

          (* class inheritance *)
          self#generate_class_type_inheritance_info b ~indent:"" clt;
          (* a horizontal line *)
          bs b "\n----\n";
          (* the various elements *)
          List.iter (self#html_of_class_element ~indent:"" b)
            (Class.class_type_elements ~trans: false clt);
          Buffer.output_buffer chanout b;
          close_out chanout;

          (* generate the file with the complete class type *)
          self#output_class_type
            clt.clt_name
            (Filename.concat !Conf.target_dir type_file)
            clt.clt_type
        with
          Sys_error s ->
          raise (Failure s)

      (** Generate the html file for the given module type.
          @raise Failure if an error occurs.*)
      method generate_for_module_type pre post mt =
        try
          let wiki_file = Naming.wiki_file mt.mt_name in
          let type_file = Naming.file_type_module_complete_target mt.mt_name in
          let chanout = open_out (Filename.concat !Conf.target_dir wiki_file) in
          let b = new_buf () in
          bp b "=";
          bs b (Odoc_messages.module_type^" ");
          (
            match mt.mt_type with
              Some _ ->
              bp b "<<a_api_type%s | module type %s >> " (get_subproject ()) mt.mt_name
            | None-> bs b mt.mt_name
          );
          bs b "=\n" ;
          self#html_of_modtype b ~with_link: false ~indent:"" mt;

          (* parameters for functors *)
          self#html_of_module_parameter_list b
            (Name.father mt.mt_name)
            (Module.module_type_parameters mt);
          (* a horizontal line *)
          bs b "\n----\n";
          (* module elements *)
          List.iter
            (self#html_of_module_element b ~indent:"" (Name.father mt.mt_name))
            (Module.module_type_elements mt);

          Buffer.output_buffer chanout b;
          close_out chanout;

          (* generate html files for submodules *)
          self#generate_elements self#generate_for_module (Module.module_type_modules mt);
          (* generate html files for module types *)
          self#generate_elements self#generate_for_module_type (Module.module_type_module_types mt);
          (* generate html files for classes *)
          self#generate_elements self#generate_for_class (Module.module_type_classes mt);
          (* generate html files for class types *)
          self#generate_elements self#generate_for_class_type (Module.module_type_class_types mt);

          (* generate the file with the complete module type *)
          (
            match mt.mt_type with
              None -> ()
            | Some mty ->
              self#output_module_type
                mt.mt_name
                (Filename.concat !Conf.target_dir type_file)
                mty
          )
        with
          Sys_error s ->
          raise (Failure s)

      (** Generate the html file for the given module.
          @raise Failure if an error occurs.*)
      method generate_for_module pre post modu =
        try
          Odoc_info.verbose ("Generate for module "^modu.m_name);
          let wiki_file = Naming.wiki_file modu.m_name in
          let type_file = Naming.file_type_module_complete_target modu.m_name in
          let code_file = Naming.file_code_module_complete_target modu.m_name in
          let chanout = open_out (Filename.concat !Conf.target_dir wiki_file) in
          let b = new_buf () in
          bs b "=";
          if modu.m_text_only then
            bs b modu.m_name
          else
            (
              bs b
                (
                  if Module.module_is_functor modu then
                    Odoc_messages.functo
                  else
                    Odoc_messages.modul
                );
              bp b " <<a_api_type%s | module %s >> " (get_subproject ()) modu.m_name;
              (
                match modu.m_code with
                  None -> ()
                | Some _ ->
                  bp b " <<a_api_code%s text=\".ml\" | module %s >> "
                    (get_subproject ()) modu.m_name
              )
            );
          bs b "=\n";

          if not modu.m_text_only then self#html_of_module b ~with_link: false ~indent:"" modu;

          (* parameters for functors *)
          self#html_of_module_parameter_list b
            (Name.father modu.m_name)
            (Module.module_parameters modu);

          (* a horizontal line *)
          if not modu.m_text_only then bs b "\n----\n";

          (* module elements *)
          List.iter
            (self#html_of_module_element b ~indent:"" (Name.father modu.m_name))
            (Module.module_elements modu);

          Buffer.output_buffer chanout b;
          close_out chanout;

          (* generate html files for submodules *)
          self#generate_elements  self#generate_for_module (Module.module_modules modu);
          (* generate html files for module types *)
          self#generate_elements  self#generate_for_module_type (Module.module_module_types modu);
          (* generate html files for classes *)
          self#generate_elements  self#generate_for_class (Module.module_classes modu);
          (* generate html files for class types *)
          self#generate_elements  self#generate_for_class_type (Module.module_class_types modu);

          (* generate the file with the complete module type *)
          self#output_module_type
            modu.m_name
            (Filename.concat !Conf.target_dir type_file)
            modu.m_type;

          match modu.m_code with
            None -> ()
          | Some code ->
            self#output_code
              modu.m_name
              (Filename.concat !Conf.target_dir code_file)
              code
        with
          Sys_error s ->
          raise (Failure s)

      (** Generate the [<index_prefix>.html] file corresponding to the given module list.
          @raise Failure if an error occurs.*)
      method generate_index module_list =
        try
          let chanout = open_out (Filename.concat !Conf.target_dir self#index) in
          let b = new_buf () in
          let title = match !Conf.title with None -> "" | Some t -> self#escape t in
          bs b "=";
          bs b title;
          bs b "=\n" ;
          let info = Odoc_info.apply_opt
              (Odoc_info.info_of_comment_file module_list)
              !Conf.intro_file
          in
          (
            match info with
              None ->
              self#html_of_Index_list b;
              bs b "\\";
              self#html_of_Module_list b
                (List.map (fun m -> m.m_name) module_list);
            | Some i -> self#html_of_info ~indent: false b info
          );
          Buffer.output_buffer chanout b;
          close_out chanout
        with
          Sys_error s ->
          raise (Failure s)

      (** Generate the [<index_prefix>.html] file corresponding to the given module list.
          @raise Failure if an error occurs.*)
      method generate_menu module_list =
        try
          let chanout = open_out (Filename.concat !Conf.target_dir self#menu) in
          let b = new_buf () in
          let current_level = ref "" in
          let module M = Odoc_info in
          let rec wiki_of_text_element parent = function
            | M.Module_list s ->
              let sub = match !subproject with None -> "" | Some d -> d ^ "/" in
              List.iter (fun s -> bp b "=%s[[%s%s|%s]]\n" !current_level sub s s) s;
            | M.Title (level, label, text) ->
              let level = String.make level '=' in
              current_level := level;
              bs b level;
              begin match label with
                | Some s -> bp b "@@label=\"%s\"@@" s
                | _ -> ()
              end;
              List.iter (wiki_of_text_element `Title) text;
            | M.Raw s when parent = `Title ->
              bs b (self#escape s); bs b "\n"
            | M.Latex s when parent = `Title ->
              bs b s; bs b "\n"
            | M.Index_list ->
              let index_if_not_empty l url m =
                match l with
                |        [] -> ()
                | _ ->
                  bp b "=%s <<a_api%s text=%S | index %s >>\n" !current_level
                    (get_subproject ()) m url
              in
              index_if_not_empty self#list_types "types" Odoc_messages.index_of_types;
              index_if_not_empty self#list_exceptions "exceptions" Odoc_messages.index_of_exceptions;
              index_if_not_empty self#list_values "values" Odoc_messages.index_of_values;
              index_if_not_empty self#list_attributes "attributes" Odoc_messages.index_of_attributes;
              index_if_not_empty self#list_methods "methods" Odoc_messages.index_of_methods;
              index_if_not_empty self#list_classes "classes" Odoc_messages.index_of_classes;
              index_if_not_empty self#list_class_types "class types" Odoc_messages.index_of_class_types;
              index_if_not_empty self#list_modules "modules" Odoc_messages.index_of_modules;
              index_if_not_empty self#list_module_types "module types" Odoc_messages.index_of_module_types
            | _ -> ()
          in
          let info = Odoc_info.apply_opt
              (Odoc_info.info_of_comment_file module_list)
              !Conf.intro_file
          in
          (
            match info with
            | None | Some { M.i_desc = None; } -> ()
            | Some { M.i_desc = Some d; } when d = [M.Raw ""] -> ()
            | Some { M.i_desc = Some d; } ->
              List.iter (wiki_of_text_element `None) d
          );
          Buffer.output_buffer chanout b;
          close_out chanout
        with
          Sys_error s ->
          raise (Failure s)

      (** Generate the values index in the file [index_values.html]. *)
      method generate_values_index module_list =
        self#generate_elements_index
          "value"
          self#list_values
          (fun v -> v.val_name)
          (fun v -> v.val_info)
          (fun v -> v.val_name)
          Odoc_messages.index_of_values
          self#index_values

      (** Generate the exceptions index in the file [index_exceptions.html]. *)
      method generate_exceptions_index module_list =
        self#generate_elements_index
          "exception"
          self#list_exceptions
          (fun e -> e.ex_name)
          (fun e -> e.ex_info)
          (fun e -> e.ex_name)
          Odoc_messages.index_of_exceptions
          self#index_exceptions

      (** Generate the types index in the file [index_types.html]. *)
      method generate_types_index module_list =
        self#generate_elements_index
          "type"
          self#list_types
          (fun t -> t.ty_name)
          (fun t -> t.ty_info)
          (fun t -> t.ty_name)
          Odoc_messages.index_of_types
          self#index_types

      (** Generate the attributes index in the file [index_attributes.html]. *)
      method generate_attributes_index module_list =
        self#generate_elements_index
          "attr"
          self#list_attributes
          (fun a -> a.att_value.val_name)
          (fun a -> a.att_value.val_info)
          (fun a ->
             let s = Bytes.of_string a.att_value.val_name in
             Bytes.set s (Bytes.rindex s '.') '#';
             Bytes.to_string s
          )
          Odoc_messages.index_of_attributes
          self#index_attributes

      (** Generate the methods index in the file [index_methods.html]. *)
      method generate_methods_index module_list =
        self#generate_elements_index
          "method"
          self#list_methods
          (fun m -> m.met_value.val_name)
          (fun m -> m.met_value.val_info)
          (fun m ->
             let s = Bytes.of_string m.met_value.val_name in
             Bytes.set s (Bytes.rindex s '.') '#';
             Bytes.to_string s
          )
          Odoc_messages.index_of_methods
          self#index_methods

      (** Generate the classes index in the file [index_classes.html]. *)
      method generate_classes_index module_list =
        self#generate_elements_index
          "class"
          self#list_classes
          (fun c -> c.cl_name)
          (fun c -> c.cl_info)
          (fun c -> c.cl_name)
          Odoc_messages.index_of_classes
          self#index_classes

      (** Generate the class types index in the file [index_class_types.html]. *)
      method generate_class_types_index module_list =
        self#generate_elements_index
          "class type"
          self#list_class_types
          (fun ct -> ct.clt_name)
          (fun ct -> ct.clt_info)
          (fun ct -> ct.clt_name)
          Odoc_messages.index_of_class_types
          self#index_class_types

      (** Generate the modules index in the file [index_modules.html]. *)
      method generate_modules_index module_list =
        self#generate_elements_index
          "module"
          self#list_modules
          (fun m -> m.m_name)
          (fun m -> m.m_info)
          (fun m -> m.m_name)
          Odoc_messages.index_of_modules
          self#index_modules

      (** Generate the module types index in the file [index_module_types.html]. *)
      method generate_module_types_index module_list =
        self#generate_elements_index
          "module type"
          self#list_module_types
          (fun mt -> mt.mt_name)
          (fun mt -> mt.mt_info)
          (fun mt -> mt.mt_name)
          Odoc_messages.index_of_module_types
          self#index_module_types

      (** Generate all the html files from a module list. The main
          file is [<index_prefix>.html]. *)
      method generate module_list =
        (* init the lists of elements *)
        list_values <- Odoc_info.Search.values module_list ;
        list_exceptions <- Odoc_info.Search.exceptions module_list ;
        list_types <- Odoc_info.Search.types module_list ;
        list_attributes <- Odoc_info.Search.attributes module_list ;
        list_methods <- Odoc_info.Search.methods module_list ;
        list_classes <- Odoc_info.Search.classes module_list ;
        list_class_types <- Odoc_info.Search.class_types module_list ;
        list_modules <- Odoc_info.Search.modules module_list ;
        list_module_types <- Odoc_info.Search.module_types module_list ;

        (* Get the names of all known types. *)
        let types = Odoc_info.Search.types module_list in
        known_types_names <-
          List.fold_left
            (fun acc t -> StringSet.add t.ty_name acc)
            known_types_names
            types ;
        (* Get the names of all class and class types. *)
        let classes = Odoc_info.Search.classes module_list in
        let class_types = Odoc_info.Search.class_types module_list in
        known_classes_names <-
          List.fold_left
            (fun acc c -> StringSet.add c.cl_name acc)
            known_classes_names
            classes ;
        known_classes_names <-
          List.fold_left
            (fun acc ct -> StringSet.add ct.clt_name acc)
            known_classes_names
            class_types ;
        (* Get the names of all known modules and module types. *)
        let module_types = Odoc_info.Search.module_types module_list in
        let modules = Odoc_info.Search.modules module_list in
        known_modules_names <-
          List.fold_left
            (fun acc m -> StringSet.add m.m_name acc)
            known_modules_names
            modules ;
        known_modules_names <-
          List.fold_left
            (fun acc mt -> StringSet.add mt.mt_name acc)
            known_modules_names
            module_types ;
        (* generate html for each module *)
        if not !Conf.index_only then
          self#generate_elements self#generate_for_module module_list ;

        try
          self#generate_index module_list;
          self#generate_values_index module_list ;
          self#generate_exceptions_index module_list ;
          self#generate_types_index module_list ;
          self#generate_attributes_index module_list ;
          self#generate_methods_index module_list ;
          self#generate_classes_index module_list ;
          self#generate_class_types_index module_list ;
          self#generate_modules_index module_list ;
          self#generate_module_types_index module_list ;
          self#generate_menu module_list;
        with
          Failure s ->
          prerr_endline s ;
          incr Odoc_info.errors

      initializer
        Odoc_ocamlhtml.html_of_comment :=
          (fun s ->
             let b = new_buf () in
             self#html_of_text b (Odoc_text.Texter.text_of_string s);
             Buffer.contents b
          )
    end

  class generator =
    object
      val wiki = new wiki
      method generate = wiki#generate
    end

end


module type Wiki_generator = module type of Generator
let doc_generator = (module Generator : Odoc_gen.Base)
let _ = Odoc_args.set_generator (Odoc_gen.Base (module Generator : Odoc_gen.Base))
