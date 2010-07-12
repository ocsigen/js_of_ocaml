(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(* modif for <div> files by Ocsigen team                               *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)



(** Generation of html documentation of type <div>. *)

open Odoc_info
open Parameter
open Value
open Type
open Exception
open Class
open Module

let new_buf = Odoc_html.new_buf
let bs = Odoc_html.bs
let bp = Odoc_html.bp
let opt = Odoc_html.opt


module Naming = Odoc_html.Naming

let newline_to_indented_br s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      '\n' -> Buffer.add_string b "<br/>     "
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b



(** class for generating a <div> html documentation.*)
class div =
  object (self)
    inherit Odoc_html.html as html

    (** Output the given ocaml code to the given file name. *)
    method private output_code in_title file code =
      try
        let chanout = open_out file in
        let b = new_buf () in
        self#html_of_code b code;
        Buffer.output_buffer chanout b;
        close_out chanout
      with
        Sys_error s ->
          incr Odoc_info.errors ;
          prerr_endline s

    (** A method to create index files. *)
    method generate_elements_index :
        'a.
        'a list ->
          ('a -> Odoc_info.Name.t) ->
            ('a -> Odoc_info.info option) ->
              ('a -> string) -> string -> string -> unit =
    fun elements name info target title simple_file ->
      try
        let chanout = open_out (Filename.concat !Args.target_dir simple_file) in
        let b = new_buf () in
        bs b "<center><h1>";
        bs b title;
        bs b "</h1></center>\n" ;

        let sorted_elements = List.sort
            (fun e1 e2 -> compare (Name.simple (name e1)) (Name.simple (name e2)))
            elements
        in
        let groups = Odoc_info.create_index_lists sorted_elements (fun e -> Name.simple (name e)) in
        let f_ele e =
          let simple_name = Name.simple (name e) in
          let father_name = Name.father (name e) in
          bp b "<tr><td><a href=\"%s\">%s</a> " (target e) (self#escape simple_name);
          if simple_name <> father_name && father_name <> "" then
            bp b "[<a href=\"%s\">%s</a>]" (fst (Naming.html_files father_name)) father_name;
          bs b "</td>\n<td>";
          self#html_of_info_first_sentence b (info e);
          bs b "</td></tr>\n";
        in
        let f_group l =
          match l with
            [] -> ()
          | e :: _ ->
              let s =
                match (Char.uppercase (Name.simple (name e)).[0]) with
                  'A'..'Z' as c -> String.make 1 c
                | _ -> ""
              in
              bs b "<tr><td align=\"left\"><br/>";
              bs b s ;
              bs b "</td></tr>\n" ;
              List.iter f_ele l
        in
        bs b "<table>\n";
        List.iter f_group groups ;
        bs b "</table><br/>";
        Buffer.output_buffer chanout b;
        close_out chanout
      with
        Sys_error s ->
          raise (Failure s)


    method html_of_Module_list b l =
      bs b "<br/>\n<table class=\"indextable\">\n";
      List.iter
        (fun name ->
	  bs b "<tr><td>";
	  (
	   try
	     let m = 
	       List.find (fun m -> m.m_name = name) self#list_modules 
	     in
	     let (html, _) = Naming.html_files m.m_name in
	     bp b "<a href=\"%s\">%s</a></td>" html m.m_name;
	     bs b "<td>";
	     self#html_of_info_first_sentence b m.m_info;
	   with
	     Not_found ->
	       Odoc_messages.pwarning (Odoc_messages.cross_module_not_found name);
	       bp b "%s</td><td>" name
	  );
	  bs b "</td></tr>\n"
	)
        l;
      bs b "</table>\n</body>\n</html>";

    method html_of_Index_list b =
      let index_if_not_empty l url m =
        match l with
          [] -> ()
        | _ -> bp b "<a href=\"%s\">%s</a><br/>\n" url m
      in
      index_if_not_empty self#list_types self#index_types Odoc_messages.index_of_types;
      index_if_not_empty self#list_exceptions self#index_exceptions Odoc_messages.index_of_exceptions;
      index_if_not_empty self#list_values self#index_values Odoc_messages.index_of_values;
      index_if_not_empty self#list_attributes self#index_attributes Odoc_messages.index_of_attributes;
      index_if_not_empty self#list_methods self#index_methods Odoc_messages.index_of_methods;
      index_if_not_empty self#list_classes self#index_classes Odoc_messages.index_of_classes;
      index_if_not_empty self#list_class_types self#index_class_types Odoc_messages.index_of_class_types;
      index_if_not_empty self#list_modules self#index_modules Odoc_messages.index_of_modules;
      index_if_not_empty self#list_module_types self#index_module_types Odoc_messages.index_of_module_types

    (** Print html for an author list. *)
    method html_of_author_list b l =
      match l with
        [] -> ()
      | _ ->
          bp b "<b>%s:</b> %s<br/>\n"
	    Odoc_messages.authors
            (String.concat ", " l)

    (** Print html code for the given optional version information.*)
    method html_of_version_opt b v_opt =
      match v_opt with
        None -> ()
      | Some v -> 
	   bp b "<b>%s:</b> %s<br/>\n" Odoc_messages.version v

    (** Print html code for the given optional since information.*)
    method html_of_since_opt b s_opt =
      match s_opt with
        None -> ()
      | Some s -> 
	  bp b "<b>%s</b> %s<br/>\n" Odoc_messages.since s

    (** Print html code for the given list of raised exceptions.*)
    method html_of_raised_exceptions b l =
      match l with
        [] -> ()
      | (s, t) :: [] -> 
	  bp b "<b>%s</b> <code>%s</code> "
	    Odoc_messages.raises
	    s;
	  self#html_of_text b t;
	  bs b "<br/>\n"
      | _ ->
          bp b "<b>%s</b><ul>" Odoc_messages.raises;
	  List.iter
            (fun (ex, desc) -> 
	      bp b "<li><code>%s</code> " ex ;
	      self#html_of_text b desc;
	      bs b "</li>\n"
	    )
            l;
          bs b "</ul>\n"

    (** Print html code for the given list of "see also" references.*)
    method html_of_sees b l =
      match l with
        [] -> ()
      | see :: [] -> 
	  bp b "<b>%s</b> " Odoc_messages.see_also;
	  self#html_of_see b see;
	  bs b "<br/>\n"
      | _ ->
          bp b "<b>%s</b><ul>" Odoc_messages.see_also;
          List.iter
            (fun see -> 
	      bs b "<li>" ;
	      self#html_of_see b see;
	      bs b "</li>\n"
	    )
            l;
          bs b "</ul>\n"

    (** Print html code for the given optional return information.*)
    method html_of_return_opt b return_opt =
      match return_opt with
        None -> ()
      | Some s -> 
	  bp b "<b>%s</b> " Odoc_messages.returns;
	  self#html_of_text b s;
	  bs b "<br/>\n" 


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
          if indent then bs b "<div class=\"info\">\n";
          (
	   match info.M.i_deprecated with
            None -> ()
           | Some d -> 
               bs b "<span class=\"warning\">";
	       bs b Odoc_messages.deprecated ;
	       bs b "</span>" ;
	       self#html_of_text b d;
               bs b "<br/>\n" 
          );
          (
	   match info.M.i_desc with
             None -> ()
           | Some d when d = [Odoc_info.Raw ""] -> ()
           | Some d -> self#html_of_text b d; bs b "<br/>\n"
          );
          self#html_of_author_list b info.M.i_authors;
          self#html_of_version_opt b info.M.i_version;
          self#html_of_since_opt b info.M.i_since;
          self#html_of_raised_exceptions b info.M.i_raised_exceptions;
          self#html_of_return_opt b info.M.i_return_value;
          self#html_of_sees b info.M.i_sees;
          self#html_of_custom b info.M.i_custom;
          if indent then bs b "</div>\n"

    (** Print xml for use by the website for the left column *)
    method xml_of_info b info =
      let module M = Odoc_info in
      let rec xml_of_text_element parent = function
        | M.Module_list s ->
            bs b "<modules>";
            List.iter (fun s -> bs b "<module>"; bs b s; bs b "</module>") s;
            bs b "</modules>"
        | M.Title (level, label, text) ->
            bp b "<h%d" level;
            begin match label with
               | Some s -> bs b " label=\""; bs b s; bs b "\""
               | _ -> ()
            end;
            bs b ">";
            List.iter (xml_of_text_element `Title) text;
            bp b "</h%d>" level
        | M.Raw s when parent = `Title -> bs b (self#escape s)
        | _ -> ()
      in
      match info.M.i_desc with
        | None -> ()
        | Some d when d = [M.Raw ""] -> ()
        | Some d -> bs b "<info>"; List.iter (xml_of_text_element `None) d; bs b "</info>"


(*     (\** Print html code for a type. *\) *)
(*     method html_of_type b t = *)
(*       Odoc_info.reset_type_names (); *)
(*       let father = Name.father t.ty_name in *)
(*       bs b *)
(*         (match t.ty_manifest, t.ty_kind with *)
(*           None, Type_abstract -> "<pre>" *)
(*         | None, Type_variant _ *)
(*         | None, Type_record _ -> "<br/><code>" *)
(*         | Some _, Type_abstract -> "<pre>" *)
(*         | Some _, Type_variant _ *)
(*         | Some _, Type_record _ -> "<pre>" *)
(*         ); *)
(*       bs b ((self#keyword "type")^" "); *)
(*       (\* html mark *\) *)
(*       bp b "<a name=\"%s\"></a>" (Naming.type_target t); *)
(*       self#html_of_type_expr_param_list b father t; *)
(*       (match t.ty_parameters with [] -> () | _ -> bs b " "); *)
(*       bs b ((Name.simple t.ty_name)^" "); *)
(*       let priv = t.ty_private = Asttypes.Private in *)
(*       ( *)
(*        match t.ty_manifest with *)
(*          None -> () *)
(*        | Some typ -> *)
(*            bs b "= "; *)
(*            if priv then bs b "private "; *)
(*            self#html_of_type_expr b father typ; *)
(*            bs b " " *)
(*       ); *)
(*       (match t.ty_kind with *)
(*         Type_abstract -> bs b "</pre>" *)
(*       | Type_variant l -> *)
(*           bs b "= "; *)
(*           if priv then bs b "private "; *)
(*           bs b *)
(*             ( *)
(*              match t.ty_manifest with *)
(*                None -> "</code>" *)
(*              | Some _ -> "</pre>" *)
(*             ); *)
(*           bs b "<table class=\"typetable\">\n"; *)
(*           let print_one constr = *)
(*             bs b "<tr>\n<td align=\"left\" valign=\"top\" >\n"; *)
(*             bs b "<code>"; *)
(*             bs b (self#keyword "|"); *)
(*             bs b "</code></td>\n<td align=\"left\" valign=\"top\" >\n"; *)
(*             bs b "<code>"; *)
(*             bs b (self#constructor constr.vc_name); *)
(*             ( *)
(*              match constr.vc_args with *)
(*                [] -> () *)
(*              | l -> *)
(*                  bs b (" " ^ (self#keyword "of") ^ " "); *)
(*                  self#html_of_type_expr_list ~par: false b father " * " l; *)
(*             ); *)
(*             bs b "</code></td>\n"; *)
(*             ( *)
(*              match constr.vc_text with *)
(*                None -> () *)
(*              | Some t -> *)
(*                  bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >"; *)
(*                  bs b "<code>"; *)
(*                  bs b "(\*"; *)
(*                  bs b "</code></td>"; *)
(*                  bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >"; *)
(*                  self#html_of_text b t; *)
(*                  bs b "</td>"; *)
(*                  bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >"; *)
(*                  bs b "<code>"; *)
(*                  bs b "*\)"; *)
(*                  bs b "</code></td>"; *)
(*             ); *)
(*             bs b "\n</tr>" *)
(*           in *)
(*           print_concat b "\n" print_one l; *)
(*           bs b "</table>\n" *)

(*       | Type_record l -> *)
(*           bs b "= "; *)
(*           if priv then bs b "private " ; *)
(*           bs b "{"; *)
(*           bs b *)
(*             ( *)
(*              match t.ty_manifest with *)
(*                None -> "</code>" *)
(*              | Some _ -> "</pre>" *)
(*             ); *)
(*           bs b "<table class=\"typetable\">\n" ; *)
(*           let print_one r = *)
(*             bs b "<tr>\n<td align=\"left\" valign=\"top\" >\n"; *)
(*             bs b "<code>&nbsp;&nbsp;</code>"; *)
(*             bs b "</td>\n<td align=\"left\" valign=\"top\" >\n"; *)
(*             bs b "<code>"; *)
(*             if r.rf_mutable then bs b (self#keyword "mutable&nbsp;") ; *)
(*             bs b (r.rf_name ^ "&nbsp;: ") ; *)
(*             self#html_of_type_expr b father r.rf_type; *)
(*             bs b ";</code></td>\n"; *)
(*             ( *)
(*              match r.rf_text with *)
(*                None -> () *)
(*              | Some t -> *)
(*                  bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >"; *)
(*                  bs b "<code>"; *)
(*                  bs b "(\*"; *)
(*                  bs b "</code></td>"; *)
(*                  bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >"; *)
(*                  self#html_of_text b t; *)
(*                  bs b "</td><td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >"; *)
(*                  bs b "<code>*\)</code></td>"; *)
(*             ); *)
(*             bs b "\n</tr>" *)
(*           in *)
(*           print_concat b "\n" print_one l; *)
(*           bs b "</table>\n}\n" *)
(*       ); *)
(*       bs b "\n"; *)
(*       self#html_of_info b t.ty_info; *)
(*       bs b "\n" *)



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
		bs b "<code>";
		bs b n;
		bs b "</code> : ";
		self#html_of_text b t
	  in
	  Odoc_html.print_concat b "<br/>\n" print_one l2

    (** Print html code for a list of parameters. *)
    method html_of_parameter_list b m_name l =
      match l with
        [] -> ()
      | _ ->
          bs b "<div class=\"info\">";
          bs b "<table border=\"0\" cellpadding=\"3\" width=\"100%\">\n";
          bs b "<tr>\n<td align=\"left\" valign=\"top\" width=\"1%\">";
	  bs b "<b>";
	  bs b Odoc_messages.parameters;
	  bs b ": </b></td>\n" ;
          bs b "<td>\n<table class=\"paramstable\">\n";
	  let print_one p =
            bs b "<tr>\n<td align=\"center\" valign=\"top\" width=\"15%\" class=\"code\">\n";
            bs b 
	      (
	       match Parameter.complete_name p with
		 "" -> "?"
               | s -> s
              );
	    bs b "</td>\n<td align=\"center\" valign=\"top\">:</td>\n";
            bs b "<td>";
	    self#html_of_type_expr b m_name (Parameter.typ p);
	    bs b "<br/>\n";
            self#html_of_parameter_description b p;
	    bs b "\n</tr>\n";
	  in
          List.iter print_one l;
          bs b "</table>\n</td>\n</tr>\n</table></div>\n"

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
        bs b "<div class=\"info\"><code class=\"code\">";
	bs b (Parameter.complete_name p);
	bs b "</code> : " ;
        self#html_of_parameter_description b p;
	bs b "</div>\n"
      in
      match l2 with
        [] -> ()
      | _ -> 
	  bs b "<br/>";
	  List.iter f l2


    (** Print html code for a list of module parameters. *)
    method html_of_module_parameter_list b m_name l =
      match l with
        [] ->
          ()
      | _ ->
          bs b "<table border=\"0\" cellpadding=\"3\" width=\"100%\">\n";
          bs b "<tr>\n";
          bs b "<td align=\"left\" valign=\"top\" width=\"1%%\"><b>";
	  bs b Odoc_messages.parameters ;
	  bs b ": </b></td>\n<td>\n";
          bs b "<table class=\"paramstable\">\n";
	  List.iter
            (fun (p, desc_opt) ->
              bs b "<tr>\n";
              bs b "<td align=\"center\" valign=\"top\" width=\"15%\">\n<code>" ;
	      bs b p.mp_name;
              bs b "</code></td>\n" ;
              bs b "<td align=\"center\" valign=\"top\">:</td>\n";
              bs b "<td>" ;
	      self#html_of_module_parameter_type b m_name p;
	      bs b "\n";
              (
	       match desc_opt with
                 None -> ()
               | Some t -> 
		   bs b "<br/>";
		   self#html_of_text b t;
		   bs b "\n</tr>\n" ;
              )
	    )
            l;
          bs b "</table>\n</td>\n</tr>\n</table>\n"

    (** Print html code for a module comment.*)
    method html_of_module_comment b text =
      bs b "<br/>\n";
      self#html_of_text b text;
      bs b "<br/>\n"



    (** Generate the code of the html page for the given class.*)
    method generate_for_class pre post cl =
      Odoc_info.reset_type_names ();
      let (html_file, _) = Naming.html_files cl.cl_name in
      let type_file = Naming.file_type_class_complete_target cl.cl_name in
      try
        let chanout = open_out (Filename.concat !Args.target_dir html_file) in
        let b = new_buf () in
        let pre_name = opt (fun c -> c.cl_name) pre in
        let post_name = opt (fun c -> c.cl_name) post in
        self#print_navbar b pre_name post_name cl.cl_name;
        bs b "<center><h1>";
        bs b (Odoc_messages.clas^" ");
        if cl.cl_virtual then bs b "virtual " ;
        bp b "<a href=\"%s\">%s</a>" type_file cl.cl_name;
        bs b "</h1></center>\n<br/>\n";
        self#html_of_class b ~with_link: false cl;
        (* parameters *)
        self#html_of_described_parameter_list b
          (Name.father cl.cl_name) cl.cl_parameters;
        (* class inheritance *)
        self#generate_class_inheritance_info b cl;
        (* a horizontal line *)
        bs b "<hr/>\n";
        (* the various elements *)
        List.iter (self#html_of_class_element b)
          (Class.class_elements ~trans:false cl);
        Buffer.output_buffer chanout b;
        close_out chanout;

        (* generate the file with the complete class type *)
        self#output_class_type
          cl.cl_name
          (Filename.concat !Args.target_dir type_file)
          cl.cl_type
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the code of the html page for the given class type.*)
    method generate_for_class_type pre post clt =
      Odoc_info.reset_type_names ();
      let (html_file, _) = Naming.html_files clt.clt_name in
      let type_file = Naming.file_type_class_complete_target clt.clt_name in
      try
        let chanout = open_out (Filename.concat !Args.target_dir html_file) in
        let b = new_buf () in
        let pre_name = opt (fun ct -> ct.clt_name) pre in
        let post_name = opt (fun ct -> ct.clt_name) post in
        self#print_navbar b pre_name post_name clt.clt_name;
        bs b "<center><h1>";
        bs b (Odoc_messages.class_type^" ");
        if clt.clt_virtual then bs b "virtual ";
        bp b "<a href=\"%s\">%s</a>" type_file clt.clt_name;
        bs b "</h1></center>\n<br/>\n";
        self#html_of_class_type b ~with_link: false clt;

        (* class inheritance *)
        self#generate_class_type_inheritance_info b clt;
        (* a horizontal line *)
        bs b "<hr/>\n";
        (* the various elements *)
        List.iter (self#html_of_class_element b)
          (Class.class_type_elements ~trans: false clt);
        Buffer.output_buffer chanout b;
        close_out chanout;

        (* generate the file with the complete class type *)
        self#output_class_type
          clt.clt_name
          (Filename.concat !Args.target_dir type_file)
          clt.clt_type
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the html file for the given module type.
       @raise Failure if an error occurs.*)
    method generate_for_module_type pre post mt =
      try
        let (html_file, _) = Naming.html_files mt.mt_name in
        let type_file = Naming.file_type_module_complete_target mt.mt_name in
        let chanout = open_out (Filename.concat !Args.target_dir html_file) in
        let b = new_buf () in
        let pre_name = opt (fun mt -> mt.mt_name) pre in
        let post_name = opt (fun mt -> mt.mt_name) post in
        self#print_navbar b pre_name post_name mt.mt_name;
        bp b "<center><h1>";
        bs b (Odoc_messages.module_type^" ");
        (
         match mt.mt_type with
           Some _ -> bp b "<a href=\"%s\">%s</a>" type_file mt.mt_name
         | None-> bs b mt.mt_name
        );
        bs b "</h1></center>\n<br/>\n" ;
        self#html_of_modtype b ~with_link: false mt;

        (* parameters for functors *)
        self#html_of_module_parameter_list b
          (Name.father mt.mt_name)
          (Module.module_type_parameters mt);
        (* a horizontal line *)
        bs b "<hr/>\n";
        (* module elements *)
        List.iter
          (self#html_of_module_element b (Name.father mt.mt_name))
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
               (Filename.concat !Args.target_dir type_file)
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
        let (html_file, _) = Naming.html_files modu.m_name in
        let type_file = Naming.file_type_module_complete_target modu.m_name in
        let code_file = Naming.file_code_module_complete_target modu.m_name in
        let chanout = open_out (Filename.concat !Args.target_dir html_file) in
        let b = new_buf () in
        let pre_name = opt (fun m -> m.m_name) pre in
        let post_name = opt (fun m -> m.m_name) post in
        self#print_navbar b pre_name post_name modu.m_name ;
        bs b "<center><h1>";
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
           bp b " <a href=\"%s\">%s</a>" type_file modu.m_name;
           (
            match modu.m_code with
              None -> ()
            | Some _ -> bp b " (<a href=\"%s\">.ml</a>)" code_file
           )
          );
        bs b "</h1></center>\n<br/>\n";

        if not modu.m_text_only then self#html_of_module b ~with_link: false modu;

        (* parameters for functors *)
        self#html_of_module_parameter_list b
          (Name.father modu.m_name)
          (Module.module_parameters modu);

        (* a horizontal line *)
        if not modu.m_text_only then bs b "<hr/>\n";

        (* module elements *)
        List.iter
          (self#html_of_module_element b (Name.father modu.m_name))
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
          (Filename.concat !Args.target_dir type_file)
          modu.m_type;

        match modu.m_code with
          None -> ()
        | Some code ->
            self#output_code
              modu.m_name
              (Filename.concat !Args.target_dir code_file)
              code
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the [<index_prefix>.html] file corresponding to the given module list.
       @raise Failure if an error occurs.*)
    method generate_index module_list =
      try
        let chanout = open_out (Filename.concat !Args.target_dir self#index) in
        let xmlname = self#index_prefix ^ ".xml" in
        let chanxml = open_out (Filename.concat !Args.target_dir xmlname) in
        let b = new_buf () and b' = new_buf () in
        let title = match !Args.title with None -> "" | Some t -> self#escape t in
        bs b "<center><h1>";
        bs b title;
        bs b "</h1></center>\n" ;
        let info = Odoc_info.apply_opt
            (Odoc_info.info_of_comment_file module_list)
            !Odoc_info.Args.intro_file
        in
        (
         match info with
           None ->
             self#html_of_Index_list b;
             bs b "<br/>";
             self#html_of_Module_list b
               (List.map (fun m -> m.m_name) module_list);
         | Some i ->
             self#html_of_info ~indent: false b info;
             self#xml_of_info b' i;
        );
        Buffer.output_buffer chanxml b';
        Buffer.output_buffer chanout b;
        close_out chanxml;
        close_out chanout
      with
        Sys_error s ->
          raise (Failure s)


  end


let doc_generator = ((new div) :> Odoc_args.doc_generator)
let _ = Odoc_args.set_doc_generator (Some doc_generator)
