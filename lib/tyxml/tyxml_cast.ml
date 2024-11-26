(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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

open Js_of_ocaml
open! Import

module MakeTo (C : sig
  type 'a elt

  val elt : 'a elt -> Dom.node Js.t
end) : Tyxml_cast_sigs.TO with type 'a elt = 'a C.elt = struct
  type 'a elt = 'a C.elt

  let rebuild_node _ x = Js.Unsafe.coerce (C.elt x)

  let of_element elt = rebuild_node "of_element" elt

  let of_node elt = rebuild_node "of_node" elt

  let of_pcdata elt = rebuild_node "of_pcdata" elt

  let of_html elt = rebuild_node "of_html" elt

  let of_head elt = rebuild_node "of_head" elt

  let of_link elt = rebuild_node "of_link" elt

  let of_title elt = rebuild_node "of_title" elt

  let of_meta elt = rebuild_node "of_meta" elt

  let of_base elt = rebuild_node "of_base" elt

  let of_style elt = rebuild_node "of_style" elt

  let of_body elt = rebuild_node "of_body" elt

  let of_form elt = rebuild_node "of_form" elt

  let of_optgroup elt = rebuild_node "of_optgroup" elt

  let of_option elt = rebuild_node "of_option" elt

  let of_select elt = rebuild_node "of_select" elt

  let of_input elt = rebuild_node "of_input" elt

  let of_textarea elt = rebuild_node "of_textarea" elt

  let of_button elt = rebuild_node "of_button" elt

  let of_label elt = rebuild_node "of_label" elt

  let of_fieldset elt = rebuild_node "of_fieldset" elt

  let of_legend elt = rebuild_node "of_legend" elt

  let of_ul elt = rebuild_node "of_ul" elt

  let of_ol elt = rebuild_node "of_ol" elt

  let of_dl elt = rebuild_node "of_dl" elt

  let of_li elt = rebuild_node "of_li" elt

  let of_details elt = rebuild_node "of_details" elt

  let of_dialog elt = rebuild_node "of_dialog" elt

  let of_div elt = rebuild_node "of_div" elt

  let of_p elt = rebuild_node "of_p" elt

  let of_heading elt = rebuild_node "of_heading" elt

  let of_blockquote elt = rebuild_node "of_blockquote" elt

  let of_pre elt = rebuild_node "of_pre" elt

  let of_br elt = rebuild_node "of_br" elt

  let of_hr elt = rebuild_node "of_hr" elt

  let of_ins elt = rebuild_node "of_ins" elt

  let of_del elt = rebuild_node "of_del" elt

  let of_a elt = rebuild_node "of_a" elt

  let of_img elt = rebuild_node "of_img" elt

  let of_object elt = rebuild_node "of_object" elt

  let of_param elt = rebuild_node "of_param" elt

  let of_area elt = rebuild_node "of_area" elt

  let of_map elt = rebuild_node "of_map" elt

  let of_script elt = rebuild_node "of_script" elt

  let of_td elt = rebuild_node "of_td" elt

  let of_tr elt = rebuild_node "of_tr" elt

  let of_col elt = rebuild_node "of_col" elt

  let of_tfoot elt = rebuild_node "of_tfoot" elt

  let of_thead elt = rebuild_node "of_thead" elt

  let of_tbody elt = rebuild_node "of_tbody" elt

  let of_caption elt = rebuild_node "of_caption" elt

  let of_table elt = rebuild_node "of_table" elt

  let of_canvas elt = rebuild_node "of_canvas" elt

  let of_iframe elt = rebuild_node "of_iframe" elt

  let of_audio elt = rebuild_node "of_audio" elt

  let of_video elt = rebuild_node "of_video" elt

  (* Dom_html.headingElement *)

  let of_h1 elt = rebuild_node "of_h1" elt

  let of_h2 elt = rebuild_node "of_h2" elt

  let of_h3 elt = rebuild_node "of_h3" elt

  let of_h4 elt = rebuild_node "of_h4" elt

  let of_h5 elt = rebuild_node "of_h5" elt

  let of_h6 elt = rebuild_node "of_h6" elt

  (* Dom_html.element *)

  let of_abbr elt = rebuild_node "of_abbr" elt

  let of_address elt = rebuild_node "of_address" elt

  let of_article elt = rebuild_node "of_article" elt

  let of_aside elt = rebuild_node "of_aside" elt

  let of_b elt = rebuild_node "of_b" elt

  let of_bdo elt = rebuild_node "of_bdo" elt

  let of_cite elt = rebuild_node "of_cite" elt

  let of_code elt = rebuild_node "of_code" elt

  let of_colgroup elt = rebuild_node "of_colgroup" elt

  let of_command elt = rebuild_node "of_command" elt

  let of_datalist elt = rebuild_node "of_datalist" elt

  let of_dd elt = rebuild_node "of_dd" elt

  let of_dfn elt = rebuild_node "of_dfn" elt

  let of_dt elt = rebuild_node "of_dt" elt

  let of_em elt = rebuild_node "of_em" elt

  let of_embed elt = rebuild_node "of_embed" elt

  let of_figcaption elt = rebuild_node "of_figcaption" elt

  let of_figure elt = rebuild_node "of_figure" elt

  let of_footer elt = rebuild_node "of_footer" elt

  let of_header elt = rebuild_node "of_header" elt

  let of_hgroup elt = rebuild_node "of_hgroup" elt

  let of_i elt = rebuild_node "of_i" elt

  let of_kbd elt = rebuild_node "of_kbd" elt

  let of_keygen elt = rebuild_node "of_keygen" elt

  let of_main elt = rebuild_node "of_main" elt

  let of_mark elt = rebuild_node "of_mark" elt

  let of_menu elt = rebuild_node "of_menu" elt

  let of_meter elt = rebuild_node "of_meter" elt

  let of_nav elt = rebuild_node "of_nav" elt

  let of_noscript elt = rebuild_node "of_noscript" elt

  let of_output elt = rebuild_node "of_output" elt

  let of_progress elt = rebuild_node "of_progress" elt

  let of_q elt = rebuild_node "of_q" elt

  let of_rp elt = rebuild_node "of_rp" elt

  let of_rt elt = rebuild_node "of_rt" elt

  let of_ruby elt = rebuild_node "of_ruby" elt

  let of_samp elt = rebuild_node "of_samp" elt

  let of_section elt = rebuild_node "of_section" elt

  let of_small elt = rebuild_node "of_small" elt

  let of_source elt = rebuild_node "of_source" elt

  let of_span elt = rebuild_node "of_span" elt

  let of_strong elt = rebuild_node "of_strong" elt

  let of_sub elt = rebuild_node "of_sub" elt

  let of_summary elt = rebuild_node "of_summary" elt

  let of_sup elt = rebuild_node "of_sup" elt

  let of_th elt = rebuild_node "of_th" elt

  let of_time elt = rebuild_node "of_time" elt

  let of_u elt = rebuild_node "of_u" elt

  let of_var elt = rebuild_node "of_var" elt

  let of_wbr elt = rebuild_node "of_wbr" elt
end

module MakeOf (C : sig
  type 'a elt

  val elt : Dom.node Js.t -> 'a elt
end) : Tyxml_cast_sigs.OF with type 'a elt = 'a C.elt = struct
  type 'a elt = 'a C.elt

  let rebuild_node _ x = C.elt (Js.Unsafe.coerce x)

  let of_element elt = rebuild_node "of_element" elt

  let of_html elt = rebuild_node "of_html" elt

  let of_head elt = rebuild_node "of_head" elt

  let of_link elt = rebuild_node "of_link" elt

  let of_title elt = rebuild_node "of_title" elt

  let of_meta elt = rebuild_node "of_meta" elt

  let of_base elt = rebuild_node "of_base" elt

  let of_style elt = rebuild_node "of_style" elt

  let of_body elt = rebuild_node "of_body" elt

  let of_form elt = rebuild_node "of_form" elt

  let of_optGroup elt = rebuild_node "of_optGroup" elt

  let of_option elt = rebuild_node "of_option" elt

  let of_select elt = rebuild_node "of_select" elt

  let of_input elt = rebuild_node "of_input" elt

  let of_textArea elt = rebuild_node "of_textArea" elt

  let of_button elt = rebuild_node "of_button" elt

  let of_label elt = rebuild_node "of_label" elt

  let of_fieldSet elt = rebuild_node "of_fieldSet" elt

  let of_legend elt = rebuild_node "of_legend" elt

  let of_uList elt = rebuild_node "of_uList" elt

  let of_oList elt = rebuild_node "of_oList" elt

  let of_dList elt = rebuild_node "of_dList" elt

  let of_li elt = rebuild_node "of_li" elt

  let of_details elt = rebuild_node "of_details" elt

  let of_dialog elt = rebuild_node "of_dialog" elt

  let of_div elt = rebuild_node "of_div" elt

  let of_paragraph elt = rebuild_node "of_paragraph" elt

  let of_heading elt = rebuild_node "of_heading" elt

  let of_quote elt = rebuild_node "of_quote" elt

  let of_pre elt = rebuild_node "of_pre" elt

  let of_br elt = rebuild_node "of_br" elt

  let of_hr elt = rebuild_node "of_hr" elt

  let of_mod elt = rebuild_node "of_mod" elt

  let of_anchor elt = rebuild_node "of_anchor" elt

  let of_image elt = rebuild_node "of_image" elt

  let of_object elt = rebuild_node "of_object" elt

  let of_param elt = rebuild_node "of_param" elt

  let of_area elt = rebuild_node "of_area" elt

  let of_map elt = rebuild_node "of_map" elt

  let of_script elt = rebuild_node "of_script" elt

  let of_embed elt = rebuild_node "of_embed" elt

  let of_tableCell elt = rebuild_node "of_tableCell" elt

  let of_tableRow elt = rebuild_node "of_tableRow" elt

  let of_tableCol elt = rebuild_node "of_tableCol" elt

  let of_tableSection elt = rebuild_node "of_tableSection" elt

  let of_tableCaption elt = rebuild_node "of_tableCaption" elt

  let of_table elt = rebuild_node "of_table" elt

  let of_canvas elt = rebuild_node "of_canvas" elt

  let of_iFrame elt = rebuild_node "of_iFrame" elt

  let of_audio elt = rebuild_node "of_audio" elt

  let of_video elt = rebuild_node "of_video" elt
end
