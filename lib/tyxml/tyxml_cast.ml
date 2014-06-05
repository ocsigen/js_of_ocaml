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

module MakeTo( C : sig type 'a elt val elt : 'a elt -> Dom.node Js.t end) : Tyxml_cast_sigs.TO with type 'a elt = 'a C.elt = struct
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
  let of_embed elt = rebuild_node "of_embed" elt
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

end


module MakeOf( C : sig type 'a elt val elt : Dom.node Js.t -> 'a elt end) : Tyxml_cast_sigs.OF with type 'a elt = 'a C.elt = struct
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
