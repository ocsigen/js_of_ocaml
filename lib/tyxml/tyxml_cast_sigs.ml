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

(** Signatures to cast to and from Tyxml types *)

open Js_of_ocaml
open! Import

module type OF = sig
  type 'a elt

  val of_element : Dom_html.element Js.t -> 'a elt

  val of_html : Dom_html.htmlElement Js.t -> [> Html_types.html ] elt

  val of_head : Dom_html.headElement Js.t -> [> Html_types.head ] elt

  val of_link : Dom_html.linkElement Js.t -> [> Html_types.link ] elt

  val of_title : Dom_html.titleElement Js.t -> [> Html_types.title ] elt

  val of_meta : Dom_html.metaElement Js.t -> [> Html_types.meta ] elt

  val of_base : Dom_html.baseElement Js.t -> [> Html_types.base ] elt

  val of_style : Dom_html.styleElement Js.t -> [> Html_types.style ] elt

  val of_body : Dom_html.bodyElement Js.t -> [> Html_types.body ] elt

  val of_form : Dom_html.formElement Js.t -> [> Html_types.form ] elt

  val of_optGroup : Dom_html.optGroupElement Js.t -> [> Html_types.optgroup ] elt

  val of_option : Dom_html.optionElement Js.t -> [> Html_types.selectoption ] elt

  val of_select : Dom_html.selectElement Js.t -> [> Html_types.select ] elt

  val of_input : Dom_html.inputElement Js.t -> [> Html_types.input ] elt

  val of_textArea : Dom_html.textAreaElement Js.t -> [> Html_types.textarea ] elt

  val of_button : Dom_html.buttonElement Js.t -> [> Html_types.button ] elt

  val of_label : Dom_html.labelElement Js.t -> [> Html_types.label ] elt

  val of_fieldSet : Dom_html.fieldSetElement Js.t -> [> Html_types.fieldset ] elt

  val of_legend : Dom_html.legendElement Js.t -> [> Html_types.legend ] elt

  val of_uList : Dom_html.uListElement Js.t -> [> Html_types.ul ] elt

  val of_oList : Dom_html.oListElement Js.t -> [> Html_types.ol ] elt

  val of_dList : Dom_html.dListElement Js.t -> [> Html_types.dl ] elt

  val of_li : Dom_html.liElement Js.t -> [> Html_types.li ] elt

  val of_details : Dom_html.detailsElement Js.t -> [> Html_types.details ] elt

  val of_dialog : Dom_html.dialogElement Js.t -> [> Html_types.dialog ] elt

  val of_div : Dom_html.divElement Js.t -> [> Html_types.div ] elt

  val of_paragraph : Dom_html.paragraphElement Js.t -> [> Html_types.p ] elt

  val of_heading : Dom_html.headingElement Js.t -> [> Html_types.heading ] elt

  val of_quote : Dom_html.quoteElement Js.t -> [> Html_types.blockquote ] elt

  val of_pre : Dom_html.preElement Js.t -> [> Html_types.pre ] elt

  val of_br : Dom_html.brElement Js.t -> [> Html_types.br ] elt

  val of_hr : Dom_html.hrElement Js.t -> [> Html_types.hr ] elt

  val of_mod : Dom_html.modElement Js.t -> [> 'a Html_types.del | 'a Html_types.ins ] elt

  val of_anchor : Dom_html.anchorElement Js.t -> [> 'a Html_types.a ] elt

  val of_image : Dom_html.imageElement Js.t -> [> Html_types.img ] elt

  val of_object : Dom_html.objectElement Js.t -> [> 'a Html_types.object_ ] elt

  val of_param : Dom_html.paramElement Js.t -> [> Html_types.param ] elt

  val of_area : Dom_html.areaElement Js.t -> [> Html_types.area ] elt

  val of_map : Dom_html.mapElement Js.t -> [> 'a Html_types.map ] elt

  val of_script : Dom_html.scriptElement Js.t -> [> Html_types.script ] elt

  val of_embed : Dom_html.embedElement Js.t -> [> Html_types.embed ] elt

  val of_tableCell :
    Dom_html.tableCellElement Js.t -> [> Html_types.td | Html_types.th ] elt

  val of_tableRow : Dom_html.tableRowElement Js.t -> [> Html_types.tr ] elt

  val of_tableCol : Dom_html.tableColElement Js.t -> [> Html_types.col ] elt

  val of_tableSection :
       Dom_html.tableSectionElement Js.t
    -> [> Html_types.tfoot | Html_types.thead | Html_types.tbody ] elt

  val of_tableCaption : Dom_html.tableCaptionElement Js.t -> [> Html_types.caption ] elt

  val of_table : Dom_html.tableElement Js.t -> [> Html_types.table ] elt

  val of_canvas : Dom_html.canvasElement Js.t -> [> 'a Html_types.canvas ] elt

  val of_iFrame : Dom_html.iFrameElement Js.t -> [> Html_types.iframe ] elt

  val of_audio : Dom_html.audioElement Js.t -> [> 'a Html_types.audio ] elt

  val of_video : Dom_html.videoElement Js.t -> [> 'a Html_types.video ] elt
end

module type TO = sig
  type 'a elt

  val of_element : 'a elt -> Dom_html.element Js.t

  val of_node : 'a elt -> Dom.node Js.t

  val of_pcdata : [< Html_types.pcdata ] elt -> Dom.text Js.t

  val of_html : [< Html_types.html ] elt -> Dom_html.htmlElement Js.t

  val of_head : [< Html_types.head ] elt -> Dom_html.headElement Js.t

  val of_link : [< Html_types.link ] elt -> Dom_html.linkElement Js.t

  val of_title : [< Html_types.title ] elt -> Dom_html.titleElement Js.t

  val of_meta : [< Html_types.meta ] elt -> Dom_html.metaElement Js.t

  val of_base : [< Html_types.base ] elt -> Dom_html.baseElement Js.t

  val of_style : [< Html_types.style ] elt -> Dom_html.styleElement Js.t

  val of_body : [< Html_types.body ] elt -> Dom_html.bodyElement Js.t

  val of_form : [< Html_types.form ] elt -> Dom_html.formElement Js.t

  val of_optgroup : [< Html_types.optgroup ] elt -> Dom_html.optGroupElement Js.t

  val of_option : [< Html_types.selectoption ] elt -> Dom_html.optionElement Js.t

  val of_select : [< Html_types.select ] elt -> Dom_html.selectElement Js.t

  val of_input : [< Html_types.input ] elt -> Dom_html.inputElement Js.t

  val of_textarea : [< Html_types.textarea ] elt -> Dom_html.textAreaElement Js.t

  val of_button : [< Html_types.button ] elt -> Dom_html.buttonElement Js.t

  val of_label : [< Html_types.label ] elt -> Dom_html.labelElement Js.t

  val of_fieldset : [< Html_types.fieldset ] elt -> Dom_html.fieldSetElement Js.t

  val of_legend : [< Html_types.legend ] elt -> Dom_html.legendElement Js.t

  val of_ul : [< Html_types.ul ] elt -> Dom_html.uListElement Js.t

  val of_ol : [< Html_types.ol ] elt -> Dom_html.oListElement Js.t

  val of_dl : [< Html_types.dl ] elt -> Dom_html.dListElement Js.t

  val of_li : [< Html_types.li ] elt -> Dom_html.liElement Js.t

  val of_details : [< Html_types.details ] elt -> Dom_html.detailsElement Js.t

  val of_dialog : [< Html_types.dialog ] elt -> Dom_html.dialogElement Js.t

  val of_div : [< Html_types.div ] elt -> Dom_html.divElement Js.t

  val of_p : [< Html_types.p ] elt -> Dom_html.paragraphElement Js.t

  val of_heading : [< Html_types.heading ] elt -> Dom_html.headingElement Js.t

  val of_blockquote : [< Html_types.blockquote ] elt -> Dom_html.quoteElement Js.t

  val of_pre : [< Html_types.pre ] elt -> Dom_html.preElement Js.t

  val of_br : [< Html_types.br ] elt -> Dom_html.brElement Js.t

  val of_hr : [< Html_types.hr ] elt -> Dom_html.hrElement Js.t

  val of_del : [< 'a Html_types.del ] elt -> Dom_html.modElement Js.t

  val of_ins : [< 'a Html_types.ins ] elt -> Dom_html.modElement Js.t

  val of_a : [< 'a Html_types.a ] elt -> Dom_html.anchorElement Js.t

  val of_img : [< Html_types.img_interactive ] elt -> Dom_html.imageElement Js.t

  val of_object : [< 'a Html_types.object_ ] elt -> Dom_html.objectElement Js.t

  val of_param : [< Html_types.param ] elt -> Dom_html.paramElement Js.t

  val of_area : [< Html_types.area ] elt -> Dom_html.areaElement Js.t

  val of_map : [< 'a Html_types.map ] elt -> Dom_html.mapElement Js.t

  val of_script : [< Html_types.script ] elt -> Dom_html.scriptElement Js.t

  val of_td : [< Html_types.td | Html_types.td ] elt -> Dom_html.tableCellElement Js.t

  val of_tr : [< Html_types.tr ] elt -> Dom_html.tableRowElement Js.t

  val of_col : [< Html_types.col ] elt -> Dom_html.tableColElement Js.t

  val of_tfoot : [< Html_types.tfoot ] elt -> Dom_html.tableSectionElement Js.t

  val of_thead : [< Html_types.thead ] elt -> Dom_html.tableSectionElement Js.t

  val of_tbody : [< Html_types.tbody ] elt -> Dom_html.tableSectionElement Js.t

  val of_caption : [< Html_types.caption ] elt -> Dom_html.tableCaptionElement Js.t

  val of_table : [< Html_types.table ] elt -> Dom_html.tableElement Js.t

  val of_canvas : [< 'a Html_types.canvas ] elt -> Dom_html.canvasElement Js.t

  val of_iframe : [< Html_types.iframe ] elt -> Dom_html.iFrameElement Js.t

  val of_audio : [< 'a Html_types.audio_interactive ] elt -> Dom_html.audioElement Js.t

  val of_video : [< 'a Html_types.video_interactive ] elt -> Dom_html.videoElement Js.t

  (* Dom_html.headingElement *)

  val of_h1 : Html_types.heading elt -> Dom_html.headingElement Js.t

  val of_h2 : Html_types.heading elt -> Dom_html.headingElement Js.t

  val of_h3 : Html_types.heading elt -> Dom_html.headingElement Js.t

  val of_h4 : Html_types.heading elt -> Dom_html.headingElement Js.t

  val of_h5 : Html_types.heading elt -> Dom_html.headingElement Js.t

  val of_h6 : Html_types.heading elt -> Dom_html.headingElement Js.t

  (* Dom_html.element *)

  val of_abbr : [> Html_types.abbr ] elt -> Dom_html.element Js.t

  val of_address : [> Html_types.address ] elt -> Dom_html.element Js.t

  val of_article : [> Html_types.article ] elt -> Dom_html.element Js.t

  val of_aside : [> Html_types.aside ] elt -> Dom_html.element Js.t

  val of_b : [> Html_types.b ] elt -> Dom_html.element Js.t

  val of_bdo : [> Html_types.bdo ] elt -> Dom_html.element Js.t

  val of_cite : [> Html_types.cite ] elt -> Dom_html.element Js.t

  val of_code : [> Html_types.code ] elt -> Dom_html.element Js.t

  val of_colgroup : [> Html_types.colgroup ] elt -> Dom_html.element Js.t

  val of_command : [> Html_types.command ] elt -> Dom_html.element Js.t

  val of_datalist : [> Html_types.datalist ] elt -> Dom_html.element Js.t

  val of_dd : [> Html_types.dd ] elt -> Dom_html.element Js.t

  val of_dfn : [> Html_types.dfn ] elt -> Dom_html.element Js.t

  val of_dt : [> Html_types.dt ] elt -> Dom_html.element Js.t

  val of_em : [> Html_types.em ] elt -> Dom_html.element Js.t

  val of_embed : [> Html_types.embed ] elt -> Dom_html.element Js.t

  val of_figcaption : [> Html_types.figcaption ] elt -> Dom_html.element Js.t

  val of_figure : [> Html_types.figure ] elt -> Dom_html.element Js.t

  val of_footer : [> Html_types.footer ] elt -> Dom_html.element Js.t

  val of_header : [> Html_types.header ] elt -> Dom_html.element Js.t

  val of_hgroup : [> Html_types.hgroup ] elt -> Dom_html.element Js.t

  val of_i : [> Html_types.i ] elt -> Dom_html.element Js.t

  val of_kbd : [> Html_types.kbd ] elt -> Dom_html.element Js.t

  val of_keygen : [> Html_types.keygen ] elt -> Dom_html.element Js.t

  val of_main : [> Html_types.main ] elt -> Dom_html.element Js.t

  val of_mark : [> Html_types.mark ] elt -> Dom_html.element Js.t

  val of_menu : [> Html_types.menu ] elt -> Dom_html.element Js.t

  val of_meter : [> Html_types.meter ] elt -> Dom_html.element Js.t

  val of_nav : [> Html_types.nav ] elt -> Dom_html.element Js.t

  val of_noscript : [> Html_types.noscript ] elt -> Dom_html.element Js.t

  val of_output : [> Html_types.output_elt ] elt -> Dom_html.element Js.t

  val of_progress : [> Html_types.progress ] elt -> Dom_html.element Js.t

  val of_q : [> Html_types.q ] elt -> Dom_html.element Js.t

  val of_rp : [> Html_types.rp ] elt -> Dom_html.element Js.t

  val of_rt : [> Html_types.rt ] elt -> Dom_html.element Js.t

  val of_ruby : [> Html_types.ruby ] elt -> Dom_html.element Js.t

  val of_samp : [> Html_types.samp ] elt -> Dom_html.element Js.t

  val of_section : [> Html_types.section ] elt -> Dom_html.element Js.t

  val of_small : [> Html_types.small ] elt -> Dom_html.element Js.t

  val of_source : [> Html_types.source ] elt -> Dom_html.element Js.t

  val of_span : [> Html_types.span ] elt -> Dom_html.element Js.t

  val of_strong : [> Html_types.strong ] elt -> Dom_html.element Js.t

  val of_sub : [> Html_types.sub ] elt -> Dom_html.element Js.t

  val of_summary : [> Html_types.summary ] elt -> Dom_html.element Js.t

  val of_sup : [> Html_types.sup ] elt -> Dom_html.element Js.t

  val of_th : [> Html_types.th ] elt -> Dom_html.element Js.t

  val of_time : [> Html_types.time ] elt -> Dom_html.element Js.t

  val of_u : [> Html_types.u ] elt -> Dom_html.element Js.t

  val of_var : [> Html_types.var ] elt -> Dom_html.element Js.t

  val of_wbr : [> Html_types.wbr ] elt -> Dom_html.element Js.t
end
