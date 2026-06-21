
# Module `Tyxml_js.Of_dom`

```ocaml
type 'a elt = 'a Html.elt
```
```ocaml
val of_element : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> 'a elt
```
```ocaml
val of_html : 
  Js_of_ocaml.Dom_html.htmlElement Js_of_ocaml.Js.t ->
  [> Html_types.html ] elt
```
```ocaml
val of_head : 
  Js_of_ocaml.Dom_html.headElement Js_of_ocaml.Js.t ->
  [> Html_types.head ] elt
```
```ocaml
val of_link : 
  Js_of_ocaml.Dom_html.linkElement Js_of_ocaml.Js.t ->
  [> Html_types.link ] elt
```
```ocaml
val of_title : 
  Js_of_ocaml.Dom_html.titleElement Js_of_ocaml.Js.t ->
  [> Html_types.title ] elt
```
```ocaml
val of_meta : 
  Js_of_ocaml.Dom_html.metaElement Js_of_ocaml.Js.t ->
  [> Html_types.meta ] elt
```
```ocaml
val of_base : 
  Js_of_ocaml.Dom_html.baseElement Js_of_ocaml.Js.t ->
  [> Html_types.base ] elt
```
```ocaml
val of_style : 
  Js_of_ocaml.Dom_html.styleElement Js_of_ocaml.Js.t ->
  [> Html_types.style ] elt
```
```ocaml
val of_body : 
  Js_of_ocaml.Dom_html.bodyElement Js_of_ocaml.Js.t ->
  [> Html_types.body ] elt
```
```ocaml
val of_form : 
  Js_of_ocaml.Dom_html.formElement Js_of_ocaml.Js.t ->
  [> Html_types.form ] elt
```
```ocaml
val of_optGroup : 
  Js_of_ocaml.Dom_html.optGroupElement Js_of_ocaml.Js.t ->
  [> Html_types.optgroup ] elt
```
```ocaml
val of_option : 
  Js_of_ocaml.Dom_html.optionElement Js_of_ocaml.Js.t ->
  [> Html_types.selectoption ] elt
```
```ocaml
val of_select : 
  Js_of_ocaml.Dom_html.selectElement Js_of_ocaml.Js.t ->
  [> Html_types.select ] elt
```
```ocaml
val of_input : 
  Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t ->
  [> Html_types.input ] elt
```
```ocaml
val of_textArea : 
  Js_of_ocaml.Dom_html.textAreaElement Js_of_ocaml.Js.t ->
  [> Html_types.textarea ] elt
```
```ocaml
val of_button : 
  Js_of_ocaml.Dom_html.buttonElement Js_of_ocaml.Js.t ->
  [> Html_types.button ] elt
```
```ocaml
val of_label : 
  Js_of_ocaml.Dom_html.labelElement Js_of_ocaml.Js.t ->
  [> Html_types.label ] elt
```
```ocaml
val of_fieldSet : 
  Js_of_ocaml.Dom_html.fieldSetElement Js_of_ocaml.Js.t ->
  [> Html_types.fieldset ] elt
```
```ocaml
val of_legend : 
  Js_of_ocaml.Dom_html.legendElement Js_of_ocaml.Js.t ->
  [> Html_types.legend ] elt
```
```ocaml
val of_uList : 
  Js_of_ocaml.Dom_html.uListElement Js_of_ocaml.Js.t ->
  [> Html_types.ul ] elt
```
```ocaml
val of_oList : 
  Js_of_ocaml.Dom_html.oListElement Js_of_ocaml.Js.t ->
  [> Html_types.ol ] elt
```
```ocaml
val of_dList : 
  Js_of_ocaml.Dom_html.dListElement Js_of_ocaml.Js.t ->
  [> Html_types.dl ] elt
```
```ocaml
val of_li : 
  Js_of_ocaml.Dom_html.liElement Js_of_ocaml.Js.t ->
  [> Html_types.li ] elt
```
```ocaml
val of_details : 
  Js_of_ocaml.Dom_html.detailsElement Js_of_ocaml.Js.t ->
  [> Html_types.details ] elt
```
```ocaml
val of_dialog : 
  Js_of_ocaml.Dom_html.dialogElement Js_of_ocaml.Js.t ->
  [> Html_types.dialog ] elt
```
```ocaml
val of_div : 
  Js_of_ocaml.Dom_html.divElement Js_of_ocaml.Js.t ->
  [> Html_types.div ] elt
```
```ocaml
val of_paragraph : 
  Js_of_ocaml.Dom_html.paragraphElement Js_of_ocaml.Js.t ->
  [> Html_types.p ] elt
```
```ocaml
val of_heading : 
  Js_of_ocaml.Dom_html.headingElement Js_of_ocaml.Js.t ->
  [> Html_types.heading ] elt
```
```ocaml
val of_quote : 
  Js_of_ocaml.Dom_html.quoteElement Js_of_ocaml.Js.t ->
  [> Html_types.blockquote ] elt
```
```ocaml
val of_pre : 
  Js_of_ocaml.Dom_html.preElement Js_of_ocaml.Js.t ->
  [> Html_types.pre ] elt
```
```ocaml
val of_br : 
  Js_of_ocaml.Dom_html.brElement Js_of_ocaml.Js.t ->
  [> Html_types.br ] elt
```
```ocaml
val of_hr : 
  Js_of_ocaml.Dom_html.hrElement Js_of_ocaml.Js.t ->
  [> Html_types.hr ] elt
```
```ocaml
val of_mod : 
  Js_of_ocaml.Dom_html.modElement Js_of_ocaml.Js.t ->
  [> 'a Html_types.del | 'a Html_types.ins ] elt
```
```ocaml
val of_anchor : 
  Js_of_ocaml.Dom_html.anchorElement Js_of_ocaml.Js.t ->
  [> 'a Html_types.a ] elt
```
```ocaml
val of_image : 
  Js_of_ocaml.Dom_html.imageElement Js_of_ocaml.Js.t ->
  [> Html_types.img ] elt
```
```ocaml
val of_object : 
  Js_of_ocaml.Dom_html.objectElement Js_of_ocaml.Js.t ->
  [> 'a Html_types.object_ ] elt
```
```ocaml
val of_param : 
  Js_of_ocaml.Dom_html.paramElement Js_of_ocaml.Js.t ->
  [> Html_types.param ] elt
```
```ocaml
val of_area : 
  Js_of_ocaml.Dom_html.areaElement Js_of_ocaml.Js.t ->
  [> Html_types.area ] elt
```
```ocaml
val of_map : 
  Js_of_ocaml.Dom_html.mapElement Js_of_ocaml.Js.t ->
  [> 'a Html_types.map ] elt
```
```ocaml
val of_script : 
  Js_of_ocaml.Dom_html.scriptElement Js_of_ocaml.Js.t ->
  [> Html_types.script ] elt
```
```ocaml
val of_embed : 
  Js_of_ocaml.Dom_html.embedElement Js_of_ocaml.Js.t ->
  [> Html_types.embed ] elt
```
```ocaml
val of_tableCell : 
  Js_of_ocaml.Dom_html.tableCellElement Js_of_ocaml.Js.t ->
  [> Html_types.td | Html_types.th ] elt
```
```ocaml
val of_tableRow : 
  Js_of_ocaml.Dom_html.tableRowElement Js_of_ocaml.Js.t ->
  [> Html_types.tr ] elt
```
```ocaml
val of_tableCol : 
  Js_of_ocaml.Dom_html.tableColElement Js_of_ocaml.Js.t ->
  [> Html_types.col ] elt
```
```ocaml
val of_tableSection : 
  Js_of_ocaml.Dom_html.tableSectionElement Js_of_ocaml.Js.t ->
  [> Html_types.tfoot | Html_types.thead | Html_types.tbody ] elt
```
```ocaml
val of_tableCaption : 
  Js_of_ocaml.Dom_html.tableCaptionElement Js_of_ocaml.Js.t ->
  [> Html_types.caption ] elt
```
```ocaml
val of_table : 
  Js_of_ocaml.Dom_html.tableElement Js_of_ocaml.Js.t ->
  [> Html_types.table ] elt
```
```ocaml
val of_canvas : 
  Js_of_ocaml.Dom_html.canvasElement Js_of_ocaml.Js.t ->
  [> 'a Html_types.canvas ] elt
```
```ocaml
val of_iFrame : 
  Js_of_ocaml.Dom_html.iFrameElement Js_of_ocaml.Js.t ->
  [> Html_types.iframe ] elt
```
```ocaml
val of_audio : 
  Js_of_ocaml.Dom_html.audioElement Js_of_ocaml.Js.t ->
  [> 'a Html_types.audio ] elt
```
```ocaml
val of_video : 
  Js_of_ocaml.Dom_html.videoElement Js_of_ocaml.Js.t ->
  [> 'a Html_types.video ] elt
```