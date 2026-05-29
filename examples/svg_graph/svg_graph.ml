(* Js_of_ocaml example
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
 *)

(* An interactive SVG node-graph editor, used to exercise a broad slice of the
   {!Js_of_ocaml.Dom_svg} bindings:

   - [graphicsElement] transforms and [getScreenCTM] / [getBBox];
   - [matrix##inverse] for screen->user coordinate conversion while dragging;
   - [geometryElement] [getTotalLength] / [getPointAtLength] to animate a
     packet travelling along each edge;
   - a radial gradient using the SVG 2 [fr] focal radius;
   - a [marker] arrowhead with [orient="auto"];
   - a [feDropShadow] filter whose blur is driven live via [setStdDeviation];
   - [create*] / [CoerceTo] helpers and the inherited DOM event target. *)

open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix

let doc = Dom_svg.document

(* {2 Small helpers} *)

let attr (e : #Dom.element Js.t) k v = e##setAttribute (Js.string k) (Js.string v)

let attrf e k v = attr e k (Printf.sprintf "%g" v)

let append (p : #Dom.node Js.t) (c : #Dom.node Js.t) = Dom.appendChild p c

let set_text (e : #Dom.node Js.t) s = append e (doc##createTextNode (Js.string s))

(* [number_t] is a boxed JS number, so reads/writes need explicit conversions. *)
let to_f = Js.to_float

let of_f = Js.number_of_float

(* {2 Model} *)

type node =
  { mutable x : float
  ; mutable y : float
  ; g : Dom_svg.gElement Js.t
  ; mutable sel : Dom_svg.rectElement Js.t option
  }

type edge =
  { src : node
  ; dst : node
  ; path : Dom_svg.pathElement Js.t
  ; packet : Dom_svg.circleElement Js.t
  }

let nodes : node list ref = ref []

let edges : edge list ref = ref []

(* Two layers so edges always render under nodes. Set up in [start]. *)
let edge_layer : Dom_svg.gElement Js.t option ref = ref None

let node_layer : Dom_svg.gElement Js.t option ref = ref None

let layer r =
  match !r with
  | Some g -> g
  | None -> assert false

(* Selected node and the source of an edge being drawn (link mode). *)
let selected : node option ref = ref None

let link_src : node option ref = ref None

let node_w = 96.

let node_h = 44.

(* {2 Definitions: gradient, marker, drop-shadow filter} *)

(* Filter primitives kept around so the controls can update them live. *)
let shadow = Dom_svg.createFeDropShadow doc

let hue = Dom_svg.createFeColorMatrix doc

let turb = Dom_svg.createFeTurbulence doc

let disp = Dom_svg.createFeDisplacementMap doc

let build_defs () =
  let defs = Dom_svg.createDefs doc in
  (* Radial gradient with an SVG 2 focal radius [fr] for a glossy highlight. *)
  let grad = Dom_svg.createRadialgradient doc in
  attr grad "id" "node";
  List.iter
    (fun (k, v) -> attrf grad k v)
    [ "cx", 0.35; "cy", 0.3; "r", 0.9; "fx", 0.35; "fy", 0.3; "fr", 0.02 ];
  let stop off color =
    let s = Dom_svg.createStop doc in
    attrf s "offset" off;
    attr s "stop-color" color;
    append grad s
  in
  stop 0. "#fefefe";
  stop 0.55 "#7aa7ff";
  stop 1. "#2c5fd6";
  append defs grad;
  (* Arrowhead marker with orient="auto". *)
  let marker = Dom_svg.createMarker doc in
  attr marker "id" "arrow";
  attr marker "viewBox" "0 0 10 10";
  attr marker "orient" "auto";
  attr marker "markerUnits" "userSpaceOnUse";
  List.iter
    (fun (k, v) -> attrf marker k v)
    [ "refX", 9.; "refY", 5.; "markerWidth", 11.; "markerHeight", 11. ];
  let tip = Dom_svg.createPath doc in
  attr tip "d" "M0,0 L10,5 L0,10 z";
  attr tip "fill" "#888";
  append marker tip;
  append defs marker;
  (* Node filter: a hue-rotate (feColorMatrix) chained into a drop-shadow. *)
  let node_fx = Dom_svg.createFilter doc in
  attr node_fx "id" "node-fx";
  attr node_fx "x" "-30%";
  attr node_fx "y" "-30%";
  attr node_fx "width" "160%";
  attr node_fx "height" "160%";
  attr hue "type" "hueRotate";
  attr hue "values" "0";
  attr hue "in" "SourceGraphic";
  attr hue "result" "hued";
  attr shadow "in" "hued";
  attr shadow "dx" "0";
  attr shadow "dy" "2";
  attr shadow "stdDeviation" "2.5";
  attr shadow "flood-color" "#16213e";
  attr shadow "flood-opacity" "0.45";
  append node_fx hue;
  append node_fx shadow;
  append defs node_fx;
  (* "Wobble" filter for edges: turbulence drives a displacement map. *)
  let wobble = Dom_svg.createFilter doc in
  attr wobble "id" "wobble";
  attr wobble "x" "-20%";
  attr wobble "y" "-20%";
  attr wobble "width" "140%";
  attr wobble "height" "140%";
  attr turb "type" "fractalNoise";
  attr turb "baseFrequency" "0.015";
  attr turb "numOctaves" "2";
  attr turb "result" "noise";
  attr disp "in" "SourceGraphic";
  attr disp "in2" "noise";
  attr disp "scale" "12";
  attr disp "xChannelSelector" "R";
  attr disp "yChannelSelector" "G";
  append wobble turb;
  append wobble disp;
  append defs wobble;
  defs

(* {2 Coordinate conversion}

   [getScreenCTM] maps user space to client (screen) coordinates, so a point
   carrying the mouse position run through the inverse matrix lands back in the
   SVG user space. *)
let client_to_user (svg : Dom_svg.svgElement Js.t) ev =
  let pt = svg##createSVGPoint in
  pt##.x := ev##.clientX;
  pt##.y := ev##.clientY;
  let p = pt##matrixTransform svg##getScreenCTM##inverse in
  to_f p##.x, to_f p##.y

(* {2 Edges} *)

let edge_d (a : node) (b : node) =
  let dx = (b.x -. a.x) *. 0.4 in
  Printf.sprintf
    "M %g %g C %g %g %g %g %g %g"
    a.x
    a.y
    (a.x +. dx)
    a.y
    (b.x -. dx)
    b.y
    b.x
    b.y

let update_edge e = attr e.path "d" (edge_d e.src e.dst)

let update_edges_of n =
  List.iter (fun e -> if e.src == n || e.dst == n then update_edge e) !edges

let make_edge a b =
  let path = Dom_svg.createPath doc in
  attr path "fill" "none";
  attr path "stroke" "#888";
  attr path "stroke-width" "2";
  attr path "marker-end" "url(#arrow)";
  let packet = Dom_svg.createCircle doc in
  attrf packet "r" 4.;
  attr packet "fill" "#ffcc55";
  append (layer edge_layer) path;
  append (layer edge_layer) packet;
  let e = { src = a; dst = b; path; packet } in
  update_edge e;
  edges := e :: !edges

(* {2 Nodes} *)

let update_node n = attr n.g "transform" (Printf.sprintf "translate(%g,%g)" n.x n.y)

let deselect () =
  match !selected with
  | None -> ()
  | Some n ->
      (match n.sel with
      | Some r ->
          Dom.removeChild n.g r;
          n.sel <- None
      | None -> ());
      selected := None

let select n =
  deselect ();
  (* Draw a dashed outline from the node's own bounding box (getBBox is in the
     element's local coordinate system, so we add the rect inside [n.g]). *)
  let b = n.g##getBBox in
  let r = Dom_svg.createRect doc in
  attrf r "x" (to_f b##.x -. 4.);
  attrf r "y" (to_f b##.y -. 4.);
  attrf r "width" (to_f b##.width +. 8.);
  attrf r "height" (to_f b##.height +. 8.);
  attr r "fill" "none";
  attr r "stroke" "#ffcc55";
  attr r "stroke-width" "1.5";
  attr r "stroke-dasharray" "5 4";
  append n.g r;
  n.sel <- Some r;
  selected := Some n

let rec make_node svg x y label =
  let g = Dom_svg.createG doc in
  let n = { x; y; g; sel = None } in
  let rect = Dom_svg.createRect doc in
  attrf rect "x" (-.node_w /. 2.);
  attrf rect "y" (-.node_h /. 2.);
  attrf rect "width" node_w;
  attrf rect "height" node_h;
  attrf rect "rx" 10.;
  attrf rect "ry" 10.;
  attr rect "fill" "url(#node)";
  attr rect "filter" "url(#node-fx)";
  let t = Dom_svg.createTextElement doc in
  attr t "text-anchor" "middle";
  attr t "dy" "5";
  attr t "font-family" "sans-serif";
  attr t "font-size" "15";
  attr t "fill" "#10233f";
  attr t "pointer-events" "none";
  set_text t label;
  append g rect;
  append g t;
  append (layer node_layer) g;
  update_node n;
  nodes := n :: !nodes;
  attach_drag svg n;
  n

(* Drag a node, or — in link mode — use it as an edge endpoint. *)
and attach_drag svg n =
  let link_mode () =
    match Dom_html.getElementById_coerce "link" Dom_html.CoerceTo.input with
    | Some i -> Js.to_bool i##.checked
    | None -> false
  in
  Lwt.async (fun () ->
      Lwt_js_events.mousedowns n.g (fun ev _ ->
          Dom.preventDefault ev;
          if link_mode ()
          then (
            (match !link_src with
            | None -> link_src := Some n
            | Some s ->
                if s != n then make_edge s n;
                link_src := None);
            Lwt.return ())
          else begin
            let px, py = client_to_user svg ev in
            let ox = n.x -. px and oy = n.y -. py in
            let moved = ref false in
            let mv =
              Lwt_js_events.mousemoves Dom_html.document (fun ev _ ->
                  moved := true;
                  let mx, my = client_to_user svg ev in
                  n.x <- mx +. ox;
                  n.y <- my +. oy;
                  update_node n;
                  update_edges_of n;
                  Lwt.return ())
            in
            Lwt_js_events.mouseup Dom_html.document
            >>= fun _ ->
            Lwt.cancel mv;
            if not !moved then select n;
            Lwt.return ()
          end))

(* {2 Animation: a packet rides along each edge} *)

let frame = ref 0

let rec animate () =
  Lwt_js_events.request_animation_frame ()
  >>= fun () ->
  incr frame;
  let phase = float_of_int !frame /. 90. in
  List.iter
    (fun e ->
      let len = to_f e.path##getTotalLength in
      if len > 0.
      then begin
        let d = Float.rem (phase *. len) len in
        let p = e.path##getPointAtLength (of_f d) in
        attrf e.packet "cx" (to_f p##.x);
        attrf e.packet "cy" (to_f p##.y)
      end)
    !edges;
  (* Shimmer the turbulence so the edge "wobble" looks alive. *)
  attrf turb "baseFrequency" (0.012 +. (0.004 *. sin (float_of_int !frame *. 0.05)));
  animate ()

(* {2 Wiring} *)

let start () =
  let container = Dom_html.getElementById "canvas" in
  let svg = Dom_svg.createSvg doc in
  attr svg "width" "100%";
  attr svg "height" "100%";
  attr svg "viewBox" "0 0 900 560";
  append container svg;
  append svg (build_defs ());
  let el = Dom_svg.createG doc in
  let nl = Dom_svg.createG doc in
  append svg el;
  append svg nl;
  edge_layer := Some el;
  node_layer := Some nl;
  (* A starting graph. *)
  let a = make_node svg 180. 140. "parse" in
  let b = make_node svg 460. 110. "optimize" in
  let c = make_node svg 460. 300. "inline" in
  let d = make_node svg 720. 220. "generate" in
  make_edge a b;
  make_edge a c;
  make_edge b d;
  make_edge c d;
  (* Double-click the background to add a node. *)
  Lwt.async (fun () ->
      Lwt_js_events.dblclicks svg (fun ev _ ->
          let x, y = client_to_user svg ev in
          ignore (make_node svg x y "node");
          Lwt.return ()));
  (* Click empty background to clear the selection. *)
  Lwt.async (fun () ->
      Lwt_js_events.clicks svg (fun ev _ ->
          (match Js.Opt.to_option ev##.target with
          | Some t when (t :> Dom.element Js.t) == (svg :> Dom.element Js.t) ->
              deselect ()
          | _ -> ());
          Lwt.return ()));
  (* Slider: drive the drop-shadow blur via the typed setStdDeviation. *)
  (match Dom_html.getElementById_coerce "blur" Dom_html.CoerceTo.input with
  | Some input ->
      let apply () =
        let v = try float_of_string (Js.to_string input##.value) with _ -> 2.5 in
        shadow##setStdDeviation (of_f v) (of_f v)
      in
      apply ();
      Lwt.async (fun () ->
          Lwt_js_events.inputs input (fun _ _ ->
              apply ();
              Lwt.return ()))
  | None -> ());
  (* Hue rotation of the nodes via feColorMatrix [values]. *)
  (match Dom_html.getElementById_coerce "hue" Dom_html.CoerceTo.input with
  | Some i ->
      let apply () = attr hue "values" (Js.to_string i##.value) in
      apply ();
      Lwt.async (fun () ->
          Lwt_js_events.inputs i (fun _ _ ->
              apply ();
              Lwt.return ()))
  | None -> ());
  (* Displacement amount of the edge "wobble" via feDisplacementMap [scale]. *)
  (match Dom_html.getElementById_coerce "scale" Dom_html.CoerceTo.input with
  | Some i ->
      let apply () = attr disp "scale" (Js.to_string i##.value) in
      apply ();
      Lwt.async (fun () ->
          Lwt_js_events.inputs i (fun _ _ ->
              apply ();
              Lwt.return ()))
  | None -> ());
  (* Toggle the turbulence/displacement filter on the edge layer. *)
  (match Dom_html.getElementById_coerce "wobble" Dom_html.CoerceTo.input with
  | Some i ->
      let apply () =
        if Js.to_bool i##.checked
        then attr el "filter" "url(#wobble)"
        else el##removeAttribute (Js.string "filter")
      in
      apply ();
      Lwt.async (fun () ->
          Lwt_js_events.changes i (fun _ _ ->
              apply ();
              Lwt.return ()))
  | None -> ());
  Lwt.async animate

let () =
  Dom_html.window##.onload :=
    Dom_html.handler (fun _ ->
        start ();
        Js._true)
