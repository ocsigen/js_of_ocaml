
# Module `Graphics_js`

Javascript binding for Graphics lib

```ocaml
exception Graphic_failure of string
```
```ocaml
val close_graph : unit -> unit
```
```ocaml
val set_window_title : string -> unit
```
```ocaml
val resize_window : int -> int -> unit
```
```ocaml
val clear_graph : unit -> unit
```
```ocaml
val size_x : unit -> int
```
```ocaml
val size_y : unit -> int
```
```ocaml
type color = int
```
```ocaml
val rgb : int -> int -> int -> color
```
```ocaml
val set_color : color -> unit
```
```ocaml
val background : color
```
```ocaml
val foreground : color
```
```ocaml
val black : color
```
```ocaml
val white : color
```
```ocaml
val red : color
```
```ocaml
val green : color
```
```ocaml
val blue : color
```
```ocaml
val yellow : color
```
```ocaml
val cyan : color
```
```ocaml
val magenta : color
```
```ocaml
val plot : int -> int -> unit
```
```ocaml
val plots : (int * int) array -> unit
```
```ocaml
val point_color : int -> int -> color
```
```ocaml
val moveto : int -> int -> unit
```
```ocaml
val rmoveto : int -> int -> unit
```
```ocaml
val current_x : unit -> int
```
```ocaml
val current_y : unit -> int
```
```ocaml
val current_point : unit -> int * int
```
```ocaml
val lineto : int -> int -> unit
```
```ocaml
val rlineto : int -> int -> unit
```
```ocaml
val curveto : (int * int) -> (int * int) -> (int * int) -> unit
```
```ocaml
val draw_rect : int -> int -> int -> int -> unit
```
```ocaml
val draw_poly_line : (int * int) array -> unit
```
```ocaml
val draw_poly : (int * int) array -> unit
```
```ocaml
val draw_segments : (int * int * int * int) array -> unit
```
```ocaml
val draw_arc : int -> int -> int -> int -> int -> int -> unit
```
```ocaml
val draw_ellipse : int -> int -> int -> int -> unit
```
```ocaml
val draw_circle : int -> int -> int -> unit
```
```ocaml
val set_line_width : int -> unit
```
```ocaml
val draw_char : char -> unit
```
```ocaml
val draw_string : string -> unit
```
```ocaml
val set_font : string -> unit
```
```ocaml
val set_text_size : int -> unit
```
```ocaml
val text_size : string -> int * int
```
```ocaml
val fill_rect : int -> int -> int -> int -> unit
```
```ocaml
val fill_poly : (int * int) array -> unit
```
```ocaml
val fill_arc : int -> int -> int -> int -> int -> int -> unit
```
```ocaml
val fill_ellipse : int -> int -> int -> int -> unit
```
```ocaml
val fill_circle : int -> int -> int -> unit
```
```ocaml
type image
```
```ocaml
val transp : color
```
```ocaml
val make_image : color array array -> image
```
```ocaml
val dump_image : image -> color array array
```
```ocaml
val draw_image : image -> int -> int -> unit
```
```ocaml
val get_image : int -> int -> int -> int -> image
```
```ocaml
val create_image : int -> int -> image
```
```ocaml
val blit_image : image -> int -> int -> unit
```
```ocaml
type status = {
  mouse_x : int;
  mouse_y : int;
  button : bool;
  keypressed : bool;
  key : char;
}
```
```ocaml
type event = 
  | Button_down
  | Button_up
  | Key_pressed
  | Mouse_motion
  | Poll
```
```ocaml
val wait_next_event : event list -> status
```
```ocaml
val loop_at_exit : event list -> (status -> unit) -> unit
```
```ocaml
val key_pressed : unit -> bool
```
```ocaml
val sound : int -> int -> unit
```
```ocaml
val auto_synchronize : bool -> unit
```
```ocaml
val synchronize : unit -> unit
```
```ocaml
val display_mode : bool -> unit
```
```ocaml
val remember_mode : bool -> unit
```

### Initializations

```ocaml
type context
```
type of a graphic context

```ocaml
val open_graph : string -> unit
```
Open a graphics window. The graphics window is cleared and the current point is set to (0, 0\). The string argument is used to pass optional information on the desired graphics mode, the graphics window size, and so on. Specification can be found at http://www.w3schools.com/jsref/met\_win\_open.asp. Note: an extra specification is available, "target", to specifies the target attribute or the name of the window.

```ocaml
val open_canvas : Js_of_ocaml.Dom_html.canvasElement Js_of_ocaml.Js.t -> unit
```
use a canvas to setup the current context

```ocaml
val get_context : unit -> context
```
Get the current context

```ocaml
val set_context : context -> unit
```
Set the current context


### Mouse and keyboard events

```ocaml
val loop : event list -> (status -> unit) -> unit
```
Loops forever and listen to the given events. Those events automatically returns a status record, which is used by the function given in argument.


### Mouse and keyboard polling

```ocaml
val mouse_pos : unit -> (int * int) Lwt.t
```
Return the position of the mouse cursor, relative to the graphics window. If the mouse cursor is outside of the graphics window, `mouse_pos()` returns a point outside of the range `0..size_x()-1, 0..size_y()-1`.

```ocaml
val button_down : unit -> bool Lwt.t
```
Return `true` if the mouse button is pressed, `false` otherwise.

```ocaml
val read_key : unit -> char Lwt.t
```
Wait for a key to be pressed, and return the corresponding character. Keypresses are queued.
