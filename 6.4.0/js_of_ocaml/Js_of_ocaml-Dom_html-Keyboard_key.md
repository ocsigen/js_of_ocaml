
# Module `Dom_html.Keyboard_key`

Use `Keyboard_key` when you want to identify the character that the user typed. This should only be invoked on keypress events, not keydown or keyup events.

```ocaml
type t = Stdlib.Uchar.t option
```
```ocaml
val of_event : keyboardEvent Js.t -> t
```