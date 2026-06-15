
# Module `Dom_html.Keyboard_code`

Use `Keyboard_code` when you want to identify a key that the user pressed. This should be invoked for keydown and keyup events, not keypress events.

If the browser supports the standardized `key` and `code` properties, then `of_event` on a keypress event will have the correct behavior. Otherwise, it might not identify or might mis-identify which key was pressed. This occurs because the keypress event is designed for printable characters while keydown and keyup are designed for physical keys. Thus, the older properties of `keyEvent` change behavior between keydown and keypress events. This change in behavior is what causes the mapping to be incorrect.

```ocaml
type t = 
  | Unidentified
  | KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  | Digit0
  | Digit1
  | Digit2
  | Digit3
  | Digit4
  | Digit5
  | Digit6
  | Digit7
  | Digit8
  | Digit9
  | Minus
  | Equal
  | Tab
  | Enter
  | Space
  | Escape
  | Backspace
  | Insert
  | Delete
  | CapsLock
  | BracketLeft
  | BracketRight
  | Semicolon
  | Quote
  | Backquote
  | Backslash
  | Comma
  | Period
  | Slash
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | Numpad0
  | Numpad1
  | Numpad2
  | Numpad3
  | Numpad4
  | Numpad5
  | Numpad6
  | Numpad7
  | Numpad8
  | Numpad9
  | NumpadMultiply
  | NumpadSubtract
  | NumpadAdd
  | NumpadDecimal
  | NumpadEqual
  | NumpadEnter
  | NumpadDivide
  | NumLock
  | ControlLeft
  | ControlRight
  | MetaLeft
  | MetaRight
  | ShiftLeft
  | ShiftRight
  | AltLeft
  | AltRight
  | ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowDown
  | PageUp
  | PageDown
  | Home
  | End
  | VolumeMute
  | VolumeDown
  | VolumeUp
  | MediaTrackPrevious
  | MediaTrackNext
  | MediaPlayPause
  | MediaStop
  | ContextMenu
  | BrowserSearch
  | BrowserHome
  | BrowserFavorites
  | BrowserRefresh
  | BrowserStop
  | BrowserForward
  | BrowserBack
  | OSLeft
  | OSRight
  | ScrollLock
  | PrintScreen
  | IntlBackslash
  | IntlYen
  | Pause
```
```ocaml
val of_event : keyboardEvent Js.t -> t
```
```ocaml
val of_key_code : int -> t
```