# js_of_ocaml Standard API Bindings Status

> **Note:** This document was generated with the help of an LLM and may contain
> inaccurate or outdated information. Please verify against the actual source
> code before relying on it.

This document lists standard JavaScript/Web APIs and their support status in js_of_ocaml and [Brr](https://github.com/dbuenzli/brr/).

- **Yes** — Full or near-complete bindings available
- **Partial** — Some parts of the API are bound
- **No** — No bindings provided (use `Js.Unsafe` / `Jv` to access directly)

## Core JavaScript

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| JS types/objects | Yes | Yes | `Js` · Brr: `Jv` |
| RegExp | Yes | Yes | `Regexp` · Brr: `Brr.Regexp` |
| JSON | Yes | Yes | `Json` · Brr: `Brr.Json` |
| Date | Yes | No | `Js` (Js.date) |
| Math | Yes | No | `Js` (Js.math) |
| Promise | Yes | Yes | `Promise` · Brr: `Jv.Promise`, `Fut` |
| Console | Yes | Yes | `Console` · Brr: `Brr.Console` |

## DOM

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| DOM Core (Node, Element, Document) | Yes | Yes | `Dom` · Brr: `Brr.El`, `Brr.Document` |
| DOM HTML | Yes | Partial | `Dom_html` · Brr: `Brr.El` (generic), `Brr_io.Media.El`, `Brr_canvas.Canvas` |
| DOM SVG | Yes | No | `Dom_svg` |
| DOM Events | Yes | Yes | `Dom_events`, `Dom_html` · Brr: `Brr.Ev`, `Brr.Ev.Target` |

## CSS

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| CSS Values (colors, lengths, angles) | Yes | No | `CSS` |
| CSSOM | Partial | Partial | `Dom_html` (CSSStyleDeclaration) · Brr: `Brr.El` (inline_style, computed_style) |
| CSS Typed OM | No | No | |

## Network / Communication

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| XMLHttpRequest | Yes | No | `XmlHttpRequest` |
| Fetch API | Yes | Yes | `Fetch` · Brr: `Brr_io.Fetch` |
| MessageChannel / MessagePort | Yes | Yes | `MessageChannel` · Brr: `Brr_io.Message` |
| WebSocket | Yes | Yes | `WebSockets` · Brr: `Brr_io.Websocket` |
| Server-Sent Events (EventSource) | Yes | No | `EventSource` |
| Beacon API | No | No | |

## Storage

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| Web Storage (localStorage/sessionStorage) | Yes | Yes | `Dom_html` (storage) · Brr: `Brr_io.Storage` |
| IndexedDB | No | No | |
| Cache API | Yes | Yes | `Cache` · Brr: `Brr_io.Fetch.Cache` |

## Workers

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| Web Workers | Yes | Yes | `Worker` · Brr: `Brr_webworkers.Worker` |
| Service Workers | Yes | Yes | `ServiceWorker` · Brr: `Brr_webworkers.Service_worker` |
| Shared Workers | No | Yes | Brr: `Brr_webworkers.Worker.Shared` |

## File & Binary Data

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| File API (File, Blob, FileReader) | Yes | Yes | `File` · Brr: `Brr.File`, `Brr.Blob` |
| Typed Arrays / ArrayBuffer | Yes | Yes | `Typed_array` · Brr: `Brr.Tarray` |
| SharedArrayBuffer / Atomics | No | No | [#1930](https://github.com/ocsigen/js_of_ocaml/issues/1930) |
| Streams API | No | No | |
| Encoding API (TextEncoder/TextDecoder) | No | Partial | Brr: `Brr.Tarray` (of_jstr/to_jstr) |

## Form

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| FormData | Yes | Yes | `Form` · Brr: `Brr_io.Form.Data` |

## Graphics

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| Canvas 2D | Yes | Yes | `Dom_html` (canvasRenderingContext2D) · Brr: `Brr_canvas.C2d` |
| WebGL | Yes | Yes | `WebGL` · Brr: `Brr_canvas.Gl` |
| WebGL2 | Yes | Yes | `WebGL2` · Brr: `Brr_canvas.Gl` (WebGL2 by default) |

## Media

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| HTMLMediaElement (audio/video) | Partial | Yes | `Dom_html` (audioElement, videoElement) · Brr: `Brr_io.Media.El` |
| Web Audio API | No | Yes | Brr: `Brr_webaudio` |
| Media Capture (getUserMedia) | No | Yes | Brr: `Brr_io.Media.Devices` |
| Media Source Extensions | No | No | |
| WebRTC | No | No | |

## UI / Input

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| Mouse Events | Yes | Yes | `Dom_html` · Brr: `Brr.Ev.Mouse` |
| Keyboard Events | Yes | Yes | `Dom_html` · Brr: `Brr.Ev.Keyboard` |
| Touch Events | Yes | No | `Dom_html` |
| Pointer Events | Yes | Yes | `Dom_html` · Brr: `Brr.Ev.Pointer` |
| Wheel Events | Yes | Yes | `Dom_html` · Brr: `Brr.Ev.Wheel` |
| Drag and Drop Events | Yes | Yes | `Dom_html` · Brr: `Brr.Ev.Drag` |
| Clipboard API | Yes | Yes | `Clipboard` · Brr: `Brr_io.Clipboard` |
| Fullscreen API | Yes | Yes | `Dom_html` (`requestFullscreen` / `exitFullscreen`) · Brr: `Brr.El.request_fullscreen`, `Brr.Document.exit_fullscreen` |
| Gamepad API | No | No | |
| Pointer Lock API | Yes | Yes | `Dom_html` (`requestPointerLock`) · Brr: `Brr.El.request_pointer_lock` |
| Popover API | Yes | Partial | `Dom_html` (`showPopover`, `hidePopover`, `togglePopover` with `?force`, `popover` attribute, `popoverTargetElement`/`popoverTargetAction` on input/button, `beforetoggle`/`toggle` events, `ToggleEvent.source`) · Brr: `Brr.El.{show,hide,toggle}_popover` (no `force` arg, no toggle event bindings) |
| Selection API | Yes | No | `Dom_html` (`selection`, `range`) |

## Observers

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| Intersection Observer | Yes | No | `IntersectionObserver` |
| Mutation Observer | Yes | No | `MutationObserver` |
| Resize Observer | Yes | No | `ResizeObserver` |
| Performance Observer | Yes | No | `PerformanceObserver` |

## Location / Sensors

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| Geolocation | Yes | Yes | `Geolocation` · Brr: `Brr_io.Geolocation` |
| Screen Orientation | No | No | |

## Internationalization

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| Intl (Collator, DateTimeFormat, NumberFormat, PluralRules, RelativeTimeFormat) | Yes | No | `Intl` |

## Navigation

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| History API | Partial | Yes | `Dom_html` (history) · Brr: `Brr.Window.History` |
| URL API | Yes | Yes | `Url` · Brr: `Brr.Uri` |

## Other

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| requestAnimationFrame | Yes | Yes | `Dom_html` (window) · Brr: `Brr.G.request_animation_frame` |
| Performance API (now, mark, measure, entries) | Yes | Partial | `Performance` · Brr: `Brr.Performance` |
| Web Animations API | Yes | No | `Dom_html` (`animate`, `getAnimations`; `animation`, `animationEffect`, `keyframeEffect`, `computedKeyframe`, `animationTimeline`, `documentTimeline`, `optionalEffectTiming`, `computedEffectTiming`, `keyframeAnimationOptions`, `animationPlaybackEvent`) |
| Web Components (Custom Elements, Shadow DOM) | Partial | No | `Dom_html` (Shadow DOM — `attachShadow`, `shadowRoot`, `assignedSlot`, `slot`); Custom Elements not bound |
| Web Crypto API | Yes | Yes | `Crypto` · Brr: `Brr_webcrypto` |
| Notifications API | Yes | Yes | `Notification` · Brr: `Brr_io.Notification` |
| Broadcast Channel API | No | Yes | Brr: `Brr_io.Message.Broadcast_channel` |
| AbortController / AbortSignal | Yes | Yes | `Abort` · Brr: `Brr.Abort` |

---

## Priority classification of missing APIs

APIs missing or incomplete in js_of_ocaml, ranked by importance. Criteria: how
widely used the API is in modern web development, whether an open issue exists,
whether Brr already provides it (proving OCaml ecosystem demand), and whether
other APIs depend on it.

### Tier 1 — High

All previously Tier 1 APIs are now implemented: **Web Crypto** (`Crypto`),
**Clipboard** (`Clipboard`), **Notifications** (`Notification`), **Service
Workers** (`ServiceWorker`), the **Cache API** (`Cache`) and **MessageChannel /
MessagePort** (`MessageChannel`) — together enabling PWAs and structured
Worker/iframe/window communication.

### Tier 2 — Medium

| API | Issue | In Brr | Why |
|-----|-------|--------|-----|
| Broadcast Channel API | — | Yes | Cross-tab communication (sync auth state, shared data). Simple API. |
| Web Audio API | — | Yes | Audio processing, games, music apps. Large API surface but well-defined. |
| Media Capture (getUserMedia) | — | Yes | Video calls, camera/mic access. Growing use with remote work tooling. |
| Encoding API (TextEncoder/TextDecoder) | — | Partial | Needed for binary protocol work and streaming text. Small surface. |
| Shared Workers | — | Yes | Shared state across tabs. Niche but Brr covers it. |
| IndexedDB | — | No | Client-side database for offline apps. Large API but important for data-heavy PWAs. |
| Streams API | — | No | Modern data processing. Fetch response bodies are ReadableStreams. Increasingly foundational. |
| History API (upgrade to full) | — | Yes | SPA routing depends on pushState/replaceState. Current binding is limited. |
| HTMLMediaElement (upgrade to full) | — | Yes | Better audio/video control. Current binding only covers basic element types. |

### Tier 3 — Lower priority

| API | Issue | In Brr | Why |
|-----|-------|--------|-----|
| SharedArrayBuffer / Atomics | [#1930](https://github.com/ocsigen/js_of_ocaml/issues/1930) | No | Advanced concurrency. Requires cross-origin isolation headers. |
| WebRTC | — | No | Peer-to-peer media/data. Very large, complex API. |
| Web Components — Custom Elements | — | No | Large surface, competes with framework approaches. Shadow DOM portion of Web Components is already bound. |
| CSS Typed OM | — | No | Emerging API, limited browser adoption until recently. |
| Media Source Extensions | — | No | Video streaming (HLS/DASH players). Very niche. |
| Gamepad API | — | No | Gaming only. |
| Screen Orientation | — | No | Mobile-specific. |
| Beacon API | — | No | Analytics pings. Trivial to call via `Js.Unsafe`. |

### Suggested implementation order

1. **Broadcast Channel** — small API that fills out the
   remaining communication gaps.
