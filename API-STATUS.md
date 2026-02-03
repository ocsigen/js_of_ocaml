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
| Promise | Partial | Yes | `Js.Promise` / lwt bindings — [#2031](https://github.com/ocsigen/js_of_ocaml/issues/2031) · Brr: `Jv.Promise`, `Fut` |
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
| Fetch API | No | Yes | [#596](https://github.com/ocsigen/js_of_ocaml/issues/596) · Brr: `Brr_io.Fetch` |
| MessageChannel / MessagePort | No | Yes | [#1464](https://github.com/ocsigen/js_of_ocaml/issues/1464) · Brr: `Brr_io.Message` |
| WebSocket | Yes | Yes | `WebSockets` · Brr: `Brr_io.Websocket` |
| Server-Sent Events (EventSource) | Yes | No | `EventSource` |
| Beacon API | No | No | |

## Storage

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| Web Storage (localStorage/sessionStorage) | Yes | Yes | `Dom_html` (storage) · Brr: `Brr_io.Storage` |
| IndexedDB | No | No | |
| Cache API | No | Yes | Brr: `Brr_io.Fetch.Cache` |

## Workers

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| Web Workers | Yes | Yes | `Worker` · Brr: `Brr_webworkers.Worker` |
| Service Workers | No | Yes | Brr: `Brr_webworkers.Service_worker` |
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
| WebGL2 | No | Yes | [#1226](https://github.com/ocsigen/js_of_ocaml/issues/1226) · Brr: `Brr_canvas.Gl` (WebGL2 by default) |

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
| Clipboard API | No | Yes | Brr: `Brr_io.Clipboard` |
| Fullscreen API | No | Yes | Brr: `Brr.El.request_fullscreen`, `Brr.Document.exit_fullscreen` |
| Gamepad API | No | No | |
| Pointer Lock API | No | Yes | Brr: `Brr.El.request_pointer_lock` |
| Selection API | Partial | No | `Dom_html` (selection, range) — [#453](https://github.com/ocsigen/js_of_ocaml/issues/453) |

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
| Intl (Collator, DateTimeFormat, NumberFormat, PluralRules) | Yes | No | `Intl` |

## Navigation

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| History API | Partial | Yes | `Dom_html` (history) · Brr: `Brr.Window.History` |
| URL API | Yes | Yes | `Url` · Brr: `Brr.Uri` |

## Other

| API | jsoo | Brr | jsoo Module / Notes |
|-----|------|-----|---------------------|
| requestAnimationFrame | Yes | Yes | `Dom_html` (window) · Brr: `Brr.G.request_animation_frame` |
| Performance.now | No | Partial | [#679](https://github.com/ocsigen/js_of_ocaml/issues/679) · Brr: `Brr.Performance` (now, timing entries) |
| Web Animations API | No | No | |
| Web Components (Custom Elements, Shadow DOM) | No | No | |
| Web Crypto API | No | Yes | Brr: `Brr_webcrypto` |
| Notifications API | No | Yes | Brr: `Brr_io.Notification` |
| Broadcast Channel API | No | Yes | Brr: `Brr_io.Message.Broadcast_channel` |
| AbortController / AbortSignal | No | Yes | Brr: `Brr.Abort` |

---

## Priority classification of missing APIs

APIs missing or incomplete in js_of_ocaml, ranked by importance. Criteria: how
widely used the API is in modern web development, whether an open issue exists,
whether Brr already provides it (proving OCaml ecosystem demand), and whether
other APIs depend on it.

### Tier 1 — Critical

These form a dependency chain and should be tackled together.

| API | Issue | In Brr | Why |
|-----|-------|--------|-----|
| Promise (upgrade to full) | [#2031](https://github.com/ocsigen/js_of_ocaml/issues/2031) | Yes | Core async primitive of JavaScript. Prerequisite for idiomatic Fetch, Web Crypto, and most modern APIs. |
| AbortController / AbortSignal | — | Yes | Required for cancelling Fetch requests, event listeners, and streams. Foundational primitive that Fetch and Streams depend on. |
| Fetch API | [#596](https://github.com/ocsigen/js_of_ocaml/issues/596) | Yes | The standard replacement for XHR. Virtually every modern web app uses it. The single most impactful missing binding. |

### Tier 2 — High

| API | Issue | In Brr | Why |
|-----|-------|--------|-----|
| Performance.now | [#679](https://github.com/ocsigen/js_of_ocaml/issues/679) | Partial | Essential for profiling, benchmarking, and animation timing. Tiny API surface — quick win. |
| Web Crypto API | — | Yes | Required for authentication (JWT, PKCE), token generation, hashing. No safe workaround. |
| Clipboard API | — | Yes | Copy/paste is a basic UX expectation. The older `document.execCommand` path is deprecated. |
| Service Workers | — | Yes | Required for PWAs and offline-capable apps. Cache API depends on it. |
| Cache API | — | Yes | Paired with Service Workers for offline support and network caching strategies. |
| MessageChannel / MessagePort | [#1464](https://github.com/ocsigen/js_of_ocaml/issues/1464) | Yes | Structured communication between Workers, iframes, and windows. Needed for non-trivial Worker usage. |
| Notifications API | — | Yes | Common engagement feature in web apps. Small API surface. |

### Tier 3 — Medium

| API | Issue | In Brr | Why |
|-----|-------|--------|-----|
| Fullscreen API | — | Yes | Media players, presentations, games. Small API — a few methods on Element and Document. |
| Broadcast Channel API | — | Yes | Cross-tab communication (sync auth state, shared data). Simple API. |
| Web Audio API | — | Yes | Audio processing, games, music apps. Large API surface but well-defined. |
| Media Capture (getUserMedia) | — | Yes | Video calls, camera/mic access. Growing use with remote work tooling. |
| Encoding API (TextEncoder/TextDecoder) | — | Partial | Needed for binary protocol work and streaming text. Small surface. |
| Shared Workers | — | Yes | Shared state across tabs. Niche but Brr covers it. |
| IndexedDB | — | No | Client-side database for offline apps. Large API but important for data-heavy PWAs. |
| Streams API | — | No | Modern data processing. Fetch response bodies are ReadableStreams. Increasingly foundational. |
| History API (upgrade to full) | — | Yes | SPA routing depends on pushState/replaceState. Current binding is limited. |
| HTMLMediaElement (upgrade to full) | — | Yes | Better audio/video control. Current binding only covers basic element types. |
| Pointer Lock API | — | Yes | 3D/game applications. Small API. |

### Tier 4 — Lower priority

| API | Issue | In Brr | Why |
|-----|-------|--------|-----|
| WebGL2 | [#1226](https://github.com/ocsigen/js_of_ocaml/issues/1226) | Yes | 3D graphics. Large API surface, niche audience. |
| SharedArrayBuffer / Atomics | [#1930](https://github.com/ocsigen/js_of_ocaml/issues/1930) | No | Advanced concurrency. Requires cross-origin isolation headers. |
| WebRTC | — | No | Peer-to-peer media/data. Very large, complex API. |
| Web Components (Custom Elements, Shadow DOM) | — | No | Large surface, competes with framework approaches. |
| CSS Typed OM | — | No | Emerging API, limited browser adoption until recently. |
| Web Animations API | — | No | CSS animations cover most use cases. |
| Media Source Extensions | — | No | Video streaming (HLS/DASH players). Very niche. |
| Gamepad API | — | No | Gaming only. |
| Screen Orientation | — | No | Mobile-specific. |
| Beacon API | — | No | Analytics pings. Trivial to call via `Js.Unsafe`. |
| Selection API (upgrade) | [#453](https://github.com/ocsigen/js_of_ocaml/issues/453) | No | Rich text editors. Niche but has an open issue. |

### Suggested implementation order

1. **Promise (full) + AbortController + Fetch API** — as a single effort, since
   they are interdependent. Closes the largest gap and addresses the oldest open
   feature request ([#596](https://github.com/ocsigen/js_of_ocaml/issues/596), from 2017).
2. **Performance.now** — quick win, tiny surface, has an open issue.
3. **Web Crypto API** — security-critical, no safe workaround.
4. **Clipboard API** — small surface, high user-facing value.
5. **Service Workers + Cache API** — enables PWAs, the main class of apps jsoo
   cannot fully support today.
6. **MessageChannel / Notifications / Broadcast Channel** — small APIs that fill
   out the remaining communication gaps.
