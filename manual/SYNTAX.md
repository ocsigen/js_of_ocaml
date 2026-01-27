# Wiki Syntax Reference

This document describes the syntax used in the `.wiki` files in this directory.

## Headings

Use `=` characters at the start of a line. More `=` means a deeper level.
Do not use trailing `=` (unbalanced style).

```
= Level 1 heading
== Level 2 heading
=== Level 3 heading
```

### Anchors

Add an anchor to a heading with `@@id="name"@@` immediately after the leading `=`:

```
==@@id="my-section"@@ Section Title
```

Reference it with `[[#my-section|link text]]`.

## Text Formatting

```
**bold text**
```

## Code

### Inline code

Use triple braces:

```
{{{inline code}}}
```

### Code blocks

Use `<<code language="..."|...>>` for syntax-highlighted blocks:

```
<<code language="ocaml"|
let x = 42
>>
```

Supported languages include `ocaml`, `javascript`, `html`.

For plain preformatted text without highlighting, use triple braces on their own lines:

```
{{{
plain preformatted text
}}}
```

## Links

### Internal manual links

Link to another chapter in the manual:

```
<<a_manual chapter="bindings"|link text>>
```

Link to a specific anchor within another chapter using `fragment`:

```
<<a_manual chapter="options" fragment="optimizations"|link text>>
```

### API documentation links

Link to API documentation with type specifier (`val`, `module`, `type`, `exception`):

```
<<a_api subproject="js_of_ocaml"|val Js_of_ocaml.Js.string>>
<<a_api subproject="js_of_ocaml"|module Js_of_ocaml.Js.Opt>>
<<a_api subproject="js_of_ocaml"|type Js_of_ocaml.Js.t>>
<<a_api subproject="js_of_ocaml"|exception Js_of_ocaml.Js_error.Exn>>
```

Common subprojects: `js_of_ocaml`, `js_of_ocaml-lwt`, `js_of_ocaml-toplevel`.

### External links

```
[[https://example.com|link text]]
```

### Anchor links

Link to an anchor within the same page:

```
[[#anchor-name|link text]]
```

## Lists

### Ordered lists

Use `#` at the start of each line:

```
# First item
# Second item
# Third item
```

### Unordered lists

Use `*` at the start of each line:

```
* Item one
* Item two
* Item three
```

### Nested lists

Indent with spaces:

```
* Item one
  * Nested item
  * Another nested
* Item two
```

## Tables

Use `|` to separate cells. Use `|=` for header cells:

```
|= Header 1 |= Header 2 |= Header 3 |
| Cell 1    | Cell 2    | Cell 3    |
| Cell 4    | Cell 5    | Cell 6    |
```

## Images

```
<<a_img src="path/to/image.png"|alt text>>
```

## File links

Link to a file in the repository:

```
<<a_file src="toplevel/index.html"|link text>>
```
