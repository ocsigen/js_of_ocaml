
# Module `Js_of_ocaml.Regexp`

Types for regexps.

*Warning:* the regexp syntax is the javascript one. It differs from the syntax used by the `Str` module from the OCaml standard library.

see [https://developer.mozilla.org/en/JavaScript/Reference/Global\_Objects/RegExp\#section\_5](https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/RegExp#section_5) Regexp object on Mozilla Developer Network.
```ocaml
type regexp
```
The type for regexps.

```ocaml
type result
```
The type for match result.

Constructors

```ocaml
val regexp : string -> regexp
```
Simple regexp constructor.

```ocaml
val regexp_case_fold : string -> regexp
```
Same as `regexp` but matching will be performed in a case insensitive way.

```ocaml
val regexp_with_flag : string -> string -> regexp
```
Regexp constructor with flag that allow for case-insensitive search or multiline search (the global flag is always set).

```ocaml
val quote : string -> string
```
Escapes characters with special meaning in the regexp context.

```ocaml
val regexp_string : string -> regexp
```
`regexp_string s` creates a regexp matching the exact string `s`.

```ocaml
val regexp_string_case_fold : string -> regexp
```
Same as `regexp_string` but matching will be performed in a case insensitive way.

Functions

```ocaml
val string_match : regexp -> string -> int -> result option
```
`string_match r s i` matches the string `s` starting from the `i`th character. Evaluates to `None` if `s` (from the `i`th character) doesn't match `r`.

```ocaml
val search : regexp -> string -> int -> (int * result) option
```
`search r s i` evaluates to the index of the match and the match result or `None` if `s` (starting from `i`) doesn't match `r`.

```ocaml
val search_forward : regexp -> string -> int -> (int * result) option
```
Same as `search`.

```ocaml
val matched_string : result -> string
```
`matched_string r` return the exact substring that matched when evaluating `r`.

```ocaml
val matched_group : result -> int -> string option
```
`matched_group r i` is the `i`th group matched. Groups in matches are \* obtained with parentheses. Groups are 1-based.

```ocaml
val global_replace : regexp -> string -> string -> string
```
`global_replace r s by` replaces all of the matches of `r` in `s` by `by`.

```ocaml
val replace_first : regexp -> string -> string -> string
```
`replace_first r s by` replaces the first match of `r` in `s` by `by`.

```ocaml
val split : regexp -> string -> string list
```
`split r s` splits the string `s` erasing matches with `r`. `split (regexp " ") "toto tutu tata"` is `["toto";"tutu";"tata"]`.

```ocaml
val bounded_split : regexp -> string -> int -> string list
```
`bounded_split r s i` is like `split r s` except that the result's length is less than `i`.
