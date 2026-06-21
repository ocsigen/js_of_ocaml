
# Module `Tyxml_js.Register`

```ocaml
val html : ?head:Html_types.head Html.elt -> Html_types.body Html.elt -> unit
```
`Register.html head body` uses the given head and body elements as document. It replaces the previous body and head.

`head` and `body` can be reactive.

```ocaml
val body : ?keep:bool -> [< Html_types.body_content ] Html.elt list -> unit
```
`Register.body elements` add `elements` as children of `body`. If `keep` is false (default is true), the children of the body are removed before adding the new elements.

```ocaml
val head : ?keep:bool -> [< Html_types.head_content ] Html.elt list -> unit
```
`Register.head elements` add `elements` as children of `body`. If `keep` is false (default is true), the children of the head are removed before adding the new elements.

```ocaml
val id : ?keep:bool -> string -> 'a Html.elt list -> unit
```
`Register.id "some_id" elements` add `elements` as children of the node with the id `"some_id"`. If `keep` is false (default is true), the children of the node are removed before adding the new elements.

Beware, this function ignores tyxml's type information.
