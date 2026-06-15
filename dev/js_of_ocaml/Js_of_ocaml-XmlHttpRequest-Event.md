
# Module `XmlHttpRequest.Event`

```ocaml
type typ = xmlHttpRequest File.progressEvent Js.t Dom.Event.typ
```
```ocaml
val readystatechange : xmlHttpRequest Dom.event Js.t Dom.Event.typ
```
```ocaml
val loadstart : typ
```
```ocaml
val progress : typ
```
```ocaml
val abort : typ
```
```ocaml
val error : typ
```
```ocaml
val load : typ
```
```ocaml
val timeout : typ
```
```ocaml
val loadend : typ
```