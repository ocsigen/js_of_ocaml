
# Module `Js_of_ocaml.IntersectionObserver`

The Intersection Observer API provides a way to asynchronously observe changes in the intersection of a target element with an ancestor element or with a top-level document's viewport.

https://developer.mozilla.org/en-US/docs/Web/API/Intersection\_Observer\_API

```ocaml
class type  intersectionObserverEntry = object ... end
```
```ocaml
class type  intersectionObserverOptions = object ... end
```
```ocaml
class type  intersectionObserver = object ... end
```
```ocaml
val empty_intersection_observer_options : 
  unit ->
  intersectionObserverOptions Js.t
```
```ocaml
val is_supported : unit -> bool
```
```ocaml
val intersectionObserver : 
  ((intersectionObserverEntry Js.t Js.js_array Js.t ->
     intersectionObserver Js.t ->
     unit)
     Js.callback ->
    intersectionObserverOptions Js.t ->
    intersectionObserver Js.t)
    Js.constr
```