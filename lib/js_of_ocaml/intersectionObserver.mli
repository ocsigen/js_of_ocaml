(** The Intersection Observer API provides a way to asynchronously observe changes in the
    intersection of a target element with an ancestor element or with a top-level
    document's viewport.

    https://developer.mozilla.org/en-US/docs/Web/API/Intersection_Observer_API *)

class type intersectionObserverEntry = object
  method target : Dom.node Js.t Js.readonly_prop

  method boundingClientRect : Dom_html.clientRect Js.t Js.readonly_prop

  method rootBounds : Dom_html.clientRect Js.t Js.opt Js.readonly_prop

  method intersectionRect : Dom_html.clientRect Js.t Js.readonly_prop

  method intersectionRatio : Js.number_t Js.readonly_prop

  method isIntersecting : bool Js.t Js.readonly_prop

  method time : Js.number_t Js.readonly_prop
end

class type intersectionObserverOptions = object
  method root : Dom.node Js.t Js.writeonly_prop

  method rootMargin : Js.js_string Js.t Js.writeonly_prop

  method threshold : Js.number_t Js.js_array Js.t Js.writeonly_prop
end

class type intersectionObserver = object
  method root : Dom.node Js.t Js.opt Js.readonly_prop

  method rootMargin : Js.js_string Js.t Js.readonly_prop

  method thresholds : Js.number_t Js.js_array Js.t Js.readonly_prop

  method observe : #Dom.node Js.t -> unit Js.meth

  method unobserve : #Dom.node Js.t -> unit Js.meth

  method disconnect : unit Js.meth

  method takeRecords : intersectionObserverEntry Js.t Js.js_array Js.meth
end

val empty_intersection_observer_options : unit -> intersectionObserverOptions Js.t

val is_supported : unit -> bool

val intersectionObserver :
  (   (   intersectionObserverEntry Js.t Js.js_array Js.t
       -> intersectionObserver Js.t
       -> unit)
      Js.callback
   -> intersectionObserverOptions Js.t
   -> intersectionObserver Js.t)
  Js.constr
