open Js
open Dom_html
open Promise

class type extendableEvent = object 
  inherit event
  method waitUntil: 'a. 'a promise t -> unit meth
end

module Notification : sig

  class type notification = object
    inherit eventTarget

    method permission : js_string t readonly_prop
    method actions: 'a . 'a js_array t readonly_prop
    method badge : js_string t readonly_prop
    method body : js_string t readonly_prop
    method data : 'a . 'a readonly_prop
    method dir : js_string t readonly_prop
    method lang : js_string t readonly_prop
    method tag : js_string t readonly_prop
    method icon : js_string t readonly_prop
    method requireInteraction : bool t readonly_prop
    method slient : bool t readonly_prop
    method timestamp : int readonly_prop
    method title : js_string t readonly_prop
    method vibrate : int js_array t readonly_prop
    method requestPermisson : js_string t promise t meth
    method close : unit meth
  end

  class type notificationEvent = object
    inherit extendableEvent

    method notification : notification t readonly_prop
    method action : js_string t Optdef.t readonly_prop
  end

  val create_notification :
    js_string t ->
    ?dir:js_string t ->
    ?lang:js_string t ->
    ?badge:js_string t ->
    ?body:js_string t ->
    ?tag:js_string t ->
    ?icon:js_string t ->
    ?image:js_string t ->
    ?data:'a ->
    ?vibrate:int js_array t ->
    ?renotify:bool t ->
    ?requireInteraction:bool t ->
    ?actions:'a js_array t ->
    ?slient:bool t ->
    ?sound:js_string t ->
    ?noscreen:bool t -> 
    ?sticky:bool t -> 
    unit -> notification t
end

module Push : sig

  class type pushManager = object
    method getSubscription : pushSubscription t Opt.t promise t meth
    method permissionState : js_string t promise t meth
    method subscribe : pushSubscription t promise t meth
  end

  and pushSubscription = object
    method endpoint : js_string t readonly_prop
    method options : 'a. 'a readonly_prop
    method getKey : js_string t -> Typed_array.arrayBuffer t Opt.t meth
    method toJSON : 'a. 'a meth
    method unsubscribe : bool t promise t meth
  end

  class type pushMessageData = object
    method arrayBuffer : Typed_array.arrayBuffer t meth
    method blob : File.blob t meth
    method json : 'a. 'a meth
    method text : js_string t meth
  end

  class type pushEvent = object
    inherit extendableEvent
    method data : pushMessageData t readonly_prop
  end

  val permission_state_withOptions :
    ?unserVisibleOnly:bool t ->
    ?applicationServerKey:js_string t -> pushManager t -> 'a
  val subscribe_withOptions :
    ?unserVisibleOnly:bool t ->
    ?applicationServerKey:js_string t -> pushManager t -> 'a

end

class type statechangeEvent = object
  inherit event
  method state : js_string t readonly_prop
end

class type ['a,'b] serviceWorker = object ('self)
  inherit ['a,'b] Worker.worker

  method scriptURL : js_string t readonly_prop
  method state : js_string t readonly_prop
  method onstatechange : ('self t, statechangeEvent) event_listener readonly_prop  
end

class type ['a,'b] serviceWorkerRegistration = object ('self)
  inherit eventTarget

  method scope : js_string t readonly_prop
  method installing : ('a, 'b) serviceWorker t Opt.t readonly_prop
  method waiting : ('a,'b) serviceWorker t Opt.t readonly_prop
  method active : ('a,'b) serviceWorker t Opt.t readonly_prop
  method pushManager : Push.pushManager t promise t meth
  method getNotificatons : 
    Notification.notification t js_array t promise t meth
  method showNotification : 
    js_string t -> Notification.notificationEvent t promise t meth
  method update : unit meth
  method unregister : unit meth
end

val showNotification_withOptions :
  ('a, 'b) serviceWorkerRegistration ->
  js_string t ->
  ?dir:js_string t ->
  ?lang:js_string t ->
  ?badge:js_string t ->
  ?body:js_string t ->
  ?tag:js_string t ->
  ?icon:js_string t ->
  ?image:js_string t ->
  ?data:'c ->
  ?vibrate:int js_array t ->
  ?renotify:bool t ->
  ?requireInteraction:bool t ->
  ?actions:'c js_array t ->
  ?slient:bool t ->
  ?sound:js_string t ->
  ?noscreen:bool t -> 
  ?sticky:bool t -> 
  unit -> 
  Notification.notificationEvent t promise t

class type controllerchangeEvent = event

class type ['a,'b] serviceWorkerContainer = object ('self)
  inherit eventTarget

  method controller : ('a,'b) serviceWorker t Opt.t readonly_prop
  method ready: ('a,'b) serviceWorkerRegistration t promise t meth
  method oncontrollerchange : 
    ('self t, controllerchangeEvent t) event_listener writeonly_prop
  method onerror: ('self t, Worker.errorEvent t) event_listener writeonly_prop
  method onmessage: ('self t, 'b Worker.messageEvent t) event_listener writeonly_prop
  method register : js_string t -> ('a,'b) serviceWorkerRegistration t promise t meth 
  method getRegistration : ('a,'b) serviceWorkerRegistration t Optdef.t promise t meth
  method getRegistration_withScope : 
    js_string t -> ('a,'b) serviceWorkerRegistration t Optdef.t promise t meth
  method getRegistrations : 
    ('a,'b) serviceWorkerRegistration t js_array t promise t meth
end

val register_withOptions :
  ('a, 'b) serviceWorkerContainer ->
  ?scope:js_string t ->
  js_string t ->
  ('a, 'b) serviceWorkerRegistration t Promise.promise t

module Fetch : sig

  class type _body = object
    method bodyUsed : bool t readonly_prop
    method arrayBuffer : Typed_array.arrayBuffer t promise t meth
    method blob : File.blob t promise t meth
    method formData : Form.formData t promise t meth
    method json : 'a . 'a promise t meth
    method text : js_string t promise t meth
  end

  and headers = object
    method append : js_string t -> js_string t -> unit meth
    method delete : js_string t -> unit meth
    method get : js_string t -> js_string t opt meth
    method has : js_string t -> bool t meth
    method set : js_string t -> js_string t -> unit meth
  end

  and response = object
    inherit _body

    method headers : headers t readonly_prop
    method ok : bool t readonly_prop
    method redirected : bool t readonly_prop
    method status : int readonly_prop
    method statusText : js_string t readonly_prop
    method _type : js_string t readonly_prop
    method url : js_string t readonly_prop
    method clone : response t meth
    method error : Unsafe.any -> unit meth
    method redirect : js_string t -> response t meth
    method redirectwithStatus : js_string t -> int -> response t meth
  end

  and request = object
    method _method: js_string t readonly_prop
    method url: js_string t readonly_prop
    method headers : headers t readonly_prop
    method referrer : js_string t readonly_prop
    method referrerPolicy : js_string t readonly_prop
    method mode : js_string t readonly_prop
    method credentials : js_string t readonly_prop
  end

  type response_elt =
    [`Blob of File.blob t|
     `ArrayBuffer of Typed_array.arrayBuffer t |
     `BufferSource of Typed_array.arrayBufferView t |
     `FormData of Form.formData t|
     `URLSearchParams of Url.urlSearchParams t |
     `String of js_string t
    ]

  val create_response :
    ?body:response_elt ->
    ?status:int ->
    ?statusText:js_string t ->
    ?headers:headers t -> unit -> response t

end

module Cache : sig
  open Fetch

  type request_elt =
    [`Request of request t|`String of js_string t]

  class type cache_elt = object
    method _match : request t -> response t Opt.t promise t meth
    method match_withUrl : js_string t -> response t Opt.t promise t meth
  end

  class type cache = object
    inherit cache_elt
    method has : js_string t -> bool t promise t meth
    method matchAll : request t -> response t js_array t promise t meth
    method add : request t -> unit promise t meth
    method add_withUrl : js_string t  -> unit promise t meth
    method addAll : request t js_array t -> unit promise t meth
    method addAll_withUrl : js_string t js_array t -> unit promise t meth
    method put : request t -> response t -> unit promise t meth
    method put_withUrl : js_string t -> response t -> unit promise t meth
    method delete : request t -> bool t promise t meth 
    method delete_withUrl : js_string t -> bool t promise t meth 
    method keys : js_string t js_array t promise t meth
  end

  class type cacheStorage = object 
    inherit cache_elt

    method has : js_string t -> bool t promise t meth
    method _open : js_string t -> cache t promise t meth
    method keys : js_string t js_array t promise t meth
    method delete : js_string t -> bool t promise t meth 
  end

  val match_withOptions :
    ?ignoreSearch:bool t ->
    ?ignoreMethod:bool t ->
    ?ignoreVary:bool t ->
    ?cacheName:js_string t -> 
    cache_elt t -> request_elt -> response t Opt.t promise t
  val matchAll_withOptions :
    ?ignoreSearch:bool t ->
    ?ignoreMethod:bool t ->
    ?ignoreVary:bool t ->
    ?cacheName:js_string t -> 
    cache_elt t -> request_elt -> response t js_array t promise t
  val delete_withOptions :
    ?ignoreSearch:bool t ->
    ?ignoreMethod:bool t ->
    ?ignoreVary:bool t ->
    ?cacheName:js_string t -> 
    cache_elt t -> request_elt -> bool t promise t 
  val keys_withOptions :
    ?ignoreSearch:bool t ->
    ?ignoreMethod:bool t ->
    ?ignoreVary:bool t ->
    ?cacheName:js_string t -> 
    cache_elt t -> request_elt -> js_string t js_array t promise t

end

module Client : sig

  class type client = object
    method frameType : js_string t readonly_prop
    method id : js_string t readonly_prop
    method _type : js_string t readonly_prop
    method url : js_string t readonly_prop
  end

  class type windowClient = object
    inherit client

    method focus : windowClient t promise t meth
    method navigate : js_string t -> windowClient t promise t meth
    method focused : bool t readonly_prop
    method visibilityState : js_string t readonly_prop
  end

  class type clients = object
    method get : js_string t -> client t Opt.t promise t meth
    method matchAll : client t js_array t promise t meth
    method openWindow : js_string t -> windowClient t promise t meth
    method claim : unit promise t meth
  end

  val post_message : client -> ?transfer:'a -> 'b -> unit

  val matchAll_withOptions :
      ?includeUncontrolled:bool t ->
      ?_type:js_string t ->
      clients -> client t js_array t Promise.promise t

end

class type installEvent = extendableEvent
class type activateEvent = extendableEvent
class type fetchEvent = object
  inherit extendableEvent

  method respondWith : Fetch.response t promise t -> unit meth
  method request: Fetch.request t readonly_prop
end

class type sw_navigator = object
  inherit navigator

  method serviceWorker : 'a 'b. ('a,'b) serviceWorkerContainer t readonly_prop
end

class type ['a,'b] serviceWorkerGlobalScope = object ('self)
  inherit eventTarget

  method clients: Client.clients t readonly_prop
  method registration : ('a,'b) serviceWorkerRegistration t readonly_prop
  method caches : Cache.cacheStorage t readonly_prop
  method navigator : sw_navigator t readonly_prop
  method onactivate : ('self t, activateEvent t) event_listener writeonly_prop
  method onfetch : ('self t, fetchEvent t) event_listener writeonly_prop
  method oninstall : ('self t, installEvent t) event_listener writeonly_prop
  method onmessage : ('self t, 'b Worker.messageEvent t) event_listener writeonly_prop
  method onnotificationclick : 
    ('self t, Notification.notificationEvent t) event_listener writeonly_prop
  method onnotificationclose :
    ('self t, Notification.notificationEvent t) event_listener writeonly_prop
  method onpush : ('self t , Push.pushEvent t) event_listener writeonly_prop
  method onpushsubscriptionchange :('self t, event t) event_listener writeonly_prop
  method skipWaiting : unit promise t meth
  method fetch : Fetch.request t -> Fetch.response t promise t meth
  method fetch_withUrl : js_string t -> Fetch.response t promise t meth
end

val get_self : unit -> ('a, 'b) serviceWorkerGlobalScope t

val addInstallListener :
  (('a, 'b) serviceWorkerGlobalScope t, installEvent t) event_listener -> event_listener_id
val addActivateListener :
  (('a, 'b) serviceWorkerGlobalScope t, installEvent t) event_listener -> event_listener_id
val addFetchListener :
  (('a, 'b) serviceWorkerGlobalScope t, fetchEvent t) event_listener -> event_listener_id
