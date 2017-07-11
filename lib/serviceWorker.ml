open Js
open Dom_html
open Promise

class type extendableEvent = object 
  inherit event
  method waitUntil: 'a. 'a promise t -> unit meth
end

module Notification = struct

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

  let create_fun_with_notification_options 
      ?(dir=string "auto")
      ?(lang = string "")
      ?(badge = string "")
      ?(body = string "")
      ?(tag = string "")
      ?(icon = string "")
      ?(image= string "")
      ?(data : 'a option)
      ?(vibrate : int js_array t option)
      ?(renotify=_false)
      ?(requireInteraction = _false )
      ?(actions : 'a js_array t option)
      ?(slient=_false)
      ?(sound= string "")
      ?(noscreen =_false)
      ?(sticky =_false)
      ~f 
      ()
    =
    let l = object%js 
      val dir = dir
      val lang = lang
      val badge = badge
      val body = body
      val tag = tag
      val icon = icon
      val image = image
      val data = 
        match data with
        | None -> undefined
        | Some data -> def data
      val vibrate = 
        match vibrate with
        | None -> undefined
        | Some vibrate -> def vibrate
      val renotify = renotify
      val requireInteraction = requireInteraction
      val actions = 
        match actions with
        |None -> undefined
        |Some actions -> def actions
      val slient = slient
      val sound = sound
      val noscreen = noscreen
      val sticky = sticky
    end in f l

  let create_notification (title: js_string t) =
    create_fun_with_notification_options
      ~f:(fun l ->
          let _Notification = Unsafe.global##._Notification in
          new%js _Notification title l) 
end

module Push = struct

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

  let pushManager_meth_withOptions 
      ?(unserVisibleOnly:bool t option)
      ?(applicationServerKey: js_string t option)
      ~(meth_name:string) 
      (pushManager: pushManager t) =
    let l =object%js
      val unserVisibleOnly = unserVisibleOnly
      val applicationServerKey = applicationServerKey
    end in
    Unsafe.meth_call pushManager meth_name [|Unsafe.inject l|]

  let permission_state_withOptions = 
    pushManager_meth_withOptions ~meth_name:"permissionState"

  let subscribe_withOptions =
    pushManager_meth_withOptions ~meth_name:"subscribe"

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

let showNotification_withOptions 
    (sw_registration:('a,'b) serviceWorkerRegistration) 
    (title : js_string t) =
  Notification.create_fun_with_notification_options
    ~f:(fun l -> 
        Unsafe.meth_call sw_registration "showNotification" 
          [|Unsafe.inject title; Unsafe.inject l|])

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

let register_withOptions 
    (sw_container:('a,'b) serviceWorkerContainer)
    ?(scope: js_string t option)
    (scriptURL : js_string t )
  : ('a,'b) serviceWorkerRegistration t promise t =
  let l = object%js 
    val scope = match scope with
      |None -> undefined
      |Some scope -> def scope
  end in
  Unsafe.meth_call sw_container "register" [|Unsafe.inject scriptURL ; Unsafe.inject l|]

module Fetch = struct

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

  let create_response 
      ?(body:response_elt option)
      ?(status: int option)
      ?(statusText : js_string t option)
      ?(headers : headers t option)
      () =
    let _Response = Unsafe.global##._Response in 
    let l = object%js 
      val status = status
      val statusText = statusText
      val headers = headers
    end in
    match body with
    | None -> new%js _Response l
    | Some body -> new%js _Response body l

end

module Cache = struct
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

  let cache_fun_withOptions 
      ?(ignoreSearch=_false)
      ?(ignoreMethod=_false)
      ?(ignoreVary=_false)
      ?(cacheName:js_string t option)
      ~(fun_name: string )
      (cache: cache_elt t)
      (request : request_elt)
    =
    let l = object%js 
      val ignoreSearch = ignoreSearch
      val ignoreMethod = ignoreMethod
      val ignoreVary = ignoreVary
      val cacheName = cacheName
    end in
    Unsafe.meth_call cache fun_name [|Unsafe.inject request; Unsafe.inject l|]

  let match_withOptions = cache_fun_withOptions ~fun_name:"match"
  let matchAll_withOptions = cache_fun_withOptions ~fun_name:"matchAll"
  let delete_withOptions = cache_fun_withOptions ~fun_name:"delete"
  let keys_withOptions = cache_fun_withOptions ~fun_name:"keys"

end

module Client = struct

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

  let post_message (client:client) ?transfer message : unit =
    match transfer with
    | None -> Unsafe.meth_call client "postMessage" [|Unsafe.inject message|]
    | Some transfer ->
      Unsafe.meth_call 
        client "postMessage" [|Unsafe.inject message; Unsafe.inject transfer|]

  let matchAll_withOptions ?(includeUncontrolled=_false) ?(_type = string "all")
      (clients:clients): client t js_array t promise t =
    Unsafe.meth_call clients "matchAll" 
      [|Unsafe.inject includeUncontrolled; Unsafe.inject _type|]

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

class type installEvent = extendableEvent
class type activateEvent = extendableEvent
class type fetchEvent = object
  inherit extendableEvent

  method respondWith : Fetch.response t promise t -> unit meth
  method request: Fetch.request t readonly_prop
end

let get_self () : ('a,'b) serviceWorkerGlobalScope t = Unsafe.global##.self

let addEvent e h =
  addEventListener (get_self ()) e h _false

let addInstallListener handler : event_listener_id =
  addEvent (Event.make "install" : installEvent t Event.typ) handler

let addActivateListener handler : event_listener_id =
  addEvent (Event.make "activate" : installEvent t Event.typ) handler

let addFetchListener handler : event_listener_id =
  addEvent (Event.make "fetch" : fetchEvent t Event.typ) handler