// oBrowser compatibility functions

// Caml_js

function caml_js_alert(msg) {
  window.alert(msg.toString()); return 0;
}

function caml_js_http_get_with_status (url) {
  var xmlhttp = new XMLHttpRequest();
  xmlhttp.open("GET", url, false);
  xmlhttp.send(null);
  return [0, xmlhttp.status, new MlString (xmlhttp.responseText)];
}

function caml_js_mutex_create (unit) { return [0]; }
function caml_js_mutex_lock (m) { m[0]=1; return 0; }
function caml_js_mutex_unlock (m) { m[0]=0; return 0; }
function caml_js_mutex_try_lock (m) {
  if (m[0] == 0) {
    m[0] = 1;
    return 1;
  } else
    return 0;
}

function caml_js_node_children (node) {
  var res = 0, children = node.childNodes, l = children.length;
  for (c = l - 1; c >= 0; c--) res = [0, children[c], res];
  return res;
}

function caml_js_params (e) { return [0]; }

// Jsoo

function jsoo_call (d, args, o){ return o.apply (d, args.slice(1)); }
function jsoo_eval(s) { return eval(s.toString()); }
function jsoo_extract (o) {
  //   | Obj of obj        0
  //   | Num of float      1
  //   | String of string  2
  //   | Block of Obj.t    3
  //   | Nil
  if (o == null)
    return 0;
  if (typeof o == 'string')
    return [2, new MlString (o)];
  if (typeof o == 'number')
    return [1, o];
  return [0, o];
}
function jsoo_inject(x) {
  switch (typeof x) {
    case "object": return x[1].toString();
    default:       return null;
  }
}

function jsoo_get (f, o) { return o[f.toString()]; }
function jsoo_set(f, v, o) { o[f.toString()] = v; return 0; }

var event_args;
function jsoo_wrap_event (clos) {
  return function(evt) { event_args = evt; caml_call_gen(clos, [0]); }
}
function jsoo_get_event_args (unit) {
  return event_args;
}

function thread_kill (unit) { return 0; }
function thread_delay (unit) { return 0; }
function thread_self (unit) { return 0; }
function thread_uncaught_exception(e){ throw (e); }
function thread_new (clos, arg) { }
