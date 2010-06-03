// oBrowser compatibility functions

// Caml_js

//Provides: caml_js_alert
function caml_js_alert(msg) {
  window.alert(msg.toString()); return 0;
}

//Provides: caml_js_http_get_with_status
function caml_js_http_get_with_status (url) {
  var xmlhttp = new XMLHttpRequest();
  xmlhttp.open("GET", url, false);
  xmlhttp.send(null);
  return [0, xmlhttp.status, new MlString (xmlhttp.responseText)];
}

//Provides: caml_js_mutex_create const
function caml_js_mutex_create (unit) { return [0]; }
//Provides: caml_js_mutex_lock
function caml_js_mutex_lock (m) { m[0]=1; return 0; }
//Provides: caml_js_mutex_unlock
function caml_js_mutex_unlock (m) { m[0]=0; return 0; }
//Provides: caml_js_mutex_trylock
function caml_js_mutex_try_lock (m) {
  if (m[0] == 0) {
    m[0] = 1;
    return 1;
  } else
    return 0;
}

//Provides: caml_js_params const
function caml_js_params (e) { return [0]; }

// Jsoo

//Provides: jsoo_call
function jsoo_call (d, args, o){ return o.apply (d, args.slice(1)); }
//Provides: jsoo_eval
function jsoo_eval(s) { return eval(s.toString()); }
//Provides: jsoo_extract const
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
  if (o instanceof Array)
    return [3, o];
  return [0, o];
}
//Provides: jsoo_inject const
function jsoo_inject(x) {
  switch (typeof x) {
    case "object": return x[1].toString();
    default:       return null;
  }
}

//Provides: jsoo_get mutable
function jsoo_get (f, o) { return o[f.toString()]; }
//Provides: jsoo_set
function jsoo_set(f, v, o) { o[f.toString()] = v; return 0; }

//Provides: jsoo_wrap_event
//Provides: jsoo_get_event_args const
var event_args;
function jsoo_wrap_event (clos) {
  return function(evt) { event_args = evt; caml_call_gen(clos, [0]); }
}
function jsoo_get_event_args (unit) {
  return event_args;
}

//Provides: thread_kill const
function thread_kill (unit) { return 0; }
//Provides: thread_delay const
function thread_delay (unit) { return 0; }
//Provides: thread_self const
function thread_self (unit) { return 0; }
//Provides: thread_uncaught_exception
function thread_uncaught_exception(e){ throw (e); }
//Provides: thread_new const
function thread_new (clos, arg) { }

// Rtjs

//Provides: caml_js_dom_of_xml const
function caml_js_dom_of_xml (str)
{
  var sstr = str.toString ();
  try { //IE
    xmlDoc = new ActiveXObject("Microsoft.XMLDOM");
    xmlDoc.async = "false";
    xmlDoc.loadXML(sstr);
    return xmlDoc;
  } catch(e) {
    parser = new DOMParser();
    return parser.parseFromString(sstr,"text/xml");
  }
}

//Provides: caml_js_node_children mutable
function caml_js_node_children (node) {
  var res = 0, children = node.childNodes, c = children.length;
  while (c--) res = [0, children[c], res];
  return res;
}

///////////////////////////////////////////////////////////////

// Regexp

//Provides: caml_regexp_make mutable
function caml_regexp_make (vs, vf) {
  var s = vs.toString();
  var f = vf.toString();
  return new RegExp (s, f);
}

//Provides: caml_regexp_split mutable
function caml_regexp_split (vr, vs) {
    var r = vr ;
    var s = vs.toString() ;
    var res = s.split (r);
    var vres = [0];
    for (var i = 0;i < res.length;i++) {
        vres[i] = new MlString (res[i]);
    }
    return vres;
}

// input_val
//XXX caml_input_value_from_string
//XXX caml_marshal_data_size
//XXX caml_output_value_to_string
