function jsoo_inject(x) {
    //    window.dump("<"+x+">");
try{
switch (typeof x){
case "object":
//document.write ("[", typeof (x[1]), "]");
//document.write ("[", x[1].toString(), "]");
  return x[1].toString();
default:
  return null;
}}
catch(e) {
document.write ("<<", x, ">>"); throw(e);
}
}
function jsoo_extract (o) {
    //   | Obj of obj        0
    //   | Num of float      1
    //   | String of string  2
    //   | Block of Obj.t    3
    //   | Nil
    if (o == null)
        return 0;
    if (typeof o == 'string') {
        return [2, new MlString (o)];
    }
    if (typeof o == 'number') {
        return [1, o];
    }
    return [0, o];
}
var init_time = (new Date ()).getTime () * 0.001;
function caml_sys_time (unit) {
  return ((new Date ()).getTime () * 0.001 - init_time);
}
function caml_create_string(len) {
return new MlString(len);
}
function caml_blit_string(s1, i1, s2, i2, len) {
s2.replace (i2, s1.contents, i1, len);
return 0
}
var event_args;
function jsoo_wrap_event (clos) {
return function(evt) { event_args = evt; caml_call_1(clos, 0); }
}
function jsoo_get_event_args (unit) {
return event_args;
}

function caml_string_notequal(s1,s2) {
//document.write ("(",s1.toString(),")");
//document.write ("(",s2.toString(),")");
//document.write ("(",s1.notEqual(s2),")");
return (s1.notEqual(s2))?1:0;}
function caml_string_set(s, i, v) {
s.setCharAt(i, v);
return 0;
}
function caml_int64_float_of_bits(x) {return x;}
function caml_ge_float(x, y) {return (x >= y);}
function caml_sub_float(x, y) {return (x - y);}
function caml_register_named_value(dz,dx) { return ;}
function caml_sys_get_argv(xx) { return [0, "foo", [0, "foo", "bar"]]; }
function caml_sys_get_config (e) { return [0, "Unix", 32]; }
function caml_js_params (e) { return [0]; }
function jsoo_eval(s) { return eval(s.toString()); }
function caml_format_int(fmt, i) { return new MlString(String(i)); }
function caml_greaterequal (x, y) {return (x >= y);}
function caml_lessequal (x, y) {return (x <= y);}
function caml_compare (a, b) {
  if (a === b) return 0;
  if (a instanceof MlString) {
    if (b instanceof MlString)
      return a.compare(b)
    else
      return (-1);
  } else if (a instanceof Array) {
    if (b instanceof Array) {
      if (a.length != b.length)
        return (a.length - b.length);
      for (var i = 0;i < a.length;i++) {
        var t = caml_compare (a[i], b[i]);
        if (t != 0) return t;
      }
      return 0;
    } else
      return (-1);
  } else if (b instanceof MlString || b instanceof Array)
      return 1;
  else if (a < b) return (-1); else if (a == b) return 0; else return 1;
}
function caml_equal (x, y) {
return (caml_compare(x,y) == 0)?1:0;
}
function caml_format_float (fmt, x) {
return new MlString(x.toString(10));
}
function caml_js_node_children (n) {
    var node = n;
    try {
        var res = 0;
        var cur = 0;
        var children = node.childNodes;
        for (c = 0;c < children.length;c++) {
            if (res == 0) {
                res = [0, children[c],0];
                cur = res;
            } else {
                cur[2]=[0, children[c],0];
                cur = cur[2];
            }
        }
        return res;
    } catch (e) {
        throw ("caml_js_node_children: " + e.message);
    }
}
function caml_js_mutex_create (unit){
return [0];
}
function caml_js_mutex_lock (m){
m[0]=1;
return 0;
}
function caml_js_mutex_unlock (m){
m[0]=0;
return 0;
}
function caml_js_mutex_try_lock (m){
if (m[0] == 0) {
m[0] = 1;
return 1;
} else
return 0;
}
function caml_make_vect (len, init){
var b = new Array();
 b[0]= 0;
 for (i = 1; i <= len; i++) b[i]=init;
return b;
}
function caml_make_array (a) { return a; }
function caml_hash_univ_param (count, limit, obj) {
var hash_accu = 0;
if (obj instanceof MlString) {
var s = obj.contents;
for (var p = 0;p < s.length - 1; p++) hash_accu = (hash_accu*19+s.charCodeAt(p)) & 0x3FFFFFFF;
//document.write("hash:", hash_accu);
return (hash_accu& 0x3FFFFFFF);
} else {
document.write("hash(", obj, "):", typeof obj);
}
}
function caml_array_get_addr (array, index) {
return array[index+1];
}
function caml_array_set_addr (array, index, newval) {
 array[index+1]=newval;
return 0;
}
function caml_array_set (array, index, newval) {
 array[index+1]=newval;
return 0;
}
function caml_array_unsafe_set (array, index, newval) {
 array[index+1]=newval;
return 0;
}
function jsoo_get (f, o){
//document.write("{", o, "|", f, "}");
res = o[f.toString()];
//document.write("==>", res, ".");
return res;
}
function jsoo_set(f, v, o) {
o[f.toString()] = v;
return 0;
}
function jsoo_call (d, args, o){
//document.write("call{", d, "|", args.slice(1), "|", o, "}");
res = o.apply (d, args.slice(1));
//document.write("==>", res, ".");
return res;
return o.apply (d, args.slice(1));
}
function caml_call_1 (f, v1) {
switch (f[1].length) {
case 1:
  return f[1](v1);
case 2:
  return [247, function(v2){return f[1](v1,v2);}, 1]
case 3:
  return [247, function(v2,v3){return f[1](v1,v2,v3);}, 2]
default:
  document.write(f[1].length);
  document.write("(1)");
}
}
function caml_call_2 (f, v1, v2) {
switch (f[1].length) {
case 1:
//document.write(f);
res = f[1](v1);
//document.write ("<", res, ">");
    return caml_call_1(res, v2);
case 2:
  return f[1](v1, v2);
case 3:
  return [247, function(v3){return f[1](v1,v2,v3);}, 1]
case 4:
  return [247, function(v3,v4){return f[1](v1,v2,v3,v4);}, 2]
default:
  document.write(f[1].length);
  document.write("(2)");
}
}
function caml_call_3 (f, v1, v2, v3) {
switch (f[1].length) {
case 3:
  return f[1](v1, v2,v3);
default:
  document.write(f);
  document.write("(3)");
}
}
function caml_call_4 (f, v1, v2, v3, v4) {
switch (f[1].length) {
case 4:
  return f[1](v1, v2,v3, v4);
default:
  document.write(f);
  document.write("(4)");
}
}
function caml_call_5 (f, v1, v2, v3, v4, v5) {
switch (f[1].length) {
case 5:
  return f[1](v1, v2,v3, v4,v5);
default:
  document.write(f);
  document.write("(5)");
}
}
function caml_call_6 (f, v1, v2, v3, v4, v5, v6) {
switch (f[1].length) {
case 6:
  return f[1](v1, v2, v3, v4, v5, v6);
case 7:
    return [247, function(v7){return f[1](v1,v2,v3,v4,v5,v6,v7);}, 1]
default:
  document.write(f);
  document.write("(6)");
}
}
function caml_call_7 (f, v1, v2, v3, v4, v5, v6, v7) {
switch (f[1].length) {
case 7:
  return f[1](v1, v2,v3, v4,v5,v6,v7);
default:
  document.write(f);
  document.write("(7)");
}
}
function caml_call_8 (f, v1, v2, v3, v4, v5,v6,v7,v8) {
switch (f[1].length) {
case 8:
  return f[1](v1, v2,v3, v4,v5,v6,v7,v8);
default:
  document.write(f);
  document.write("(8)");
}
}
function caml_call_9 (f, v1, v2, v3, v4, v5,v6,v7,v8,v9) {
switch (f[1].length) {
case 9:
  return f[1](v1, v2,v3, v4,v5,v6,v7,v8,v9);
default:
  document.write(f);
  document.write("(9)");
}
}
function thread_kill (unit) {
return 0;
}
function thread_delay (unit) {
return 0;
}
function thread_self (unit) {
return 0;
}
function thread_uncaught_exception(e){
document.write (e);
}
function thread_new (clos, arg) { }
function caml_js_http_get_with_status (url) {
//document.write (url);
    var xmlhttp = false;
    var vm = this;
    /* get request object */
    xmlhttp = new XMLHttpRequest();
    /* do request */
//        xmlhttp.onreadystatechange = function () {
//            vm.thread_notify_all (xmlhttp);
//        }
        xmlhttp.open("GET", url, false);
        xmlhttp.send(null);
        var b = [0, xmlhttp.status, new MlString (xmlhttp.responseText)];
        return b;
//        vm.thread_wait (xmlhttp, cont);
}
function caml_js_alert(msg) {
window.alert(msg.toString());
return 0;
}
