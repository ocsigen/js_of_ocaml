
// Weak API, but without the weak semantics

//Provides: caml_weak_create
function caml_weak_create (n) {
  var x = [0];
  x.length = n + 2;
  return x;
}
//Provides: caml_weak_set
function caml_weak_set(x, i, v) { x[i] = v; return 0; }
//Provides: caml_weak_get
function caml_weak_get(x, i) { return (x[i]===undefined)?0:x[i]; }
//Provides: caml_weak_copy
//Requires: caml_weak_get
function caml_weak_copy(x, i) {
  var y = caml_weak_get(x, i);
  if (y == 0) return y;
  var z = y[1];
  if (z instanceof Array && z[1] == (z[1]|0)) return [0, z.slice()];
  return y;
}
//Provides: caml_weak_check
function caml_weak_check(x, i) { return x[i]!==undefined && x[i] !===0; }
