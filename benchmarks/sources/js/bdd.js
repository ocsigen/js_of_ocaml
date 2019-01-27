// (***********************************************************************)
// (*                                                                     *)
// (*                           Objective Caml                            *)
// (*                                                                     *)
// (*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
// (*                                                                     *)
// (*  Copyright 1996 Institut National de Recherche en Informatique et   *)
// (*  en Automatique.  All rights reserved.  This file is distributed    *)
// (*  under the terms of the Q Public License version 1.0.               *)
// (*                                                                     *)
// (***********************************************************************)
// Transaltion to js by VB
// (* Translated to Caml by Xavier Leroy *)
// (* Original code written in SML by ... *)

//var sys = require('sys');

function arr(size,v) {
  var t = [];
  for (var i=0; i < size; i++) t[i] = v;
  return t;
}

function arr2(size) {
  var t = [];
  for (var i=0; i < size; i++) t[i] = [];
  return t;
}

function eval2(bdd, vars) {
  switch (bdd.id) {
  case 0:
    return false;
  case 1:
    return true;
  default:
    return vars[bdd.v]?eval2(bdd.h,vars):eval2(bdd.l,vars);
  }
}

function getId(bdd) { return bdd.id; };

var initSize_1 = 8*1024 - 1;
var nodeC      = 1;
var sz_1       = initSize_1;
var htab       = arr2(sz_1+1);
var n_items    = 0;

function hashVal(x,y,v) { return ((x << 1) + y + (v << 2)); };

function resize(newSize) {
  var newSz_1 = newSize-1;
  var newArr  = arr2(newSize);
  for (var i = 0; i <= sz_1; i++) {
    var b = htab[i];
    for (var j = 0; j < b.length; j++) {
      var n = b[j];
      var ind = hashVal(getId(n.l), getId(n.h), n.v) & newSz_1;
      newArr[ind].push(n);
    }
  }
  htab = newArr;
  sz_1 = newSz_1;
}

function insert(idl,idh,v,ind,bucket,newNode) {
  if (n_items <= sz_1) {
    htab[ind].push(newNode);
    n_items ++;
  }
  else {
    resize(sz_1 + sz_1 + 2);
    ind = hashVal(idl,idh,v) & sz_1;
    htab[ind].push(newNode);
  };
};


function mkNode(low,v,high) {
  var idl = getId(low);
  var idh = getId(high);
  if (idl == idh) return low; else {
    var ind      = hashVal(idl,idh,v) & sz_1;
    var bucket   = htab[ind];
    for (i = 0; i < bucket.length; i++) {
      var n = bucket[i];
      if ((v == n.v) && (idl == getId(n.l)) && (idh == getId(n.h)))
        return n;
    }
    nodeC ++;
    var nn = {l:low, v:v, id:nodeC, h:high};
    insert(getId(low),getId(high),v,ind,bucket,nn);
    return nn;
  };
};


function cmpVar(x,y) {
  if (x<y) { return -1; }
  else if (x>y) { return 1; }
  else return 0;
};

var zero = {id:0}
var one = {id:1}

function mkVar(xx) { return mkNode(zero,xx,one); };

var cacheSize = 1999;

var andslot1  = arr(cacheSize,0);
var andslot2  = arr(cacheSize,0);
var andslot3  = arr(cacheSize,zero);
var xorslot1  = arr(cacheSize,0);
var xorslot2  = arr(cacheSize,0);
var xorslot3  = arr(cacheSize,zero);
var notslot1  = arr(cacheSize,0);
var notslot2  = arr(cacheSize,one);

function hash(x,y) { return (((x << 1)+y) % cacheSize); };

function not(n) {
  var id = n.id;
  switch (id) {
  case 0:
    return one;
  case 1:
    return zero;
  default:
    var h = id % cacheSize;
    if (id == notslot1[h]) {
      return notslot2[h];
    } else {
      var f = mkNode(not(n.l),n.v,not(n.h));
      notslot1[h] = id;
      notslot2[h] = f;
      return f;
    };
  };
};

function and2(n1,n2) {
  var i1 = n1.id;
  switch (i1) {
  case 0:
    return zero;
  case 1:
    return n2;
  default:
    var i2 = n2.id;
    switch (i2) {
    case 0:
      return zero;
    case 1:
      return n1;
    default:
      var h = hash(i1,i2);
      if ((i1 == andslot1[h]) && (i2 == andslot2[h])) {
        return andslot3[h];
      } else {
        var f;
        switch (cmpVar(n1.v,n2.v)) {
        case 0:
          f = mkNode(and2(n1.l,n2.l),n1.v,and2(n1.h,n2.h));
          break;
        case -1:
          f = mkNode(and2(n1.l,n2),n1.v,and2(n1.h,n2));
          break;
        default:
          f = mkNode(and2(n1,n2.l),n2.v,and2(n1,n2.h));
          break;
        };
        andslot1[h] = i1;
        andslot2[h] = i2;
        andslot3[h] = f;
        return f;
      }
    }
  }
}

function xor(n1,n2) {
  var i1 = n1.id;
  switch (i1) {
  case 0:
    return n2;
  case 1:
    return not(n2);
  default:
    var i2 = n2.id;
    switch (i2) {
    case 0:
      return n1;
    case 1:
      return not(n1);
    default:
      var h = hash(i1,i2);
      if ((i1 == andslot1[h]) && (i2 == andslot2[h])) {
        return andslot3[h];
      } else {
        var f;
        switch (cmpVar(n1.v,n2.v)) {
        case 0:
          f = mkNode(xor(n1.l,n2.l),n1.v,xor(n1.h,n2.h));
          break;
        case -1:
          f = mkNode(xor(n1.l,n2),n1.v,xor(n1.h,n2));
          break;
        default:
          f = mkNode(xor(n1,n2.l),n2.v,xor(n1,n2.h));
          break;
        };
        andslot1[h] = i1;
        andslot2[h] = i2;
        andslot3[h] = f;
        return f;
      }
    }
  }
}

function hwb(n) {
  function h(i,j) {
    if (i==j) {
      return mkVar(i);
    } else {
      return xor(and2(not(mkVar(j)),h(i,j-1)),
                 and2(mkVar(j),g(i,j-1)));
    };
  };
  function g(i,j) {
    if (i==j) {
      return mkVar(i);
    } else {
      return xor(and2(not(mkVar(i)),h(i+1,j)),
                 and2(mkVar(i),g(i+1,j)));
    };
  };
  return h(0,n-1);
};

/* Testing */
var seed = 0;

function random() {
  seed = (seed * 25173 + 17431)|0;
  return (seed & 1) > 0;
};

function random_vars(n) {
  var vars = [];
  for (var i = 0; i < n; i++) vars[i] = random();
  return vars;
};

function test_hwb(bdd,vars) {
  /* We should have
     eval bdd vars = vars.(n-1) if n > 0
     eval bdd vars = false if n = 0
     where n is the number of "true" elements in vars. */
  var ntrue = 0;
  for (var i = 0; i < vars.length; i++) {
    if (vars[i]) ntrue++;
  };
  return (eval2(bdd,vars) == ((ntrue > 0) ? vars[ntrue-1] : false))
};

var n = 22;
var ntests = 100;

var bdd = hwb(n);
var succeeded = true;

for (var i = 1; i <= ntests; i++) {
  succeeded = succeeded && test_hwb(bdd,random_vars(n));
};

//print(nodeC);
//if (succeeded) print("ok"); else print("failed");
