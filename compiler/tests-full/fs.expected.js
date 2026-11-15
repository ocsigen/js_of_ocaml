(function(a){
   typeof globalThis !== "object"
   &&
    (this
      ? b()
      : (a.defineProperty
         (a.prototype, "_T_", {configurable: true, get: b}),
        _T_));
   function b(){
    var b = this || self;
    b.globalThis = b;
    delete a.prototype._T_;
   }
  }
  (Object));
(function(c){
   "use strict";
   function e(a){
    function h(a){
     return a >= 97
             ? a - 71
             : a >= 65 ? a - 65 : a >= 48 ? a + 4 : a === 43 ? 62 : 63;
    }
    var b = a.length;
    while(b > 0 && a.charCodeAt(b - 1) === 61) b--;
    var f = new Array(b * 3 >> 2), d = 0, c = 0, g = 0;
    for(var e = 0; e < b; e++){
     d = d << 6 | h(a.charCodeAt(e));
     c += 6;
     if(c >= 8){c -= 8; f[g++] = String.fromCharCode(d >> c & 0xff);}
    }
    return f.join("");
   }
   function d(a){return typeof c.atob === "function" ? c.atob(a) : e(a);}
   function a(a, b){
    if(c.jsoo_create_file)
     c.jsoo_create_file(a, b);
    else{
     if(! c.jsoo_fs_tmp) c.jsoo_fs_tmp = [];
     c.jsoo_fs_tmp.push({name: a, content: b});
    }
    return 0;
   }
   var
    b =
      d
       ("gIGCg4SFhoeIiYqLjI2Oj5CRkpOUlZaXmJmam5ydnp+goaKjpKWmp6ipqqusra6vsLGys7S1tre4ubq7vL2+v8DBwsPExcbHyMnKy8zNzs/Q0dLT1NXW19jZ2tvc3d7f4OHi4+Tl5ufo6err7O3u7/Dx8vP09fb3+Pn6+/z9/v8");
   a("/static/file1", "This is file 1");
   a("/static/dir/file2", "This is file 2");
   a("/static/file3", b);
   return;
  }
  (globalThis));
