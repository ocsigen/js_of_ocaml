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
(function(d){
   "use strict";
   function a(a, b, c){
    if(d.jsoo_create_file)
     d.jsoo_create_file(a, b, c);
    else{
     if(! d.jsoo_fs_tmp) d.jsoo_fs_tmp = [];
     d.jsoo_fs_tmp.push({name: a, content: b, base64: c});
    }
    return 0;
   }
   a("/static/file1", "This is file 1", 0);
   a("/static/dir/file2", "This is file 2", 0);
   a
    ("/static/file3",
     "gIGCg4SFhoeIiYqLjI2Oj5CRkpOUlZaXmJmam5ydnp+goaKjpKWmp6ipqqusra6vsLGys7S1tre4ubq7vL2+v8DBwsPExcbHyMnKy8zNzs/Q0dLT1NXW19jZ2tvc3d7f4OHi4+Tl5ufo6err7O3u7/Dx8vP09fb3+Pn6+/z9/v8",
     1);
   return;
  }
  (globalThis));
