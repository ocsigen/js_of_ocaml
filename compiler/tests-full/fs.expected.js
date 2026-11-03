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
(function(g){
   "use strict";
   function a(a, b){
    if(g.jsoo_create_file)
     g.jsoo_create_file(a, b);
    else{
     if(! g.jsoo_fs_tmp) g.jsoo_fs_tmp = [];
     g.jsoo_fs_tmp.push({name: a, content: b});
    }
    return 0;
   }
   a("/static/file1", "This is file 1");
   a("/static/dir/file2", "This is file 2");
   return;
  }
  (globalThis));
