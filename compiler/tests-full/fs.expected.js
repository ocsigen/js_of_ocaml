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
   function a(a, b){
    if(c.jsoo_create_file)
     c.jsoo_create_file(a, b);
    else{
     if(! c.jsoo_fs_tmp) c.jsoo_fs_tmp = [];
     c.jsoo_fs_tmp.push({name: a, content: b});
    }
    return 0;
   }
   a("/static/file1", "This is file 1");
   a("/static/dir/file2", "This is file 2");
   return;
  }
  (globalThis));
