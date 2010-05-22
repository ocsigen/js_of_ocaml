function exec_caml (url) {
  var s = document.createElement("script");
  // Changes foo.uue into foo.js
  s.src = url.slice(0,url.lastIndexOf(".")-url.length) + ".js";
  s.type ='text/javascript';
  var head = document.getElementsByTagName("head")[0];
  head.appendChild(s);

  var vm =
    { thread_new : function (f) { f(0); },
      run : function () {},
      failwith : function (s) { throw s; } }
      // FIX: better error handling function
  return vm;
}
