var document = {write: function(x){WScript.Echo(x)}};

//adapted from https://regmedia.co.uk/2007/10/31/jscriptdeviationsfromes3.pdf §2.11
function foo(t) {
  var a = 1;
  try {
    if (t) throw "hello";
  }
  catch(a) {
    document.write(a);
  }
  document.write(a); //choose wisely
}

document.write("throw:");
foo(true);
document.write("don't:");
foo(false);
