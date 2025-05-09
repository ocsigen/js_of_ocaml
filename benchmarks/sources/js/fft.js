var pi = 3.14159265358979323846;

var tpi = 2.0 * pi;

function fft (px, py, np) {
  var i = 2;
  var m = 1;

  while (i < np) {
    i = i + i;
    m ++;
  }

  var n = i;

  if (n != np) {
    for (i = np+1; i <= n; i++) {
      px[i] = 0;
      py[i] = 0;
    }
  }

  var n2 = n + n;
  for (var k = 1; k < m; k++) {
    n2 = n2 / 2;
    var n4 = n2 / 4;
    var e  = tpi / n2;

    for (var j = 1; j <= n4; j++) {
      var a = e * (j - 1);
      var a3 = 3.0 * a;
      var cc1 = Math.cos(a);
      var ss1 = Math.sin(a);
      var cc3 = Math.cos(a3);
      var ss3 = Math.sin(a3);
      var is = j;
      var id = 2 * n2;

      while (is < n) {
        var i0r = is;
        while (i0r < n) {
          var i0 = i0r;
          var i1 = i0 + n4;
          var i2 = i1 + n4;
          var i3 = i2 + n4;
          var r1 = px[i0] - px[i2];
          px[i0] += px[i2];
          var r2 = px[i1] - px[i3];
          px[i1] += px[i3];
          var s1 = py[i0] - py[i2];
          py[i0] += py[i2];
          var s2 = py[i1] - py[i3];
          py[i1] += py[i3];
          var s3 = r1 - s2;
          var r1 = r1 + s2;
          var s2 = r2 - s1;
          var r2 = r2 + s1;
          px[i2] = r1*cc1 - s2*ss1;
          py[i2] = -s2*cc1 - r1*ss1;
          px[i3] = s3*cc3 + r2*ss3;
          py[i3] = r2*cc3 - s3*ss3;
          i0r = i0 + id
        }
        is = 2 * id - n2 + j;
        id = 4 * id
      }
    }
  };

  /************************************/
  /*  Last stage, length=2 butterfly  */
  /************************************/

  var is = 1;
  var id = 4;

  while (is < n) {
    var i0r = is;
    while (i0r <= n) {
      var i0 = i0r;
      var i1 = i0 + 1;
      var r1 = px[i0];
      px[i0] = r1 + px[i1];
      px[i1] = r1 - px[i1];
      var r1 = py[i0];
      py[i0] = r1 + py[i1];
      py[i1] = r1 - py[i1];
      i0r = i0 + id
    };
    is = 2 * id - 1;
    id = 4 * id
  };

  /*************************/
  /*  Bit reverse counter  */
  /*************************/

  var j = 1;

  for (var i = 1; i < n; i++) {
    if (i < j) {
      var xt = px[j];
      px[j] = px[i];
      px[i] = xt;
      var xt = py[j];
      py[j] = py[i];
      py[i] = xt
    }
    var k = n / 2;
    while (k < j) {
      j = j - k;
      k = k / 2
    };
    j = j + k
  };

  return n;
}


function test (np) {
  /*  print_int np; print_string "... "; flush stdout;*/
  var enp = np;
  var npm = np / 2 - 1;
  var pxr = [];
  var pxi = [];
  for (var i = 0; i < np +2; i++) {
    pxr[i] = 0.0;
    pxi[i] = 0.0;
  }

  var t = pi / enp;
  pxr[1] = (enp - 1.0) * 0.5;
  pxi[1] = 0.0;

  var n2 = np / 2;
  pxr[n2+1] = -0.5;
  pxi[n2+1] =  0.0;

  for (var i = 1; i <= npm; i++) {
    var j = np - i;
    pxr[i+1] = -0.5;
    pxr[j+1] = -0.5;
    var z = t * i;
    var y = -0.5*(Math.cos(z)/Math.sin(z));
    pxi[i+1] = y;
    pxi[j+1] = -y
  };
  /**
     print_newline();
     for i=0 to 15 do Printf.printf "%d  %f  %f\n" i pxr[i+1) pxi[i+1] done;
  **/
  fft (pxr, pxi, np);
  /**
     for i=0 to 15 do Printf.printf "%d  %f  %f\n" i pxr[i+1] pxi[i+1] done;
     .**/
  var zr = 0.0;
  var zi = 0.0;
  var kr = 0;
  var ki = 0;
  for (var i = 0; i < np; i++) {
    var a = Math.abs (pxr[i+1] - i);
    if (zr < a) {
      zr = a;
      kr = i;
    }
    var a = Math.abs(pxi[i+1]);
    if (zi < a) {
      zi = a;
      ki = i;
    }
  }
  //  print (zr); print (zi);
  if (Math.abs( zr) <= 1e-8 && Math.abs( zi) <= 1e-8) {} else throw "ERROR";
}

var np = 16; for (var i = 1; i<= 19; i++) { test (np); np = np*2; }
