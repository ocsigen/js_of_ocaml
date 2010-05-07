// FIX: should use unusual method names so that any error like using a
// string rather than a mlstring is immediately visible

// catch URIError exceptions in conversion from/to UTF-8

function MlString(param) {
    switch (typeof param) {
    case "number":
        var s = new Array(param + 1).join(String.fromCharCode (0));
        break;
    case "string":
        var s = unescape (encodeURIComponent (param));
    }
    this.length = s.length;
    this.contents = s;
}

MlString.prototype.safeGet = function (i) {
    if ((i < 0) || (i >= this.length)) caml_array_bound_error ();
    return this.contents.charCodeAt(i);
}

MlString.prototype.get = function (i) {
    return this.contents.charCodeAt(i);
}

MlString.prototype.notEqual = function (s) {
    return (this.contents != s.contents);
}

MlString.prototype.compare = function (s) {
    if (this.contents < s.contents) return -1;
    else if (this.contents > s.contents) return 1;
    else return 0;
}

MlString.prototype.set = function (i, c) {
    var s = String.fromCharCode (c & 0xff);
    this.replace(i, s, 0, 1);
}

MlString.prototype.safeSet = function (i, c) {
    if ((i < 0) || (i >= this.length)) caml_array_bound_error ();
    var s = String.fromCharCode (c & 0xff);
    this.replace(i, s, 0, 1);
}

MlString.prototype.replace = function (i, s, j, l) {
    this.contents =
    String.concat (this.contents.substring (0, i),
                   s.substring (j, j + l),
                   this.contents.substring (i + l, this.length));
}

MlString.prototype.fill = function (i, len, c) {
    var s = new Array(len + 1).join(String.fromCharCode (c & 0xff));
    this.replace (i, s, 0, len);
}

MlString.prototype.toString = function () {
    return decodeURIComponent (escape(this.contents));
}
