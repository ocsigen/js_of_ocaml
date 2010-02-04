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

MlString.prototype.charAt = function (i) {
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

MlString.prototype.setCharAt = function (i, c) {
    var s = String.fromCharCode (c & 0xff);
    this.replace(i, s, 0, 1);
}

MlString.prototype.replace = function (i, s, j, l) {
    this.contents =
    String.concat (String.substring (this.contents, 0, i),
                   String.substring (s, j, j + l),
                   String.substring (this.contents, i + l, this.length));
}

MlString.prototype.fill = function (i, len, c) {
    var s = new Array(len + 1).join(String.fromCharCode (c & 0xff));
    this.replace (i, s, 0, len);
}

MlString.prototype.toString = function () {
    return decodeURIComponent (escape(this.contents));
}
