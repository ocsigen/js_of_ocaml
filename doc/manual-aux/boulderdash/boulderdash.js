// This program was compiled from OCaml by js_of_ocaml 2.00dev+git-f7cce66
(function(b){"use strict";var
bX=125,b1=123,dK=254,af=255,dJ=108,a$='"',K=16777215,b0="=",dA='Content-Disposition: form-data; name="',dB=250,dI="sprites/guy.png",du="jsError",bh="POST",bW=2147483,dm=-550809787,aK=115,b6=102,dH="&",bZ=120,bV="--",bY=117,bg=126925477,d="",a8=781515420,dt="sprites/boulder.png",be=100,G="0",b5=103,dG="fd ",dl=936573133,dz=1e3,X="src/core/lwt.ml",ds="x",ap=".",bd=65535,aH="+",aG="g",bU="f",ah=105,dr="%d",dq=443,dF=-88,aJ=110,dy=785140586,dk="sprites/end.png",a7="?",a_="'",a9="int_of_string",b4=-32,dp=982028505,b3=111,F=" ",aI="e",dE="1",dj=0.001,dx="lastIndex",bf=891486873,di=":",ag="-",ao=-48,dw="nan",bT=116,b2="eos",bc="\r\n",dv="%.12g",b8=0.05,b9=" : file already exists",dn=-83,W="/",bb=114,ba="#",dD=101,b7="index out of bounds",B="number",dC="select";function
dY(a,b){throw[0,a,b]}function
cc(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
c=b.console;c&&c.error&&c.error(a)}var
g=[0];function
aM(a,b){if(!a)return d;if(a&1)return aM(a-1,b)+b;var
c=aM(a>>1,b);return c+c}function
t(a){if(a!=null){this.bytes=this.fullBytes=a;this.last=this.len=a.length}}function
dZ(){dY(g[4],new
t(b7))}t.prototype={string:null,bytes:null,fullBytes:null,array:null,len:null,last:0,toJsString:function(){var
a=this.getFullBytes();try{return this.string=decodeURIComponent(escape(a))}catch(f){cc('MlString.toJsString: wrong encoding for "%s" ',a);return a}},toBytes:function(){if(this.string!=null)try{var
a=unescape(encodeURIComponent(this.string))}catch(f){cc('MlString.toBytes: wrong encoding for "%s" ',this.string);var
a=this.string}else{var
a=d,c=this.array,e=c.length;for(var
b=0;b<e;b++)a+=String.fromCharCode(c[b])}this.bytes=this.fullBytes=a;this.last=this.len=a.length;return a},getBytes:function(){var
a=this.bytes;if(a==null)a=this.toBytes();return a},getFullBytes:function(){var
a=this.fullBytes;if(a!==null)return a;a=this.bytes;if(a==null)a=this.toBytes();if(this.last<this.len){this.bytes=a+=aM(this.len-this.last,"\0");this.last=this.len}this.fullBytes=a;return a},toArray:function(){var
c=this.bytes;if(c==null)c=this.toBytes();var
b=[],d=this.last;for(var
a=0;a<d;a++)b[a]=c.charCodeAt(a);for(d=this.len;a<d;a++)b[a]=0;this.string=this.bytes=this.fullBytes=null;this.last=this.len;this.array=b;return b},getArray:function(){var
a=this.array;if(!a)a=this.toArray();return a},getLen:function(){var
a=this.len;if(a!==null)return a;this.toBytes();return this.len},toString:function(){var
a=this.string;return a?a:this.toJsString()},valueOf:function(){var
a=this.string;return a?a:this.toJsString()},blitToArray:function(a,b,c,d){var
g=this.array;if(g)if(c<=a)for(var
e=0;e<d;e++)b[c+e]=g[a+e];else
for(var
e=d-1;e>=0;e--)b[c+e]=g[a+e];else{var
f=this.bytes;if(f==null)f=this.toBytes();var
h=this.last-a;if(d<=h)for(var
e=0;e<d;e++)b[c+e]=f.charCodeAt(a+e);else{for(var
e=0;e<h;e++)b[c+e]=f.charCodeAt(a+e);for(;e<d;e++)b[c+e]=0}}},get:function(a){var
c=this.array;if(c)return c[a];var
b=this.bytes;if(b==null)b=this.toBytes();return a<this.last?b.charCodeAt(a):0},safeGet:function(a){if(this.len==null)this.toBytes();if(a<0||a>=this.len)dZ();return this.get(a)},set:function(a,b){var
c=this.array;if(!c){if(this.last==a){this.bytes+=String.fromCharCode(b&af);this.last++;return 0}c=this.toArray()}else
if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;c[a]=b&af;return 0},safeSet:function(a,b){if(this.len==null)this.toBytes();if(a<0||a>=this.len)dZ();this.set(a,b)},fill:function(a,b,c){if(a>=this.last&&this.last&&c==0)return;var
d=this.array;if(!d)d=this.toArray();else
if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;var
f=a+b;for(var
e=a;e<f;e++)d[e]=c},compare:function(a){if(this.string!=null&&a.string!=null){if(this.string<a.string)return-1;if(this.string>a.string)return 1;return 0}var
b=this.getFullBytes(),c=a.getFullBytes();if(b<c)return-1;if(b>c)return 1;return 0},equal:function(a){if(this.string!=null&&a.string!=null)return this.string==a.string;return this.getFullBytes()==a.getFullBytes()},lessThan:function(a){if(this.string!=null&&a.string!=null)return this.string<a.string;return this.getFullBytes()<a.getFullBytes()},lessEqual:function(a){if(this.string!=null&&a.string!=null)return this.string<=a.string;return this.getFullBytes()<=a.getFullBytes()}};function
H(a){this.string=a}H.prototype=new
t();function
cb(a,b){dY(a,new
H(b))}function
aq(a){cb(g[4],a)}function
dM(){aq(b7)}function
hA(a,b){if(b<0||b>=a.length-1)dM();return a[b+1]}function
hB(a,b,c){if(b<0||b>=a.length-1)dM();a[b+1]=c;return 0}function
dN(a,b,c,d,e){if(e===0)return;if(d===c.last&&c.bytes!=null){var
f=a.bytes;if(f==null)f=a.toBytes();if(b>0||a.last>e)f=f.slice(b,b+e);c.bytes+=f;c.last+=f.length;return}var
g=c.array;if(!g)g=c.toArray();else
c.bytes=c.string=null;a.blitToArray(b,g,d,e)}function
Y(c,b){if(c.fun)return Y(c.fun,b);var
a=c.length,d=a-b.length;if(d==0)return c.apply(null,b);else
if(d<0)return Y(c.apply(null,b.slice(0,a)),b.slice(a));else
return function(a){return Y(c,b.concat([a]))}}function
hC(a){if(isFinite(a)){if(Math.abs(a)>=2.22507385850720138e-308)return 0;if(a!=0)return 1;return 2}return isNaN(a)?4:3}function
hD(){return 0}function
dL(a){this.bytes=d;this.len=a}dL.prototype=new
t();function
dP(a){if(a<0)aq("String.create");return new
dL(a)}function
hM(a,b){var
c=a[3]<<16,d=b[3]<<16;if(c>d)return 1;if(c<d)return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
hW(a,b){if(a<b)return-1;if(a==b)return 0;return 1}function
dO(a,b,c){var
e=[];for(;;){if(!(c&&a===b))if(a
instanceof
t)if(b
instanceof
t){if(a!==b){var
d=a.compare(b);if(d!=0)return d}}else
return 1;else
if(a
instanceof
Array&&a[0]===(a[0]|0)){var
f=a[0];if(f===dK)f=0;if(f===dB){a=a[1];continue}else
if(b
instanceof
Array&&b[0]===(b[0]|0)){var
g=b[0];if(g===dK)g=0;if(g===dB){b=b[1];continue}else
if(f!=g)return f<g?-1:1;else
switch(f){case
248:var
d=hW(a[2],b[2]);if(d!=0)return d;break;case
251:aq("equal: abstract value");case
af:var
d=hM(a,b);if(d!=0)return d;break;default:if(a.length!=b.length)return a.length<b.length?-1:1;if(a.length>1)e.push(a,b,1)}}else
return 1}else
if(b
instanceof
t||b
instanceof
Array&&b[0]===(b[0]|0))return-1;else{if(a<b)return-1;if(a>b)return 1;if(a!=b){if(!c)return NaN;if(a==a)return 1;if(b==b)return-1}}if(e.length==0)return 0;var
h=e.pop();b=e.pop();a=e.pop();if(h+1<a.length)e.push(a,b,h+1);a=a[h];b=b[h]}}function
hF(a,b){return+(dO(a,b,false)==0)}function
hG(a,b,c,d){a.fill(b,c,d)}function
ca(a){a=a.toString();var
e=a.length;if(e>31)aq("format_int: format too long");var
b={justify:aH,signstyle:ag,filler:F,alternate:false,base:0,signedconv:false,width:0,uppercase:false,sign:1,prec:-1,conv:bU};for(var
d=0;d<e;d++){var
c=a.charAt(d);switch(c){case
ag:b.justify=ag;break;case
aH:case
F:b.signstyle=c;break;case
G:b.filler=G;break;case
ba:b.alternate=true;break;case
dE:case"2":case"3":case"4":case"5":case"6":case"7":case"8":case"9":b.width=0;while(c=a.charCodeAt(d)-48,c>=0&&c<=9){b.width=b.width*10+c;d++}d--;break;case
ap:b.prec=0;d++;while(c=a.charCodeAt(d)-48,c>=0&&c<=9){b.prec=b.prec*10+c;d++}d--;case"d":case"i":b.signedconv=true;case"u":b.base=10;break;case
ds:b.base=16;break;case"X":b.base=16;b.uppercase=true;break;case"o":b.base=8;break;case
aI:case
bU:case
aG:b.signedconv=true;b.conv=c;break;case"E":case"F":case"G":b.signedconv=true;b.uppercase=true;b.conv=c.toLowerCase();break}}return b}function
b_(a,b){if(a.uppercase)b=b.toUpperCase();var
f=b.length;if(a.signedconv&&(a.sign<0||a.signstyle!=ag))f++;if(a.alternate){if(a.base==8)f+=1;if(a.base==16)f+=2}var
c=d;if(a.justify==aH&&a.filler==F)for(var
e=f;e<a.width;e++)c+=F;if(a.signedconv)if(a.sign<0)c+=ag;else
if(a.signstyle!=ag)c+=a.signstyle;if(a.alternate&&a.base==8)c+=G;if(a.alternate&&a.base==16)c+="0x";if(a.justify==aH&&a.filler==G)for(var
e=f;e<a.width;e++)c+=G;c+=b;if(a.justify==ag)for(var
e=f;e<a.width;e++)c+=F;return new
H(c)}function
hH(a,b){var
c,f=ca(a),e=f.prec<0?6:f.prec;if(b<0){f.sign=-1;b=-b}if(isNaN(b)){c=dw;f.filler=F}else
if(!isFinite(b)){c="inf";f.filler=F}else
switch(f.conv){case
aI:var
c=b.toExponential(e),d=c.length;if(c.charAt(d-3)==aI)c=c.slice(0,d-1)+G+c.slice(d-1);break;case
bU:c=b.toFixed(e);break;case
aG:e=e?e:1;c=b.toExponential(e-1);var
i=c.indexOf(aI),h=+c.slice(i+1);if(h<-4||b.toFixed(0).length>e){var
d=i-1;while(c.charAt(d)==G)d--;if(c.charAt(d)==ap)d--;c=c.slice(0,d+1)+c.slice(i);d=c.length;if(c.charAt(d-3)==aI)c=c.slice(0,d-1)+G+c.slice(d-1);break}else{var
g=e;if(h<0){g-=h+1;c=b.toFixed(g)}else
while(c=b.toFixed(g),c.length>e+1)g--;if(g){var
d=c.length-1;while(c.charAt(d)==G)d--;if(c.charAt(d)==ap)d--;c=c.slice(0,d+1)}}break}return b_(f,c)}function
hI(a,b){if(a.toString()==dr)return new
H(d+b);var
c=ca(a);if(b<0)if(c.signedconv){c.sign=-1;b=-b}else
b>>>=0;var
e=b.toString(c.base);if(c.prec>=0){c.filler=F;var
f=c.prec-e.length;if(f>0)e=aM(f,G)+e}return b_(c,e)}function
hK(){return 0}function
hP(a){return(a[3]|a[2]|a[1])==0}function
hS(a){return[af,a&K,a>>24&K,a>>31&bd]}function
hT(a,b){var
c=a[1]-b[1],d=a[2]-b[2]+(c>>24),e=a[3]-b[3]+(d>>24);return[af,c&K,d&K,e&bd]}function
dR(a,b){if(a[3]>b[3])return 1;if(a[3]<b[3])return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
dQ(a){a[3]=a[3]<<1|a[2]>>23;a[2]=(a[2]<<1|a[1]>>23)&K;a[1]=a[1]<<1&K}function
hQ(a){a[1]=(a[1]>>>1|a[2]<<23)&K;a[2]=(a[2]>>>1|a[3]<<23)&K;a[3]=a[3]>>>1}function
hV(a,b){var
e=0,d=a.slice(),c=b.slice(),f=[af,0,0,0];while(dR(d,c)>0){e++;dQ(c)}while(e>=0){e--;dQ(f);if(dR(d,c)>=0){f[1]++;d=hT(d,c)}hQ(c)}return[0,f,d]}function
hU(a){return a[1]|a[2]<<24}function
hO(a){return a[3]<<16<0}function
hR(a){var
b=-a[1],c=-a[2]+(b>>24),d=-a[3]+(c>>24);return[af,b&K,c&K,d&bd]}function
hN(a,b){var
c=ca(a);if(c.signedconv&&hO(b)){c.sign=-1;b=hR(b)}var
e=d,i=hS(c.base),h="0123456789abcdef";do{var
g=hV(b,i);b=g[1];e=h.charAt(hU(g[2]))+e}while(!hP(b));if(c.prec>=0){c.filler=F;var
f=c.prec-e.length;if(f>0)e=aM(f,G)+e}return b_(c,e)}function
id(a){var
b=0,c=10,d=a.get(0)==45?(b++,-1):1;if(a.get(b)==48)switch(a.get(b+1)){case
bZ:case
88:c=16;b+=2;break;case
b3:case
79:c=8;b+=2;break;case
98:case
66:c=2;b+=2;break}return[b,d,c]}function
dW(a){if(a>=48&&a<=57)return a-48;if(a>=65&&a<=90)return a-55;if(a>=97&&a<=122)return a-87;return-1}function
bi(a){cb(g[3],a)}function
hX(a){var
g=id(a),f=g[0],h=g[1],d=g[2],i=-1>>>0,e=a.get(f),c=dW(e);if(c<0||c>=d)bi(a9);var
b=c;for(;;){f++;e=a.get(f);if(e==95)continue;c=dW(e);if(c<0||c>=d)break;b=d*b+c;if(b>i)bi(a9)}if(f!=a.getLen())bi(a9);b=h*b;if(d==10&&(b|0)!=b)bi(a9);return b|0}function
hY(a){return+(a>31&&a<127)}function
hZ(a){return a.getFullBytes()}function
h0(){var
c=b.console?b.console:{},d=["log","debug","info","warn","error","assert","dir","dirxml","trace","group","groupCollapsed","groupEnd","time","timeEnd"];function
e(){}for(var
a=0;a<d.length;a++)if(!c[d[a]])c[d[a]]=e;return c}var
bj={amp:/&/g,lt:/</g,quot:/\"/g,all:/[&<\"]/};function
h1(a){if(!bj.all.test(a))return a;return a.replace(bj.amp,"&amp;").replace(bj.lt,"&lt;").replace(bj.quot,"&quot;")}function
h2(){var
a=b.navigator?b.navigator.userAgent:d;return a.indexOf("MSIE")!=-1&&a.indexOf("Opera")!=0}function
h3(a){return new
t(a)}function
h4(a){var
c=Array.prototype.slice;return function(){var
b=arguments.length>0?c.call(arguments):[undefined];return Y(a,b)}}function
h5(a,b){var
d=[0];for(var
c=1;c<=a;c++)d[c]=b;return d}function
I(a){cb(g[2],a)}function
dS(a){if(!a.opened)I("Cannot flush a closed channel");if(a.buffer==d)return 0;if(a.output)switch(a.output.length){case
2:a.output(a,a.buffer);break;default:a.output(a.buffer)}a.buffer=d}function
dX(a){a=a
instanceof
t?a.toString():a;I(a+": No such file or directory")}var
hE=W;function
bk(a){a=a
instanceof
t?a.toString():a;if(a.charCodeAt(0)!=47)a=hE+a;var
e=a.split(W),b=[];for(var
c=0;c<e.length;c++)switch(e[c]){case"..":if(b.length>1)b.pop();break;case
ap:case
d:if(b.length==0)b.push(d);break;default:b.push(e[c]);break}b.orig=a;return b}function
ai(){this.content={}}ai.prototype={exists:function(a){return this.content[a]?1:0},mk:function(a,b){this.content[a]=b},get:function(a){return this.content[a]},list:function(){var
a=[];for(var
b
in
this.content)a.push(b);return a},remove:function(a){delete
this.content[a]}};var
bm=new
ai();bm.mk(d,new
ai());function
b$(a){var
b=bm;for(var
c=0;c<a.length;c++){if(!(b.exists&&b.exists(a[c])))dX(a.orig);b=b.get(a[c])}return b}function
io(a){var
c=bk(a),b=b$(c);return b
instanceof
ai?1:0}function
aL(a){this.data=a}aL.prototype={content:function(){return this.data},truncate:function(){this.data.length=0}};function
hJ(a,b){var
e=bk(a),c=bm;for(var
f=0;f<e.length-1;f++){var
d=e[f];if(!c.exists(d))c.mk(d,new
ai());c=c.get(d);if(!(c
instanceof
ai))I(e.orig+b9)}var
d=e[e.length-1];if(c.exists(d))I(e.orig+b9);if(b
instanceof
ai)c.mk(d,b);else
if(b
instanceof
aL)c.mk(d,b);else
if(b
instanceof
t)c.mk(d,new
aL(b.getArray()));else
if(b
instanceof
Array)c.mk(d,new
aL(b));else
if(b.toString)c.mk(d,new
aL(new
t(b.toString()).getArray()));else
aq("caml_fs_register")}function
im(a){var
b=bm,d=bk(a),e;for(var
c=0;c<d.length;c++){if(b.auto)e=b.auto;if(!(b.exists&&b.exists(d[c])))return e?e(d.join(W)):0;b=b.get(d[c])}return 1}function
aN(a,b,c){if(g.fds===undefined)g.fds=new
Array();c=c?c:{};var
d={};d.array=b;d.offset=c.append?d.array.length:0;d.flags=c;g.fds[a]=d;g.fd_last_idx=a;return a}function
ix(a,b,c){var
d={};while(b){switch(b[1]){case
0:d.rdonly=1;break;case
1:d.wronly=1;break;case
2:d.append=1;break;case
3:d.create=1;break;case
4:d.truncate=1;break;case
5:d.excl=1;break;case
6:d.binary=1;break;case
7:d.text=1;break;case
8:d.nonblock=1;break}b=b[2]}var
f=a.toString(),i=bk(a);if(d.rdonly&&d.wronly)I(f+" : flags Open_rdonly and Open_wronly are not compatible");if(d.text&&d.binary)I(f+" : flags Open_text and Open_binary are not compatible");if(im(a)){if(io(a))I(f+" : is a directory");if(d.create&&d.excl)I(f+b9);var
h=g.fd_last_idx?g.fd_last_idx:0,e=b$(i);if(d.truncate)e.truncate();return aN(h+1,e.content(),d)}else
if(d.create){var
h=g.fd_last_idx?g.fd_last_idx:0;hJ(a,[]);var
e=b$(i);return aN(h+1,e.content(),d)}else
dX(a)}aN(0,[]);aN(1,[]);aN(2,[]);function
h6(a){var
b=g.fds[a];if(b.flags.wronly)I(dG+a+" is writeonly");return{data:b,fd:a,opened:true}}function
iu(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
c=b.console;c&&c.log&&c.log(a)}var
bl=new
Array();function
ih(a,b){var
e=new
t(b),d=e.getLen();for(var
c=0;c<d;c++)a.data.array[a.data.offset+c]=e.get(c);a.data.offset+=d;return 0}function
h7(a){var
b;switch(a){case
1:b=iu;break;case
2:b=cc;break;default:b=ih}var
e=g.fds[a];if(e.flags.rdonly)I(dG+a+" is readonly");var
c={data:e,fd:a,opened:true,buffer:d,output:b};bl[c.fd]=c;return c}function
h8(){var
a=0;for(var
b
in
bl)if(bl[b].opened)a=[0,bl[b],a];return a}function
dT(a,b,c,d){if(!a.opened)I("Cannot output to a closed channel");var
f;if(c==0&&b.getLen()==d)f=b;else{f=dP(d);dN(b,c,f,0,d)}var
e=f.toString(),g=e.lastIndexOf("\n");if(g<0)a.buffer+=e;else{a.buffer+=e.substr(0,g+1);dS(a);a.buffer+=e.substr(g+1)}}function
dV(a){return new
t(a)}function
h9(a,b){var
c=dV(String.fromCharCode(b));dT(a,c,0,1)}if(!Math.imul)Math.imul=function(a,b){return((a>>16)*b<<16)+(a&bd)*b|0};var
h_=Math.imul;function
ia(a,b){return+(dO(a,b,false)!=0)}function
ib(a){return+(a
instanceof
Array)}function
ic(a){return a
instanceof
Array?a[0]:dz}function
ie(a,b){g[a+1]=b}var
dU={};function
ig(a,b){dU[a.toString()]=b;return 0}function
ii(a,b){var
c=a.fullBytes,d=b.fullBytes;if(c!=null&&d!=null)return c==d?1:0;return a.getFullBytes()==b.getFullBytes()?1:0}function
ij(a,b){return 1-ii(a,b)}function
ik(){return 32}function
il(){aq("Function 'exit' not implemented")}var
hL=new
Date()*dj;function
ip(){return new
Date()*dj-hL}function
iq(a){var
b=1;while(a&&a.joo_tramp){a=a.joo_tramp.apply(null,a.joo_args);b++}return a}function
ir(a,b){return{joo_tramp:a,joo_args:b}}function
is(a,b){if(typeof
b==="function"){a.fun=b;return 0}if(b.fun){a.fun=b.fun;return 0}var
c=b.length;while(c--)a[c]=b[c];return 0}function
h$(a){return dU[a]}function
it(a){if(a
instanceof
Array)return a;if(b.RangeError&&a
instanceof
b.RangeError&&a.message&&a.message.match(/maximum call stack/i))return[0,g[9]];if(b.InternalError&&a
instanceof
b.InternalError&&a.message&&a.message.match(/too much recursion/i))return[0,g[9]];if(a
instanceof
b.Error)return[0,h$(du),a];return[0,g[3],new
H(String(a))]}var
h=hA,k=hB,ad=dN,Q=dP,dg=hF,bP=hH,a3=hI,dd=hX,bQ=hY,p=hZ,dh=h1,V=h3,a6=h4,R=h5,c$=dS,c_=h7,db=h9,dc=h_,c=dV,de=ic,a=ie,da=ig,A=ij,aF=ip,a5=iq,T=ir,df=is,z=it;function
j(a,b){return a.length==1?a(b):Y(a,[b])}function
n(a,b,c){return a.length==2?a(b,c):Y(a,[b,c])}function
s(a,b,c,d){return a.length==3?a(b,c,d):Y(a,[b,c,d])}function
a4(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):Y(a,[b,c,d,e,f])}var
ar=[0,c("Failure")],bn=[0,c("Invalid_argument")],bp=[0,c("Not_found")],cz=[0,c("Match_failure")],cy=[0,c("Stack_overflow")],u=[0,c("Assert_failure")],cA=[0,c("Undefined_recursive_module")],bw=c('File "%s", line %d, characters %d-%d: %s'),cY=c(dC);a(11,cA);a(8,cy);a(7,cz);a(6,bp);a(5,[0,c("Division_by_zero")]);a(4,[0,c("End_of_file")]);a(3,bn);a(2,ar);a(1,[0,c("Sys_error")]);var
eO=[0,c("Out_of_memory")],d3=c(dv),d2=c(ap),d0=c("true"),d1=c("false"),d4=c("Pervasives.do_at_exit"),d6=c("nth"),d7=c("List.nth"),d_=c("\\b"),d$=c("\\t"),ea=c("\\n"),eb=c("\\r"),d9=c("\\\\"),d8=c("\\'"),ee=c(d),ed=c("String.blit"),ec=c("String.sub"),ef=c("Queue.Empty"),eh=c("Buffer.add: cannot grow buffer"),ex=c(d),ey=c(d),eB=c(dv),eC=c(a$),eD=c(a$),ez=c(a_),eA=c(a_),ew=c(dw),eu=c("neg_infinity"),ev=c("infinity"),et=c(ap),es=c("printf: bad positional specification (0)."),er=c("%_"),eq=[0,c("printf.ml"),143,8],eo=c(a_),ep=c("Printf: premature end of format string '"),ek=c(a_),el=c(" in format string '"),em=c(", at char number "),en=c("Printf: bad conversion %"),ei=c("Sformat.index_of_int: negative argument "),eI=c(d),eJ=c(", %s%s"),e0=[1,1],e1=c("%s\n"),e2=c("(Program not linked with -g, cannot print stack backtrace)\n"),eU=c("Raised at"),eX=c("Re-raised at"),eY=c("Raised by primitive operation at"),eZ=c("Called from"),eV=c('%s file "%s", line %d, characters %d-%d'),eW=c("%s unknown location"),eP=c("Out of memory"),eQ=c("Stack overflow"),eR=c("Pattern matching failed"),eS=c("Assertion failed"),eT=c("Undefined recursive module"),eK=c("(%s%s)"),eL=c(d),eM=c(d),eN=c("(%s)"),eH=c(dr),eF=c("%S"),eG=c("_"),e3=c("Lwt_sequence.Empty"),fg=[0,c(X),692,20],fh=[0,c(X),695,8],fe=[0,c(X),670,20],ff=[0,c(X),673,8],fc=[0,c(X),648,20],fd=[0,c(X),651,8],fa=[0,c(X),498,8],e$=[0,c(X),487,9],e_=c("Lwt.wakeup_later_result"),e9=c("Lwt.wakeup_result"),e6=c("Fatal error: exception "),e5=c("Lwt.Canceled"),fb=[0,0],fm=c("Js.Error"),fn=c(du),fu=c("table"),ft=c("img"),fs=c("br"),fr=c("h1"),fq=c("div"),fp=c("option"),fy=c("browser can't read file: unimplemented"),fx=[0,c("file.ml"),131,15],fv=c("can't retrieve file name: not implemented"),fB=c("Exception during Lwt.async: "),fD=c("[\\][()\\\\|+*.?{}^$]"),fQ=[0,c(d),0],fR=c(d),f4=c(d),f5=c(ba),gb=c(d),f6=c(a7),ga=c(d),f7=c(W),f8=c(W),f$=c(di),f9=c(d),f_=c("http://"),gc=c(d),gd=c(ba),gl=c(d),ge=c(a7),gk=c(d),gf=c(W),gg=c(W),gj=c(di),gh=c(d),gi=c("https://"),gm=c(d),gn=c(ba),gs=c(d),go=c(a7),gr=c(d),gp=c(W),gq=c("file://"),f3=c(d),f2=c(d),f1=c(d),f0=c(d),fZ=c(d),fY=c(d),fS=c(b0),fT=c(dH),fK=c("file"),fL=c("file:"),fM=c("http"),fN=c("http:"),fO=c("https"),fP=c("https:"),fH=c("%2B"),fF=c("Url.Local_exn"),fG=c(aH),fI=c("Url.Not_an_http_protocol"),fU=c("^([Hh][Tt][Tt][Pp][Ss]?)://([0-9a-zA-Z.-]+|\\[[0-9a-zA-Z.-]+\\]|\\[[0-9A-Fa-f:.]+\\])?(:([0-9]+))?/([^\\?#]*)(\\?([^#]*))?(#(.*))?$"),fW=c("^([Ff][Ii][Ll][Ee])://([^\\?#]*)(\\?([^#]*))?(#(.*))?$"),gJ=c(bh),gL=c("multipart/form-data; boundary="),gM=c(bh),gN=[0,c(bh),[0,c("application/x-www-form-urlencoded")],bg],gO=[0,c(bh),0,bg],gP=c("GET"),gK=c(a7),gE=c(b0),gF=c(b0),gG=c(dH),gA=c('"; filename="'),gB=c(dA),gy=c(bc),gz=c(bV),gC=c('"\r\n\r\n'),gD=c(dA),gw=c("--\r\n"),gx=c(bV),gv=c("js_of_ocaml-------------------"),gu=[0,c("xmlHttpRequest.ml"),85,2],gH=c("XmlHttpRequest.Wrong_headers"),g7=c(dI),g8=c(dt),g6=c(b7),g9=c("YOU WIN !"),g_=c("YOU LOSE !"),g$=c(dk),ha=c("sprites/R.png"),hb=c("sprites/L.png"),hc=c("sprites/U.png"),hd=c("sprites/D.png"),he=c("sprites/push_r.png"),hf=c("sprites/bR.png"),hg=c("sprites/push_l.png"),hh=c("sprites/bL.png"),hy=c(b2),hw=c(b2),hx=c(b2),ho=c("%g"),hn=c(dE),hp=c("malformed level"),hl=c("border-collapse:collapse;line-height: 0; opacity: 0; margin-left:auto; margin-right:auto"),hm=c("padding: 0; width: 20px; height: 20px;"),hq=c("font-family: sans-serif; text-align: center; background-color: #e8e8e8;"),hr=c("Boulder Dash in Ocaml"),hs=c("Elapsed time: "),ht=c(" Remaining diamonds: "),hu=c(F),hv=c("Choose a level"),hi=[0,c("boulderdash.ml"),294,17],hj=c("boulderdash"),hk=c(bV),hz=c("maps.txt"),gX=c("sprites/empty.png"),gY=c("sprites/grass.png"),gZ=c("sprites/diamond.png"),g0=c(dt),g1=c("sprites/door.png"),g2=c(dk),g3=c(dI),g4=c("sprites/wall.png"),g5=c("sprites/bam.png"),gV=c("%02d:%02d:%02d"),gU=c("--:--:--"),gT=c("LOADING..."),gQ=c("border: 1px black solid; background-color: white ; display: inline ; padding-right: .5em; padding-left: .5em;"),gR=c("background-color: red; color: white; display:inline; position: absolute; top:0; right:0;"),gW=c("Boulderdash.Death");function
L(a){throw[0,ar,a]}function
Z(a){throw[0,bn,a]}function
i(a,b){var
c=a.getLen(),e=b.getLen(),d=Q(c+e|0);ad(a,0,d,0,c);ad(b,0,d,c,e);return d}function
_(a){return c(d+a)}function
cd(a,b){if(a){var
c=a[1];return[0,c,cd(a[2],b)]}return b}h6(0);c_(1);var
as=c_(2);function
ce(a,b){return dT(a,b,0,b.getLen())}function
cf(a){return ce(as,a)}function
bo(a){var
b=h8(0);for(;;){if(b){var
c=b[2],d=b[1];try{c$(d)}catch(f){}var
b=c;continue}return 0}}da(d4,bo);function
d5(a,b){return db(a,b)}function
cg(a){return c$(a)}function
ch(a,b){var
d=b.length-1;if(0===d)return[0];var
e=R(d,j(a,b[0+1])),f=d-1|0,g=1;if(!(f<1)){var
c=g;for(;;){e[c+1]=j(a,b[c+1]);var
h=c+1|0;if(f!==c){var
c=h;continue}break}}return e}function
ci(a){if(a){var
d=0,c=a,g=a[2],h=a[1];for(;;){if(c){var
d=d+1|0,c=c[2];continue}var
f=R(d,h),e=1,b=g;for(;;){if(b){var
i=b[2];f[e+1]=b[1];var
e=e+1|0,b=i;continue}return f}}}return[0]}function
aj(a){var
b=a,c=0;for(;;){if(b){var
d=[0,b[1],c],b=b[2],c=d;continue}return c}}function
$(a,b){if(b){var
c=b[2],d=j(a,b[1]);return[0,d,$(a,c)]}return 0}function
ak(a,b){var
c=b;for(;;){if(c){var
d=c[2];j(a,c[1]);var
c=d;continue}return 0}}function
at(a,b){var
c=Q(a);hG(c,0,a,b);return c}function
U(a,b,c){if(0<=b)if(0<=c)if(!((a.getLen()-c|0)<b)){var
d=Q(c);ad(a,b,d,0,c);return d}return Z(ec)}function
aO(a,b,c,d,e){if(0<=e)if(0<=b)if(!((a.getLen()-e|0)<b))if(0<=d)if(!((c.getLen()-e|0)<d))return ad(a,b,c,d,e);return Z(ed)}function
au(d,b){if(b){var
a=b[1],g=[0,0],f=[0,0],h=b[2];ak(function(a){g[1]++;f[1]=f[1]+a.getLen()|0;return 0},b);var
e=Q(f[1]+dc(d.getLen(),g[1]-1|0)|0);ad(a,0,e,0,a.getLen());var
c=[0,a.getLen()];ak(function(a){ad(d,0,e,c[1],d.getLen());c[1]=c[1]+d.getLen()|0;ad(a,0,e,c[1],a.getLen());c[1]=c[1]+a.getLen()|0;return 0},h);return e}return ee}var
bq=ik(0),av=dc(bq/8|0,(1<<(bq-10|0))-1|0)-1|0,eg=[0,ef];function
br(a){var
b=1<=a?a:1,c=av<b?av:b,d=Q(c);return[0,d,0,c,d]}function
bs(a){return U(a[1],0,a[2])}function
cj(a,b){var
c=[0,a[3]];for(;;){if(c[1]<(a[2]+b|0)){c[1]=2*c[1]|0;continue}if(av<c[1])if((a[2]+b|0)<=av)c[1]=av;else
L(eh);var
d=Q(c[1]);aO(a[1],0,d,0,a[2]);a[1]=d;a[3]=c[1];return 0}}function
aw(a,b){var
c=a[2];if(a[3]<=c)cj(a,1);a[1].safeSet(c,b);a[2]=c+1|0;return 0}function
bt(a,b){var
c=b.getLen(),d=a[2]+c|0;if(a[3]<d)cj(a,c);aO(b,0,a[1],a[2],c);a[2]=d;return 0}function
bu(a){return 0<=a?a:L(i(ei,_(a)))}function
ck(a,b){return bu(a+b|0)}var
ej=1;function
cl(a){return ck(ej,a)}function
cm(a){return U(a,0,a.getLen())}function
cn(a,b,c){var
d=i(el,i(a,ek)),e=i(em,i(_(b),d));return Z(i(en,i(at(1,c),e)))}function
ax(a,b,c){return cn(cm(a),b,c)}function
aP(a){return Z(i(ep,i(cm(a),eo)))}function
aa(e,b,c,d){function
h(a){if((e.safeGet(a)+ao|0)<0||9<(e.safeGet(a)+ao|0))return a;var
b=a+1|0;for(;;){var
c=e.safeGet(b);if(48<=c){if(!(58<=c)){var
b=b+1|0;continue}}else
if(36===c)return b+1|0;return a}}var
i=h(b+1|0),f=br((c-i|0)+10|0);aw(f,37);var
a=i,g=aj(d);for(;;){if(a<=c){var
j=e.safeGet(a);if(42===j){if(g){var
k=g[2];bt(f,_(g[1]));var
a=h(a+1|0),g=k;continue}throw[0,u,eq]}aw(f,j);var
a=a+1|0;continue}return bs(f)}}function
co(a,b,c,d,e){var
f=aa(b,c,d,e);if(78!==a)if(aJ!==a)return f;f.safeSet(f.getLen()-1|0,bY);return f}function
cp(a){return function(d,b){var
k=d.getLen();function
l(a,b){var
m=40===a?41:bX,c=b;for(;;){if(k<=c)return aP(d);if(37===d.safeGet(c)){var
e=c+1|0;if(k<=e)return aP(d);var
f=d.safeGet(e),g=f-40|0;if(g<0||1<g){var
i=g+dn|0;if(i<0||2<i)var
h=1;else
switch(i){case
1:var
h=1;break;case
2:var
j=1,h=0;break;default:var
j=0,h=0}if(h){var
c=e+1|0;continue}}else
var
j=0===g?0:1;if(j)return f===m?e+1|0:ax(d,b,f);var
c=l(f,e+1|0)+1|0;continue}var
c=c+1|0;continue}}return l(a,b)}}function
cq(i,b,c){var
l=i.getLen()-1|0;function
r(a){var
k=a;a:for(;;){if(k<l){if(37===i.safeGet(k)){var
f=0,h=k+1|0;for(;;){if(l<h)var
e=aP(i);else{var
m=i.safeGet(h);if(58<=m){if(95===m){var
f=1,h=h+1|0;continue}}else
if(32<=m)switch(m+b4|0){case
1:case
2:case
4:case
5:case
6:case
7:case
8:case
9:case
12:case
15:break;case
0:case
3:case
11:case
13:var
h=h+1|0;continue;case
10:var
h=s(b,f,h,ah);continue;default:var
h=h+1|0;continue}var
d=h;b:for(;;){if(l<d)var
e=aP(i);else{var
j=i.safeGet(d);if(126<=j)var
g=0;else
switch(j){case
78:case
88:case
be:case
ah:case
b3:case
bY:case
bZ:var
e=s(b,f,d,ah),g=1;break;case
69:case
70:case
71:case
dD:case
b6:case
b5:var
e=s(b,f,d,b6),g=1;break;case
33:case
37:case
44:case
64:var
e=d+1|0,g=1;break;case
83:case
91:case
aK:var
e=s(b,f,d,aK),g=1;break;case
97:case
bb:case
bT:var
e=s(b,f,d,j),g=1;break;case
76:case
dJ:case
aJ:var
t=d+1|0;if(l<t)var
e=s(b,f,d,ah),g=1;else{var
p=i.safeGet(t)+dF|0;if(p<0||32<p)var
q=1;else
switch(p){case
0:case
12:case
17:case
23:case
29:case
32:var
e=n(c,s(b,f,d,j),ah),g=1,q=0;break;default:var
q=1}if(q)var
e=s(b,f,d,ah),g=1}break;case
67:case
99:var
e=s(b,f,d,99),g=1;break;case
66:case
98:var
e=s(b,f,d,66),g=1;break;case
41:case
bX:var
e=s(b,f,d,j),g=1;break;case
40:var
e=r(s(b,f,d,j)),g=1;break;case
b1:var
u=s(b,f,d,j),v=n(cp(j),i,u),o=u;for(;;){if(o<(v-2|0)){var
o=n(c,o,i.safeGet(o));continue}var
d=v-1|0;continue b}default:var
g=0}if(!g)var
e=ax(i,d,j)}break}}var
k=e;continue a}}var
k=k+1|0;continue}return k}}r(0);return 0}function
cr(a){var
d=[0,0,0,0];function
b(a,b,c){var
f=41!==c?1:0,g=f?bX!==c?1:0:f;if(g){var
e=97===c?2:1;if(bb===c)d[3]=d[3]+1|0;if(a)d[2]=d[2]+e|0;else
d[1]=d[1]+e|0}return b+1|0}cq(a,b,function(a,b){return a+1|0});return d[1]}function
cs(a,b,c){var
g=a.safeGet(c);if((g+ao|0)<0||9<(g+ao|0))return n(b,0,c);var
e=g+ao|0,d=c+1|0;for(;;){var
f=a.safeGet(d);if(48<=f){if(!(58<=f)){var
e=(10*e|0)+(f+ao|0)|0,d=d+1|0;continue}}else
if(36===f)return 0===e?L(es):n(b,[0,bu(e-1|0)],d+1|0);return n(b,0,c)}}function
w(a,b){return a?b:cl(b)}function
ct(a,b){return a?a[1]:b}function
cu(aI,b,c,d,e,f,g){var
B=j(b,g);function
ae(a){return n(d,B,a)}function
aL(a,b,k,aM){var
o=k.getLen();function
C(q,b){var
m=b;for(;;){if(o<=m)return j(a,B);var
d=k.safeGet(m);if(37===d){var
l=function(a,b){return h(aM,ct(a,b))},as=function(g,f,c,d){var
a=d;for(;;){var
_=k.safeGet(a)+b4|0;if(!(_<0||25<_))switch(_){case
1:case
2:case
4:case
5:case
6:case
7:case
8:case
9:case
12:case
15:break;case
10:return cs(k,function(a,b){var
d=[0,l(a,f),c];return as(g,w(a,f),d,b)},a+1|0);default:var
a=a+1|0;continue}var
o=k.safeGet(a);if(!(124<=o))switch(o){case
78:case
88:case
be:case
ah:case
b3:case
bY:case
bZ:var
a7=l(g,f),a8=a3(co(o,k,m,a,c),a7);return p(w(g,f),a8,a+1|0);case
69:case
71:case
dD:case
b6:case
b5:var
aZ=l(g,f),a0=bP(aa(k,m,a,c),aZ);return p(w(g,f),a0,a+1|0);case
76:case
dJ:case
aJ:var
ac=k.safeGet(a+1|0)+dF|0;if(!(ac<0||32<ac))switch(ac){case
0:case
12:case
17:case
23:case
29:case
32:var
R=a+1|0,ad=o-108|0;if(ad<0||2<ad)var
af=0;else{switch(ad){case
1:var
af=0,ag=0;break;case
2:var
a6=l(g,f),aB=a3(aa(k,m,R,c),a6),ag=1;break;default:var
a5=l(g,f),aB=a3(aa(k,m,R,c),a5),ag=1}if(ag)var
aA=aB,af=1}if(!af)var
a4=l(g,f),aA=hN(aa(k,m,R,c),a4);return p(w(g,f),aA,R+1|0)}var
a1=l(g,f),a2=a3(co(aJ,k,m,a,c),a1);return p(w(g,f),a2,a+1|0);case
37:case
64:return p(f,at(1,o),a+1|0);case
83:case
aK:var
v=l(g,f);if(aK===o)var
x=v;else{var
b=[0,0],al=v.getLen()-1|0,aN=0;if(!(al<0)){var
J=aN;for(;;){var
u=v.safeGet(J),bd=14<=u?34===u?1:92===u?1:0:11<=u?13<=u?1:0:8<=u?1:0,aR=bd?2:bQ(u)?1:4;b[1]=b[1]+aR|0;var
aS=J+1|0;if(al!==J){var
J=aS;continue}break}}if(b[1]===v.getLen())var
aD=v;else{var
h=Q(b[1]);b[1]=0;var
am=v.getLen()-1|0,aP=0;if(!(am<0)){var
I=aP;for(;;){var
t=v.safeGet(I),y=t-34|0;if(y<0||58<y)if(-20<=y)var
S=1;else{switch(y+34|0){case
8:h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],98);var
H=1;break;case
9:h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],bT);var
H=1;break;case
10:h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],aJ);var
H=1;break;case
13:h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],bb);var
H=1;break;default:var
S=1,H=0}if(H)var
S=0}else
var
S=(y-1|0)<0||56<(y-1|0)?(h.safeSet(b[1],92),b[1]++,h.safeSet(b[1],t),0):1;if(S)if(bQ(t))h.safeSet(b[1],t);else{h.safeSet(b[1],92);b[1]++;h.safeSet(b[1],48+(t/be|0)|0);b[1]++;h.safeSet(b[1],48+((t/10|0)%10|0)|0);b[1]++;h.safeSet(b[1],48+(t%10|0)|0)}b[1]++;var
aQ=I+1|0;if(am!==I){var
I=aQ;continue}break}}var
aD=h}var
x=i(eD,i(aD,eC))}if(a===(m+1|0))var
aC=x;else{var
G=aa(k,m,a,c);try{var
T=0,r=1;for(;;){if(G.getLen()<=r)var
an=[0,0,T];else{var
V=G.safeGet(r);if(49<=V)if(58<=V)var
ai=0;else
var
an=[0,dd(U(G,r,(G.getLen()-r|0)-1|0)),T],ai=1;else{if(45===V){var
T=1,r=r+1|0;continue}var
ai=0}if(!ai){var
r=r+1|0;continue}}var
X=an;break}}catch(f){f=z(f);if(f[1]!==ar)throw f;var
X=cn(G,0,aK)}var
K=X[1],A=x.getLen(),aT=X[2],L=0,aU=32;if(K===A)if(0===L)var
Y=x,aj=1;else
var
aj=0;else
var
aj=0;if(!aj)if(K<=A)var
Y=U(x,L,A);else{var
W=at(K,aU);if(aT)aO(x,L,W,0,A);else
aO(x,L,W,K-A|0,A);var
Y=W}var
aC=Y}return p(w(g,f),aC,a+1|0);case
67:case
99:var
q=l(g,f);if(99===o)var
ay=at(1,q);else{if(39===q)var
s=d8;else
if(92===q)var
s=d9;else{if(14<=q)var
D=0;else
switch(q){case
8:var
s=d_,D=1;break;case
9:var
s=d$,D=1;break;case
10:var
s=ea,D=1;break;case
13:var
s=eb,D=1;break;default:var
D=0}if(!D)if(bQ(q)){var
ak=Q(1);ak.safeSet(0,q);var
s=ak}else{var
E=Q(4);E.safeSet(0,92);E.safeSet(1,48+(q/be|0)|0);E.safeSet(2,48+((q/10|0)%10|0)|0);E.safeSet(3,48+(q%10|0)|0);var
s=E}}var
ay=i(eA,i(s,ez))}return p(w(g,f),ay,a+1|0);case
66:case
98:var
aX=a+1|0,aY=l(g,f)?d0:d1;return p(w(g,f),aY,aX);case
40:case
b1:var
P=l(g,f),au=n(cp(o),k,a+1|0);if(b1===o){var
M=br(P.getLen()),ao=function(a,b){aw(M,b);return a+1|0};cq(P,function(a,b,c){if(a)bt(M,er);else
aw(M,37);return ao(b,c)},ao);var
aV=bs(M);return p(w(g,f),aV,au)}var
av=w(g,f),bc=ck(cr(P),av);return aL(function(a){return C(bc,au)},av,P,aM);case
33:j(e,B);return C(f,a+1|0);case
41:return p(f,ex,a+1|0);case
44:return p(f,ey,a+1|0);case
70:var
$=l(g,f);if(0===c)var
az=eB;else{var
Z=aa(k,m,a,c);if(70===o)Z.safeSet(Z.getLen()-1|0,b5);var
az=Z}var
aq=hC($);if(3===aq)var
ab=$<0?eu:ev;else
if(4<=aq)var
ab=ew;else{var
O=bP(az,$),N=0,aW=O.getLen();for(;;){if(aW<=N)var
ap=i(O,et);else{var
F=O.safeGet(N)-46|0,bf=F<0||23<F?55===F?1:0:(F-1|0)<0||21<(F-1|0)?1:0;if(!bf){var
N=N+1|0;continue}var
ap=O}var
ab=ap;break}}return p(w(g,f),ab,a+1|0);case
91:return ax(k,a,o);case
97:var
aE=l(g,f),aF=cl(ct(g,f)),aG=l(0,aF),a9=a+1|0,a_=w(g,aF);if(aI)ae(n(aE,0,aG));else
n(aE,B,aG);return C(a_,a9);case
bb:return ax(k,a,o);case
bT:var
aH=l(g,f),a$=a+1|0,ba=w(g,f);if(aI)ae(j(aH,0));else
j(aH,B);return C(ba,a$)}return ax(k,a,o)}},f=m+1|0,g=0;return cs(k,function(a,b){return as(a,q,g,b)},f)}n(c,B,d);var
m=m+1|0;continue}}function
p(a,b,c){ae(b);return C(a,c)}return C(b,0)}var
p=bu(0);function
l(a,b){return aL(f,p,a,b)}var
m=cr(g);if(m<0||6<m){var
o=function(f,b){if(m<=f){var
h=R(m,0),i=function(a,b){return k(h,(m-a|0)-1|0,b)},c=0,a=b;for(;;){if(a){var
d=a[2],e=a[1];if(d){i(c,e);var
c=c+1|0,a=d;continue}i(c,e)}return l(g,h)}}return function(a){return o(f+1|0,[0,a,b])}};return o(0,0)}switch(m){case
1:return function(a){var
b=R(1,0);k(b,0,a);return l(g,b)};case
2:return function(a,b){var
c=R(2,0);k(c,0,a);k(c,1,b);return l(g,c)};case
3:return function(a,b,c){var
d=R(3,0);k(d,0,a);k(d,1,b);k(d,2,c);return l(g,d)};case
4:return function(a,b,c,d){var
e=R(4,0);k(e,0,a);k(e,1,b);k(e,2,c);k(e,3,d);return l(g,e)};case
5:return function(a,b,c,d,e){var
f=R(5,0);k(f,0,a);k(f,1,b);k(f,2,c);k(f,3,d);k(f,4,e);return l(g,f)};case
6:return function(a,b,c,d,e,f){var
h=R(6,0);k(h,0,a);k(h,1,b);k(h,2,c);k(h,3,d);k(h,4,e);k(h,5,f);return l(g,h)};default:return l(g,[0])}}function
cv(d){function
e(a){return 0}function
b(a){return d}var
c=0;return function(a){return cu(c,b,d5,ce,cg,e,a)}}function
eE(a){return br(2*a.getLen()|0)}function
x(a){function
b(a){var
b=bs(a);a[2]=0;return b}return cu(1,eE,aw,bt,function(a){return 0},b,a)}var
bv=[0,0];function
bx(a,b){var
c=a[b+1];if(ib(c)){if(de(c)===252)return j(x(eF),c);if(de(c)===253){var
e=bP(d3,c),d=0,g=e.getLen();for(;;){if(g<=d)return i(e,d2);var
f=e.safeGet(d),h=48<=f?58<=f?0:1:45===f?1:0;if(h){var
d=d+1|0;continue}return e}}return eG}return j(x(eH),c)}function
cw(a,b){if(a.length-1<=b)return eI;var
c=cw(a,b+1|0),d=bx(a,b);return n(x(eJ),d,c)}function
cx(a){var
b=bv[1];for(;;){if(b){var
r=b[2],s=b[1];try{var
t=j(s,a),e=t}catch(f){var
e=0}if(e)return e[1];var
b=r;continue}if(a[1]===eO)return eP;if(a[1]===cy)return eQ;if(a[1]===cz){var
f=a[2],k=f[3],v=f[2],w=f[1];return a4(x(bw),w,v,k,k+5|0,eR)}if(a[1]===u){var
g=a[2],l=g[3],y=g[2],z=g[1];return a4(x(bw),z,y,l,l+6|0,eS)}if(a[1]===cA){var
h=a[2],m=h[3],A=h[2],B=h[1];return a4(x(bw),B,A,m,m+6|0,eT)}var
d=a.length-1,C=a[0+1][0+1];if(d<0||2<d)var
o=cw(a,2),p=bx(a,1),c=n(x(eK),p,o);else
switch(d){case
1:var
c=eM;break;case
2:var
q=bx(a,1),c=j(x(eN),q);break;default:var
c=eL}return i(C,c)}}function
cB(a){var
i=hD(hK(0));if(i){var
d=i[1],f=d.length-1-1|0,q=0;if(!(f<0)){var
c=q;for(;;){if(ia(h(d,c),e0)){var
b=h(d,c),k=0===b[0]?b[1]:b[1],e=k?0===c?eU:eX:0===c?eY:eZ;if(0===b[0])var
l=b[5],m=b[4],o=b[3],p=b[2],g=a4(x(eV),e,p,o,m,l);else
var
g=j(x(eW),e);n(cv(a),e1,g)}var
r=c+1|0;if(f!==c){var
c=r;continue}break}}return 0}return j(cv(a),e2)}function
cC(a){bv[1]=[0,a,bv[1]];return 0}32===bq;var
e4=[0,e3];function
cD(a){var
b=a[4],c=b?(a[4]=0,a[1][2]=a[2],a[2][1]=a[1],0):b;return c}function
by(a){var
b=[];df(b,[0,b,b]);return b}function
bz(a){return a[2]===a?1:0}var
aQ=[0,e5],C=[0,0];function
aR(a){var
c=a[1];if(3===c[0]){var
d=c[1],b=aR(d);if(b!==d)a[1]=[3,b];return b}return a}function
ab(a){return aR(a)}var
cE=[0,function(a){cf(e6);cf(cx(a));db(as,10);cB(as);cg(as);bo(0);return il(2)}];function
cF(a,b){try{var
c=j(a,b)}catch(f){f=z(f);return j(cE[1],f)}return c}function
bR(a,b,c,d){var
f=c,e=d;for(;;)if(typeof
f===B)return a<50?S(1+a,b,e):T(S,[0,b,e]);else
switch(f[0]){case
1:j(f[1],b);return a<50?S(1+a,b,e):T(S,[0,b,e]);case
2:var
h=[0,f[2],e],f=f[1],e=h;continue;default:var
g=f[1][1];if(g){j(g[1],b);return a<50?S(1+a,b,e):T(S,[0,b,e])}else
return a<50?S(1+a,b,e):T(S,[0,b,e])}}function
S(a,b,c){return c?a<50?bR(1+a,b,c[1],c[2]):T(bR,[0,b,c[1],c[2]]):0}function
e7(b,c,d){return a5(bR(0,b,c,d))}function
iv(b,c){return a5(S(0,b,c))}function
bS(a,b,c){var
e=b,d=c;for(;;)if(typeof
e===B)return a<50?ae(1+a,d):T(ae,[0,d]);else
switch(e[0]){case
1:cD(e[1]);return a<50?ae(1+a,d):T(ae,[0,d]);case
2:var
g=[0,e[2],d],e=e[1],d=g;continue;default:var
f=e[2];C[1]=e[1];cF(f,0);return a<50?ae(1+a,d):T(ae,[0,d])}}function
ae(a,b){return b?a<50?bS(1+a,b[1],b[2]):T(bS,[0,b[1],b[2]]):0}function
e8(b,c){return a5(bS(0,b,c))}function
iw(b){return a5(ae(0,b))}function
aS(a,b){var
c=1===b[0]?b[1][1]===aQ?(e8(a[4],0),1):0:0;return e7(b,a[2],0)}var
aT=[0,0],v=[0,0,0];function
cG(a,b){var
f=C[1],g=aT[1]?1:(aT[1]=1,0);aS(a,b);if(g){C[1]=f;return 0}for(;;){if(0===v[1]){aT[1]=0;C[1]=f;return 0}if(0===v[1])throw[0,eg];v[1]=v[1]-1|0;var
c=v[2],d=c[2];if(d===c)v[2]=0;else
c[2]=d[2];var
e=d[1];aS(e[1],e[2]);continue}}function
cH(a){return[0,a]}function
cI(a,b){var
d=aR(a),c=d[1];switch(c[0]){case
1:if(c[1][1]===aQ)return 0;break;case
2:var
e=c[1];d[1]=b;return cG(e,b)}return Z(e9)}function
aU(a,b){return cI(a,cH(b))}function
cJ(a,b){return typeof
a===B?b:typeof
b===B?a:[2,a,b]}function
bA(a){if(typeof
a!==B)switch(a[0]){case
2:var
b=a[1],c=bA(a[2]);return cJ(bA(b),c);case
1:break;default:if(!a[1][1])return 0}return a}function
cK(a,b){var
d=ab(a),g=ab(b),j=d[1];if(2===j[0]){var
c=j[1];if(d===g)return 0;var
e=g[1];if(2===e[0]){var
f=e[1];g[1]=[3,d];c[1]=f[1];var
k=cJ(c[2],f[2]),l=c[3]+f[3]|0;if(42<l){c[3]=0;c[2]=bA(k)}else{c[3]=l;c[2]=k}var
h=f[4],i=c[4],m=typeof
i===B?h:typeof
h===B?i:[2,i,h];c[4]=m;return 0}d[1]=e;return aS(c,e)}throw[0,u,e$]}function
aV(a,b){var
c=ab(a),d=c[1];if(2===d[0]){var
e=d[1];c[1]=b;return aS(e,b)}throw[0,u,fa]}function
l(a){return[0,[0,a]]}var
cL=[0,fb];function
ay(a){return[0,[1,a]]}function
bB(a){return[0,[2,[0,[0,[0,a]],0,0,0]]]}function
bC(a){var
b=[0,[2,[0,1,0,0,0]]];return[0,b,b]}function
bD(a,b){var
d=[1,b],c=a[2],e=typeof
c===B?d:[2,d,c];a[2]=e;return 0}function
bE(a,b){var
c=ab(a)[1];switch(c[0]){case
1:if(c[1][1]===aQ)return cF(b,0);break;case
2:var
d=c[1],e=[0,C[1],b],f=d[4],g=typeof
f===B?e:[2,e,f];d[4]=g;return 0}return 0}function
m(a,b){var
e=ab(a),c=e[1];switch(c[0]){case
1:return[0,c];case
2:var
f=c[1],d=bB(e),g=C[1];bD(f,function(a){switch(a[0]){case
0:var
e=a[1];C[1]=g;try{var
f=j(b,e),c=f}catch(f){f=z(f);var
c=ay(f)}return cK(d,c);case
1:return aV(d,a);default:throw[0,u,fc]}});return d;case
3:throw[0,u,fd];default:return j(b,c[1])}}function
cM(a,b){return m(a,b)}function
cN(a,b){var
e=ab(a),c=e[1];switch(c[0]){case
1:return[0,c];case
2:var
i=c[1],d=bB(e),k=C[1];bD(i,function(a){switch(a[0]){case
0:var
e=a[1];C[1]=k;try{var
f=[0,j(b,e)],c=f}catch(f){f=z(f);var
c=[1,f]}return aV(d,c);case
1:return aV(d,a);default:throw[0,u,fe]}});return d;case
3:throw[0,u,ff];default:var
f=c[1];try{var
h=[0,j(b,f)],g=h}catch(f){f=z(f);var
g=[1,f]}return[0,g]}}function
cO(a,b){try{var
i=j(a,0),g=i}catch(f){f=z(f);var
g=ay(f)}var
c=ab(g),d=c[1];switch(d[0]){case
1:return j(b,d[1]);case
2:var
f=d[1],e=bB(c),h=C[1];bD(f,function(a){switch(a[0]){case
0:return aV(e,a);case
1:var
d=a[1];C[1]=h;try{var
f=j(b,d),c=f}catch(f){f=z(f);var
c=ay(f)}return cK(e,c);default:throw[0,u,fg]}});return e;case
3:throw[0,u,fh];default:return c}}var
fi=[0,function(a){return 0}],M=by(0),fj=[0,0];function
fk(a){var
e=1-bz(M);if(e){var
b=by(0);b[1][2]=M[2];M[2][1]=b[1];b[1]=M[1];M[1][2]=b;M[1]=M;M[2]=M;fj[1]=0;var
c=b[2];for(;;){var
d=c!==b?1:0;if(d){if(c[4])aU(c[3],0);var
c=c[2];continue}return d}}return e}function
cP(c,b){if(b){var
d=b[2],a=b[1],e=function(a){return cP(c,d)};return cM(j(c,a),e)}return cL}var
D=b,N=null,al=undefined;function
aW(a,b,c){return a==N?j(b,0):j(c,a)}function
cQ(a){function
b(a){return[0,a]}return aW(a,function(a){return 0},b)}function
az(a){return a!==al?1:0}function
bF(a,b,c){return a===al?j(b,0):j(c,a)}function
J(a,b){return a===al?j(b,0):a}function
aX(a){function
b(a){return[0,a]}return bF(a,function(a){return 0},b)}var
bG=true,aY=false,aA=RegExp,bH=Array;function
y(a,b){return a[b]}function
cR(a){return a}var
cS=[0,fm];da(fn,[0,cS,{}][0+1]);var
fl=Math;function
cT(a){return escape(a)}cC(function(a){return a[1]===cS?[0,new
H(a[2].toString())]:0});cC(function(a){return a
instanceof
bH?0:[0,new
H(a.toString())]});function
am(a){return a}function
aB(a){return a}function
o(a,b){a.appendChild(b);return 0}function
cU(a,b){a.removeChild(b);return 0}function
E(d){return aB(a6(function(a){if(a){var
e=j(d,a);if(!(e|0))a.preventDefault();return e}var
c=event,b=j(d,c);if(!(b|0))c.returnValue=b;return b}))}var
cV=h2(0)|0,fo=D.document;function
aZ(a,b){return a?j(b,a[1]):0}function
bI(a,b){return a.createElement(b.toString())}function
an(a,b){return bI(a,b)}var
cW=[0,dy];function
cX(a){return an(a,fp)}function
aC(a){return an(a,fq)}function
cZ(a){return an(a,fs)}am(D.HTMLElement)===al;var
fw=D.FileReader,fz=h0(0),fA=bW;function
a0(a){var
b=bC(0),c=b[1],d=[0,0],g=b[2];function
e(a,b){var
c=bW<a?[0,fA,a-bW]:[0,a,0],f=c[2],h=c[1],i=f==0?function(a){return aU(g,a)}:function(a){return e(f,a)};d[1]=[0,D.setTimeout(a6(i),h*dz)];return 0}e(a,0);bE(c,function(a){var
b=d[1];return b?D.clearTimeout(b[1]):0});return c}fi[1]=function(a){return 1===a?(D.setTimeout(a6(fk),0),0):0};function
c0(a){return fz.log(a.toString())}cE[1]=function(a){c0(fB);c0(cx(a));return cB(as)};function
c1(a){return new
aA(p(a),aG)}var
fC=new
aA("[$]",aG),fE=c1(fD);function
c3(a,b){return b.split(at(1,a).toString())}var
c4=[0,fF];function
ac(a){throw[0,c4]}var
c2=c1(V(p(fG).replace(fE,"\\$&"))),c5=new
aA("\\+",aG);function
O(a){c5[c(dx)]=0;return V(unescape(a.replace(c5,F)))}function
q(a,b){var
e=a?a[1]:1;if(e){var
f=V(cT(p(b)));c2[c(dx)]=0;var
d=p(f);return V(d.replace(c2,p(fH).replace(fC,"$$$$")))}return V(cT(p(b)))}var
fJ=[0,fI];function
aD(a){try{var
c=a.getLen();if(0===c)var
d=fQ;else{var
b=0,g=47,f=a.getLen();for(;;){if(f<=b)throw[0,bp];if(a.safeGet(b)!==g){var
b=b+1|0;continue}if(0===b)var
e=[0,fR,aD(U(a,1,c-1|0))];else
var
h=aD(U(a,b+1|0,(c-b|0)-1|0)),e=[0,U(a,0,b),h];var
d=e;break}}}catch(f){f=z(f);if(f[1]===bp)return[0,a,0];throw f}return d}function
a1(a){return au(fT,$(function(a){var
b=a[1],c=i(fS,q(0,a[2]));return i(q(0,b),c)},a))}function
bJ(a){var
d=c3(38,a),b=d.length;function
e(a,b){var
c=b;for(;;){if(0<=c){try{var
f=c-1|0,g=function(a){function
e(a){var
c=a[2],d=a[1];function
b(a){return O(J(a,ac))}var
e=b(c);return[0,b(d),e]}var
b=c3(61,a);if(2===b.length)var
d=y(b,1),c=am([0,y(b,0),d]);else
var
c=al;return bF(c,ac,e)},h=e([0,bF(y(d,c),ac,g),a],f)}catch(f){f=z(f);if(f[1]===c4){var
c=c-1|0;continue}throw f}return h}return a}}return e(0,b-1|0)}var
fV=new
aA(p(fU)),fX=new
aA(p(fW));function
c6(a){switch(a[0]){case
1:var
c=a[1],h=c[6],j=c[5],k=c[2],w=c[3],x=c[1],y=A(h,gc)?i(gd,q(0,h)):gl,z=j?i(ge,a1(j)):gk,B=i(z,y),C=i(gg,i(au(gf,$(function(a){return q(0,a)},w)),B)),D=dq===k?gh:i(gj,_(k)),E=i(D,C);return i(gi,i(q(0,x),E));case
2:var
d=a[1],l=d[4],m=d[3],F=d[1],G=A(l,gm)?i(gn,q(0,l)):gs,H=m?i(go,a1(m)):gr,I=i(H,G);return i(gq,i(au(gp,$(function(a){return q(0,a)},F)),I));default:var
b=a[1],e=b[6],f=b[5],g=b[2],n=b[3],o=b[1],p=A(e,f4)?i(f5,q(0,e)):gb,r=f?i(f6,a1(f)):ga,s=i(r,p),t=i(f8,i(au(f7,$(function(a){return q(0,a)},n)),s)),u=80===g?f9:i(f$,_(g)),v=i(u,t);return i(f_,i(q(0,o),v))}}var
aE=location;O(aE.hostname);O(aE.protocol);try{}catch(f){f=z(f);if(f[1]!==ar)throw f}aD(O(aE.pathname));bJ(aE.search);O(aE.href);var
gt=D.FormData;function
c7(a,b){if(bf<=a[1]){var
d=a[2];d[1]=[0,b,d[1]];return 0}var
e=a[2],c=b[2],f=b[1];return a8<=c[1]?e.append(f.toString(),c[2]):e.append(f.toString(),c[2])}function
bK(a){return ActiveXObject}var
gI=[0,gH];function
e(a){return a.toString()}var
f=D.document;function
P(a,b){return o(a,f.createTextNode(e(b)))}function
bL(a,b){var
c=a.firstChild;if(c!=N)cU(a,c);return o(a,b)}var
c8=e(gQ),bM=[0,gW],gS=e(gR);function
c9(a){switch(a){case
1:return e(gY);case
2:return e(gZ);case
3:return e(g0);case
4:return e(g1);case
5:return e(g2);case
6:return e(g3);case
7:return e(g4);case
8:return e(g5);default:return e(gX)}}function
r(a,b,c,d){k(h(a[1],c),b,d);var
e=h(h(a[2],c),b);return e.src=c9(d)}function
a2(b){var
d=[0,0],e=b[1].length-1-2|0;if(!(e<1)){var
a=e;for(;;){var
f=h(b[1],a).length-1-2|0,j=1;if(!(f<1)){var
c=j;for(;;){var
g=6===h(h(b[1],a+1|0),c)?1:0,n=g?3===h(h(b[1],a),c)?1:0:g,p=0===h(h(b[1],a),c)?3===h(h(b[1],a-1|0),c)?(r(b,c,a-1|0,0),r(b,c,a,3),d[1]=1,1):0:0,q=0===h(h(b[1],a),c)?0===h(h(b[1],a-1|0),c)?3===h(h(b[1],a),c-1|0)?3===h(h(b[1],a-1|0),c-1|0)?(r(b,c-1|0,a-1|0,0),r(b,c,a,3),d[1]=1,1):0:0:0:0,s=0===h(h(b[1],a),c)?0===h(h(b[1],a-1|0),c)?3===h(h(b[1],a),c+1|0)?3===h(h(b[1],a-1|0),c+1|0)?(r(b,c+1|0,a-1|0,0),r(b,c,a,3),d[1]=1,1):0:0:0:0;if(!n)if(6===h(h(b[1],a+1|0),c))if(3===h(h(b[1],a),c)){r(b,c,a+1|0,8);throw[0,bM]}var
o=c+1|0;if(f!==c){var
c=o;continue}break}}var
k=a-1|0;if(1!==a){var
a=k;continue}break}}if(d[1]){var
i=function(a){return a2(b)};return m(a0(b8),i)}return l(0)}function
bN(g,x,w){var
L=w[3];function
n(a){var
M=g[1].length-1-1|0,Y=0;if(!(M<0)){var
c=Y;for(;;){var
V=h(g[1],c).length-1-1|0,ah=0;if(!(V<0)){var
d=ah;for(;;){h(h(g[2],c),d).onmouseover=N;h(h(g[2],c),d).onmouseout=N;h(h(g[2],c),d).onclick=N;var
aj=d+1|0;if(V!==d){var
d=aj;continue}break}}var
ai=c+1|0;if(M!==c){var
c=ai;continue}break}}function
n(a,b){if(!g[8]){g[8]=1;var
c=function(a){g[8]=0;return l(0)};m(j(a,0),c)}return aY}function
O(a,b,c){function
d(a){g[9][1]=[0,b];return l(0)}return m(j(a,0),d)}function
o(a,b){var
c=g[9][1];return c?(j(c[1],0),g[9][1]=0,j(a,0)):j(a,0)}function
i(a,I,H,G,e,f){var
t=a,p=G,k=e,i=f;for(;;){var
b=t[2],c=t[1],D=h(h(g[1],b),c);if(5===D)var
C=0;else
if(3<=D)var
F=0,C=1;else
var
C=0;if(!C)var
F=1;if(F){var
d=h(h(g[2],b),c).src,q=function(p,b,c){return function(a){h(h(g[2],b),c).src=H;return j(p,0)}}(p,b,c),s=function(k,b,c,d){return function(a){h(h(g[2],b),c).src=d;return j(k,0)}}(k,b,c,d),J=function(i,b,c){return function(a){function
d(a){if(2===h(h(g[1],b),c))g[5]=g[5]-1|0;r(g,c,b,6);function
d(a){function
d(a){r(g,c,b,0);return l(0)}return m(a2(g),d)}return m(a0(b8),d)}return m(j(i,0),d)}}(i,b,c),u=function(i,b,c){return function(a){var
d=g[3];r(g,d[1],d[2],0);function
e(a){return bN(g,x,w)}function
f(a){return a[1]===bM?(g[6]=1,l(0)):ay(a)}return m(cO(function(a){function
d(a){if(2===h(h(g[1],b),c))g[5]=g[5]-1|0;r(g,c,b,6);g[3]=[0,c,b];return a2(g)}return m(j(i,0),d)},f),e)}}(i,b,c),K=h(h(g[2],b),c),v=function(q){return function(a){return o(q,a)}}(q),y=function(s,v){return function(a){return O(v,s,a)}}(s,v);K.onmouseover=E(function(y){return function(a){return n(y,a)}}(y));var
L=h(h(g[2],b),c),z=function(a){return l(0)},A=function(z){return function(a){return o(z,a)}}(z);L.onmouseout=E(function(A){return function(a){return n(A,a)}}(A));var
M=h(h(g[2],b),c),B=function(u){return function(a){return o(u,a)}}(u);M.onclick=E(function(B){return function(a){return n(B,a)}}(B));if(5===h(h(g[1],b),c))return 0;var
t=j(I,[0,c,b]),p=q,k=s,i=J;continue}return 0}}function
P(a,b,c,d){var
p=a[2],q=a[1],k=j(b,a),f=k[2],i=k[1],t=j(b,k),u=t[2],v=t[1];try{var
B=3===h(h(g[1],f),i)?1:0,P=B?0===h(h(g[1],u),v)?1:0:B,y=P}catch(f){f=z(f);if(f[1]===bn)if(A(f[2],g6))var
s=0;else
var
y=0,s=1;else
var
s=0;if(!s)throw f}if(y){var
C=function(a){h(h(g[2],p),q).src=d;h(h(g[2],f),i).src=c;return l(0)},D=function(a){var
b=h(h(g[2],p),q);b.src=e(g7);var
c=h(h(g[2],f),i);return c.src=e(g8)},F=function(a){r(g,q,p,0);r(g,i,f,6);g[3]=k;r(g,v,u,3);function
b(a){return bN(g,x,w)}function
c(a){return a[1]===bM?(g[6]=1,l(0)):ay(a)}return m(cO(function(a){return a2(g)},c),b)},G=h(h(g[2],f),i),H=function(a){return o(C,a)},I=function(a){return O(H,D,a)};G.onmouseover=E(function(a){return n(I,a)});var
J=h(h(g[2],f),i),K=function(a){return l(0)},L=function(a){return o(K,a)};J.onmouseout=E(function(a){return n(L,a)});var
M=h(h(g[2],f),i),N=function(a){return o(F,a)};return M.onclick=E(function(a){return n(N,a)})}return 0}if(dg(g[3],g[4])){j(L,0);D.alert(e(g9))}else
if(g[6]){j(L,0);D.alert(e(g_))}else{if(0===g[5]){var
Q=g[4],R=Q[2],S=Q[1],_=h(h(g[2],R),S);_.src=e(g$);k(h(g[1],R),S,5)}var
y=function(a){return[0,a[1]+1|0,a[2]]},B=function(a){return[0,a[1]-1|0,a[2]]},T=function(a){return[0,a[1],a[2]-1|0]},U=function(a){return[0,a[1],a[2]+1|0]},q=function(a){return 0},b=function(a){return l(0)},$=e(ha);i(y(g[3]),y,$,b,q,b);var
aa=e(hb);i(B(g[3]),B,aa,b,q,b);var
ab=e(hc);i(T(g[3]),T,ab,b,q,b);var
ac=e(hd);i(U(g[3]),U,ac,b,q,b);var
ad=e(he),ae=e(hf);P(g[3],y,ae,ad);var
af=e(hg),ag=e(hh);P(g[3],B,ag,af);j(x,g[5])}var
p=g[7];if(p[1])if(bz(p[2]))p[1]=0;else{var
K=p[2],X=0;if(bz(K))throw[0,e4];var
G=K[2];cD(G);var
W=G[3],u=cH(X),H=aR(W),t=H[1];switch(t[0]){case
1:var
f=t[1][1]===aQ?1:0;break;case
2:var
I=t[1];H[1]=u;if(aT[1]){var
J=[0,I,u];if(0===v[1]){var
s=[];df(s,[0,J,s]);v[1]=1;v[2]=s;var
f=1}else{var
C=v[2],F=[0,J,C[2]];v[1]=v[1]+1|0;C[2]=F;v[2]=F;var
f=1}}else{cG(I,u);var
f=1}break;default:var
f=0}if(!f)Z(e_)}return l(0)}var
c=g[7];if(c[1]){var
a=c[2],d=[0,1,0,0,0],f=[0,[2,d]],b=[0,a[1],a,f,1];a[1][2]=b;a[1]=b;d[4]=[1,b];var
i=f}else{c[1]=1;var
i=cL}return m(i,n)}function
bO(a,b){return b?a.style.cssText=b[1]:0}D.onload=E(function(a){var
k=f.getElementById(e(hj));if(k==N)throw[0,u,hi];var
C=aC(f),r=[0,aF(0)],b=aC(f);b.style.cssText=c8;P(b,gU);var
c=[0,1];function
v(a){var
g=aF(0)-r[1];if(!c[1]){var
d=g|0;bL(b,f.createTextNode(e(s(x(gV),d/3600|0,(d/60|0)%60|0,d%60|0))))}function
h(a){return v(0)}return m(a0(1),h)}v(0);function
w(a){c[1]=1;return 0}var
t=[0,b,function(a){r[1]=aF(0);c[1]=0;return 0},w],Q=t[2],F=t[1];function
D(a,b){var
v=aC(f);v.style.cssText=gS;P(v,gT);o(k,v);function
bd(a){function
c(a){cU(k,v);return l(a)}return m(j(b,a),c)}function
bb(a){var
b=a[2],c=a[4];if(0!==b)if(200!==b)return[0,[2,[0,0,0,0,0]]];return l(c)}var
ay=0,aA=0,aE=0,aF=0,aG=0,aH=0,t=0,K=0,ba=0,aV=0?ba[1]:0,aZ=aH?aH[1]:0,a0=aF?aF[1]:function(a,b){return 1};if(aG){var
aa=aG[1];if(t){var
a2=t[1];ak(function(a){return c7(aa,[0,a[1],a[2]])},a2)}var
g=[0,aa]}else
if(t){var
a$=t[1],S=aX(am(gt)),ax=S?[0,808620462,new(S[1])()]:[0,bf,[0,0]];ak(function(a){return c7(ax,[0,a[1],a[2]])},a$);var
g=[0,ax]}else
var
g=0;if(g){var
ab=g[1];if(K)var
ad=[0,gJ,K,bg];else{if(bf<=ab[1]){var
z=0,x=0,h=ab[2][1];for(;;){if(h){var
M=h[2],C=h[1],aI=a8<=C[2][1]?0:1;if(aI){var
z=[0,C,z],h=M;continue}var
x=[0,C,x],h=M;continue}var
aJ=aj(x);aj(z);if(aJ)var
T=function(a){return _(fl.random()*1e9|0)},aQ=T(0),U=i(gv,i(T(0),aQ)),av=[0,gM,[0,i(gL,U)],[0,164354597,U]];else
var
av=gN;var
aw=av;break}}else
var
aw=gO;var
ad=aw}var
r=ad}else
var
r=[0,gP,K,bg];var
ae=r[3],af=r[2],R=p(a),a3=r[1];function
aK(a){var
c=cR(a),b=V(J(y(c,1),ac).toLowerCase());if(A(b,fK))if(A(b,fL)){if(A(b,fM))if(A(b,fN)){if(A(b,fO))if(A(b,fP))var
d=1,f=0;else
var
f=1;else
var
f=1;if(f)var
e=1,d=2}else
var
d=0;else
var
d=0;switch(d){case
1:var
g=0;break;case
2:var
g=1;break;default:var
e=0,g=1}if(g){var
h=O(J(y(c,5),ac)),k=function(a){return p(fZ)},l=O(J(y(c,9),k)),m=function(a){return p(f0)},n=bJ(J(y(c,7),m)),o=aD(h),q=function(a){return p(f1)},i=V(J(y(c,4),q)),r=A(i,fY)?dd(i):e?dq:80,j=[0,O(J(y(c,2),ac)),r,o,h,n,l],s=e?[1,j]:[0,j];return[0,s]}}throw[0,fJ]}function
aL(a){function
b(a){var
b=cR(a),c=O(J(y(b,2),ac));function
d(a){return p(f2)}var
e=V(J(y(b,6),d));function
f(a){return p(f3)}var
g=bJ(J(y(b,4),f));return[0,[2,[0,aD(c),c,g,e]]]}function
c(a){return 0}return aW(fX.exec(R),c,b)}var
Q=aW(fV.exec(R),aL,aK);if(Q){var
D=Q[1];switch(D[0]){case
0:var
W=D[1],X=W.slice(),aS=W[5];X[5]=0;var
s=[0,c6([0,X]),aS],w=1;break;case
1:var
Y=D[1],Z=Y.slice(),aT=Y[5];Z[5]=0;var
s=[0,c6([1,Z]),aT],w=1;break;default:var
w=0}}else
var
w=0;if(!w)var
s=[0,a,0];var
ag=s[1],ah=cd(s[2],aZ),ai=ah?i(ag,i(gK,a1(ah))):ag,an=bC(0),ao=an[2],ap=an[1];try{var
aP=new
XMLHttpRequest(),c=aP}catch(f){try{var
aO=new(bK(0))("Msxml2.XMLHTTP"),c=aO}catch(f){try{var
aN=new(bK(0))("Msxml3.XMLHTTP"),c=aN}catch(f){try{var
aM=new(bK(0))("Microsoft.XMLHTTP")}catch(f){throw[0,u,gu]}var
c=aM}}}if(ay)c.overrideMimeType(ay[1].toString());c.open(a3.toString(),ai.toString(),bG);if(af)c.setRequestHeader("Content-type",af[1].toString());ak(function(a){return c.setRequestHeader(a[1].toString(),a[2].toString())},aV);function
F(a){function
b(a){return[0,new
H(a)]}function
d(a){return 0}return aW(c.getResponseHeader(p(a)),d,b)}var
aq=[0,0];function
G(a){var
b=aq[1]?0:n(a0,c.status,F)?0:(cI(ao,[1,[0,gI,[0,c.status,F]]]),c.abort(),1);aq[1]=1;return 0}c.onreadystatechange=a6(function(a){switch(c.readyState){case
2:if(!cV)return G(0);break;case
3:if(cV)return G(0);break;case
4:G(0);var
b=function(a){var
b=cQ(c.responseXML);if(b){var
d=b[1];return aB(d.documentElement)===N?0:[0,d]}return 0};return aU(ao,[0,ai,c.status,F,new
H(c.responseText),b])}return 0});if(aE){var
a4=aE[1];c.onprogress=E(function(a){n(a4,a.loaded,a.total);return bG})}var
ar=c.upload;if(ar!==al)if(aA){var
a5=aA[1];ar.onprogress=E(function(a){n(a5,a.loaded,a.total);return bG})}if(g){var
I=g[1];if(bf<=I[1]){var
as=I[2];if(typeof
ae===B){var
a7=as[1];c.send(aB(au(gG,$(function(a){var
b=a[2],c=a[1];if(a8<=b[1]){var
d=i(gE,q(0,new
H(b[2].name)));return i(q(0,c),d)}var
e=i(gF,q(0,new
H(b[2])));return i(q(0,c),e)},a7)).toString()))}else{var
at=ae[2],a9=function(a){var
b=aB(a.join(d));return az(c.sendAsBinary)?c.sendAsBinary(b):c.send(b)},a_=as[1],e=new
bH(),aR=function(a){e.push(i(gx,i(at,gw)).toString());return e};cN(cN(cP(function(a){e.push(i(gz,i(at,gy)).toString());var
g=a[2],n=a[1];if(a8<=g[1]){var
b=g[2],r=function(a){var
c=aX(b.name),g="Content-Type: application/octet-stream\r\n",h='"\r\n';if(c)var
f=c[1];else
var
d=aX(b.fileName),f=d?d[1]:L(fv);e.push(i(gB,i(n,gA)).toString(),f,h,g);e.push(bc,a,bc);return l(0)},k=aX(am(fw)),d=-1041425454;if(k){var
c=new(k[1])(),h=bC(0),j=h[1],p=h[2];c.onloadend=E(function(a){if(2===c.readyState){var
b=c.result,e=dg(typeof
b,"string")?aB(b):N,d=cQ(e);if(!d)throw[0,u,fx];aU(p,d[1])}return aY});bE(j,function(a){return c.abort()});if(typeof
d===B)if(dm===d)c.readAsDataURL(b);else
if(dl<=d)c.readAsText(b);else
c.readAsBinaryString(b);else
c.readAsText(b,d[2]);var
o=j}else{var
f=function(a){return L(fy)};if(typeof
d===B)var
m=dm===d?az(b.getAsDataURL)?b.getAsDataURL():f(0):dl<=d?az(b.getAsText)?b.getAsText("utf8"):f(0):az(b.getAsBinary)?b.getAsBinary():f(0);else
var
q=d[2],m=az(b.getAsText)?b.getAsText(q):f(0);var
o=l(m)}return cM(o,r)}var
s=g[2];e.push(i(gD,i(n,gC)).toString(),s,bc);return l(0)},a_),aR),a9)}}else
c.send(I[2])}else
c.send(N);bE(ap,function(a){return c.abort()});return m(m(ap,bb),bd)}var
g=aC(f);g.style.cssText=c8;P(g,hk);function
R(a){return bL(g,f.createTextNode(_(a).toString()))}function
G(n){var
G=k.style;G.cssText=e(hq);var
y=an(f,fr);P(y,hr);o(k,y);var
a=aC(f);P(a,hs);o(a,F);P(a,ht);o(a,g);P(a,hu);var
p=0,q=0;for(;;){if(0===q)if(0===p)var
b=bI(f,cY),r=1;else
var
r=0;else
var
r=0;if(!r){var
s=cW[1];if(dy===s){try{var
v=fo.createElement('<input name="x">'),w=v.tagName.toLowerCase()==="input"?1:0,B=w?v.name===ds?1:0:w,u=B}catch(f){var
u=0}var
A=u?dp:-1003883683;cW[1]=A;continue}if(dp<=s){var
c=new
bH();c.push("<",dC);aZ(q,function(a){c.push(' type="',dh(a),a$);return 0});aZ(p,function(a){c.push(' name="',dh(a),a$);return 0});c.push(">");var
b=f.createElement(c.join(d))}else{var
i=bI(f,cY);aZ(q,function(a){return i.type=a});aZ(p,function(a){return i.name=a});var
b=i}}var
z=cX(f);P(z,hv);o(b,z);ak(function(a){var
d=a[2],c=cX(f);P(c,d);return o(b,c)},n);b.onchange=E(function(a){var
d=b.selectedIndex-1|0;if(0<=d){var
i=0,g=n;for(;;){if(g){var
i=i+1|0,g=g[2];continue}if(d<i){if(0<=d){var
c=n,k=d;for(;;){if(c){var
r=c[2],s=c[1];if(0!==k){var
c=r,k=k-1|0;continue}var
p=s}else
var
p=L(d6);break}}else
var
p=Z(d7);var
u=p[1];D(u,function(a){var
v=[0,0],b=[0,0],A=a.getLen()-1|0,O=0;if(!(A<0)){var
r=O;for(;;){var
d=a.safeGet(r);if(47<=d)if(83<=d)if(89<=d)var
g=0;else{switch(d+dn|0){case
0:b[1]=[0,6,b[1]];var
s=1;break;case
4:b[1]=[0,6,b[1]];var
s=1;break;case
5:b[1]=[0,3,b[1]];var
s=1;break;default:var
g=0,s=0}if(s)var
g=1}else
var
g=69===d?(b[1]=[0,4,b[1]],1):0;else
if(10===d){var
X=v[1];v[1]=[0,aj(b[1]),X];b[1]=0;var
g=1}else
if(32<=d){switch(d+b4|0){case
0:b[1]=[0,0,b[1]];var
n=1;break;case
3:b[1]=[0,7,b[1]];var
n=1;break;case
11:b[1]=[0,2,b[1]];var
n=1;break;case
14:b[1]=[0,1,b[1]];var
n=1;break;default:var
g=0,n=0}if(n)var
g=1}else
var
g=0;if(!g)L(hp);var
W=r+1|0;if(A!==r){var
r=W;continue}break}}var
w=ci($(ci,aj(v[1])));function
P(a){var
b=an(f,ft);b.src=c9(a);return b}var
q=ch(function(a){return ch(P,a)},w),B=[0,0],D=[0,0],E=[0,0],F=[0,0],G=[0,0],S=e(hl),T=[0,e(hm)],k=an(f,fu);bO(k,[0,S]);var
y=q.length-1-1|0,U=0,I=0;if(!(y<0)){var
c=I;for(;;){var
p=k.insertRow(-1);bO(p,U);var
z=h(q,c).length-1-1|0,J=0;if(!(z<0)){var
i=J;for(;;){var
u=p.insertCell(-1);bO(u,T);var
M=h(h(q,c),i);switch(h(h(w,c),i)){case
2:G[1]++;break;case
4:E[1]=i;F[1]=c;break;case
6:B[1]=i;D[1]=c;break}o(u,M);o(p,u);var
N=i+1|0;if(z!==i){var
i=N;continue}break}}o(k,p);var
K=c+1|0;if(y!==c){var
c=K;continue}break}}bL(C,k);function
V(a){var
b=aF(0);function
c(a){var
d=aF(0);if(1<=d-b){var
f=k.style;f.opacity=am(e(hn));return l(0)}function
g(a){var
f=k.style;f.opacity=am(e(j(x(ho),d-b)));return c(0)}return m(a0(b8),g)}function
d(a){j(Q,0);return l(0)}return m(c(0),d)}var
H=[0,0,by(0)];return m(bN([0,w,q,[0,B[1],D[1]],[0,E[1],F[1]],G[1],0,H,0,[0,0]],R,t),V)});var
q=1}else
var
q=0;break}}else
var
q=0;return aY});o(a,b);o(a,cZ(f));o(a,cZ(f));o(a,C);o(k,a);return l(0)}}m(D(hz,function(d){function
e(a){var
f=d.getLen(),b=a;for(;;){if(f<=b)return L(hw);if(34===d.safeGet(b)){var
e=b+1|0,c=b+2|0;for(;;){if(f<=e)return L(hx);if(34===d.safeGet(c))return[0,U(d,e,c-e|0),c+1|0];var
c=c+1|0;continue}}var
b=b+1|0;continue}}var
f=0,a=0;for(;;){try{var
h=e(f),j=h[1],i=e(h[2]),k=[0,[0,[0,j,i[1]],i[2]]],b=k}catch(f){f=z(f);if(f[1]===ar)if(A(f[2],hy))var
c=0;else
var
b=0,c=1;else
var
c=0;if(!c)throw f}if(b){var
g=b[1],f=g[2],a=[0,g[1],a];continue}return l(aj(a))}}),G);return aY});bo(0);return}(this));
