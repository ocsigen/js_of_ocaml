// This program was compiled from OCaml by js_of_ocaml 2.0+git-883a1ec
(function(b){"use strict";var
b3=125,b9=123,V=254,ac=255,dx=108,dm='"',G=16777215,b8="=",dr='Content-Disposition: form-data; name="',ds=250,ce=0.5,dl="jsError",bl="POST",b2=2147483,di=-550809787,aL=115,cb=102,dw="&",b7=120,dh="--",b6=117,bk=126925477,d="",ba=781515420,bi=100,A="0",e=248,ca=103,dv="fd ",dg=936573133,bh=1e3,U="src/core/lwt.ml",am=".",bg=65535,aI="+",aH="g",a$="f",ae=105,dk="%d",dj=443,du=-88,aK=110,a_="?",bc="'",bb="int_of_string",dq=-32,b$=111,F=" ",aJ="e",b_="lastIndex",bj=891486873,df=":",ad="-",al=-48,dp="nan",b1=116,b5="canvas",bf="\r\n",dn="%.12g",cd=" : file already exists",T="/",be=114,bd="#",dt=101,b4=0.1,cc="index out of bounds",u="number";function
dN(a,b){throw[0,a,b]}function
ci(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
c=b.console;c&&c.error&&c.error(a)}var
f=[0];function
aO(a,b){if(!a)return d;if(a&1)return aO(a-1,b)+b;var
c=aO(a>>1,b);return c+c}function
n(a){if(a!=null){this.bytes=this.fullBytes=a;this.last=this.len=a.length}}function
dO(){dN(f[4],new
n(cc))}n.prototype={string:null,bytes:null,fullBytes:null,array:null,len:null,last:0,toJsString:function(){var
a=this.getFullBytes();try{return this.string=decodeURIComponent(escape(a))}catch(f){ci('MlString.toJsString: wrong encoding for "%s" ',a);return a}},toBytes:function(){if(this.string!=null)try{var
a=unescape(encodeURIComponent(this.string))}catch(f){ci('MlString.toBytes: wrong encoding for "%s" ',this.string);var
a=this.string}else{var
a=d,c=this.array,e=c.length;for(var
b=0;b<e;b++)a+=String.fromCharCode(c[b])}this.bytes=this.fullBytes=a;this.last=this.len=a.length;return a},getBytes:function(){var
a=this.bytes;if(a==null)a=this.toBytes();return a},getFullBytes:function(){var
a=this.fullBytes;if(a!==null)return a;a=this.bytes;if(a==null)a=this.toBytes();if(this.last<this.len){this.bytes=a+=aO(this.len-this.last,"\0");this.last=this.len}this.fullBytes=a;return a},toArray:function(){var
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
b=this.bytes;if(b==null)b=this.toBytes();return a<this.last?b.charCodeAt(a):0},safeGet:function(a){if(this.len==null)this.toBytes();if(a<0||a>=this.len)dO();return this.get(a)},set:function(a,b){var
c=this.array;if(!c){if(this.last==a){this.bytes+=String.fromCharCode(b&ac);this.last++;return 0}c=this.toArray()}else
if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;c[a]=b&ac;return 0},safeSet:function(a,b){if(this.len==null)this.toBytes();if(a<0||a>=this.len)dO();this.set(a,b)},fill:function(a,b,c){if(a>=this.last&&this.last&&c==0)return;var
d=this.array;if(!d)d=this.toArray();else
if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;var
f=a+b;for(var
e=a;e<f;e++)d[e]=c},compare:function(a){if(this.string!=null&&a.string!=null){if(this.string<a.string)return-1;if(this.string>a.string)return 1;return 0}var
b=this.getFullBytes(),c=a.getFullBytes();if(b<c)return-1;if(b>c)return 1;return 0},equal:function(a){if(this.string!=null&&a.string!=null)return this.string==a.string;return this.getFullBytes()==a.getFullBytes()},lessThan:function(a){if(this.string!=null&&a.string!=null)return this.string<a.string;return this.getFullBytes()<a.getFullBytes()},lessEqual:function(a){if(this.string!=null&&a.string!=null)return this.string<=a.string;return this.getFullBytes()<=a.getFullBytes()}};function
v(a){this.string=a}v.prototype=new
n();function
ch(a,b){dN(a,new
v(b))}function
ao(a){ch(f[4],a)}function
dA(){ao(cc)}function
g$(a,b){if(b<0||b>=a.length-1)dA();return a[b+1]}function
ha(a,b,c){if(b<0||b>=a.length-1)dA();a[b+1]=c;return 0}function
dB(a,b,c,d,e){if(e===0)return;if(d===c.last&&c.bytes!=null){var
f=a.bytes;if(f==null)f=a.toBytes();if(b>0||a.last>e)f=f.slice(b,b+e);c.bytes+=f;c.last+=f.length;return}var
g=c.array;if(!g)g=c.toArray();else
c.bytes=c.string=null;a.blitToArray(b,g,d,e)}function
W(c,b){if(c.fun)return W(c.fun,b);var
a=c.length,d=a-b.length;if(d==0)return c.apply(null,b);else
if(d<0)return W(c.apply(null,b.slice(0,a)),b.slice(a));else
return function(a){return W(c,b.concat([a]))}}function
hb(a){if(isFinite(a)){if(Math.abs(a)>=2.22507385850720138e-308)return 0;if(a!=0)return 1;return 2}return isNaN(a)?4:3}function
hc(){return 0}function
dy(a){this.bytes=d;this.len=a}dy.prototype=new
n();function
dD(a){if(a<0)ao("String.create");return new
dy(a)}function
hl(a,b){var
c=a[3]<<16,d=b[3]<<16;if(c>d)return 1;if(c<d)return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
hv(a,b){if(a<b)return-1;if(a==b)return 0;return 1}function
dC(a,b,c){var
f=[];for(;;){if(!(c&&a===b))if(a
instanceof
n)if(b
instanceof
n){if(a!==b){var
d=a.compare(b);if(d!=0)return d}}else
return 1;else
if(a
instanceof
Array&&a[0]===(a[0]|0)){var
g=a[0];if(g===V)g=0;if(g===ds){a=a[1];continue}else
if(b
instanceof
Array&&b[0]===(b[0]|0)){var
h=b[0];if(h===V)h=0;if(h===ds){b=b[1];continue}else
if(g!=h)return g<h?-1:1;else
switch(g){case
e:var
d=hv(a[2],b[2]);if(d!=0)return d;break;case
251:ao("equal: abstract value");case
ac:var
d=hl(a,b);if(d!=0)return d;break;default:if(a.length!=b.length)return a.length<b.length?-1:1;if(a.length>1)f.push(a,b,1)}}else
return 1}else
if(b
instanceof
n||b
instanceof
Array&&b[0]===(b[0]|0))return-1;else
if(typeof
a!=u&&a&&a.compare)return a.compare(b,c);else{if(a<b)return-1;if(a>b)return 1;if(a!=b){if(!c)return NaN;if(a==a)return 1;if(b==b)return-1}}if(f.length==0)return 0;var
i=f.pop();b=f.pop();a=f.pop();if(i+1<a.length)f.push(a,b,i+1);a=a[i];b=b[i]}}function
he(a,b){return+(dC(a,b,false)==0)}function
hf(a,b,c,d){a.fill(b,c,d)}function
aM(a){ch(f[3],a)}function
hg(a){var
b;a=a.getFullBytes();b=+a;if(a.length>0&&b===b)return b;a=a.replace(/_/g,d);b=+a;if(a.length>0&&b===b||/^[+-]?nan$/i.test(a))return b;if(/^ *0x[0-9a-f_]+p[+-]?[0-9_]+/i.test(a)){var
c=a.indexOf("p");c=c==-1?a.indexOf("P"):c;var
e=+a.substring(c+1);b=+a.substring(0,c);return b*Math.pow(2,e)}aM("float_of_string")}function
cg(a){a=a.toString();var
e=a.length;if(e>31)ao("format_int: format too long");var
b={justify:aI,signstyle:ad,filler:F,alternate:false,base:0,signedconv:false,width:0,uppercase:false,sign:1,prec:-1,conv:a$};for(var
d=0;d<e;d++){var
c=a.charAt(d);switch(c){case
ad:b.justify=ad;break;case
aI:case
F:b.signstyle=c;break;case
A:b.filler=A;break;case
bd:b.alternate=true;break;case"1":case"2":case"3":case"4":case"5":case"6":case"7":case"8":case"9":b.width=0;while(c=a.charCodeAt(d)-48,c>=0&&c<=9){b.width=b.width*10+c;d++}d--;break;case
am:b.prec=0;d++;while(c=a.charCodeAt(d)-48,c>=0&&c<=9){b.prec=b.prec*10+c;d++}d--;case"d":case"i":b.signedconv=true;case"u":b.base=10;break;case"x":b.base=16;break;case"X":b.base=16;b.uppercase=true;break;case"o":b.base=8;break;case
aJ:case
a$:case
aH:b.signedconv=true;b.conv=c;break;case"E":case"F":case"G":b.signedconv=true;b.uppercase=true;b.conv=c.toLowerCase();break}}return b}function
cf(a,b){if(a.uppercase)b=b.toUpperCase();var
f=b.length;if(a.signedconv&&(a.sign<0||a.signstyle!=ad))f++;if(a.alternate){if(a.base==8)f+=1;if(a.base==16)f+=2}var
c=d;if(a.justify==aI&&a.filler==F)for(var
e=f;e<a.width;e++)c+=F;if(a.signedconv)if(a.sign<0)c+=ad;else
if(a.signstyle!=ad)c+=a.signstyle;if(a.alternate&&a.base==8)c+=A;if(a.alternate&&a.base==16)c+="0x";if(a.justify==aI&&a.filler==A)for(var
e=f;e<a.width;e++)c+=A;c+=b;if(a.justify==ad)for(var
e=f;e<a.width;e++)c+=F;return new
v(c)}function
hh(a,b){var
c,f=cg(a),e=f.prec<0?6:f.prec;if(b<0){f.sign=-1;b=-b}if(isNaN(b)){c=dp;f.filler=F}else
if(!isFinite(b)){c="inf";f.filler=F}else
switch(f.conv){case
aJ:var
c=b.toExponential(e),d=c.length;if(c.charAt(d-3)==aJ)c=c.slice(0,d-1)+A+c.slice(d-1);break;case
a$:c=b.toFixed(e);break;case
aH:e=e?e:1;c=b.toExponential(e-1);var
i=c.indexOf(aJ),h=+c.slice(i+1);if(h<-4||b.toFixed(0).length>e){var
d=i-1;while(c.charAt(d)==A)d--;if(c.charAt(d)==am)d--;c=c.slice(0,d+1)+c.slice(i);d=c.length;if(c.charAt(d-3)==aJ)c=c.slice(0,d-1)+A+c.slice(d-1);break}else{var
g=e;if(h<0){g-=h+1;c=b.toFixed(g)}else
while(c=b.toFixed(g),c.length>e+1)g--;if(g){var
d=c.length-1;while(c.charAt(d)==A)d--;if(c.charAt(d)==am)d--;c=c.slice(0,d+1)}}break}return cf(f,c)}function
hi(a,b){if(a.toString()==dk)return new
v(d+b);var
c=cg(a);if(b<0)if(c.signedconv){c.sign=-1;b=-b}else
b>>>=0;var
e=b.toString(c.base);if(c.prec>=0){c.filler=F;var
f=c.prec-e.length;if(f>0)e=aO(f,A)+e}return cf(c,e)}function
hP(a){throw a}function
hQ(){hP(f[7])}function
dz(a){var
b=a.length;this.array=a;this.len=this.last=b}dz.prototype=new
n();function
af(){this.content={}}af.prototype={exists:function(a){return this.content[a]?1:0},mk:function(a,b){this.content[a]=b},get:function(a){return this.content[a]},list:function(){var
a=[];for(var
b
in
this.content)a.push(b);return a},remove:function(a){delete
this.content[a]}};var
bo=new
af();bo.mk(d,new
af());function
B(a){ch(f[2],a)}function
dM(a){a=a
instanceof
n?a.toString():a;B(a+": No such file or directory")}function
bm(a){var
b=bo;for(var
c=0;c<a.length;c++){if(!(b.exists&&b.exists(a[c])))dM(a.orig);b=b.get(a[c])}return b}var
hd=T;function
aN(a){a=a
instanceof
n?a.toString():a;if(a.charCodeAt(0)!=47)a=hd+a;var
e=a.split(T),b=[];for(var
c=0;c<e.length;c++)switch(e[c]){case"..":if(b.length>1)b.pop();break;case
am:case
d:if(b.length==0)b.push(d);break;default:b.push(e[c]);break}b.orig=a;return b}function
an(a){this.data=a}an.prototype={content:function(){return this.data},truncate:function(){this.data.length=0}};function
hj(a){var
c=aN(a),b=bm(c);if(b
instanceof
an)return new
dz(b.content());hQ()}function
dE(a,b){var
e=aN(a),c=bo;for(var
f=0;f<e.length-1;f++){var
d=e[f];if(!c.exists(d))c.mk(d,new
af());c=c.get(d);if(!(c
instanceof
af))B(e.orig+cd)}var
d=e[e.length-1];if(c.exists(d))B(e.orig+cd);if(b
instanceof
af)c.mk(d,b);else
if(b
instanceof
an)c.mk(d,b);else
if(b
instanceof
n)c.mk(d,new
an(b.getArray()));else
if(b
instanceof
Array)c.mk(d,new
an(b));else
if(b.toString)c.mk(d,new
an(new
n(b.toString()).getArray()));else
ao("caml_fs_register")}function
hk(){return 0}function
ho(a){return(a[3]|a[2]|a[1])==0}function
hr(a){return[ac,a&G,a>>24&G,a>>31&bg]}function
hs(a,b){var
c=a[1]-b[1],d=a[2]-b[2]+(c>>24),e=a[3]-b[3]+(d>>24);return[ac,c&G,d&G,e&bg]}function
dG(a,b){if(a[3]>b[3])return 1;if(a[3]<b[3])return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
dF(a){a[3]=a[3]<<1|a[2]>>23;a[2]=(a[2]<<1|a[1]>>23)&G;a[1]=a[1]<<1&G}function
hp(a){a[1]=(a[1]>>>1|a[2]<<23)&G;a[2]=(a[2]>>>1|a[3]<<23)&G;a[3]=a[3]>>>1}function
hu(a,b){var
e=0,d=a.slice(),c=b.slice(),f=[ac,0,0,0];while(dG(d,c)>0){e++;dF(c)}while(e>=0){e--;dF(f);if(dG(d,c)>=0){f[1]++;d=hs(d,c)}hp(c)}return[0,f,d]}function
ht(a){return a[1]|a[2]<<24}function
hn(a){return a[3]<<16<0}function
hq(a){var
b=-a[1],c=-a[2]+(b>>24),d=-a[3]+(c>>24);return[ac,b&G,c&G,d&bg]}function
hm(a,b){var
c=cg(a);if(c.signedconv&&hn(b)){c.sign=-1;b=hq(b)}var
e=d,i=hr(c.base),h="0123456789abcdef";do{var
g=hu(b,i);b=g[1];e=h.charAt(ht(g[2]))+e}while(!ho(b));if(c.prec>=0){c.filler=F;var
f=c.prec-e.length;if(f>0)e=aO(f,A)+e}return cf(c,e)}function
hO(a){var
b=0,c=10,d=a.get(0)==45?(b++,-1):1;if(a.get(b)==48)switch(a.get(b+1)){case
b7:case
88:c=16;b+=2;break;case
b$:case
79:c=8;b+=2;break;case
98:case
66:c=2;b+=2;break}return[b,d,c]}function
dL(a){if(a>=48&&a<=57)return a-48;if(a>=65&&a<=90)return a-55;if(a>=97&&a<=122)return a-87;return-1}function
hw(a){var
g=hO(a),f=g[0],h=g[1],d=g[2],i=-1>>>0,e=a.get(f),c=dL(e);if(c<0||c>=d)aM(bb);var
b=c;for(;;){f++;e=a.get(f);if(e==95)continue;c=dL(e);if(c<0||c>=d)break;b=d*b+c;if(b>i)aM(bb)}if(f!=a.getLen())aM(bb);b=h*b;if(d==10&&(b|0)!=b)aM(bb);return b|0}function
hx(a){return+(a>31&&a<127)}function
hy(a){return a.getFullBytes()}function
hz(){var
c=b.console?b.console:{},d=["log","debug","info","warn","error","assert","dir","dirxml","trace","group","groupCollapsed","groupEnd","time","timeEnd"];function
e(){}for(var
a=0;a<d.length;a++)if(!c[d[a]])c[d[a]]=e;return c}function
hA(){var
a=b.navigator?b.navigator.userAgent:d;return a.indexOf("MSIE")!=-1&&a.indexOf("Opera")!=0}function
hB(a){return new
n(a)}function
hC(a){var
c=Array.prototype.slice;return function(){var
b=arguments.length>0?c.call(arguments):[undefined];return W(a,b)}}function
hD(a,b){var
d=[0];for(var
c=1;c<=a;c++)d[c]=b;return d}function
dH(a){if(!a.opened)B("Cannot flush a closed channel");if(a.buffer==d)return 0;if(a.output)switch(a.output.length){case
2:a.output(a,a.buffer);break;default:a.output(a.buffer)}a.buffer=d}function
h0(a){var
c=aN(a),b=bm(c);return b
instanceof
af?1:0}function
hZ(a){var
b=bo,d=aN(a),e;for(var
c=0;c<d.length;c++){if(b.auto)e=b.auto;if(!(b.exists&&b.exists(d[c])))return e?e(d.join(T)):0;b=b.get(d[c])}return 1}function
aP(a,b,c){if(f.fds===undefined)f.fds=new
Array();c=c?c:{};var
d={};d.array=b;d.offset=c.append?d.array.length:0;d.flags=c;f.fds[a]=d;f.fd_last_idx=a;return a}function
h8(a,b,c){var
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
g=a.toString(),i=aN(a);if(d.rdonly&&d.wronly)B(g+" : flags Open_rdonly and Open_wronly are not compatible");if(d.text&&d.binary)B(g+" : flags Open_text and Open_binary are not compatible");if(hZ(a)){if(h0(a))B(g+" : is a directory");if(d.create&&d.excl)B(g+cd);var
h=f.fd_last_idx?f.fd_last_idx:0,e=bm(i);if(d.truncate)e.truncate();return aP(h+1,e.content(),d)}else
if(d.create){var
h=f.fd_last_idx?f.fd_last_idx:0;dE(a,[]);var
e=bm(i);return aP(h+1,e.content(),d)}else
dM(a)}aP(0,[]);aP(1,[]);aP(2,[]);function
hE(a){var
b=f.fds[a];if(b.flags.wronly)B(dv+a+" is writeonly");return{data:b,fd:a,opened:true}}function
h5(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
c=b.console;c&&c.log&&c.log(a)}var
bn=new
Array();function
hU(a,b){var
e=new
n(b),d=e.getLen();for(var
c=0;c<d;c++)a.data.array[a.data.offset+c]=e.get(c);a.data.offset+=d;return 0}function
hF(a){var
b;switch(a){case
1:b=h5;break;case
2:b=ci;break;default:b=hU}var
e=f.fds[a];if(e.flags.rdonly)B(dv+a+" is readonly");var
c={data:e,fd:a,opened:true,buffer:d,output:b};bn[c.fd]=c;return c}function
hG(){var
a=0;for(var
b
in
bn)if(bn[b].opened)a=[0,bn[b],a];return a}function
dI(a,b,c,d){if(!a.opened)B("Cannot output to a closed channel");var
f;if(c==0&&b.getLen()==d)f=b;else{f=dD(d);dB(b,c,f,0,d)}var
e=f.toString(),g=e.lastIndexOf("\n");if(g<0)a.buffer+=e;else{a.buffer+=e.substr(0,g+1);dH(a);a.buffer+=e.substr(g+1)}}function
dK(a){return new
n(a)}function
hH(a,b){var
c=dK(String.fromCharCode(b));dI(a,c,0,1)}if(!Math.imul)Math.imul=function(a,b){return((a>>16)*b<<16)+(a&bg)*b|0};var
hI=Math.imul;function
hK(a,b){return+(dC(a,b,false)!=0)}function
hL(a){return+(a
instanceof
Array)}function
hM(a){return a
instanceof
Array?a[0]:bh}function
hR(a,b){f[a+1]=b}var
dJ={};function
hS(a,b){dJ[a.toString()]=b;return 0}var
hN=0;function
hT(a){a[2]=hN++;return a}function
hV(a,b){var
c=a.fullBytes,d=b.fullBytes;if(c!=null&&d!=null)return c==d?1:0;return a.getFullBytes()==b.getFullBytes()?1:0}function
hW(a,b){return 1-hV(a,b)}function
hX(){return 32}function
hY(a){if(b.quit)b.quit(a);ao("Function 'exit' not implemented")}function
h1(a){var
b=1;while(a&&a.joo_tramp){a=a.joo_tramp.apply(null,a.joo_args);b++}return a}function
h2(a,b){return{joo_tramp:a,joo_args:b}}function
h3(a,b){if(typeof
b==="function"){a.fun=b;return 0}if(b.fun){a.fun=b.fun;return 0}var
c=b.length;while(c--)a[c]=b[c];return 0}function
hJ(a){return dJ[a]}function
h4(a){if(a
instanceof
Array)return a;if(b.RangeError&&a
instanceof
b.RangeError&&a.message&&a.message.match(/maximum call stack/i))return[0,f[9]];if(b.InternalError&&a
instanceof
b.InternalError&&a.message&&a.message.match(/too much recursion/i))return[0,f[9]];if(a
instanceof
b.Error)return[0,hJ(dl),a];return[0,f[3],new
v(String(a))]}var
k=g$,j=ha,S=dB,M=dD,ak=hg,bX=hh,a4=hi,a5=hw,bY=hx,g=hy,E=hB,a9=hC,N=hD,da=dH,c$=hF,db=hH,dc=hI,c=dK,dd=hK,a6=hM,L=hR,bW=hS,a=hT,t=hW,a8=h1,P=h2,de=h3,z=h4;function
i(a,b){return a.length==1?a(b):W(a,[b])}function
l(a,b,c){return a.length==2?a(b,c):W(a,[b,c])}function
s(a,b,c,d){return a.length==3?a(b,c,d):W(a,[b,c,d])}function
a7(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):W(a,[b,c,d,e,f])}dE("/monkey.model","# Blender v2.61 (sub 0) OBJ File: ''\n# www.blender.org\no Monkey\nv 0.436361 0.667417 -0.179002\nv -0.438639 0.667417 -0.179002\nv 0.498861 0.589292 -0.108690\nv -0.501139 0.589292 -0.108690\nv 0.545736 0.479917 -0.069627\nv -0.548014 0.479917 -0.069627\nv 0.350423 0.518980 0.008498\nv -0.352702 0.518980 0.008498\nv 0.350423 0.620542 -0.046190\nv -0.352702 0.620542 -0.046190\nv 0.350423 0.683042 -0.147752\nv -0.352702 0.683042 -0.147752\nv 0.272298 0.698667 -0.179002\nv -0.274577 0.698667 -0.179002\nv 0.201986 0.643980 -0.108690\nv -0.204264 0.643980 -0.108690\nv 0.155111 0.550230 -0.069627\nv -0.157389 0.550230 -0.069627\nv 0.076986 0.558042 -0.257127\nv -0.079264 0.558042 -0.257127\nv 0.139486 0.643980 -0.257127\nv -0.141764 0.643980 -0.257127\nv 0.241048 0.698667 -0.257127\nv -0.243327 0.698667 -0.257127\nv 0.272298 0.698667 -0.343065\nv -0.274577 0.698667 -0.343065\nv 0.201986 0.643980 -0.405565\nv -0.204264 0.643980 -0.405565\nv 0.155111 0.550230 -0.452440\nv -0.157389 0.550230 -0.452440\nv 0.350423 0.518980 -0.530565\nv -0.352702 0.518980 -0.530565\nv 0.350423 0.620542 -0.468065\nv -0.352702 0.620542 -0.468065\nv 0.350423 0.683042 -0.374315\nv -0.352702 0.683042 -0.374315\nv 0.436361 0.667417 -0.343065\nv -0.438639 0.667417 -0.343065\nv 0.498861 0.589292 -0.405565\nv -0.501139 0.589292 -0.405565\nv 0.545736 0.479917 -0.452440\nv -0.548014 0.479917 -0.452440\nv 0.623861 0.464292 -0.257127\nv -0.626139 0.464292 -0.257127\nv 0.561361 0.573667 -0.257127\nv -0.563639 0.573667 -0.257127\nv 0.467611 0.659605 -0.257127\nv -0.469889 0.659605 -0.257127\nv 0.475423 0.675230 -0.257127\nv -0.477702 0.675230 -0.257127\nv 0.444173 0.683042 -0.350877\nv -0.446452 0.683042 -0.350877\nv 0.350423 0.706480 -0.389940\nv -0.352702 0.706480 -0.389940\nv 0.264486 0.722105 -0.350877\nv -0.266764 0.722105 -0.350877\nv 0.225423 0.722105 -0.257127\nv -0.227702 0.722105 -0.257127\nv 0.264486 0.722105 -0.171190\nv -0.266764 0.722105 -0.171190\nv 0.350423 0.729917 -0.257127\nv -0.352702 0.729917 -0.257127\nv 0.350423 0.706480 -0.132127\nv -0.352702 0.706480 -0.132127\nv 0.444173 0.683042 -0.171190\nv -0.446452 0.683042 -0.171190\nv -0.001139 0.643980 -0.444627\nv -0.001139 0.722105 -0.366502\nv -0.001139 0.636167 0.664748\nv -0.001139 0.683042 0.305373\nv -0.001139 0.698667 0.172560\nv -0.001139 0.620542 0.758498\nv -0.001139 0.503355 -0.421190\nv -0.001139 0.472105 -0.585252\nv -0.001139 -0.645083 -0.913377\nv -0.001139 -0.949770 -0.577440\nv -0.001139 -0.926333 -0.085252\nv -0.001139 -0.449770 0.367873\nv 0.201986 0.464292 0.172560\nv -0.204264 0.464292 0.172560\nv 0.311361 0.472105 0.422560\nv -0.313639 0.472105 0.422560\nv 0.350423 0.472105 0.680373\nv -0.352702 0.472105 0.680373\nv 0.366048 0.433042 0.875685\nv -0.368327 0.433042 0.875685\nv 0.326986 0.425230 0.930373\nv -0.329264 0.425230 0.930373\nv 0.178548 0.456480 0.953810\nv -0.180827 0.456480 0.953810\nv -0.001139 0.479917 0.969435\nv 0.436361 0.433042 0.125685\nv -0.438639 0.433042 0.125685\nv 0.631673 0.440855 0.024123\nv -0.633952 0.440855 0.024123\nv 0.826986 0.347105 -0.163377\nv -0.829264 0.347105 -0.163377\nv 0.858236 0.495542 -0.444627\nv -0.860514 0.495542 -0.444627\nv 0.709798 0.526792 -0.499315\nv -0.712077 0.526792 -0.499315\nv 0.491048 0.589292 -0.616502\nv -0.493327 0.589292 -0.616502\nv 0.319173 0.636167 -0.772752\nv -0.321452 0.636167 -0.772752\nv 0.155111 0.659605 -0.733690\nv -0.157389 0.659605 -0.733690\nv 0.061361 0.651792 -0.507127\nv -0.063639 0.651792 -0.507127\nv 0.162923 0.675230 -0.429002\nv -0.165202 0.675230 -0.429002\nv 0.123861 0.667417 -0.319627\nv -0.126139 0.667417 -0.319627\nv 0.201986 0.643980 -0.108690\nv -0.204264 0.643980 -0.108690\nv 0.373861 0.604917 -0.030565\nv -0.376139 0.604917 -0.030565\nv 0.491048 0.573667 -0.077440\nv -0.493327 0.573667 -0.077440\nv 0.623861 0.550230 -0.202440\nv -0.626139 0.550230 -0.202440\nv 0.639486 0.550230 -0.311815\nv -0.641764 0.550230 -0.311815\nv 0.600423 0.565855 -0.389940\nv -0.602702 0.565855 -0.389940\nv 0.428548 0.620542 -0.452440\nv -0.430827 0.620542 -0.452440\nv 0.248861 0.659605 -0.483690\nv -0.251139 0.659605 -0.483690\nv -0.001139 0.636167 0.750685\nv 0.108236 0.636167 0.703810\nv -0.110514 0.636167 0.703810\nv 0.116048 0.612730 0.820998\nv -0.118327 0.612730 0.820998\nv 0.061361 0.597105 0.867873\nv -0.063639 0.597105 0.867873\nv -0.001139 0.589292 0.875685\nv -0.001139 0.651792 0.180373\nv -0.001139 0.643980 0.125685\nv 0.100423 0.643980 0.133498\nv -0.102702 0.643980 0.133498\nv 0.123861 0.651792 0.211623\nv -0.126139 0.651792 0.211623\nv 0.084798 0.643980 0.274123\nv -0.087077 0.643980 0.274123\nv 0.397298 0.573667 0.031935\nv -0.399577 0.573667 0.031935\nv 0.616048 0.526792 -0.069627\nv -0.618327 0.526792 -0.069627\nv 0.725423 0.503355 -0.218065\nv -0.727702 0.503355 -0.218065\nv 0.741048 0.558042 -0.389940\nv -0.743327 0.558042 -0.389940\nv 0.686361 0.628355 -0.429002\nv -0.688639 0.628355 -0.429002\nv 0.436361 0.698667 -0.561815\nv -0.438639 0.698667 -0.561815\nv 0.311361 0.737730 -0.655565\nv -0.313639 0.737730 -0.655565\nv 0.201986 0.753355 -0.632127\nv -0.204264 0.753355 -0.632127\nv 0.100423 0.745542 -0.444627\nv -0.102702 0.745542 -0.444627\nv 0.123861 0.714292 0.086623\nv -0.126139 0.714292 0.086623\nv 0.209798 0.612730 0.430373\nv -0.212077 0.612730 0.430373\nv 0.248861 0.589292 0.688185\nv -0.251139 0.589292 0.688185\nv 0.264486 0.565855 0.805373\nv -0.266764 0.565855 0.805373\nv 0.233236 0.534605 0.899123\nv -0.235514 0.534605 0.899123\nv 0.162923 0.534605 0.914748\nv -0.165202 0.534605 0.914748\nv -0.001139 0.542417 0.930373\nv -0.001139 0.628355 -0.061815\nv -0.001139 0.667417 -0.225877\nv 0.326986 0.643980 -0.491502\nv -0.329264 0.643980 -0.491502\nv 0.162923 0.651792 -0.155565\nv -0.165202 0.651792 -0.155565\nv 0.131673 0.659605 -0.225877\nv -0.133952 0.659605 -0.225877\nv 0.116048 0.636167 0.672560\nv -0.118327 0.636167 0.672560\nv 0.076986 0.651792 0.430373\nv -0.079264 0.651792 0.430373\nv -0.001139 0.651792 0.430373\nv -0.001139 0.643980 0.313185\nv 0.092611 0.683042 0.258498\nv -0.094889 0.683042 0.258498\nv 0.131673 0.698667 0.211623\nv -0.133952 0.698667 0.211623\nv 0.108236 0.683042 0.117873\nv -0.110514 0.683042 0.117873\nv 0.037923 0.683042 0.110060\nv -0.040202 0.683042 0.110060\nv -0.001139 0.729917 0.188185\nv 0.045736 0.714292 0.133498\nv -0.048014 0.714292 0.133498\nv 0.092611 0.714292 0.141310\nv -0.094889 0.714292 0.141310\nv 0.108236 0.729917 0.211623\nv -0.110514 0.729917 0.211623\nv 0.076986 0.706480 0.235060\nv -0.079264 0.706480 0.235060\nv -0.001139 0.706480 0.274123\nv 0.256673 0.456480 0.297560\nv -0.258952 0.456480 0.297560\nv 0.162923 0.612730 0.227248\nv -0.165202 0.612730 0.227248\nv 0.178548 0.612730 0.297560\nv -0.180827 0.612730 0.297560\nv 0.233236 0.456480 0.235060\nv -0.235514 0.456480 0.235060\nv -0.001139 0.589292 0.860060\nv 0.045736 0.589292 0.852248\nv -0.048014 0.589292 0.852248\nv 0.092611 0.612730 0.805373\nv -0.094889 0.612730 0.805373\nv 0.092611 0.628355 0.727248\nv -0.094889 0.628355 0.727248\nv -0.001139 0.558042 0.766310\nv 0.092611 0.565855 0.735060\nv -0.094889 0.565855 0.735060\nv 0.092611 0.542417 0.797560\nv -0.094889 0.542417 0.797560\nv 0.045736 0.534605 0.836623\nv -0.048014 0.534605 0.836623\nv -0.001139 0.534605 0.844435\nv 0.170736 0.683042 -0.233690\nv -0.173014 0.683042 -0.233690\nv 0.186361 0.675230 -0.171190\nv -0.188639 0.675230 -0.171190\nv 0.334798 0.659605 -0.444627\nv -0.337077 0.659605 -0.444627\nv 0.272298 0.675230 -0.436815\nv -0.274577 0.675230 -0.436815\nv 0.420736 0.675230 -0.413377\nv -0.423014 0.675230 -0.413377\nv 0.561361 0.597105 -0.366502\nv -0.563639 0.597105 -0.366502\nv 0.584798 0.589292 -0.304002\nv -0.587077 0.589292 -0.304002\nv 0.576986 0.581480 -0.210252\nv -0.579264 0.581480 -0.210252\nv 0.475423 0.620542 -0.116502\nv -0.477702 0.620542 -0.116502\nv 0.373861 0.643980 -0.077440\nv -0.376139 0.643980 -0.077440\nv 0.225423 0.683042 -0.124315\nv -0.227702 0.683042 -0.124315\nv 0.178548 0.683042 -0.311815\nv -0.180827 0.683042 -0.311815\nv 0.209798 0.683042 -0.389940\nv -0.212077 0.683042 -0.389940\nv 0.233236 0.659605 -0.374315\nv -0.235514 0.659605 -0.374315\nv 0.194173 0.659605 -0.311815\nv -0.196452 0.659605 -0.311815\nv 0.241048 0.659605 -0.139940\nv -0.243327 0.659605 -0.139940\nv 0.373861 0.628355 -0.100877\nv -0.376139 0.628355 -0.100877\nv 0.459798 0.604917 -0.132127\nv -0.462077 0.604917 -0.132127\nv 0.545736 0.573667 -0.225877\nv -0.548014 0.573667 -0.225877\nv 0.553548 0.573667 -0.296190\nv -0.555827 0.573667 -0.296190\nv 0.530111 0.581480 -0.350877\nv -0.532389 0.581480 -0.350877\nv 0.412923 0.651792 -0.405565\nv -0.415202 0.651792 -0.405565\nv 0.280111 0.667417 -0.413377\nv -0.282389 0.667417 -0.413377\nv 0.334798 0.651792 -0.421190\nv -0.337077 0.651792 -0.421190\nv 0.201986 0.651792 -0.186815\nv -0.204264 0.651792 -0.186815\nv 0.194173 0.651792 -0.241502\nv -0.196452 0.651792 -0.241502\nv 0.108236 0.511167 -0.475877\nv -0.110514 0.511167 -0.475877\nv 0.194173 0.518980 -0.679002\nv -0.196452 0.518980 -0.679002\nv 0.334798 0.495542 -0.702440\nv -0.337077 0.495542 -0.702440\nv 0.483236 0.456480 -0.569627\nv -0.485514 0.456480 -0.569627\nv 0.678548 0.393980 -0.468065\nv -0.680827 0.393980 -0.468065\nv 0.795736 0.362730 -0.421190\nv -0.798014 0.362730 -0.421190\nv 0.772298 0.276792 -0.179002\nv -0.774577 0.276792 -0.179002\nv 0.600423 0.315855 -0.014940\nv -0.602702 0.315855 -0.014940\nv 0.436361 0.370542 0.078810\nv -0.438639 0.370542 0.078810\nv -0.001139 0.190855 -0.913377\nv -0.001139 -0.176333 -0.999315\nv -0.001139 -0.770083 0.180373\nv -0.001139 0.089292 0.445998\nv -0.001139 0.362730 0.961623\nv -0.001139 0.245542 0.789748\nv -0.001139 0.222105 0.555373\nv -0.001139 0.183042 0.469435\nv 0.850423 -0.043520 -0.249315\nv -0.852702 -0.043520 -0.249315\nv 0.858236 -0.145083 -0.335252\nv -0.860514 -0.145083 -0.335252\nv 0.772298 -0.535708 -0.280565\nv -0.774577 -0.535708 -0.280565\nv 0.459798 -0.801333 -0.452440\nv -0.462077 -0.801333 -0.452440\nv 0.733236 -0.027895 0.031935\nv -0.735514 -0.027895 0.031935\nv 0.592611 -0.262270 0.110060\nv -0.594889 -0.262270 0.110060\nv 0.639486 -0.527895 -0.007127\nv -0.641764 -0.527895 -0.007127\nv 0.334798 -0.762270 -0.069627\nv -0.337077 -0.762270 -0.069627\nv 0.233236 0.308042 0.336623\nv -0.235514 0.308042 0.336623\nv 0.178548 0.159605 0.399123\nv -0.180827 0.159605 0.399123\nv 0.287923 0.284605 0.695998\nv -0.290202 0.284605 0.695998\nv 0.248861 0.292417 0.485060\nv -0.251139 0.292417 0.485060\nv 0.326986 0.300230 0.899123\nv -0.329264 0.300230 0.899123\nv 0.139486 0.268980 0.742873\nv -0.141764 0.268980 0.742873\nv 0.123861 0.261167 0.524123\nv -0.126139 0.261167 0.524123\nv 0.162923 0.339292 0.930373\nv -0.165202 0.339292 0.930373\nv 0.217611 0.331480 0.266310\nv -0.219889 0.331480 0.266310\nv 0.209798 0.370542 0.211623\nv -0.212077 0.370542 0.211623\nv 0.201986 0.401792 0.156935\nv -0.204264 0.401792 0.156935\nv 0.209798 0.065855 0.375685\nv -0.212077 0.065855 0.375685\nv 0.295736 -0.363833 0.297560\nv -0.298014 -0.363833 0.297560\nv 0.342611 -0.637270 0.133498\nv -0.344889 -0.637270 0.133498\nv 0.451986 -0.481020 -0.882127\nv -0.454264 -0.481020 -0.882127\nv 0.451986 -0.168520 -0.944627\nv -0.454264 -0.168520 -0.944627\nv 0.451986 0.136167 -0.866502\nv -0.454264 0.136167 -0.866502\nv 0.459798 0.331480 -0.538377\nv -0.462077 0.331480 -0.538377\nv 0.725423 0.237730 -0.421190\nv -0.727702 0.237730 -0.421190\nv 0.631673 0.183042 -0.468065\nv -0.633952 0.183042 -0.468065\nv 0.639486 -0.043520 -0.718065\nv -0.641764 -0.043520 -0.718065\nv 0.795736 0.026792 -0.577440\nv -0.798014 0.026792 -0.577440\nv 0.795736 -0.215395 -0.632127\nv -0.798014 -0.215395 -0.632127\nv 0.639486 -0.293520 -0.764940\nv -0.641764 -0.293520 -0.764940\nv 0.639486 -0.543520 -0.694627\nv -0.641764 -0.543520 -0.694627\nv 0.795736 -0.457583 -0.554002\nv -0.798014 -0.457583 -0.554002\nv 0.616048 -0.684145 -0.343065\nv -0.618327 -0.684145 -0.343065\nv 0.483236 -0.645083 -0.038377\nv -0.485514 -0.645083 -0.038377\nv 0.819173 -0.301333 -0.343065\nv -0.821452 -0.301333 -0.343065\nv 0.405111 0.050230 0.156935\nv -0.407389 0.050230 0.156935\nv 0.428548 -0.309145 0.180373\nv -0.430827 -0.309145 0.180373\nv 0.889486 -0.332583 -0.421190\nv -0.891764 -0.332583 -0.421190\nv 0.772298 -0.223208 0.125685\nv -0.774577 -0.223208 0.125685\nv 1.037923 -0.426333 0.086623\nv -1.040202 -0.426333 0.086623\nv 1.280111 -0.527895 -0.069627\nv -1.282389 -0.527895 -0.069627\nv 1.350423 -0.520083 -0.335252\nv -1.352702 -0.520083 -0.335252\nv 1.233236 -0.520083 -0.522752\nv -1.235514 -0.520083 -0.522752\nv 1.022298 -0.410708 -0.491502\nv -1.024577 -0.410708 -0.491502\nv 1.014486 -0.387270 -0.429002\nv -1.016764 -0.387270 -0.429002\nv 1.186361 -0.488833 -0.452440\nv -1.188639 -0.488833 -0.452440\nv 1.264486 -0.504458 -0.304002\nv -1.266764 -0.504458 -0.304002\nv 1.209798 -0.504458 -0.093065\nv -1.212077 -0.504458 -0.093065\nv 1.030111 -0.402895 0.024123\nv -1.032389 -0.402895 0.024123\nv 0.826986 -0.231020 0.055373\nv -0.829264 -0.231020 0.055373\nv 0.920736 -0.316958 -0.374315\nv -0.923014 -0.316958 -0.374315\nv 0.944173 -0.387270 -0.319627\nv -0.946452 -0.387270 -0.319627\nv 0.881673 -0.309145 0.008498\nv -0.883952 -0.309145 0.008498\nv 1.037923 -0.465395 -0.014940\nv -1.040202 -0.465395 -0.014940\nv 1.186361 -0.543520 -0.108690\nv -1.188639 -0.543520 -0.108690\nv 1.233236 -0.543520 -0.264940\nv -1.235514 -0.543520 -0.264940\nv 1.170736 -0.535708 -0.374315\nv -1.173014 -0.535708 -0.374315\nv 1.022298 -0.457583 -0.358690\nv -1.024577 -0.457583 -0.358690\nv 0.842611 -0.309145 -0.304002\nv -0.844889 -0.309145 -0.304002\nv 0.834798 -0.371645 -0.186815\nv -0.837077 -0.371645 -0.186815\nv 0.756673 -0.371645 -0.108690\nv -0.758952 -0.371645 -0.108690\nv 0.819173 -0.371645 -0.100877\nv -0.821452 -0.371645 -0.100877\nv 0.842611 -0.371645 -0.030565\nv -0.844889 -0.371645 -0.030565\nv 0.811361 -0.371645 0.000685\nv -0.813639 -0.371645 0.000685\nv 0.725423 -0.168520 -0.014940\nv -0.727702 -0.168520 -0.014940\nv 0.717611 -0.270083 0.008498\nv -0.719889 -0.270083 0.008498\nv 0.717611 -0.285708 -0.054002\nv -0.719889 -0.285708 -0.054002\nv 0.795736 -0.309145 -0.218065\nv -0.798014 -0.309145 -0.218065\nv 0.889486 -0.363833 -0.257127\nv -0.891764 -0.363833 -0.257127\nv 0.889486 -0.418520 -0.249315\nv -0.891764 -0.418520 -0.249315\nv 0.811361 -0.418520 0.000685\nv -0.813639 -0.418520 0.000685\nv 0.850423 -0.418520 -0.030565\nv -0.852702 -0.418520 -0.030565\nv 0.826986 -0.418520 -0.093065\nv -0.829264 -0.418520 -0.093065\nv 0.764486 -0.418520 -0.108690\nv -0.766764 -0.418520 -0.108690\nv 0.842611 -0.418520 -0.186815\nv -0.844889 -0.418520 -0.186815\nv 1.037923 -0.512270 -0.343065\nv -1.040202 -0.512270 -0.343065\nv 1.186361 -0.582583 -0.358690\nv -1.188639 -0.582583 -0.358690\nv 1.256673 -0.590395 -0.257127\nv -1.258952 -0.590395 -0.257127\nv 1.209798 -0.582583 -0.100877\nv -1.212077 -0.582583 -0.100877\nv 1.045736 -0.520083 -0.014940\nv -1.048014 -0.520083 -0.014940\nv 0.881673 -0.363833 0.000685\nv -0.883952 -0.363833 0.000685\nv 0.951986 -0.441958 -0.304002\nv -0.954264 -0.441958 -0.304002\nv 0.889486 -0.426333 -0.124315\nv -0.891764 -0.426333 -0.124315\nv 0.936361 -0.434145 -0.077440\nv -0.938639 -0.434145 -0.077440\nv 0.998861 -0.465395 -0.139940\nv -1.001139 -0.465395 -0.139940\nv 0.959798 -0.449770 -0.186815\nv -0.962077 -0.449770 -0.186815\nv 1.014486 -0.473208 -0.249315\nv -1.016764 -0.473208 -0.249315\nv 1.053548 -0.481020 -0.202440\nv -1.055827 -0.481020 -0.202440\nv 1.108236 -0.488833 -0.225877\nv -1.110514 -0.488833 -0.225877\nv 1.084798 -0.488833 -0.288377\nv -1.087077 -0.488833 -0.288377\nv 1.022298 -0.582583 -0.452440\nv -1.024577 -0.582583 -0.452440\nv 1.248861 -0.645083 -0.483690\nv -1.251139 -0.645083 -0.483690\nv 1.366048 -0.598208 -0.311815\nv -1.368327 -0.598208 -0.311815\nv 1.311361 -0.629458 -0.069627\nv -1.313639 -0.629458 -0.069627\nv 1.037923 -0.590395 0.070998\nv -1.040202 -0.590395 0.070998\nv 0.787923 -0.426333 0.110060\nv -0.790202 -0.426333 0.110060\nv 0.858236 -0.481020 -0.397752\nv -0.860514 -0.481020 -0.397752\nvn 0.664993 0.719363 0.200752\nvn -0.664993 0.719363 0.200752\nvn 0.829427 0.468924 0.303581\nvn -0.829427 0.468924 0.303581\nvn 0.415548 0.444931 0.793320\nvn -0.415548 0.444931 0.793320\nvn 0.359950 0.781960 0.508895\nvn -0.359950 0.781960 0.508895\nvn -0.078666 0.838353 0.539423\nvn 0.078666 0.838353 0.539423\nvn -0.269627 0.468532 0.841296\nvn 0.269627 0.468532 0.841296\nvn -0.770656 0.541966 0.335204\nvn 0.770656 0.541966 0.335204\nvn -0.468941 0.861650 0.194045\nvn 0.468941 0.861650 0.194045\nvn -0.476731 0.858116 -0.190693\nvn 0.476731 0.858116 -0.190693\nvn -0.767202 0.552142 -0.326404\nvn 0.767202 0.552142 -0.326404\nvn -0.251927 0.518169 -0.817333\nvn 0.251927 0.518169 -0.817333\nvn -0.094933 0.816423 -0.569597\nvn 0.094933 0.816423 -0.569597\nvn 0.366742 0.759680 -0.537015\nvn -0.366742 0.759680 -0.537015\nvn 0.414055 0.489830 -0.767219\nvn -0.414055 0.489830 -0.767219\nvn 0.827747 0.477141 -0.295247\nvn -0.827747 0.477141 -0.295247\nvn 0.671345 0.714459 -0.197092\nvn -0.671345 0.714459 -0.197092\nvn 0.811107 -0.486664 -0.324443\nvn -0.811107 -0.486664 -0.324443\nvn 0.205152 -0.533396 -0.820610\nvn -0.205152 -0.533396 -0.820610\nvn -0.422314 -0.460706 -0.780641\nvn 0.422314 -0.460706 -0.780641\nvn -0.824061 -0.465773 -0.322458\nvn 0.824061 -0.465773 -0.322458\nvn -0.813733 -0.464991 0.348743\nvn 0.813733 -0.464991 0.348743\nvn -0.422314 -0.460706 0.780641\nvn 0.422314 -0.460706 0.780641\nvn 0.205152 -0.533396 0.820610\nvn -0.205152 -0.533396 0.820610\nvn 0.799477 -0.487486 0.350990\nvn -0.799477 -0.487486 0.350990\nvn 0.400039 0.914375 0.062344\nvn -0.400039 0.914375 0.062344\nvn 0.306938 0.935429 0.175393\nvn -0.306938 0.935429 0.175393\nvn 0.094512 0.978473 0.183464\nvn -0.094512 0.978473 0.183464\nvn -0.062353 0.997652 0.028342\nvn 0.062353 0.997652 0.028342\nvn -0.062357 0.997716 -0.025982\nvn 0.062357 0.997716 -0.025982\nvn 0.099561 0.979891 -0.172922\nvn -0.099561 0.979891 -0.172922\nvn 0.303571 0.938310 -0.165584\nvn -0.303571 0.938310 -0.165584\nvn 0.400163 0.914659 -0.057166\nvn -0.400163 0.914659 -0.057166\nvn 0.123091 0.492366 0.861641\nvn -0.123091 0.492366 0.861641\nvn 0.218986 0.452010 0.864715\nvn -0.218986 0.452010 0.864715\nvn 0.590198 0.666788 0.455038\nvn -0.590198 0.666788 0.455038\nvn 0.768894 0.637372 0.050585\nvn -0.768894 0.637372 0.050585\nvn 0.779649 0.619721 -0.089960\nvn -0.779649 0.619721 -0.089960\nvn 0.324141 0.473874 0.818765\nvn -0.324141 0.473874 0.818765\nvn 0.385730 0.641707 0.662891\nvn -0.385730 0.641707 0.662891\nvn 0.689468 0.590607 0.419306\nvn -0.689468 0.590607 0.419306\nvn 0.658751 0.658751 0.363449\nvn -0.658751 0.658751 0.363449\nvn 0.546548 0.750910 -0.370702\nvn -0.546548 0.750910 -0.370702\nvn 0.506447 0.570645 -0.646433\nvn -0.506447 0.570645 -0.646433\nvn 0.609244 0.601532 -0.516701\nvn -0.609244 0.601532 -0.516701\nvn -0.044065 0.749110 -0.660979\nvn 0.044065 0.749110 -0.660979\nvn -0.724614 0.611014 -0.318742\nvn 0.724614 0.611014 -0.318742\nvn -0.588034 0.588034 -0.555366\nvn 0.588034 0.588034 -0.555366\nvn 0.536054 0.748241 0.390872\nvn -0.536054 0.748241 0.390872\nvn 0.220695 0.855193 0.468977\nvn -0.220695 0.855193 0.468977\nvn -0.079395 0.842940 0.532117\nvn 0.079395 0.842940 0.532117\nvn -0.082465 0.748963 0.657461\nvn 0.082465 0.748963 0.657461\nvn 0.045703 0.822647 0.566712\nvn -0.045703 0.822647 0.566712\nvn 0.278428 0.936532 0.213040\nvn -0.278428 0.936532 0.213040\nvn 0.381303 0.906285 0.182362\nvn -0.381303 0.906285 0.182362\nvn 0.335744 0.896916 0.287781\nvn -0.335744 0.896916 0.287781\nvn 0.376240 0.924559 -0.060276\nvn -0.376240 0.924559 -0.060276\nvn -0.135216 0.953890 -0.267974\nvn 0.135216 0.953890 -0.267974\nvn 0.396091 0.810186 0.432099\nvn -0.396091 0.810186 0.432099\nvn 0.185557 0.950977 0.247409\nvn -0.185557 0.950977 0.247409\nvn 0.009907 0.980786 0.194836\nvn -0.009907 0.980786 0.194836\nvn 0.072066 0.713795 0.696637\nvn -0.072066 0.713795 0.696637\nvn 0.186336 0.798582 0.572317\nvn -0.186336 0.798582 0.572317\nvn 0.315685 0.909388 0.270843\nvn -0.315685 0.909388 0.270843\nvn 0.306302 0.951566 0.026481\nvn -0.306302 0.951566 0.026481\nvn 0.326550 0.936111 0.130620\nvn -0.326550 0.936111 0.130620\nvn -0.013675 0.998256 -0.057434\nvn 0.013675 0.998256 -0.057434\nvn -0.002626 0.997839 0.065647\nvn 0.002626 0.997839 0.065647\nvn 0.000000 1.000000 -0.000000\nvn 0.817393 -0.044183 0.574384\nvn -0.817393 -0.044183 0.574384\nvn 0.949363 -0.214372 -0.229685\nvn -0.949363 -0.214372 -0.229685\nvn 0.082479 -0.412393 -0.907265\nvn -0.082479 -0.412393 -0.907265\nvn -0.883624 0.304698 -0.355481\nvn 0.883624 0.304698 -0.355481\nvn 0.420706 0.221827 0.879659\nvn -0.420706 0.221827 0.879659\nvn 0.287348 0.766261 0.574696\nvn -0.287348 0.766261 0.574696\nvn -0.654224 0.457957 -0.601886\nvn 0.654224 0.457957 -0.601886\nvn 0.105227 0.605054 -0.789201\nvn -0.105227 0.605054 -0.789201\nvn 0.758175 0.583212 -0.291606\nvn -0.758175 0.583212 -0.291606\nvn 0.388922 0.583383 0.713024\nvn -0.388922 0.583383 0.713024\nvn 0.046274 0.971764 -0.231372\nvn -0.046274 0.971764 -0.231372\nvn 0.033480 0.915131 0.401765\nvn -0.033480 0.915131 0.401765\nvn -0.445163 0.880854 0.161016\nvn 0.445163 0.880854 0.161016\nvn -0.218218 0.872872 0.436436\nvn 0.218218 0.872872 0.436436\nvn 0.434064 0.891591 0.129046\nvn -0.434064 0.891591 0.129046\nvn 0.300753 0.952384 -0.050125\nvn -0.300753 0.952384 -0.050125\nvn 0.812285 0.499568 -0.301039\nvn -0.812285 0.499568 -0.301039\nvn 0.875310 0.409336 -0.257444\nvn -0.875310 0.409336 -0.257444\nvn 0.938484 0.305959 -0.160113\nvn -0.938484 0.305959 -0.160113\nvn 0.223706 0.722743 0.653910\nvn -0.223706 0.722743 0.653910\nvn -0.153610 0.967743 0.199693\nvn 0.153610 0.967743 0.199693\nvn -0.273275 0.956462 0.102478\nvn 0.273275 0.956462 0.102478\nvn -0.097590 0.975900 -0.195180\nvn 0.097590 0.975900 -0.195180\nvn -0.158235 0.271260 -0.949410\nvn 0.158235 0.271260 -0.949410\nvn -0.693430 0.132784 -0.708183\nvn 0.693430 0.132784 -0.708183\nvn -1.000000 0.000000 0.000000\nvn 1.000000 0.000000 0.000000\nvn 0.305141 0.118119 0.944953\nvn -0.305141 0.118119 0.944953\nvn 0.029814 0.954056 0.298142\nvn -0.029814 0.954056 0.298142\nvn 0.135293 0.927720 0.347895\nvn -0.135293 0.927720 0.347895\nvn -0.508542 0.815786 0.275460\nvn 0.508542 0.815786 0.275460\nvn -0.384277 0.922265 0.041921\nvn 0.384277 0.922265 0.041921\nvn -0.208288 0.977353 -0.037385\nvn 0.208288 0.977353 -0.037385\nvn -0.572078 0.667424 0.476731\nvn 0.572078 0.667424 0.476731\nvn -0.136922 0.643534 0.753071\nvn 0.136922 0.643534 0.753071\nvn 0.408843 0.681405 0.607070\nvn -0.408843 0.681405 0.607070\nvn 0.574030 0.707038 0.413022\nvn -0.574030 0.707038 0.413022\nvn 0.566534 0.818328 0.096843\nvn -0.566534 0.818328 0.096843\nvn 0.570336 0.812892 -0.118000\nvn -0.570336 0.812892 -0.118000\nvn 0.482289 0.671879 -0.562117\nvn -0.482289 0.671879 -0.562117\nvn 0.260407 0.747255 -0.611390\nvn -0.260407 0.747255 -0.611390\nvn 0.163956 0.918156 -0.360704\nvn -0.163956 0.918156 -0.360704\nvn -0.017820 0.968216 -0.249479\nvn 0.017820 0.968216 -0.249479\nvn 0.327339 0.848105 0.416613\nvn -0.327339 0.848105 0.416613\nvn 0.281070 0.923516 0.260994\nvn -0.281070 0.923516 0.260994\nvn -0.254193 0.714916 0.651368\nvn 0.254193 0.714916 0.651368\nvn -0.026016 0.533323 0.845512\nvn 0.026016 0.533323 0.845512\nvn -0.351808 0.899066 0.260599\nvn 0.351808 0.899066 0.260599\nvn -0.352308 0.935819 0.011010\nvn 0.352308 0.935819 0.011010\nvn -0.131654 0.877691 -0.460788\nvn 0.131654 0.877691 -0.460788\nvn -0.034219 0.787044 -0.615947\nvn 0.034219 0.787044 -0.615947\nvn 0.360263 0.727731 -0.583625\nvn -0.360263 0.727731 -0.583625\nvn 0.498784 0.685828 -0.529958\nvn -0.498784 0.685828 -0.529958\nvn 0.666667 0.666667 0.333333\nvn -0.666667 0.666667 0.333333\nvn 0.816466 0.572745 0.073116\nvn -0.816466 0.572745 0.073116\nvn 0.784010 0.609785 -0.116150\nvn -0.784010 0.609785 -0.116150\nvn -0.530629 -0.246147 -0.811076\nvn 0.530629 -0.246147 -0.811076\nvn -0.851109 -0.372958 -0.369480\nvn 0.851109 -0.372958 -0.369480\nvn -0.244586 -0.433121 -0.867516\nvn 0.244586 -0.433121 -0.867516\nvn 0.592382 -0.303006 -0.746506\nvn -0.592382 -0.303006 -0.746506\nvn 0.368548 -0.311777 -0.875767\nvn -0.368548 -0.311777 -0.875767\nvn 0.282140 -0.287988 -0.915128\nvn -0.282140 -0.287988 -0.915128\nvn 0.856131 -0.499077 -0.134021\nvn -0.856131 -0.499077 -0.134021\nvn 0.534226 -0.437577 0.723276\nvn -0.534226 -0.437577 0.723276\nvn 0.384903 -0.436800 0.813053\nvn -0.384903 -0.436800 0.813053\nvn 0.233519 -0.780017 0.580553\nvn -0.233519 -0.780017 0.580553\nvn 0.244866 -0.967802 0.058301\nvn -0.244866 -0.967802 0.058301\nvn 0.116271 -0.883661 0.453458\nvn -0.116271 -0.883661 0.453458\nvn 0.115196 -0.138826 0.983594\nvn -0.115196 -0.138826 0.983594\nvn 0.118366 -0.225972 0.966916\nvn -0.118366 -0.225972 0.966916\nvn 0.959736 -0.280774 0.008508\nvn -0.959736 -0.280774 0.008508\nvn 0.931868 -0.324194 -0.162851\nvn -0.931868 -0.324194 -0.162851\nvn 0.162606 -0.986474 -0.020695\nvn -0.162606 -0.986474 -0.020695\nvn -0.018766 -0.975838 0.217687\nvn 0.018766 -0.975838 0.217687\nvn 0.753776 -0.588391 0.292605\nvn -0.753776 -0.588391 0.292605\nvn 0.919601 -0.367840 -0.137940\nvn -0.919601 -0.367840 -0.137940\nvn 0.929736 -0.194399 -0.312729\nvn -0.929736 -0.194399 -0.312729\nvn 0.912018 -0.232856 -0.337641\nvn -0.912018 -0.232856 -0.337641\nvn 0.940691 -0.060690 -0.333793\nvn -0.940691 -0.060690 -0.333793\nvn 0.176090 -0.440225 0.880451\nvn -0.176090 -0.440225 0.880451\nvn 0.370784 -0.799083 0.473270\nvn -0.370784 -0.799083 0.473270\nvn 0.310668 -0.466002 0.828449\nvn -0.310668 -0.466002 0.828449\nvn 0.279339 -0.128692 0.951529\nvn -0.279339 -0.128692 0.951529\nvn 0.313873 -0.180715 0.932108\nvn -0.313873 -0.180715 0.932108\nvn 0.976161 -0.060864 0.208341\nvn -0.976161 -0.060864 0.208341\nvn 0.826725 0.244727 0.506592\nvn -0.826725 0.244727 0.506592\nvn 0.344853 -0.931486 0.115800\nvn -0.344853 -0.931486 0.115800\nvn 0.120261 0.235495 -0.964406\nvn -0.120261 0.235495 -0.964406\nvn 0.127513 -0.185137 -0.974405\nvn -0.127513 -0.185137 -0.974405\nvn 0.349226 -0.724138 -0.594697\nvn -0.349226 -0.724138 -0.594697\nvn 0.415251 -0.144855 -0.898100\nvn -0.415251 -0.144855 -0.898100\nvn 0.184540 0.686258 -0.703559\nvn -0.184540 0.686258 -0.703559\nvn 0.605564 0.160824 -0.779377\nvn -0.605564 0.160824 -0.779377\nvn 0.703301 -0.205264 -0.680614\nvn -0.703301 -0.205264 -0.680614\nvn 0.667944 -0.716631 -0.200725\nvn -0.667944 -0.716631 -0.200725\nvn 0.494774 -0.752756 -0.434231\nvn -0.494774 -0.752756 -0.434231\nvn 0.642323 -0.176121 -0.745924\nvn -0.642323 -0.176121 -0.745924\nvn 0.718225 0.152966 -0.678788\nvn -0.718225 0.152966 -0.678788\nvn 0.738828 0.544366 -0.397240\nvn -0.738828 0.544366 -0.397240\nvn 0.342771 -0.157888 -0.926056\nvn -0.342771 -0.157888 -0.926056\nvn 0.226983 0.786746 -0.574030\nvn -0.226983 0.786746 -0.574030\nvn -0.172189 -0.979491 -0.104638\nvn 0.172189 -0.979491 -0.104638\nvn 0.042460 0.401319 -0.914954\nvn -0.042460 0.401319 -0.914954\nvn -0.161572 0.969432 -0.184654\nvn 0.161572 0.969432 -0.184654\nvn 0.979149 0.048332 -0.197308\nvn -0.979149 0.048332 -0.197308\nvn 0.946968 0.307922 -0.091845\nvn -0.946968 0.307922 -0.091845\nvn 0.979450 -0.066136 -0.190536\nvn -0.979450 -0.066136 -0.190536\nvn 0.993775 -0.106953 -0.031195\nvn -0.993775 -0.106953 -0.031195\nvn 0.711563 0.050060 0.700836\nvn -0.711563 0.050060 0.700836\nvn 0.372160 0.084651 0.924300\nvn -0.372160 0.084651 0.924300\nvn 0.446529 0.231010 0.864434\nvn -0.446529 0.231010 0.864434\nvn 0.606579 0.240489 0.757778\nvn -0.606579 0.240489 0.757778\nvn 0.732489 0.240675 0.636817\nvn -0.732489 0.240675 0.636817\nvn 0.263732 0.853252 0.449896\nvn -0.263732 0.853252 0.449896\nvn 0.556817 -0.767332 0.318051\nvn -0.556817 -0.767332 0.318051\nvn 0.500431 -0.818999 0.280730\nvn -0.500431 -0.818999 0.280730\nvn 0.318954 -0.420485 0.849389\nvn -0.318954 -0.420485 0.849389\nvn 0.719759 -0.279303 0.635561\nvn -0.719759 -0.279303 0.635561\nvn 0.497205 -0.747333 0.440774\nvn -0.497205 -0.747333 0.440774\nvn 0.350559 0.855666 -0.380715\nvn -0.350559 0.855666 -0.380715\nvn 0.456551 0.873015 -0.171485\nvn -0.456551 0.873015 -0.171485\nvn 0.258262 0.960298 -0.105487\nvn -0.258262 0.960298 -0.105487\nvn 0.245528 0.966063 0.080238\nvn -0.245528 0.966063 0.080238\nvn 0.464292 0.883654 0.059909\nvn -0.464292 0.883654 0.059909\nvn 0.622461 0.720981 0.304514\nvn -0.622462 0.720981 0.304514\nvn 0.450020 0.602706 -0.658959\nvn -0.450021 0.602706 -0.658959\nvn -0.266664 0.488415 -0.830868\nvn 0.266664 0.488415 -0.830868\nvn -0.828395 0.511137 -0.229130\nvn 0.828395 0.511137 -0.229130\nvn -0.525061 0.772732 0.356645\nvn 0.525061 0.772732 0.356645\nvn 0.454637 0.687284 0.566521\nvn -0.454637 0.687284 0.566521\nvn 0.699600 0.555239 0.449743\nvn -0.699601 0.555238 0.449743\nvn 0.722010 -0.112644 0.682652\nvn -0.722010 -0.112644 0.682652\nvn -0.191904 0.938824 -0.285975\nvn 0.191904 0.938824 -0.285975\nvn 0.904808 -0.204748 0.373365\nvn -0.904808 -0.204748 0.373365\nvn 0.103418 0.982467 -0.155126\nvn -0.103418 0.982467 -0.155126\nvn 0.084056 0.353037 -0.931826\nvn -0.084056 0.353037 -0.931826\nvn 0.644606 0.759399 0.088302\nvn -0.644606 0.759399 0.088302\nvn 0.430935 0.767848 -0.474029\nvn -0.430935 0.767848 -0.474029\nvn 0.803235 0.346222 0.484711\nvn -0.803235 0.346222 0.484711\nvn 0.581122 0.701353 0.412797\nvn -0.581122 0.701353 0.412797\nvn 0.591001 0.682205 0.430482\nvn -0.591001 0.682205 0.430482\nvn 0.981815 -0.059145 0.180394\nvn -0.981815 -0.059145 0.180394\nvn 0.910486 -0.117482 0.396502\nvn -0.910486 -0.117482 0.396502\nvn 0.997202 -0.072524 0.018131\nvn -0.997202 -0.072524 0.018131\nvn 0.731310 0.192450 0.654330\nvn -0.731310 0.192450 0.654330\nvn 0.786718 0.107280 0.607919\nvn -0.786718 0.107280 0.607919\nvn 0.702247 0.117041 0.702247\nvn -0.702247 0.117041 0.702247\nvn 0.184048 -0.051124 -0.981587\nvn -0.184048 -0.051124 -0.981587\nvn 0.935190 0.128359 -0.330067\nvn -0.935190 0.128359 -0.330067\nvn 0.663348 0.055279 0.746267\nvn -0.663348 0.055279 0.746267\nvn -0.008522 0.076694 -0.997018\nvn 0.008522 0.076694 -0.997018\nvn 0.623691 0.335381 0.706066\nvn -0.623692 0.335381 0.706065\nvn 0.273312 0.358722 0.892535\nvn -0.273312 0.358722 0.892535\nvn -0.832769 -0.219977 0.508041\nvn 0.832769 -0.219977 0.508041\nvn -0.833909 -0.498081 -0.237721\nvn 0.833909 -0.498081 -0.237721\nvn -0.565464 -0.253882 -0.784726\nvn 0.565464 -0.253882 -0.784726\nvn -0.055965 0.067158 -0.996172\nvn 0.055965 0.067158 -0.996172\nvn 0.144498 0.989255 -0.022230\nvn -0.144498 0.989255 -0.022230\nvn 0.327452 0.942664 -0.064498\nvn -0.327452 0.942664 -0.064498\nvn 0.312667 0.949580 -0.023160\nvn -0.312667 0.949580 -0.023160\nvn 0.170988 0.984893 -0.027358\nvn -0.170988 0.984893 -0.027358\nvn 0.348658 0.892906 -0.284879\nvn -0.348659 0.892906 -0.284879\nvn 0.400582 0.915617 0.034336\nvn -0.400583 0.915617 0.034336\nvn 0.257194 0.964478 0.060280\nvn -0.257194 0.964478 0.060280\nvn 0.063697 0.997913 0.010616\nvn -0.063697 0.997913 0.010616\nvn -0.363700 0.610078 -0.703936\nvn 0.363700 0.610078 -0.703936\nvn 0.629882 0.775881 -0.035457\nvn -0.629882 0.775881 -0.035457\nvn 0.447210 0.871726 0.200243\nvn -0.447210 0.871726 0.200243\nvn 0.507163 0.834843 0.214062\nvn -0.507163 0.834843 0.214062\nvn 0.525823 0.809259 -0.261934\nvn -0.525823 0.809259 -0.261934\nvn 0.297964 0.757979 -0.580246\nvn -0.297964 0.757979 -0.580246\nvn 0.093038 -0.080501 0.992403\nvn -0.093038 -0.080501 0.992403\nvn 0.500580 0.007971 0.865653\nvn -0.500580 0.007971 0.865653\nvn 0.928516 0.274791 0.249696\nvn -0.928516 0.274791 0.249696\nvn 0.839260 -0.037780 -0.542416\nvn -0.839260 -0.037780 -0.542416\nvn -0.235534 -0.258908 -0.936744\nvn 0.235534 -0.258908 -0.936744\nvn -0.449919 -0.128548 -0.883769\nvn 0.449919 -0.128548 -0.883769\nvn -0.538363 -0.842656 0.009753\nvn 0.538364 -0.842656 0.009753\nvn -0.191040 -0.981286 0.024097\nvn 0.191040 -0.981286 0.024097\nvn 0.404624 -0.914097 -0.026581\nvn -0.404624 -0.914097 -0.026581\nvn -0.781868 0.019678 -0.623133\nvn 0.781868 0.019678 -0.623133\nvn 0.542773 -0.814160 0.206254\nvn -0.542773 -0.814160 0.206254\nvn -0.247398 -0.294522 0.923066\nvn 0.247398 -0.294522 0.923066\ns off\nf 47//1 1//1 3//1\nf 47//1 3//1 45//1\nf 4//2 2//2 48//2\nf 4//2 48//2 46//2\nf 45//3 3//3 5//3\nf 45//3 5//3 43//3\nf 6//4 4//4 46//4\nf 6//4 46//4 44//4\nf 3//5 9//5 7//5\nf 3//5 7//5 5//5\nf 8//6 10//6 4//6\nf 8//6 4//6 6//6\nf 1//7 11//7 9//7\nf 1//7 9//7 3//7\nf 10//8 12//8 2//8\nf 10//8 2//8 4//8\nf 11//9 13//9 15//9\nf 11//9 15//9 9//9\nf 16//10 14//10 12//10\nf 16//10 12//10 10//10\nf 9//11 15//11 17//11\nf 9//11 17//11 7//11\nf 18//12 16//12 10//12\nf 18//12 10//12 8//12\nf 15//13 21//13 19//13\nf 15//13 19//13 17//13\nf 20//14 22//14 16//14\nf 20//14 16//14 18//14\nf 13//15 23//15 21//15\nf 13//15 21//15 15//15\nf 22//16 24//16 14//16\nf 22//16 14//16 16//16\nf 23//17 25//17 27//17\nf 23//17 27//17 21//17\nf 28//18 26//18 24//18\nf 28//18 24//18 22//18\nf 21//19 27//19 29//19\nf 21//19 29//19 19//19\nf 30//20 28//20 22//20\nf 30//20 22//20 20//20\nf 27//21 33//21 31//21\nf 27//21 31//21 29//21\nf 32//22 34//22 28//22\nf 32//22 28//22 30//22\nf 25//23 35//23 33//23\nf 25//23 33//23 27//23\nf 34//24 36//24 26//24\nf 34//24 26//24 28//24\nf 35//25 37//25 39//25\nf 35//25 39//25 33//25\nf 40//26 38//26 36//26\nf 40//26 36//26 34//26\nf 33//27 39//27 41//27\nf 33//27 41//27 31//27\nf 42//28 40//28 34//28\nf 42//28 34//28 32//28\nf 39//29 45//29 43//29\nf 39//29 43//29 41//29\nf 44//30 46//30 40//30\nf 44//30 40//30 42//30\nf 37//31 47//31 45//31\nf 37//31 45//31 39//31\nf 46//32 48//32 38//32\nf 46//32 38//32 40//32\nf 47//33 37//33 51//33\nf 47//33 51//33 49//33\nf 52//34 38//34 48//34\nf 52//34 48//34 50//34\nf 37//35 35//35 53//35\nf 37//35 53//35 51//35\nf 54//36 36//36 38//36\nf 54//36 38//36 52//36\nf 35//37 25//37 55//37\nf 35//37 55//37 53//37\nf 56//38 26//38 36//38\nf 56//38 36//38 54//38\nf 25//39 23//39 57//39\nf 25//39 57//39 55//39\nf 58//40 24//40 26//40\nf 58//40 26//40 56//40\nf 23//41 13//41 59//41\nf 23//41 59//41 57//41\nf 60//42 14//42 24//42\nf 60//42 24//42 58//42\nf 13//43 11//43 63//43\nf 13//43 63//43 59//43\nf 64//44 12//44 14//44\nf 64//44 14//44 60//44\nf 11//45 1//45 65//45\nf 11//45 65//45 63//45\nf 66//46 2//46 12//46\nf 66//46 12//46 64//46\nf 1//47 47//47 49//47\nf 1//47 49//47 65//47\nf 50//48 48//48 2//48\nf 50//48 2//48 66//48\nf 61//49 65//49 49//49\nf 50//50 66//50 62//50\nf 63//51 65//51 61//51\nf 62//52 66//52 64//52\nf 61//53 59//53 63//53\nf 64//54 60//54 62//54\nf 61//55 57//55 59//55\nf 60//56 58//56 62//56\nf 61//57 55//57 57//57\nf 58//58 56//58 62//58\nf 61//59 53//59 55//59\nf 56//60 54//60 62//60\nf 61//61 51//61 53//61\nf 54//62 52//62 62//62\nf 61//63 49//63 51//63\nf 52//64 50//64 62//64\nf 89//65 174//65 176//65\nf 89//65 176//65 91//65\nf 176//66 175//66 90//66\nf 176//66 90//66 91//66\nf 87//67 172//67 174//67\nf 87//67 174//67 89//67\nf 175//68 173//68 88//68\nf 175//68 88//68 90//68\nf 85//69 170//69 172//69\nf 85//69 172//69 87//69\nf 173//70 171//70 86//70\nf 173//70 86//70 88//70\nf 83//71 168//71 170//71\nf 83//71 170//71 85//71\nf 171//72 169//72 84//72\nf 171//72 84//72 86//72\nf 81//73 166//73 168//73\nf 81//73 168//73 83//73\nf 169//74 167//74 82//74\nf 169//74 82//74 84//74\nf 79//75 92//75 146//75\nf 79//75 146//75 164//75\nf 147//76 93//76 80//76\nf 147//76 80//76 165//76\nf 92//77 94//77 148//77\nf 92//77 148//77 146//77\nf 149//78 95//78 93//78\nf 149//78 93//78 147//78\nf 94//79 96//79 150//79\nf 94//79 150//79 148//79\nf 151//80 97//80 95//80\nf 151//80 95//80 149//80\nf 96//81 98//81 152//81\nf 96//81 152//81 150//81\nf 153//82 99//82 97//82\nf 153//82 97//82 151//82\nf 98//83 100//83 154//83\nf 98//83 154//83 152//83\nf 155//84 101//84 99//84\nf 155//84 99//84 153//84\nf 100//85 102//85 156//85\nf 100//85 156//85 154//85\nf 157//86 103//86 101//86\nf 157//86 101//86 155//86\nf 102//87 104//87 158//87\nf 102//87 158//87 156//87\nf 159//88 105//88 103//88\nf 159//88 103//88 157//88\nf 104//89 106//89 160//89\nf 104//89 160//89 158//89\nf 161//90 107//90 105//90\nf 161//90 105//90 159//90\nf 106//91 108//91 162//91\nf 106//91 162//91 160//91\nf 163//92 109//92 107//92\nf 163//92 107//92 161//92\nf 108//93 67//93 68//93\nf 108//93 68//93 162//93\nf 68//94 67//94 109//94\nf 68//94 109//94 163//94\nf 110//95 128//95 160//95\nf 110//95 160//95 162//95\nf 161//96 129//96 111//96\nf 161//96 111//96 163//96\nf 128//97 179//97 158//97\nf 128//97 158//97 160//97\nf 159//98 180//98 129//98\nf 159//98 129//98 161//98\nf 126//99 156//99 158//99\nf 126//99 158//99 179//99\nf 159//100 157//100 127//100\nf 159//100 127//100 180//100\nf 124//101 154//101 156//101\nf 124//101 156//101 126//101\nf 157//102 155//102 125//102\nf 157//102 125//102 127//102\nf 122//103 152//103 154//103\nf 122//103 154//103 124//103\nf 155//104 153//104 123//104\nf 155//104 123//104 125//104\nf 120//105 150//105 152//105\nf 120//105 152//105 122//105\nf 153//106 151//106 121//106\nf 153//106 121//106 123//106\nf 118//107 148//107 150//107\nf 118//107 150//107 120//107\nf 151//108 149//108 119//108\nf 151//108 119//108 121//108\nf 116//109 146//109 148//109\nf 116//109 148//109 118//109\nf 149//110 147//110 117//110\nf 149//110 117//110 119//110\nf 114//111 164//111 146//111\nf 114//111 146//111 116//111\nf 147//112 165//112 115//112\nf 147//112 115//112 117//112\nf 114//113 181//113 177//113\nf 114//113 177//113 164//113\nf 177//114 182//114 115//114\nf 177//114 115//114 165//114\nf 110//115 162//115 68//115\nf 110//115 68//115 112//115\nf 68//116 163//116 111//116\nf 68//116 111//116 113//116\nf 112//117 68//117 178//117\nf 112//117 178//117 183//117\nf 178//118 68//118 113//118\nf 178//118 113//118 184//118\nf 177//119 181//119 183//119\nf 177//119 183//119 178//119\nf 184//120 182//120 177//120\nf 184//120 177//120 178//120\nf 135//121 137//121 176//121\nf 135//121 176//121 174//121\nf 176//122 137//122 136//122\nf 176//122 136//122 175//122\nf 133//123 135//123 174//123\nf 133//123 174//123 172//123\nf 175//124 136//124 134//124\nf 175//124 134//124 173//124\nf 131//125 133//125 172//125\nf 131//125 172//125 170//125\nf 173//126 134//126 132//126\nf 173//126 132//126 171//126\nf 166//127 187//127 185//127\nf 166//127 185//127 168//127\nf 186//128 188//128 167//128\nf 186//128 167//128 169//128\nf 131//129 170//129 168//129\nf 131//129 168//129 185//129\nf 169//130 171//130 132//130\nf 169//130 132//130 186//130\nf 144//131 190//131 189//131\nf 144//131 189//131 187//131\nf 189//132 190//132 145//132\nf 189//132 145//132 188//132\nf 185//133 187//133 189//133\nf 185//133 189//133 69//133\nf 189//134 188//134 186//134\nf 189//134 186//134 69//134\nf 130//135 131//135 185//135\nf 130//135 185//135 69//135\nf 186//135 132//135 130//135\nf 186//135 130//135 69//135\nf 142//136 193//136 191//136\nf 142//136 191//136 144//136\nf 192//137 194//137 143//137\nf 192//137 143//137 145//137\nf 140//138 195//138 193//138\nf 140//138 193//138 142//138\nf 194//139 196//139 141//139\nf 194//139 141//139 143//139\nf 139//140 197//140 195//140\nf 139//140 195//140 140//140\nf 196//141 198//141 139//141\nf 196//141 139//141 141//141\nf 138//142 71//142 197//142\nf 138//142 197//142 139//142\nf 198//143 71//143 138//143\nf 198//143 138//143 139//143\nf 190//144 144//144 191//144\nf 190//144 191//144 70//144\nf 192//145 145//145 190//145\nf 192//145 190//145 70//145\nf 70//146 191//146 206//146\nf 70//146 206//146 208//146\nf 207//147 192//147 70//147\nf 207//147 70//147 208//147\nf 71//148 199//148 200//148\nf 71//148 200//148 197//148\nf 201//149 199//149 71//149\nf 201//149 71//149 198//149\nf 197//150 200//150 202//150\nf 197//150 202//150 195//150\nf 203//151 201//151 198//151\nf 203//151 198//151 196//151\nf 195//152 202//152 204//152\nf 195//152 204//152 193//152\nf 205//153 203//153 196//153\nf 205//153 196//153 194//153\nf 193//154 204//154 206//154\nf 193//154 206//154 191//154\nf 207//155 205//155 194//155\nf 207//155 194//155 192//155\nf 199//156 204//156 202//156\nf 199//156 202//156 200//156\nf 203//157 205//157 199//157\nf 203//157 199//157 201//157\nf 199//158 208//158 206//158\nf 199//158 206//158 204//158\nf 207//159 208//159 199//159\nf 207//159 199//159 205//159\nf 139//160 140//160 164//160\nf 139//160 164//160 177//160\nf 165//161 141//161 139//161\nf 165//161 139//161 177//161\nf 140//162 142//162 211//162\nf 140//162 211//162 164//162\nf 212//163 143//163 141//163\nf 212//163 141//163 165//163\nf 142//164 144//164 213//164\nf 142//164 213//164 211//164\nf 214//165 145//165 143//165\nf 214//165 143//165 212//165\nf 144//166 187//166 166//166\nf 144//166 166//166 213//166\nf 167//167 188//167 145//167\nf 167//167 145//167 214//167\nf 81//168 209//168 213//168\nf 81//168 213//168 166//168\nf 214//169 210//169 82//169\nf 214//169 82//169 167//169\nf 209//170 215//170 211//170\nf 209//170 211//170 213//170\nf 212//171 216//171 210//171\nf 212//171 210//171 214//171\nf 79//172 164//172 211//172\nf 79//172 211//172 215//172\nf 212//173 165//173 80//173\nf 212//173 80//173 216//173\nf 131//174 130//174 72//174\nf 131//174 72//174 222//174\nf 72//175 130//175 132//175\nf 72//175 132//175 223//175\nf 133//176 131//176 222//176\nf 133//176 222//176 220//176\nf 223//177 132//177 134//177\nf 223//177 134//177 221//177\nf 135//178 133//178 220//178\nf 135//178 220//178 218//178\nf 221//179 134//179 136//179\nf 221//179 136//179 219//179\nf 137//180 135//180 218//180\nf 137//180 218//180 217//180\nf 219//181 136//181 137//181\nf 219//181 137//181 217//181\nf 217//182 218//182 229//182\nf 217//182 229//182 231//182\nf 230//183 219//183 217//183\nf 230//183 217//183 231//183\nf 218//184 220//184 227//184\nf 218//184 227//184 229//184\nf 228//185 221//185 219//185\nf 228//185 219//185 230//185\nf 220//186 222//186 225//186\nf 220//186 225//186 227//186\nf 226//187 223//187 221//187\nf 226//187 221//187 228//187\nf 222//188 72//188 224//188\nf 222//188 224//188 225//188\nf 224//189 72//189 223//189\nf 224//189 223//189 226//189\nf 224//190 231//190 229//190\nf 224//190 229//190 225//190\nf 230//191 231//191 224//191\nf 230//191 224//191 226//191\nf 225//192 229//192 227//192\nf 228//193 230//193 226//193\nf 183//194 181//194 234//194\nf 183//194 234//194 232//194\nf 235//195 182//195 184//195\nf 235//195 184//195 233//195\nf 112//196 183//196 232//196\nf 112//196 232//196 254//196\nf 233//197 184//197 113//197\nf 233//197 113//197 255//197\nf 110//198 112//198 254//198\nf 110//198 254//198 256//198\nf 255//199 113//199 111//199\nf 255//199 111//199 257//199\nf 181//200 114//200 252//200\nf 181//200 252//200 234//200\nf 253//201 115//201 182//201\nf 253//201 182//201 235//201\nf 114//202 116//202 250//202\nf 114//202 250//202 252//202\nf 251//203 117//203 115//203\nf 251//203 115//203 253//203\nf 116//204 118//204 248//204\nf 116//204 248//204 250//204\nf 249//205 119//205 117//205\nf 249//205 117//205 251//205\nf 118//206 120//206 246//206\nf 118//206 246//206 248//206\nf 247//207 121//207 119//207\nf 247//207 119//207 249//207\nf 120//208 122//208 244//208\nf 120//208 244//208 246//208\nf 245//209 123//209 121//209\nf 245//209 121//209 247//209\nf 122//210 124//210 242//210\nf 122//210 242//210 244//210\nf 243//211 125//211 123//211\nf 243//211 123//211 245//211\nf 124//212 126//212 240//212\nf 124//212 240//212 242//212\nf 241//213 127//213 125//213\nf 241//213 125//213 243//213\nf 126//214 179//214 236//214\nf 126//214 236//214 240//214\nf 237//215 180//215 127//215\nf 237//215 127//215 241//215\nf 179//216 128//216 238//216\nf 179//216 238//216 236//216\nf 239//217 129//217 180//217\nf 239//217 180//217 237//217\nf 128//218 110//218 256//218\nf 128//218 256//218 238//218\nf 257//219 111//219 129//219\nf 257//219 129//219 239//219\nf 238//220 256//220 258//220\nf 238//220 258//220 276//220\nf 259//221 257//221 239//221\nf 259//221 239//221 277//221\nf 236//222 238//222 276//222\nf 236//222 276//222 278//222\nf 277//223 239//223 237//223\nf 277//223 237//223 279//223\nf 240//224 236//224 278//224\nf 240//224 278//224 274//224\nf 279//225 237//225 241//225\nf 279//225 241//225 275//225\nf 242//226 240//226 274//226\nf 242//226 274//226 272//226\nf 275//227 241//227 243//227\nf 275//227 243//227 273//227\nf 244//228 242//228 272//228\nf 244//228 272//228 270//228\nf 273//229 243//229 245//229\nf 273//229 245//229 271//229\nf 246//230 244//230 270//230\nf 246//230 270//230 268//230\nf 271//231 245//231 247//231\nf 271//231 247//231 269//231\nf 248//232 246//232 268//232\nf 248//232 268//232 266//232\nf 269//233 247//233 249//233\nf 269//233 249//233 267//233\nf 250//234 248//234 266//234\nf 250//234 266//234 264//234\nf 267//235 249//235 251//235\nf 267//235 251//235 265//235\nf 252//236 250//236 264//236\nf 252//236 264//236 262//236\nf 265//237 251//237 253//237\nf 265//237 253//237 263//237\nf 234//238 252//238 262//238\nf 234//238 262//238 280//238\nf 263//239 253//239 235//239\nf 263//239 235//239 281//239\nf 256//240 254//240 260//240\nf 256//240 260//240 258//240\nf 261//241 255//241 257//241\nf 261//241 257//241 259//241\nf 254//242 232//242 282//242\nf 254//242 282//242 260//242\nf 283//243 233//243 255//243\nf 283//243 255//243 261//243\nf 232//244 234//244 280//244\nf 232//244 280//244 282//244\nf 281//245 235//245 233//245\nf 281//245 233//245 283//245\nf 67//246 108//246 284//246\nf 67//246 284//246 73//246\nf 285//247 109//247 67//247\nf 285//247 67//247 73//247\nf 108//248 106//248 286//248\nf 108//248 286//248 284//248\nf 287//249 107//249 109//249\nf 287//249 109//249 285//249\nf 106//250 104//250 288//250\nf 106//250 288//250 286//250\nf 289//251 105//251 107//251\nf 289//251 107//251 287//251\nf 104//252 102//252 290//252\nf 104//252 290//252 288//252\nf 291//253 103//253 105//253\nf 291//253 105//253 289//253\nf 102//254 100//254 292//254\nf 102//254 292//254 290//254\nf 293//255 101//255 103//255\nf 293//255 103//255 291//255\nf 100//256 98//256 294//256\nf 100//256 294//256 292//256\nf 295//257 99//257 101//257\nf 295//257 101//257 293//257\nf 98//258 96//258 296//258\nf 98//258 296//258 294//258\nf 297//259 97//259 99//259\nf 297//259 99//259 295//259\nf 96//260 94//260 298//260\nf 96//260 298//260 296//260\nf 299//261 95//261 97//261\nf 299//261 97//261 297//261\nf 94//262 92//262 300//262\nf 94//262 300//262 298//262\nf 301//263 93//263 95//263\nf 301//263 95//263 299//263\nf 308//264 309//264 328//264\nf 308//264 328//264 338//264\nf 329//265 309//265 308//265\nf 329//265 308//265 339//265\nf 307//266 308//266 338//266\nf 307//266 338//266 336//266\nf 339//267 308//267 307//267\nf 339//267 307//267 337//267\nf 306//268 307//268 336//268\nf 306//268 336//268 340//268\nf 337//269 307//269 306//269\nf 337//269 306//269 341//269\nf 89//270 91//270 306//270\nf 89//270 306//270 340//270\nf 306//271 91//271 90//271\nf 306//271 90//271 341//271\nf 87//272 89//272 340//272\nf 87//272 340//272 334//272\nf 341//273 90//273 88//273\nf 341//273 88//273 335//273\nf 85//274 87//274 334//274\nf 85//274 334//274 330//274\nf 335//275 88//275 86//275\nf 335//275 86//275 331//275\nf 83//276 85//276 330//276\nf 83//276 330//276 332//276\nf 331//277 86//277 84//277\nf 331//277 84//277 333//277\nf 330//278 336//278 338//278\nf 330//278 338//278 332//278\nf 339//279 337//279 331//279\nf 339//279 331//279 333//279\nf 330//280 334//280 340//280\nf 330//280 340//280 336//280\nf 341//281 335//281 331//281\nf 341//281 331//281 337//281\nf 326//282 332//282 338//282\nf 326//282 338//282 328//282\nf 339//283 333//283 327//283\nf 339//283 327//283 329//283\nf 81//284 83//284 332//284\nf 81//284 332//284 326//284\nf 333//285 84//285 82//285\nf 333//285 82//285 327//285\nf 209//286 342//286 344//286\nf 209//286 344//286 215//286\nf 345//287 343//287 210//287\nf 345//287 210//287 216//287\nf 81//288 326//288 342//288\nf 81//288 342//288 209//288\nf 343//289 327//289 82//289\nf 343//289 82//289 210//289\nf 79//290 215//290 344//290\nf 79//290 344//290 346//290\nf 345//291 216//291 80//291\nf 345//291 80//291 347//291\nf 79//292 346//292 300//292\nf 79//292 300//292 92//292\nf 301//293 347//293 80//293\nf 301//293 80//293 93//293\nf 77//294 324//294 352//294\nf 77//294 352//294 304//294\nf 353//295 325//295 77//295\nf 353//295 77//295 304//295\nf 304//296 352//296 350//296\nf 304//296 350//296 78//296\nf 351//297 353//297 304//297\nf 351//297 304//297 78//297\nf 78//298 350//298 348//298\nf 78//298 348//298 305//298\nf 349//299 351//299 78//299\nf 349//299 78//299 305//299\nf 305//300 348//300 328//300\nf 305//300 328//300 309//300\nf 329//301 349//301 305//301\nf 329//301 305//301 309//301\nf 326//302 328//302 348//302\nf 326//302 348//302 342//302\nf 349//303 329//303 327//303\nf 349//303 327//303 343//303\nf 296//304 298//304 318//304\nf 296//304 318//304 310//304\nf 319//305 299//305 297//305\nf 319//305 297//305 311//305\nf 76//306 316//306 324//306\nf 76//306 324//306 77//306\nf 325//307 317//307 76//307\nf 325//307 76//307 77//307\nf 302//308 358//308 356//308\nf 302//308 356//308 303//308\nf 357//309 359//309 302//309\nf 357//309 302//309 303//309\nf 303//310 356//310 354//310\nf 303//310 354//310 75//310\nf 355//311 357//311 303//311\nf 355//311 303//311 75//311\nf 75//312 354//312 316//312\nf 75//312 316//312 76//312\nf 317//313 355//313 75//313\nf 317//313 75//313 76//313\nf 292//314 294//314 362//314\nf 292//314 362//314 364//314\nf 363//315 295//315 293//315\nf 363//315 293//315 365//315\nf 364//316 362//316 368//316\nf 364//316 368//316 366//316\nf 369//317 363//317 365//317\nf 369//317 365//317 367//317\nf 366//318 368//318 370//318\nf 366//318 370//318 372//318\nf 371//319 369//319 367//319\nf 371//319 367//319 373//319\nf 372//320 370//320 376//320\nf 372//320 376//320 374//320\nf 377//321 371//321 373//321\nf 377//321 373//321 375//321\nf 314//322 378//322 374//322\nf 314//322 374//322 376//322\nf 375//323 379//323 315//323\nf 375//323 315//323 377//323\nf 316//324 354//324 374//324\nf 316//324 374//324 378//324\nf 375//325 355//325 317//325\nf 375//325 317//325 379//325\nf 354//326 356//326 372//326\nf 354//326 372//326 374//326\nf 373//327 357//327 355//327\nf 373//327 355//327 375//327\nf 356//328 358//328 366//328\nf 356//328 366//328 372//328\nf 367//329 359//329 357//329\nf 367//329 357//329 373//329\nf 358//330 360//330 364//330\nf 358//330 364//330 366//330\nf 365//331 361//331 359//331\nf 365//331 359//331 367//331\nf 290//332 292//332 364//332\nf 290//332 364//332 360//332\nf 365//333 293//333 291//333\nf 365//333 291//333 361//333\nf 74//334 360//334 358//334\nf 74//334 358//334 302//334\nf 359//335 361//335 74//335\nf 359//335 74//335 302//335\nf 284//336 286//336 288//336\nf 284//336 288//336 290//336\nf 289//337 287//337 285//337\nf 289//337 285//337 291//337\nf 284//338 290//338 360//338\nf 284//338 360//338 74//338\nf 361//339 291//339 285//339\nf 361//339 285//339 74//339\nf 73//340 284//340 74//340\nf 74//341 285//341 73//341\nf 294//342 296//342 310//342\nf 294//342 310//342 362//342\nf 311//343 297//343 295//343\nf 311//343 295//343 363//343\nf 310//344 312//344 368//344\nf 310//344 368//344 362//344\nf 369//345 313//345 311//345\nf 369//345 311//345 363//345\nf 312//346 382//346 370//346\nf 312//346 370//346 368//346\nf 371//347 383//347 313//347\nf 371//347 313//347 369//347\nf 314//348 376//348 370//348\nf 314//348 370//348 382//348\nf 371//349 377//349 315//349\nf 371//349 315//349 383//349\nf 348//350 350//350 386//350\nf 348//350 386//350 384//350\nf 387//351 351//351 349//351\nf 387//351 349//351 385//351\nf 318//352 384//352 386//352\nf 318//352 386//352 320//352\nf 387//353 385//353 319//353\nf 387//353 319//353 321//353\nf 298//354 300//354 384//354\nf 298//354 384//354 318//354\nf 385//355 301//355 299//355\nf 385//355 299//355 319//355\nf 300//356 344//356 342//356\nf 300//356 342//356 384//356\nf 343//357 345//357 301//357\nf 343//357 301//357 385//357\nf 342//358 348//358 384//358\nf 385//359 349//359 343//359\nf 300//360 346//360 344//360\nf 345//361 347//361 301//361\nf 314//362 322//362 380//362\nf 314//362 380//362 378//362\nf 381//363 323//363 315//363\nf 381//363 315//363 379//363\nf 316//364 378//364 380//364\nf 316//364 380//364 324//364\nf 381//365 379//365 317//365\nf 381//365 317//365 325//365\nf 320//366 386//366 380//366\nf 320//366 380//366 322//366\nf 381//367 387//367 321//367\nf 381//367 321//367 323//367\nf 350//368 352//368 380//368\nf 350//368 380//368 386//368\nf 381//369 353//369 351//369\nf 381//369 351//369 387//369\nf 324//370 380//370 352//370\nf 353//371 381//371 325//371\nf 400//372 388//372 414//372\nf 400//372 414//372 402//372\nf 415//373 389//373 401//373\nf 415//373 401//373 403//373\nf 400//374 402//374 404//374\nf 400//374 404//374 398//374\nf 405//375 403//375 401//375\nf 405//375 401//375 399//375\nf 398//376 404//376 406//376\nf 398//376 406//376 396//376\nf 407//377 405//377 399//377\nf 407//377 399//377 397//377\nf 396//378 406//378 408//378\nf 396//378 408//378 394//378\nf 409//379 407//379 397//379\nf 409//379 397//379 395//379\nf 394//380 408//380 410//380\nf 394//380 410//380 392//380\nf 411//381 409//381 395//381\nf 411//381 395//381 393//381\nf 392//382 410//382 412//382\nf 392//382 412//382 390//382\nf 413//383 411//383 393//383\nf 413//383 393//383 391//383\nf 410//384 420//384 418//384\nf 410//384 418//384 412//384\nf 419//385 421//385 411//385\nf 419//385 411//385 413//385\nf 408//386 422//386 420//386\nf 408//386 420//386 410//386\nf 421//387 423//387 409//387\nf 421//387 409//387 411//387\nf 406//388 424//388 422//388\nf 406//388 422//388 408//388\nf 423//389 425//389 407//389\nf 423//389 407//389 409//389\nf 404//390 426//390 424//390\nf 404//390 424//390 406//390\nf 425//391 427//391 405//391\nf 425//391 405//391 407//391\nf 402//392 428//392 426//392\nf 402//392 426//392 404//392\nf 427//393 429//393 403//393\nf 427//393 403//393 405//393\nf 402//394 414//394 416//394\nf 402//394 416//394 428//394\nf 417//395 415//395 403//395\nf 417//395 403//395 429//395\nf 318//396 320//396 444//396\nf 318//396 444//396 442//396\nf 445//397 321//397 319//397\nf 445//397 319//397 443//397\nf 320//398 390//398 412//398\nf 320//398 412//398 444//398\nf 413//399 391//399 321//399\nf 413//399 321//399 445//399\nf 310//400 318//400 442//400\nf 310//400 442//400 312//400\nf 443//401 319//401 311//401\nf 443//401 311//401 313//401\nf 382//402 430//402 414//402\nf 382//402 414//402 388//402\nf 415//403 431//403 383//403\nf 415//403 383//403 389//403\nf 412//404 418//404 440//404\nf 412//404 440//404 444//404\nf 441//405 419//405 413//405\nf 441//405 413//405 445//405\nf 438//406 446//406 444//406\nf 438//406 444//406 440//406\nf 445//407 447//407 439//407\nf 445//407 439//407 441//407\nf 434//408 446//408 438//408\nf 434//408 438//408 436//408\nf 439//409 447//409 435//409\nf 439//409 435//409 437//409\nf 432//410 448//410 446//410\nf 432//410 446//410 434//410\nf 447//411 449//411 433//411\nf 447//411 433//411 435//411\nf 430//412 448//412 432//412\nf 430//412 432//412 450//412\nf 433//413 449//413 431//413\nf 433//413 431//413 451//413\nf 414//414 430//414 450//414\nf 414//414 450//414 416//414\nf 451//415 431//415 415//415\nf 451//415 415//415 417//415\nf 312//416 448//416 430//416\nf 312//416 430//416 382//416\nf 431//417 449//417 313//417\nf 431//417 313//417 383//417\nf 312//418 442//418 446//418\nf 312//418 446//418 448//418\nf 447//419 443//419 313//419\nf 447//419 313//419 449//419\nf 442//420 444//420 446//420\nf 447//421 445//421 443//421\nf 416//422 450//422 452//422\nf 416//422 452//422 476//422\nf 453//423 451//423 417//423\nf 453//423 417//423 477//423\nf 450//424 432//424 462//424\nf 450//424 462//424 452//424\nf 463//425 433//425 451//425\nf 463//425 451//425 453//425\nf 432//426 434//426 460//426\nf 432//426 460//426 462//426\nf 461//427 435//427 433//427\nf 461//427 433//427 463//427\nf 434//428 436//428 458//428\nf 434//428 458//428 460//428\nf 459//429 437//429 435//429\nf 459//429 435//429 461//429\nf 436//430 438//430 456//430\nf 436//430 456//430 458//430\nf 457//431 439//431 437//431\nf 457//431 437//431 459//431\nf 438//432 440//432 454//432\nf 438//432 454//432 456//432\nf 455//433 441//433 439//433\nf 455//433 439//433 457//433\nf 440//434 418//434 474//434\nf 440//434 474//434 454//434\nf 475//435 419//435 441//435\nf 475//435 441//435 455//435\nf 428//436 416//436 476//436\nf 428//436 476//436 464//436\nf 477//437 417//437 429//437\nf 477//437 429//437 465//437\nf 426//438 428//438 464//438\nf 426//438 464//438 466//438\nf 465//439 429//439 427//439\nf 465//439 427//439 467//439\nf 424//440 426//440 466//440\nf 424//440 466//440 468//440\nf 467//441 427//441 425//441\nf 467//441 425//441 469//441\nf 422//442 424//442 468//442\nf 422//442 468//442 470//442\nf 469//443 425//443 423//443\nf 469//443 423//443 471//443\nf 420//444 422//444 470//444\nf 420//444 470//444 472//444\nf 471//445 423//445 421//445\nf 471//445 421//445 473//445\nf 418//446 420//446 472//446\nf 418//446 472//446 474//446\nf 473//447 421//447 419//447\nf 473//447 419//447 475//447\nf 458//448 456//448 480//448\nf 458//448 480//448 478//448\nf 481//449 457//449 459//449\nf 481//449 459//449 479//449\nf 478//450 480//450 482//450\nf 478//450 482//450 484//450\nf 483//451 481//451 479//451\nf 483//451 479//451 485//451\nf 484//452 482//452 488//452\nf 484//452 488//452 486//452\nf 489//453 483//453 485//453\nf 489//453 485//453 487//453\nf 486//454 488//454 490//454\nf 486//454 490//454 492//454\nf 491//455 489//455 487//455\nf 491//455 487//455 493//455\nf 464//456 476//456 486//456\nf 464//456 486//456 492//456\nf 487//457 477//457 465//457\nf 487//457 465//457 493//457\nf 452//458 484//458 486//458\nf 452//458 486//458 476//458\nf 487//459 485//459 453//459\nf 487//459 453//459 477//459\nf 452//460 462//460 478//460\nf 452//460 478//460 484//460\nf 479//461 463//461 453//461\nf 479//461 453//461 485//461\nf 458//462 478//462 462//462\nf 458//462 462//462 460//462\nf 463//463 479//463 459//463\nf 463//463 459//463 461//463\nf 454//464 474//464 480//464\nf 454//464 480//464 456//464\nf 481//465 475//465 455//465\nf 481//465 455//465 457//465\nf 472//466 482//466 480//466\nf 472//466 480//466 474//466\nf 481//467 483//467 473//467\nf 481//467 473//467 475//467\nf 470//468 488//468 482//468\nf 470//468 482//468 472//468\nf 483//469 489//469 471//469\nf 483//469 471//469 473//469\nf 468//470 490//470 488//470\nf 468//470 488//470 470//470\nf 489//471 491//471 469//471\nf 489//471 469//471 471//471\nf 466//472 492//472 490//472\nf 466//472 490//472 468//472\nf 491//473 493//473 467//473\nf 491//473 467//473 469//473\nf 464//474 492//474 466//474\nf 467//475 493//475 465//475\nf 392//476 390//476 504//476\nf 392//476 504//476 502//476\nf 505//477 391//477 393//477\nf 505//477 393//477 503//477\nf 394//478 392//478 502//478\nf 394//478 502//478 500//478\nf 503//479 393//479 395//479\nf 503//479 395//479 501//479\nf 396//480 394//480 500//480\nf 396//480 500//480 498//480\nf 501//481 395//481 397//481\nf 501//481 397//481 499//481\nf 398//482 396//482 498//482\nf 398//482 498//482 496//482\nf 499//483 397//483 399//483\nf 499//483 399//483 497//483\nf 400//484 398//484 496//484\nf 400//484 496//484 494//484\nf 497//485 399//485 401//485\nf 497//485 401//485 495//485\nf 388//486 400//486 494//486\nf 388//486 494//486 506//486\nf 495//487 401//487 389//487\nf 495//487 389//487 507//487\nf 494//488 502//488 504//488\nf 494//488 504//488 506//488\nf 505//489 503//489 495//489\nf 505//489 495//489 507//489\nf 494//490 496//490 500//490\nf 494//490 500//490 502//490\nf 501//491 497//491 495//491\nf 501//491 495//491 503//491\nf 496//492 498//492 500//492\nf 501//493 499//493 497//493\nf 314//494 382//494 388//494\nf 314//494 388//494 506//494\nf 389//495 383//495 315//495\nf 389//495 315//495 507//495\nf 314//496 506//496 504//496\nf 314//496 504//496 322//496\nf 505//497 507//497 315//497\nf 505//497 315//497 323//497\nf 320//498 322//498 504//498\nf 320//498 504//498 390//498\nf 505//499 323//499 321//499\nf 505//499 321//499 391//499\n");var
aQ=[e,c("Failure"),-3],bp=[e,c("Invalid_argument"),-4],aT=[e,c("Not_found"),-7],cF=[e,c("Match_failure"),-8],cE=[e,c("Stack_overflow"),-9],q=[e,c("Assert_failure"),-11],cG=[e,c("Undefined_recursive_module"),-12],by=c('File "%s", line %d, characters %d-%d: %s'),bV=c("monkey.model");L(11,cG);L(8,cE);L(7,cF);L(6,aT);L(5,[e,c("Division_by_zero"),-6]);L(4,[e,c("End_of_file"),-5]);L(3,bp);L(2,aQ);L(1,[e,c("Sys_error"),-2]);bW(c("Pervasives.array_bound_error"),[0,bp,c(cc)]);var
eF=[e,c("Out_of_memory"),-1],dT=c(dn),dS=c(am),dQ=c("true"),dR=c("false"),dP=c("Pervasives.Exit"),dU=c("Pervasives.do_at_exit"),dW=c("Array.Bottom"),dZ=c("\\b"),d0=c("\\t"),d1=c("\\n"),d2=c("\\r"),dY=c("\\\\"),dX=c("\\'"),d5=c(d),d4=c("String.blit"),d3=c("String.sub"),d6=c("Sys.Break"),d7=c("Queue.Empty"),d9=c("CamlinternalLazy.Undefined"),d_=c("Buffer.add: cannot grow buffer"),eo=c(d),ep=c(d),es=c(dn),et=c(dm),eu=c(dm),eq=c(bc),er=c(bc),en=c(dp),el=c("neg_infinity"),em=c("infinity"),ek=c(am),ej=c("printf: bad positional specification (0)."),ei=c("%_"),eh=[0,c("printf.ml"),143,8],ef=c(bc),eg=c("Printf: premature end of format string '"),eb=c(bc),ec=c(" in format string '"),ed=c(", at char number "),ee=c("Printf: bad conversion %"),d$=c("Sformat.index_of_int: negative argument "),ez=c(d),eA=c(", %s%s"),eR=[1,1],eS=c("%s\n"),eT=c("(Program not linked with -g, cannot print stack backtrace)\n"),eL=c("Raised at"),eO=c("Re-raised at"),eP=c("Raised by primitive operation at"),eQ=c("Called from"),eM=c('%s file "%s", line %d, characters %d-%d'),eN=c("%s unknown location"),eG=c("Out of memory"),eH=c("Stack overflow"),eI=c("Pattern matching failed"),eJ=c("Assertion failed"),eK=c("Undefined recursive module"),eB=c("(%s%s)"),eC=c(d),eD=c(d),eE=c("(%s)"),ey=c(dk),ew=c("%S"),ex=c("_"),eV=c("Lwt_sequence.Empty"),e9=[0,c(U),692,20],e_=[0,c(U),695,8],e7=[0,c(U),670,20],e8=[0,c(U),673,8],e5=[0,c(U),648,20],e6=[0,c(U),651,8],e2=[0,c(U),498,8],e1=[0,c(U),487,9],e0=c("Lwt.wakeup_result"),eX=c("Fatal error: exception "),eW=c("Lwt.Canceled"),e3=[0,0],fd=c("Js.Error"),fe=c(dl),fl=c("script"),fj=c(b5),fg=c("Dom_html.Canvas_not_available"),fp=c("browser can't read file: unimplemented"),fo=[0,c("file.ml"),131,15],fm=c("can't retrieve file name: not implemented"),fr=c("Exception during Lwt.async: "),fs=[0,c("regexp.ml"),35,64],fu=c("[\\][()\\\\|+*.?{}^$]"),fH=[0,c(d),0],fI=c(d),fV=c(d),f4=c(d),fW=c(bd),fX=c(a_),f3=c(d),fY=c(T),fZ=c(T),f2=c(df),f0=c(d),f1=c("http://"),f5=c(d),gc=c(d),f6=c(bd),f7=c(a_),gb=c(d),f8=c(T),f9=c(T),ga=c(df),f_=c(d),f$=c("https://"),gd=c(d),gj=c(d),ge=c(bd),gf=c(a_),gi=c(d),gg=c(T),gh=c("file://"),fU=c(d),fT=c(d),fS=c(d),fR=c(d),fQ=c(d),fP=c(d),fJ=c(b8),fK=c(dw),fB=c("file"),fC=c("file:"),fD=c("http"),fE=c("http:"),fF=c("https"),fG=c("https:"),fy=c("%2B"),fw=c("Url.Local_exn"),fx=c(aI),fz=c("Url.Not_an_http_protocol"),fL=c("^([Hh][Tt][Tt][Pp][Ss]?)://([0-9a-zA-Z.-]+|\\[[0-9a-zA-Z.-]+\\]|\\[[0-9A-Fa-f:.]+\\])?(:([0-9]+))?/([^\\?#]*)(\\?([^#]*))?(#(.*))?$"),fN=c("^([Ff][Ii][Ll][Ee])://([^\\?#]*)(\\?([^#]*))?(#(.*))?$"),gA=c(bl),gC=c("multipart/form-data; boundary="),gD=c(bl),gE=[0,c(bl),[0,c("application/x-www-form-urlencoded")],bk],gF=[0,c(bl),0,bk],gG=c("GET"),gB=c(a_),gv=c(b8),gw=c(b8),gx=c(dw),gr=c('"; filename="'),gs=c(dr),gp=c(bf),gq=c(dh),gt=c('"\r\n\r\n'),gu=c(dr),gn=c("--\r\n"),go=c(dh),gm=c("js_of_ocaml-------------------"),gl=[0,c("xmlHttpRequest.ml"),85,2],gy=c("XmlHttpRequest.Wrong_headers"),g_=c("uncaught exception: %s"),g9=c("fetching model"),g8=c("%.1f"),g0=c("init canvas"),g1=c(b5),g2=c("create program"),g3=c("fragment-shader"),g4=c("vertex-shader"),g5=c("use program"),g6=c("program loaded"),g7=c("ready"),gX=c("\n"),gT=[0,1,[0,2,[0,3,[0,4,0]]]],gU=c(a$),gV=c("v"),gW=c("vn"),gS=[0,1,[0,2,0]],gM=c("can't find script element %s"),gL=c("Unable to link the shader program."),gK=c("An error occurred compiling the shaders: \n%s\n%s"),gJ=c("can't initialise webgl context"),gI=c("can't find canvas element %s"),gH=c("WebGL error"),gN=c("(v|vn|f)\\ ([^\\ ]+)\\ ([^\\ ]+)\\ ([^\\ ]+)"),gP=c("([0-9]+)//([0-9]+)");function
X(a){throw[0,aQ,a]}function
ap(a){throw[0,bp,a]}a([e,dP,0]);function
h(a,b){var
c=a.getLen(),e=b.getLen(),d=M(c+e|0);S(a,0,d,0,c);S(b,0,d,c,e);return d}function
ag(a){return c(d+a)}function
cj(a,b){if(a){var
c=a[1];return[0,c,cj(a[2],b)]}return b}hE(0);c$(1);var
aq=c$(2);function
ck(a,b){return dI(a,b,0,b.getLen())}function
cl(a){return ck(aq,a)}function
bq(a){var
b=hG(0);for(;;){if(b){var
c=b[2],d=b[1];try{da(d)}catch(f){}var
b=c;continue}return 0}}bW(dU,bq);function
dV(a,b){return db(a,b)}function
cm(a){return da(a)}function
aR(a,b){if(0===a)return[0];var
d=N(a,i(b,0)),e=a-1|0,f=1;if(!(e<1)){var
c=f;for(;;){d[c+1]=i(b,c);var
g=c+1|0;if(e!==c){var
c=g;continue}break}}return d}function
aS(a){if(a){var
d=0,c=a,g=a[2],h=a[1];for(;;){if(c){var
d=d+1|0,c=c[2];continue}var
f=N(d,h),e=1,b=g;for(;;){if(b){var
i=b[2];f[e+1]=b[1];var
e=e+1|0,b=i;continue}return f}}}return[0]}a([e,dW,0]);function
ah(a){var
b=a,c=0;for(;;){if(b){var
d=[0,b[1],c],b=b[2],c=d;continue}return c}}function
H(a,b){if(b){var
c=b[2],d=i(a,b[1]);return[0,d,H(a,c)]}return 0}function
ar(a,b){var
c=b;for(;;){if(c){var
d=c[2];i(a,c[1]);var
c=d;continue}return 0}}function
as(a,b){var
c=M(a);hf(c,0,a,b);return c}function
Y(a,b,c){if(0<=b)if(0<=c)if(!((a.getLen()-c|0)<b)){var
d=M(c);S(a,b,d,0,c);return d}return ap(d3)}function
br(a,b,c,d,e){if(0<=e)if(0<=b)if(!((a.getLen()-e|0)<b))if(0<=d)if(!((c.getLen()-e|0)<d))return S(a,b,c,d,e);return ap(d4)}function
at(d,b){if(b){var
a=b[1],g=[0,0],f=[0,0],h=b[2];ar(function(a){g[1]++;f[1]=f[1]+a.getLen()|0;return 0},b);var
e=M(f[1]+dc(d.getLen(),g[1]-1|0)|0);S(a,0,e,0,a.getLen());var
c=[0,a.getLen()];ar(function(a){S(d,0,e,c[1],d.getLen());c[1]=c[1]+d.getLen()|0;S(a,0,e,c[1],a.getLen());c[1]=c[1]+a.getLen()|0;return 0},h);return e}return d5}var
bs=hX(0),au=dc(bs/8|0,(1<<(bs-10|0))-1|0)-1|0;a([e,d6,0]);var
d8=a([e,d7,0]);function
cn(a){return[0,0,0]}function
co(a){if(0===a[1])throw d8;a[1]=a[1]-1|0;var
b=a[2],c=b[2];if(c===b)a[2]=0;else
b[2]=c[2];return c[1]}function
cp(a){return a[1]}a([e,d9,0]);function
bt(a){var
b=1<=a?a:1,c=au<b?au:b,d=M(c);return[0,d,0,c,d]}function
bu(a){return Y(a[1],0,a[2])}function
cq(a,b){var
c=[0,a[3]];for(;;){if(c[1]<(a[2]+b|0)){c[1]=2*c[1]|0;continue}if(au<c[1])if((a[2]+b|0)<=au)c[1]=au;else
X(d_);var
d=M(c[1]);br(a[1],0,d,0,a[2]);a[1]=d;a[3]=c[1];return 0}}function
av(a,b){var
c=a[2];if(a[3]<=c)cq(a,1);a[1].safeSet(c,b);a[2]=c+1|0;return 0}function
bv(a,b){var
c=b.getLen(),d=a[2]+c|0;if(a[3]<d)cq(a,c);S(b,0,a[1],a[2],c);a[2]=d;return 0}function
bw(a){return 0<=a?a:X(h(d$,ag(a)))}function
cr(a,b){return bw(a+b|0)}var
ea=1;function
cs(a){return cr(ea,a)}function
ct(a){return Y(a,0,a.getLen())}function
cu(a,b,c){var
d=h(ec,h(a,eb)),e=h(ed,h(ag(b),d));return ap(h(ee,h(as(1,c),e)))}function
aw(a,b,c){return cu(ct(a),b,c)}function
aU(a){return ap(h(eg,h(ct(a),ef)))}function
Z(e,b,c,d){function
h(a){if((e.safeGet(a)+al|0)<0||9<(e.safeGet(a)+al|0))return a;var
b=a+1|0;for(;;){var
c=e.safeGet(b);if(48<=c){if(!(58<=c)){var
b=b+1|0;continue}}else
if(36===c)return b+1|0;return a}}var
i=h(b+1|0),f=bt((c-i|0)+10|0);av(f,37);var
a=i,g=ah(d);for(;;){if(a<=c){var
j=e.safeGet(a);if(42===j){if(g){var
k=g[2];bv(f,ag(g[1]));var
a=h(a+1|0),g=k;continue}throw[0,q,eh]}av(f,j);var
a=a+1|0;continue}return bu(f)}}function
cv(a,b,c,d,e){var
f=Z(b,c,d,e);if(78!==a)if(aK!==a)return f;f.safeSet(f.getLen()-1|0,b6);return f}function
cw(a){return function(d,b){var
k=d.getLen();function
l(a,b){var
m=40===a?41:b3,c=b;for(;;){if(k<=c)return aU(d);if(37===d.safeGet(c)){var
e=c+1|0;if(k<=e)return aU(d);var
f=d.safeGet(e),g=f-40|0;if(g<0||1<g){var
i=g-83|0;if(i<0||2<i)var
h=1;else
switch(i){case
1:var
h=1;break;case
2:var
j=1,h=0;break;default:var
j=0,h=0}if(h){var
c=e+1|0;continue}}else
var
j=0===g?0:1;if(j)return f===m?e+1|0:aw(d,b,f);var
c=l(f,e+1|0)+1|0;continue}var
c=c+1|0;continue}}return l(a,b)}}function
cx(i,b,c){var
m=i.getLen()-1|0;function
r(a){var
k=a;a:for(;;){if(k<m){if(37===i.safeGet(k)){var
f=0,h=k+1|0;for(;;){if(m<h)var
e=aU(i);else{var
n=i.safeGet(h);if(58<=n){if(95===n){var
f=1,h=h+1|0;continue}}else
if(32<=n)switch(n+dq|0){case
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
h=s(b,f,h,ae);continue;default:var
h=h+1|0;continue}var
d=h;b:for(;;){if(m<d)var
e=aU(i);else{var
j=i.safeGet(d);if(126<=j)var
g=0;else
switch(j){case
78:case
88:case
bi:case
ae:case
b$:case
b6:case
b7:var
e=s(b,f,d,ae),g=1;break;case
69:case
70:case
71:case
dt:case
cb:case
ca:var
e=s(b,f,d,cb),g=1;break;case
33:case
37:case
44:case
64:var
e=d+1|0,g=1;break;case
83:case
91:case
aL:var
e=s(b,f,d,aL),g=1;break;case
97:case
be:case
b1:var
e=s(b,f,d,j),g=1;break;case
76:case
dx:case
aK:var
t=d+1|0;if(m<t)var
e=s(b,f,d,ae),g=1;else{var
p=i.safeGet(t)+du|0;if(p<0||32<p)var
q=1;else
switch(p){case
0:case
12:case
17:case
23:case
29:case
32:var
e=l(c,s(b,f,d,j),ae),g=1,q=0;break;default:var
q=1}if(q)var
e=s(b,f,d,ae),g=1}break;case
67:case
99:var
e=s(b,f,d,99),g=1;break;case
66:case
98:var
e=s(b,f,d,66),g=1;break;case
41:case
b3:var
e=s(b,f,d,j),g=1;break;case
40:var
e=r(s(b,f,d,j)),g=1;break;case
b9:var
u=s(b,f,d,j),v=l(cw(j),i,u),o=u;for(;;){if(o<(v-2|0)){var
o=l(c,o,i.safeGet(o));continue}var
d=v-1|0;continue b}default:var
g=0}if(!g)var
e=aw(i,d,j)}break}}var
k=e;continue a}}var
k=k+1|0;continue}return k}}r(0);return 0}function
cy(a){var
d=[0,0,0,0];function
b(a,b,c){var
f=41!==c?1:0,g=f?b3!==c?1:0:f;if(g){var
e=97===c?2:1;if(be===c)d[3]=d[3]+1|0;if(a)d[2]=d[2]+e|0;else
d[1]=d[1]+e|0}return b+1|0}cx(a,b,function(a,b){return a+1|0});return d[1]}function
cz(a,b,c){var
g=a.safeGet(c);if((g+al|0)<0||9<(g+al|0))return l(b,0,c);var
e=g+al|0,d=c+1|0;for(;;){var
f=a.safeGet(d);if(48<=f){if(!(58<=f)){var
e=(10*e|0)+(f+al|0)|0,d=d+1|0;continue}}else
if(36===f)return 0===e?X(ej):l(b,[0,bw(e-1|0)],d+1|0);return l(b,0,c)}}function
r(a,b){return a?b:cs(b)}function
cA(a,b){return a?a[1]:b}function
cB(aH,b,c,d,e,f,g){var
B=i(b,g);function
af(a){return l(d,B,a)}function
aI(a,b,j,aJ){var
o=j.getLen();function
C(q,b){var
n=b;for(;;){if(o<=n)return i(a,B);var
d=j.safeGet(n);if(37===d){var
m=function(a,b){return k(aJ,cA(a,b))},ar=function(g,f,c,d){var
a=d;for(;;){var
$=j.safeGet(a)+dq|0;if(!($<0||25<$))switch($){case
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
10:return cz(j,function(a,b){var
d=[0,m(a,f),c];return ar(g,r(a,f),d,b)},a+1|0);default:var
a=a+1|0;continue}var
o=j.safeGet(a);if(!(124<=o))switch(o){case
78:case
88:case
bi:case
ae:case
b$:case
b6:case
b7:var
a7=m(g,f),a8=a4(cv(o,j,n,a,c),a7);return p(r(g,f),a8,a+1|0);case
69:case
71:case
dt:case
cb:case
ca:var
aY=m(g,f),aZ=bX(Z(j,n,a,c),aY);return p(r(g,f),aZ,a+1|0);case
76:case
dx:case
aK:var
ac=j.safeGet(a+1|0)+du|0;if(!(ac<0||32<ac))switch(ac){case
0:case
12:case
17:case
23:case
29:case
32:var
R=a+1|0,ad=o-108|0;if(ad<0||2<ad)var
ag=0;else{switch(ad){case
1:var
ag=0,ah=0;break;case
2:var
a6=m(g,f),aA=a4(Z(j,n,R,c),a6),ah=1;break;default:var
a3=m(g,f),aA=a4(Z(j,n,R,c),a3),ah=1}if(ah)var
az=aA,ag=1}if(!ag)var
a2=m(g,f),az=hm(Z(j,n,R,c),a2);return p(r(g,f),az,R+1|0)}var
a0=m(g,f),a1=a4(cv(aK,j,n,a,c),a0);return p(r(g,f),a1,a+1|0);case
37:case
64:return p(f,as(1,o),a+1|0);case
83:case
aL:var
w=m(g,f);if(aL===o)var
x=w;else{var
b=[0,0],al=w.getLen()-1|0,aM=0;if(!(al<0)){var
J=aM;for(;;){var
v=w.safeGet(J),bc=14<=v?34===v?1:92===v?1:0:11<=v?13<=v?1:0:8<=v?1:0,aP=bc?2:bY(v)?1:4;b[1]=b[1]+aP|0;var
aR=J+1|0;if(al!==J){var
J=aR;continue}break}}if(b[1]===w.getLen())var
aC=w;else{var
k=M(b[1]);b[1]=0;var
am=w.getLen()-1|0,aN=0;if(!(am<0)){var
I=aN;for(;;){var
u=w.safeGet(I),y=u-34|0;if(y<0||58<y)if(-20<=y)var
S=1;else{switch(y+34|0){case
8:k.safeSet(b[1],92);b[1]++;k.safeSet(b[1],98);var
H=1;break;case
9:k.safeSet(b[1],92);b[1]++;k.safeSet(b[1],b1);var
H=1;break;case
10:k.safeSet(b[1],92);b[1]++;k.safeSet(b[1],aK);var
H=1;break;case
13:k.safeSet(b[1],92);b[1]++;k.safeSet(b[1],be);var
H=1;break;default:var
S=1,H=0}if(H)var
S=0}else
var
S=(y-1|0)<0||56<(y-1|0)?(k.safeSet(b[1],92),b[1]++,k.safeSet(b[1],u),0):1;if(S)if(bY(u))k.safeSet(b[1],u);else{k.safeSet(b[1],92);b[1]++;k.safeSet(b[1],48+(u/bi|0)|0);b[1]++;k.safeSet(b[1],48+((u/10|0)%10|0)|0);b[1]++;k.safeSet(b[1],48+(u%10|0)|0)}b[1]++;var
aO=I+1|0;if(am!==I){var
I=aO;continue}break}}var
aC=k}var
x=h(eu,h(aC,et))}if(a===(n+1|0))var
aB=x;else{var
G=Z(j,n,a,c);try{var
T=0,s=1;for(;;){if(G.getLen()<=s)var
an=[0,0,T];else{var
U=G.safeGet(s);if(49<=U)if(58<=U)var
ai=0;else
var
an=[0,a5(Y(G,s,(G.getLen()-s|0)-1|0)),T],ai=1;else{if(45===U){var
T=1,s=s+1|0;continue}var
ai=0}if(!ai){var
s=s+1|0;continue}}var
W=an;break}}catch(f){f=z(f);if(f[1]!==aQ)throw f;var
W=cu(G,0,aL)}var
K=W[1],A=x.getLen(),aS=W[2],L=0,aT=32;if(K===A)if(0===L)var
X=x,aj=1;else
var
aj=0;else
var
aj=0;if(!aj)if(K<=A)var
X=Y(x,L,A);else{var
V=as(K,aT);if(aS)br(x,L,V,0,A);else
br(x,L,V,K-A|0,A);var
X=V}var
aB=X}return p(r(g,f),aB,a+1|0);case
67:case
99:var
q=m(g,f);if(99===o)var
ax=as(1,q);else{if(39===q)var
t=dX;else
if(92===q)var
t=dY;else{if(14<=q)var
D=0;else
switch(q){case
8:var
t=dZ,D=1;break;case
9:var
t=d0,D=1;break;case
10:var
t=d1,D=1;break;case
13:var
t=d2,D=1;break;default:var
D=0}if(!D)if(bY(q)){var
ak=M(1);ak.safeSet(0,q);var
t=ak}else{var
E=M(4);E.safeSet(0,92);E.safeSet(1,48+(q/bi|0)|0);E.safeSet(2,48+((q/10|0)%10|0)|0);E.safeSet(3,48+(q%10|0)|0);var
t=E}}var
ax=h(er,h(t,eq))}return p(r(g,f),ax,a+1|0);case
66:case
98:var
aW=a+1|0,aX=m(g,f)?dQ:dR;return p(r(g,f),aX,aW);case
40:case
b9:var
Q=m(g,f),at=l(cw(o),j,a+1|0);if(b9===o){var
N=bt(Q.getLen()),ao=function(a,b){av(N,b);return a+1|0};cx(Q,function(a,b,c){if(a)bv(N,ei);else
av(N,37);return ao(b,c)},ao);var
aU=bu(N);return p(r(g,f),aU,at)}var
au=r(g,f),bb=cr(cy(Q),au);return aI(function(a){return C(bb,at)},au,Q,aJ);case
33:i(e,B);return C(f,a+1|0);case
41:return p(f,eo,a+1|0);case
44:return p(f,ep,a+1|0);case
70:var
aa=m(g,f);if(0===c)var
ay=es;else{var
_=Z(j,n,a,c);if(70===o)_.safeSet(_.getLen()-1|0,ca);var
ay=_}var
aq=hb(aa);if(3===aq)var
ab=aa<0?el:em;else
if(4<=aq)var
ab=en;else{var
P=bX(ay,aa),O=0,aV=P.getLen();for(;;){if(aV<=O)var
ap=h(P,ek);else{var
F=P.safeGet(O)-46|0,bd=F<0||23<F?55===F?1:0:(F-1|0)<0||21<(F-1|0)?1:0;if(!bd){var
O=O+1|0;continue}var
ap=P}var
ab=ap;break}}return p(r(g,f),ab,a+1|0);case
91:return aw(j,a,o);case
97:var
aD=m(g,f),aE=cs(cA(g,f)),aF=m(0,aE),a9=a+1|0,a_=r(g,aE);if(aH)af(l(aD,0,aF));else
l(aD,B,aF);return C(a_,a9);case
be:return aw(j,a,o);case
b1:var
aG=m(g,f),a$=a+1|0,ba=r(g,f);if(aH)af(i(aG,0));else
i(aG,B);return C(ba,a$)}return aw(j,a,o)}},f=n+1|0,g=0;return cz(j,function(a,b){return ar(a,q,g,b)},f)}l(c,B,d);var
n=n+1|0;continue}}function
p(a,b,c){af(b);return C(a,c)}return C(b,0)}var
p=bw(0);function
m(a,b){return aI(f,p,a,b)}var
n=cy(g);if(n<0||6<n){var
o=function(f,b){if(n<=f){var
h=N(n,0),i=function(a,b){return j(h,(n-a|0)-1|0,b)},c=0,a=b;for(;;){if(a){var
d=a[2],e=a[1];if(d){i(c,e);var
c=c+1|0,a=d;continue}i(c,e)}return m(g,h)}}return function(a){return o(f+1|0,[0,a,b])}};return o(0,0)}switch(n){case
1:return function(a){var
b=N(1,0);j(b,0,a);return m(g,b)};case
2:return function(a,b){var
c=N(2,0);j(c,0,a);j(c,1,b);return m(g,c)};case
3:return function(a,b,c){var
d=N(3,0);j(d,0,a);j(d,1,b);j(d,2,c);return m(g,d)};case
4:return function(a,b,c,d){var
e=N(4,0);j(e,0,a);j(e,1,b);j(e,2,c);j(e,3,d);return m(g,e)};case
5:return function(a,b,c,d,e){var
f=N(5,0);j(f,0,a);j(f,1,b);j(f,2,c);j(f,3,d);j(f,4,e);return m(g,f)};case
6:return function(a,b,c,d,e,f){var
h=N(6,0);j(h,0,a);j(h,1,b);j(h,2,c);j(h,3,d);j(h,4,e);j(h,5,f);return m(g,h)};default:return m(g,[0])}}function
cC(d){function
e(a){return 0}function
b(a){return d}var
c=0;return function(a){return cB(c,b,dV,ck,cm,e,a)}}function
ev(a){return bt(2*a.getLen()|0)}function
aV(c){function
b(a){var
b=bu(a);a[2]=0;return i(c,b)}function
d(a){return 0}var
e=1;return function(a){return cB(e,ev,av,bv,d,b,a)}}function
w(a){return i(aV(function(a){return a}),a)}var
bx=[0,0];function
bz(a,b){var
c=a[b+1];if(hL(c)){if(a6(c)===252)return i(w(ew),c);if(a6(c)===253){var
e=bX(dT,c),d=0,g=e.getLen();for(;;){if(g<=d)return h(e,dS);var
f=e.safeGet(d),j=48<=f?58<=f?0:1:45===f?1:0;if(j){var
d=d+1|0;continue}return e}}return ex}return i(w(ey),c)}function
cD(a,b){if(a.length-1<=b)return ez;var
c=cD(a,b+1|0),d=bz(a,b);return l(w(eA),d,c)}function
bA(a){var
b=bx[1];for(;;){if(b){var
s=b[2],t=b[1];try{var
u=i(t,a),e=u}catch(f){var
e=0}if(e)return e[1];var
b=s;continue}if(a===eF)return eG;if(a===cE)return eH;if(a[1]===cF){var
f=a[2],k=f[3],v=f[2],x=f[1];return a7(w(by),x,v,k,k+5|0,eI)}if(a[1]===q){var
g=a[2],m=g[3],y=g[2],z=g[1];return a7(w(by),z,y,m,m+6|0,eJ)}if(a[1]===cG){var
j=a[2],n=j[3],A=j[2],B=j[1];return a7(w(by),B,A,n,n+6|0,eK)}if(0===a6(a)){var
d=a.length-1,C=a[0+1][0+1];if(d<0||2<d)var
o=cD(a,2),p=bz(a,1),c=l(w(eB),p,o);else
switch(d){case
1:var
c=eD;break;case
2:var
r=bz(a,1),c=i(w(eE),r);break;default:var
c=eC}return h(C,c)}return a[0+1]}}function
cH(a){var
h=hc(hk(0));if(h){var
d=h[1],f=d.length-1-1|0,q=0;if(!(f<0)){var
c=q;for(;;){if(dd(k(d,c),eR)){var
b=k(d,c),j=0===b[0]?b[1]:b[1],e=j?0===c?eL:eO:0===c?eP:eQ;if(0===b[0])var
m=b[5],n=b[4],o=b[3],p=b[2],g=a7(w(eM),e,p,o,n,m);else
var
g=i(w(eN),e);l(cC(a),eS,g)}var
r=c+1|0;if(f!==c){var
c=r;continue}break}}return 0}return i(cC(a),eT)}function
cI(a){bx[1]=[0,a,bx[1]];return 0}32===bs;a([e,eV,0]);function
cJ(a){var
b=[];de(b,[0,b,b]);return b}var
bB=a([e,eW,0]),x=[0,0];function
bC(a){var
c=a[1];if(3===c[0]){var
d=c[1],b=bC(d);if(b!==d)a[1]=[3,b];return b}return a}function
_(a){return bC(a)}var
cK=[0,function(a){cl(eX);cl(bA(a));db(aq,10);cH(aq);cm(aq);bq(0);return hY(2)}];function
cL(a,b){try{var
c=i(a,b)}catch(f){f=z(f);return i(cK[1],f)}return c}function
bZ(a,b,c,d){var
f=c,e=d;for(;;)if(typeof
f===u)return a<50?O(1+a,b,e):P(O,[0,b,e]);else
switch(f[0]){case
1:i(f[1],b);return a<50?O(1+a,b,e):P(O,[0,b,e]);case
2:var
h=[0,f[2],e],f=f[1],e=h;continue;default:var
g=f[1][1];if(g){i(g[1],b);return a<50?O(1+a,b,e):P(O,[0,b,e])}else
return a<50?O(1+a,b,e):P(O,[0,b,e])}}function
O(a,b,c){return c?a<50?bZ(1+a,b,c[1],c[2]):P(bZ,[0,b,c[1],c[2]]):0}function
eY(b,c,d){return a8(bZ(0,b,c,d))}function
h6(b,c){return a8(O(0,b,c))}function
b0(a,b,c){var
e=b,d=c;for(;;)if(typeof
e===u)return a<50?ab(1+a,d):P(ab,[0,d]);else
switch(e[0]){case
1:var
f=e[1];if(f[4]){f[4]=0;f[1][2]=f[2];f[2][1]=f[1]}return a<50?ab(1+a,d):P(ab,[0,d]);case
2:var
h=[0,e[2],d],e=e[1],d=h;continue;default:var
g=e[2];x[1]=e[1];cL(g,0);return a<50?ab(1+a,d):P(ab,[0,d])}}function
ab(a,b){return b?a<50?b0(1+a,b[1],b[2]):P(b0,[0,b[1],b[2]]):0}function
eZ(b,c){return a8(b0(0,b,c))}function
h7(b){return a8(ab(0,b))}function
aW(a,b){var
c=1===b[0]?b[1]===bB?(eZ(a[4],0),1):0:0;return eY(b,a[2],0)}var
bD=[0,0],cM=cn(0);function
cN(a,b){var
f=bC(a),c=f[1];switch(c[0]){case
1:if(c[1]===bB)return 0;break;case
2:var
h=c[1];f[1]=b;var
d=x[1],g=bD[1]?1:(bD[1]=1,0);aW(h,b);if(g){x[1]=d;return 0}for(;;){if(0===cM[1]){bD[1]=0;x[1]=d;return 0}var
e=co(cM);aW(e[1],e[2]);continue}}return ap(e0)}function
aX(a,b){return cN(a,[0,b])}function
cO(a,b){return typeof
a===u?b:typeof
b===u?a:[2,a,b]}function
bE(a){if(typeof
a!==u)switch(a[0]){case
2:var
b=a[1],c=bE(a[2]);return cO(bE(b),c);case
1:break;default:if(!a[1][1])return 0}return a}function
cP(a,b){var
d=_(a),g=_(b),j=d[1];if(2===j[0]){var
c=j[1];if(d===g)return 0;var
e=g[1];if(2===e[0]){var
f=e[1];g[1]=[3,d];c[1]=f[1];var
k=cO(c[2],f[2]),l=c[3]+f[3]|0;if(42<l){c[3]=0;c[2]=bE(k)}else{c[3]=l;c[2]=k}var
h=f[4],i=c[4],m=typeof
i===u?h:typeof
h===u?i:[2,i,h];c[4]=m;return 0}d[1]=e;return aW(c,e)}throw[0,q,e1]}function
aY(a,b){var
c=_(a),d=c[1];if(2===d[0]){var
e=d[1];c[1]=b;return aW(e,b)}throw[0,q,e2]}function
ax(a){return[0,[0,a]]}var
e4=[0,e3];function
bF(a){return[0,[1,a]]}function
bG(a){return[0,[2,[0,[0,[0,a]],0,0,0]]]}function
bH(a){var
b=[0,[2,[0,1,0,0,0]]];return[0,b,b]}function
bI(a,b){var
d=[1,b],c=a[2],e=typeof
c===u?d:[2,d,c];a[2]=e;return 0}function
bJ(a,b){var
c=_(a)[1];switch(c[0]){case
1:if(c[1]===bB)return cL(b,0);break;case
2:var
d=c[1],e=[0,x[1],b],f=d[4],g=typeof
f===u?e:[2,e,f];d[4]=g;return 0}return 0}function
ay(a,b){var
e=_(a),c=e[1];switch(c[0]){case
1:return[0,c];case
2:var
f=c[1],d=bG(e),g=x[1];bI(f,function(a){switch(a[0]){case
0:var
e=a[1];x[1]=g;try{var
f=i(b,e),c=f}catch(f){f=z(f);var
c=bF(f)}return cP(d,c);case
1:return aY(d,a);default:throw[0,q,e5]}});return d;case
3:throw[0,q,e6];default:return i(b,c[1])}}function
aZ(a,b){var
e=_(a),c=e[1];switch(c[0]){case
1:return[0,c];case
2:var
j=c[1],d=bG(e),k=x[1];bI(j,function(a){switch(a[0]){case
0:var
e=a[1];x[1]=k;try{var
f=[0,i(b,e)],c=f}catch(f){f=z(f);var
c=[1,f]}return aY(d,c);case
1:return aY(d,a);default:throw[0,q,e7]}});return d;case
3:throw[0,q,e8];default:var
f=c[1];try{var
h=[0,i(b,f)],g=h}catch(f){f=z(f);var
g=[1,f]}return[0,g]}}var
e$=[0,function(a){return 0}],C=cJ(0),fa=[0,0];function
fb(a){var
e=1-(C[2]===C?1:0);if(e){var
b=cJ(0);b[1][2]=C[2];C[2][1]=b[1];b[1]=C[1];C[1][2]=b;C[1]=C;C[2]=C;fa[1]=0;var
c=b[2];for(;;){var
d=c!==b?1:0;if(d){if(c[4])aX(c[3],0);var
c=c[2];continue}return d}}return e}function
cQ(c,b){if(b){var
d=b[2],a=b[1],e=function(a){return cQ(c,d)};return ay(i(c,a),e)}return e4}var
D=b,m=null,I=undefined;function
bK(a,b){return a==m?m:i(b,a)}function
a0(a,b,c){return a==m?i(b,0):i(c,a)}function
bL(a,b){return a==m?i(b,0):a}function
bM(a){function
b(a){return[0,a]}return a0(a,function(a){return 0},b)}function
az(a){return a!==I?1:0}function
bN(a,b,c){return a===I?i(b,0):i(c,a)}function
y(a,b){return a===I?i(b,0):a}function
aA(a){function
b(a){return[0,a]}return bN(a,function(a){return 0},b)}var
$=true,J=false,aB=RegExp,cR=Array;function
o(a,b){return a[b]}function
cS(a){return a}function
bO(a){return a}var
cT=Date,cU=a([e,fd,0]),bP=[0,cU,{}],fc=Math,eU=a6(bP)===e?bP:bP[0+1];bW(fe,eU);function
cV(a){return escape(a)}cI(function(a){return a[1]===cU?[0,new
v(a[2].toString())]:0});cI(function(a){return a
instanceof
cR?0:[0,new
v(a.toString())]});function
aC(a){return a}function
Q(a){return a}function
a1(d){return Q(a9(function(a){if(a){var
e=i(d,a);if(!(e|0))a.preventDefault();return e}var
c=event,b=i(d,c);if(!(b|0))c.returnValue=b;return b}))}var
cW=hA(0)|0,a2=D.document,ff=D.Float32Array;a([e,fg,0]);var
cX=D.HTMLElement,fh=aC(cX)===I?function(a){return aC(a.innerHTML)===I?m:Q(a)}:function(a){return a
instanceof
cX?Q(a):m};function
cY(a,b){var
c=a.toString();return b.tagName.toLowerCase()===c?Q(b):m}function
fi(a){return cY(fj,a)}function
fk(a){return cY(fl,a)}var
bQ=hz(0),fn=D.FileReader,fq=b2;e$[1]=function(a){return 1===a?(D.setTimeout(a9(fb),0),0):0};function
cZ(a){return bQ.log(a.toString())}cK[1]=function(a){cZ(fr);cZ(bA(a));return cH(aq)};function
aD(a){return new
aB(g(a),aH)}function
c0(a,b,c){a.lastIndex=c;var
d=a.exec(g(b)),e=d==m?m:bO(d);return bM(e)}function
c1(a,b){var
c=o(a,b),d=c===I?I:E(c);return aA(d)}var
ft=new
aB("[$]",aH),fv=aD(fu);function
c3(a,b){return cS(b.split(as(1,a).toString()))}var
c4=a([e,fw,0]);function
aa(a){throw c4}var
c2=aD(E(g(fx).replace(fv,"\\$&"))),c5=new
aB("\\+",aH);function
K(a){c5[c(b_)]=0;return E(unescape(a.replace(c5,F)))}function
p(a,b){var
e=a?a[1]:1;if(e){var
f=E(cV(g(b)));c2[c(b_)]=0;var
d=g(f);return E(d.replace(c2,g(fy).replace(ft,"$$$$")))}return E(cV(g(b)))}var
fA=a([e,fz,0]);function
aE(a){try{var
c=a.getLen();if(0===c)var
d=fH;else{var
b=0,g=47,f=a.getLen();for(;;){if(f<=b)throw aT;if(a.safeGet(b)!==g){var
b=b+1|0;continue}if(0===b)var
e=[0,fI,aE(Y(a,1,c-1|0))];else
var
h=aE(Y(a,b+1|0,(c-b|0)-1|0)),e=[0,Y(a,0,b),h];var
d=e;break}}}catch(f){f=z(f);if(f===aT)return[0,a,0];throw f}return d}function
a3(a){return at(fK,H(function(a){var
b=a[1],c=h(fJ,p(0,a[2]));return h(p(0,b),c)},a))}function
bR(a){var
d=c3(38,a),b=d.length;function
e(a,b){var
c=b;for(;;){if(0<=c){try{var
f=c-1|0,g=function(a){function
e(a){var
c=a[2],d=a[1];function
b(a){return K(y(a,aa))}var
e=b(c);return[0,b(d),e]}var
b=c3(61,a);if(2===b.length)var
d=o(b,1),c=aC([0,o(b,0),d]);else
var
c=I;return bN(c,aa,e)},h=e([0,bN(o(d,c),aa,g),a],f)}catch(f){f=z(f);if(f===c4){var
c=c-1|0;continue}throw f}return h}return a}}return e(0,b-1|0)}var
fM=new
aB(g(fL)),fO=new
aB(g(fN));function
c6(a){switch(a[0]){case
1:var
c=a[1],i=c[6],j=c[5],k=c[2],x=c[3],y=c[1],z=t(i,f5)?h(f6,p(0,i)):gc,A=j?h(f7,a3(j)):gb,B=h(A,z),C=h(f9,h(at(f8,H(function(a){return p(0,a)},x)),B)),D=dj===k?f_:h(ga,ag(k)),E=h(D,C);return h(f$,h(p(0,y),E));case
2:var
d=a[1],l=d[4],m=d[3],F=d[1],G=t(l,gd)?h(ge,p(0,l)):gj,I=m?h(gf,a3(m)):gi,J=h(I,G);return h(gh,h(at(gg,H(function(a){return p(0,a)},F)),J));default:var
b=a[1],e=b[6],f=b[5],g=b[2],n=b[3],o=b[1],q=t(e,fV)?h(fW,p(0,e)):f4,r=f?h(fX,a3(f)):f3,s=h(r,q),u=h(fZ,h(at(fY,H(function(a){return p(0,a)},n)),s)),v=80===g?f0:h(f2,ag(g)),w=h(v,u);return h(f1,h(p(0,o),w))}}var
aF=location;K(aF.hostname);K(aF.protocol);try{}catch(f){f=z(f);if(f[1]!==aQ)throw f}aE(K(aF.pathname));bR(aF.search);K(aF.href);var
gk=D.FormData;function
c7(a,b){if(bj<=a[1]){var
d=a[2];d[1]=[0,b,d[1]];return 0}var
e=a[2],c=b[2],f=b[1];return ba<=c[1]?e.append(f.toString(),c[2]):e.append(f.toString(),c[2])}function
bS(a){return ActiveXObject}({"alpha":$,"depth":$,"stencil":J,"antialias":$,"premultipliedAlpha":J,"preserveDrawingBuffer":J,"preferLowPowerToHighPerformance":J,"failIfMajorPerformanceCaveat":J});var
gz=a([e,gy,0]);function
ai(a){return i(aV(function(a){bQ.error(a.toString());return X(a)}),a)}function
aj(a){return i(aV(function(a){return bQ.log(a.toString())}),a)}function
bT(a){var
b=a.NO_ERROR;return dd(a.getError(),b)?ai(gH):0}function
c8(a,b,c){a.shaderSource(b,c);a.compileShader(b);if(a.getShaderParameter(b,a.COMPILE_STATUS)|0)return 0;var
d=new
v(a.getShaderInfoLog(b)),e=new
v(c);return l(ai(gK),e,d)}function
c9(b){function
a(a){return i(ai(gM),b)}return bL(bK(a2.getElementById(b.toString()),fk),a).text}function
aG(a){var
d=new
ff(a.length-1),c=a.length-1-1|0,e=0;if(!(c<0)){var
b=e;for(;;){d[b]=a[b+1];var
f=b+1|0;if(c!==b){var
b=f;continue}break}}return d}function
R(a,b){return(a*4|0)+b|0}function
bU(e,b){return aR(16,function(a){var
c=a%4|0,d=a/4|0,f=k(b,R(3,c)),g=k(e,R(d,3))*f,h=k(b,R(2,c)),i=k(e,R(d,2))*h,j=k(b,R(1,c)),l=k(e,R(d,1))*j,m=k(b,R(0,c));return k(e,R(d,0))*m+l+i+g})}var
gO=aD(gN),gQ=aD(gP);function
gR(a){var
d=c0(gQ,a,0);if(d){var
g=d[1],b=H(function(a){return c1(g,a)},gS);if(b){var
e=b[1];if(e){var
c=b[2];if(c){var
f=c[1];if(f)if(!c[2])return[0,[0,a5(e[1]),a5(f[1])]]}}}return 0}return 0}function
c_(e){var
b=[0,0],g=e.length-1-1|0,i=0;if(!(g<0)){var
a=i;a:for(;;){var
d=0,c=e[a+1],l=b[1];for(;;){if(c){var
d=d+1|0,c=c[2];continue}b[1]=l+d|0;var
m=a+1|0;if(g!==a){var
a=m;continue a}break}break}}var
h=[0,-1],f=[0,0],j=b[1];return aR(j,function(a){for(;;){var
b=f[1];if(b){var
c=b[1];f[1]=b[2];return c}h[1]++;f[1]=k(e,h[1]);continue}})}var
gY=4*0.785398163397448279;function
gZ(a){var
p=a[1],y=a[2],q=a2.createTextNode("loading"),l=bK(a2.getElementById("fps"),fh);if(l!=m)l.appendChild(q);aj(g0);function
u(a){return i(ai(gI),g1)}var
g=bL(bK(a2.getElementById(b5),fi),u);function
v(a){return i(aV(function(a){D.alert(a.toString());return X(a)}),gJ)}try{var
f=g.getContext("webgl"),x=1-(f==m?1:0)?f:g.getContext("experimental-webgl"),h=x}catch(f){var
h=m}var
b=bL(h,v);aj(g2);var
z=c9(g3),A=c9(g4),j=b.createShader(b.VERTEX_SHADER),k=b.createShader(b.FRAGMENT_SHADER);c8(b,j,A);c8(b,k,z);var
d=b.createProgram();b.attachShader(d,j);b.attachShader(d,k);b.linkProgram(d);if(!(b.getProgramParameter(d,b.LINK_STATUS)|0))ai(gL);aj(g5);b.useProgram(d);bT(b);aj(g6);b.enable(b.DEPTH_TEST);b.depthFunc(b.LESS);var
B=b.getUniformLocation(d,"u_proj"),C=b.getUniformLocation(d,"u_lightPos"),E=b.getUniformLocation(d,"u_ambientLight"),F=aG([V,3,0,-1]),G=aG([V,b4,b4,b4]);b.uniform3fv(C,F);b.uniform3fv(E,G);var
n=b.getAttribLocation(d,"a_position");b.enableVertexAttribArray(n);var
H=b.createBuffer();b.bindBuffer(b.ARRAY_BUFFER,H);b.bufferData(b.ARRAY_BUFFER,p,b.STATIC_DRAW);b.vertexAttribPointer(n,3,b.FLOAT,J,0,0);var
o=b.getAttribLocation(d,"a_normal");b.enableVertexAttribArray(o);var
I=b.createBuffer();b.bindBuffer(b.ARRAY_BUFFER,I);b.bufferData(b.ARRAY_BUFFER,y,b.STATIC_DRAW);b.vertexAttribPointer(o,3,b.FLOAT,J,0,0);var
e=gY/2,K=bU([V,1,0,0,0,0,Math.cos(e),Math.sin(e),0,0,-Math.sin(e),Math.cos(e),0,0,0,0,1],bU([V,ce,0,0,0,0,ce,0,0,0,0,ce,0,0,0,0,1],[V,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]));bT(b);aj(g7);function
r(a){return new
cT().getTime()}var
s=[0,r(0)],c=cn(0);function
t(a){var
e=1*(new
cT().getTime()/bh);b.uniformMatrix4fv(B,J,aG(bU(K,[V,Math.cos(e),0,-Math.sin(e),0,0,1,0,0,Math.sin(e),0,Math.cos(e),0,0,0,0,1])));b.clear(b.DEPTH_BUFFER_BIT|b.COLOR_BUFFER_BIT);b.drawArrays(b.TRIANGLES,0,p.length/3|0);bT(b);var
u=r(0),v=u-s[1];if(0===c[1]){var
f=[];de(f,[0,v,f]);c[1]=1;c[2]=f}else{var
g=c[2],h=[0,v,g[2]];c[1]=c[1]+1|0;g[2]=h;c[2]=h}s[1]=u;if(50<cp(c))co(c);var
C=cp(c),x=0;if(0===c[1])var
z=x;else{var
j=c[2],k=x,d=j[2];for(;;){var
y=k+d[1];if(d!==j){var
k=y,d=d[2];continue}var
z=y;break}}q.data=i(w(g8),1/z*C*bh).toString();var
l=bH(0),m=l[1],n=[0,0],E=0.02,A=l[2];function
o(a,b){var
c=b2<a?[0,fq,a-b2]:[0,a,0],d=c[2],e=c[1],f=d==0?function(a){return aX(A,a)}:function(a){return o(d,a)};n[1]=[0,D.setTimeout(a9(f),e*bh)];return 0}o(E,0);bJ(m,function(a){var
b=n[1];return b?D.clearTimeout(b[1]):0});return ay(m,t)}return t(0)}D.onload=a1(function(a){aj(g9);function
aV(a){var
b=bA(a);return i(ai(g_),b)}try{var
bz=function(a){var
ad=aD(gX);ad[c(b_)]=0;var
Q=cS(g(a).split(ad)),m=0,i=Q.length-1|0;for(;;){if(0<=i){var
aj=i-1|0,ai=function(a){throw[0,q,fs]},m=[0,E(y(o(Q,i),ai)),m],i=aj;continue}var
ae=aS(m),G=[0,0],I=[0,0],J=[0,0],P=ae.length-1-1|0,af=0;if(!(P<0)){var
h=af;for(;;){var
R=c0(gO,ae[h+1],0);if(R){var
n=R[1],f=H(function(n){return function(a){return c1(n,a)}}(n),gT);if(f){var
S=f[1];if(S){var
p=S[1];if(t(p,gU))if(t(p,gV))if(t(p,gW))var
b=0;else{var
r=f[2];if(r){var
T=r[1];if(T){var
s=r[2];if(s){var
U=s[1];if(U){var
u=s[2];if(u){var
V=u[1];if(V)if(u[2])var
b=0;else
var
e=[0,[1,[0,ak(T[1]),ak(U[1]),ak(V[1])]]],b=1;else
var
b=0}else
var
b=0}else
var
b=0}else
var
b=0}else
var
b=0}else
var
b=0}else{var
v=f[2];if(v){var
W=v[1];if(W){var
w=v[2];if(w){var
X=w[1];if(X){var
x=w[2];if(x){var
Y=x[1];if(Y)if(x[2])var
b=0;else
var
e=[0,[0,[0,ak(W[1]),ak(X[1]),ak(Y[1])]]],b=1;else
var
b=0}else
var
b=0}else
var
b=0}else
var
b=0}else
var
b=0}else
var
b=0}else{var
z=f[2];if(z){var
Z=z[1];if(Z){var
A=z[2];if(A){var
_=A[1];if(_){var
B=A[2];if(B){var
$=B[1];if($)if(B[2])var
b=0;else{var
C=H(gR,[0,Z[1],[0,_[1],[0,$[1],0]]]);if(C){var
aa=C[1];if(aa){var
D=C[2];if(D){var
ab=D[1];if(ab){var
F=D[2];if(F){var
ac=F[1];if(ac)if(F[2])var
d=1;else
var
e=[0,[2,[0,aa[1],ab[1],ac[1]]]],b=1,d=0;else
var
d=1}else
var
d=1}else
var
d=1}else
var
d=1}else
var
d=1}else
var
d=1;if(d)var
e=0,b=1}else
var
b=0}else
var
b=0}else
var
b=0}else
var
b=0}else
var
b=0}else
var
b=0}}else
var
b=0}else
var
b=0;if(!b)var
e=0}else
var
e=0;if(e){var
j=e[1];switch(j[0]){case
1:var
L=j[1];I[1]=[0,[0,L[1],L[2],L[3]],I[1]];break;case
2:var
M=j[1];J[1]=[0,[0,M[1],M[2],M[3]],J[1]];break;default:var
K=j[1];G[1]=[0,[0,K[1],K[2],K[3]],G[1]]}}var
ag=h+1|0;if(P!==h){var
h=ag;continue}break}}var
l=aS(ah(J[1])),N=aS(ah(I[1])),O=aS(ah(G[1])),al=aR(l.length-1,function(a){var
b=k(l,a),c=k(O,b[1][1]-1|0),d=k(O,b[2][1]-1|0),e=k(O,b[3][1]-1|0);return[0,c[1],[0,c[2],[0,c[3],[0,d[1],[0,d[2],[0,d[3],[0,e[1],[0,e[2],[0,e[3],0]]]]]]]]]}),am=aR(l.length-1,function(a){var
b=k(l,a),c=k(N,b[1][2]-1|0),d=k(N,b[2][2]-1|0),e=k(N,b[3][2]-1|0);return[0,c[1],[0,c[2],[0,c[3],[0,d[1],[0,d[2],[0,d[3],[0,e[1],[0,e[2],[0,e[3],0]]]]]]]]]}),an=aG(c_(al));return[0,an,aG(c_(am))]}};try{var
by=ax(hj(bV)),aU=by}catch(f){f=z(f);if(f!==aT)throw f;var
bx=function(a){return a},bw=function(a){var
b=a[2],c=a[4];if(0!==b)if(200!==b)return[0,[2,[0,0,0,0,0]]];return ax(c)},aL=0,aM=0,aN=0,aO=0,aP=0,aQ=0,w=0,N=0,bv=0,bi=0?bv[1]:0,bl=aQ?aQ[1]:0,bm=aO?aO[1]:function(a,b){return 1};if(aP){var
ae=aP[1];if(w){var
bn=w[1];ar(function(a){return c7(ae,[0,a[1],a[2]])},bn)}var
j=[0,ae]}else
if(w){var
bu=w[1],V=aA(aC(gk)),aK=V?[0,808620462,new(V[1])()]:[0,bj,[0,0]];ar(function(a){return c7(aK,[0,a[1],a[2]])},bu);var
j=[0,aK]}else
var
j=0;if(j){var
af=j[1];if(N)var
al=[0,gA,N,bk];else{if(bj<=af[1]){var
C=0,B=0,n=af[2][1];for(;;){if(n){var
O=n[2],D=n[1],aW=ba<=D[2][1]?0:1;if(aW){var
C=[0,D,C],n=O;continue}var
B=[0,D,B],n=O;continue}var
a2=ah(B);ah(C);if(a2)var
W=function(a){return ag(fc.random()*1e9|0)},bd=W(0),Y=h(gm,h(W(0),bd)),aI=[0,gD,[0,h(gC,Y)],[0,164354597,Y]];else
var
aI=gE;var
aJ=aI;break}}else
var
aJ=gF;var
al=aJ}var
r=al}else
var
r=[0,gG,N,bk];var
am=r[3],an=r[2],U=g(bV),bo=r[1],a7=function(a){var
c=bO(a),b=E(y(o(c,1),aa).toLowerCase());if(t(b,fB))if(t(b,fC)){if(t(b,fD))if(t(b,fE)){if(t(b,fF))if(t(b,fG))var
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
h=0;break;case
2:var
h=1;break;default:var
e=0,h=1}if(h){var
i=K(y(o(c,5),aa)),l=function(a){return g(fQ)},m=K(y(o(c,9),l)),n=function(a){return g(fR)},p=bR(y(o(c,7),n)),q=aE(i),r=function(a){return g(fS)},j=E(y(o(c,4),r)),s=t(j,fP)?a5(j):e?dj:80,k=[0,K(y(o(c,2),aa)),s,q,i,p,m],u=e?[1,k]:[0,k];return[0,u]}}throw fA},a8=function(a){function
b(a){var
b=bO(a),c=K(y(o(b,2),aa));function
d(a){return g(fT)}var
e=E(y(o(b,6),d));function
f(a){return g(fU)}var
h=bR(y(o(b,4),f));return[0,[2,[0,aE(c),c,h,e]]]}function
c(a){return 0}return a0(fO.exec(U),c,b)},T=a0(fM.exec(U),a8,a7);if(T){var
F=T[1];switch(F[0]){case
1:var
ac=F[1],ad=ac.slice(),bh=ac[5];ad[5]=0;var
s=[0,c6([1,ad]),bh],A=1;break;case
2:var
A=0;break;default:var
Z=F[1],ab=Z.slice(),bg=Z[5];ab[5]=0;var
s=[0,c6([0,ab]),bg],A=1}}else
var
A=0;if(!A)var
s=[0,bV,0];var
ao=s[1],ap=cj(s[2],bl),aq=ap?h(ao,h(gB,a3(ap))):ao,as=bH(0),au=as[2],av=as[1];try{var
bc=new
XMLHttpRequest(),b=bc}catch(f){try{var
bb=new(bS(0))("Msxml2.XMLHTTP"),b=bb}catch(f){try{var
a$=new(bS(0))("Msxml3.XMLHTTP"),b=a$}catch(f){try{var
a_=new(bS(0))("Microsoft.XMLHTTP")}catch(f){throw[0,q,gl]}var
b=a_}}}if(aL)b.overrideMimeType(aL[1].toString());b.open(bo.toString(),aq.toString(),$);if(an)b.setRequestHeader("Content-type",an[1].toString());ar(function(a){return b.setRequestHeader(a[1].toString(),a[2].toString())},bi);var
G=function(a){function
c(a){return[0,new
v(a)]}function
d(a){return 0}return a0(b.getResponseHeader(g(a)),d,c)},aw=[0,0],L=function(a){var
c=aw[1]?0:l(bm,b.status,G)?0:(cN(au,[1,[0,gz,[0,b.status,G]]]),b.abort(),1);aw[1]=1;return 0};b.onreadystatechange=a9(function(a){switch(b.readyState){case
2:if(!cW)return L(0);break;case
3:if(cW)return L(0);break;case
4:L(0);var
c=function(a){var
c=bM(b.responseXML);if(c){var
d=c[1];return Q(d.documentElement)===m?0:[0,d]}return 0};return aX(au,[0,aq,b.status,G,new
v(b.responseText),c])}return 0});if(aN){var
bp=aN[1];b.onprogress=a1(function(a){l(bp,a.loaded,a.total);return $})}var
aB=b.upload;if(aB!==I)if(aM){var
bq=aM[1];aB.onprogress=a1(function(a){l(bq,a.loaded,a.total);return $})}if(j){var
M=j[1];if(bj<=M[1]){var
aF=M[2];if(typeof
am===u){var
br=aF[1];b.send(Q(at(gx,H(function(a){var
b=a[2],c=a[1];if(ba<=b[1]){var
d=h(gv,p(0,new
v(b[2].name)));return h(p(0,c),d)}var
e=h(gw,p(0,new
v(b[2])));return h(p(0,c),e)},br)).toString()))}else{var
aH=am[2],bs=function(a){var
c=Q(a.join(d));return az(b.sendAsBinary)?b.sendAsBinary(c):b.send(c)},bt=aF[1],e=new
cR(),be=function(a){e.push(h(go,h(aH,gn)).toString());return e};aZ(aZ(cQ(function(a){e.push(h(gq,h(aH,gp)).toString());var
g=a[2],n=a[1];if(ba<=g[1]){var
b=g[2],s=function(a){var
c=aA(b.name),g="Content-Type: application/octet-stream\r\n",i='"\r\n';if(c)var
f=c[1];else
var
d=aA(b.fileName),f=d?d[1]:X(fm);e.push(h(gs,h(n,gr)).toString(),f,i,g);e.push(bf,a,bf);return ax(0)},k=aA(aC(fn)),d=-1041425454;if(k){var
c=new(k[1])(),i=bH(0),j=i[1],p=i[2];c.onloadend=a1(function(a){if(2===c.readyState){var
b=c.result,e=he(typeof
b,"string")?Q(b):m,d=bM(e);if(!d)throw[0,q,fo];aX(p,d[1])}return J});bJ(j,function(a){return c.abort()});if(typeof
d===u)if(di===d)c.readAsDataURL(b);else
if(dg<=d)c.readAsText(b);else
c.readAsBinaryString(b);else
c.readAsText(b,d[2]);var
o=j}else{var
f=function(a){return X(fp)};if(typeof
d===u)var
l=di===d?az(b.getAsDataURL)?b.getAsDataURL():f(0):dg<=d?az(b.getAsText)?b.getAsText("utf8"):f(0):az(b.getAsBinary)?b.getAsBinary():f(0);else
var
r=d[2],l=az(b.getAsText)?b.getAsText(r):f(0);var
o=ax(l)}return ay(o,s)}var
t=g[2];e.push(h(gu,h(n,gt)).toString(),t,bf);return ax(0)},bt),be),bs)}}else
b.send(M[2])}else
b.send(m);bJ(av,function(a){return b.abort()});var
aU=aZ(ay(av,bw),bx)}var
bB=ay(aZ(aU,bz),gZ),P=bB}catch(f){f=z(f);var
P=bF(f)}var
R=_(P),f=R[1];switch(f[0]){case
1:aV(f[1]);break;case
2:var
a4=f[1],S=bG(R),a6=x[1];bI(a4,function(a){switch(a[0]){case
0:return aY(S,a);case
1:var
c=a[1];x[1]=a6;try{var
d=aV(c),b=d}catch(f){f=z(f);var
b=bF(f)}return cP(S,b);default:throw[0,q,e9]}});break;case
3:throw[0,q,e_]}return $});bq(0);return}(this));
